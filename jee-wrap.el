;;; jee-wrap.el --- Janet wrap convenience -*- lexical-binding: t; -*-

;;; Commentary:

;; Conveniently using / dis-using wrap

;; Prerequisites

;; 1. Some janet major mode
;;
;; 2. Janet (needed to execute indentation code in external process)

;;; Code:

(require 'jee-bounds)
(require 'jee-top-level)

(defvar jee-wrap--helper-path
  (expand-file-name
   (concat (expand-file-name
	    (file-name-directory (or load-file-name
				     buffer-file-name)))
	   "janet-editor-elf/wrap.janet"))
  "Path to helper program to query form bounds.")

(defvar jee-wrap--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar jee-wrap--temp-files
  '()
  "List of files to delete when Emacs exits.")

(defvar jee-wrap--temp-buffers
  '()
  "List of buffers to clean up before executing `jee-wrap--helper'.")

(defun jee-wrap--helper (start end line col name)
  "Determine bounds for tuple with head symbol NAME near point.

A region bounded by START and END with position LINE, COL is sent to a
helper program."
  (interactive "r")
  (let ((send-buffer (generate-new-buffer "*jee-wrap-send*"))
        (recv-buffer (generate-new-buffer "*jee-wrap-recv*"))
        (err-file (if (not jee-wrap--debug-output)
                      nil
                    ;; arrangements for deletion are near end of this file
                    (let ((file-path (make-temp-file "jee-wrap-err")))
                      (add-to-list 'jee-wrap--temp-files file-path)
                      file-path))))
    ;; clean up any old buffers
    ;; XXX: assumes all previous calls have completed before this invocation
    (dolist (old-buffer jee-wrap--temp-buffers)
      (kill-buffer old-buffer))
    (add-to-list 'jee-wrap--temp-buffers send-buffer)
    (add-to-list 'jee-wrap--temp-buffers recv-buffer)
    ;;
    (condition-case err
        (let ((result nil)
              (region-string (buffer-substring-no-properties start end))
              (line-offset (1- (line-number-at-pos start))))
          (save-excursion
            (when jee-wrap--debug-output
              (message "error file: %S" err-file)
              (message "region: %S" region-string)
              (message "line-offset: %S" line-offset))
            ;; prepare content to send
            (set-buffer send-buffer)
            ;; XXX: extend to work with a region?
            (insert (format "Name: %s\n" name))
            (insert (format "Line: %d\n" (1+ line)))  ;; 0- vs 1-based
            (insert (format "Column: %d\n" (1+ col))) ;; 0- vs 1-based
            (insert "\n")
            (insert region-string)
            ;; send content
            ;; https://emacs.stackexchange.com/a/54353
            (let ((exit-code
                   (call-process-region (point-min) (point-max)
                                        "janet"
                                        nil `(,recv-buffer ,err-file) nil
                                        jee-wrap--helper-path)))
              (if (not (zerop exit-code))
                  (progn
                    (message "exit-code: %s" exit-code)
                    (error "Non-zero exit"))
                ;; retrieve content
                (set-buffer recv-buffer)
                (setq result
                      (buffer-substring-no-properties (point-min) (point-max)))
                (when jee-wrap--debug-output
                  (message "jee-wrap: %S" result))
                ;; interpret result
                (cond ((string-match
                        (concat "^"
                                ;; start line   start col
                                "\\([0-9]+\\) \\([0-9]+\\) "
                                ;; end line     end col
                                "\\([0-9]+\\) \\([0-9]+\\) "
                                ;; start line   start col
                                "\\([0-9]+\\) \\([0-9]+\\) "
                                ;; end line     end col
                                "\\([0-9]+\\) \\([0-9]+\\)"
                                "$")
                        result)
                       ;; coordinate conversions
                       (list line-offset
                             ;; form tuple bounds
                             (1- (string-to-number (match-string 1 result)))
                             (1- (string-to-number (match-string 2 result)))
                             (1- (string-to-number (match-string 3 result)))
                             (1- (string-to-number (match-string 4 result)))
                             ;; contained form bounds
                             (1- (string-to-number (match-string 5 result)))
                             (1- (string-to-number (match-string 6 result)))
                             (1- (string-to-number (match-string 7 result)))
                             (1- (string-to-number (match-string 8 result)))))
                      ;; failed to determine form bounds
                      ((string-match "^-1\\|-2$" result)
                       (message "failed to determine bounds")
                       nil)
                      (t
                       (message "result: <<%s>>" result)
                       (error "Unexpected result")))))))
      (error
       (message "Error: %s %s" (car err) (cdr err))
       ;; XXX: could do this conditionally based on `jee-wrap--debug-output`
       (kill-buffer send-buffer)
       (kill-buffer recv-buffer)
       nil))))

(defun jee-wrap--calculate (name)
  "Calculate form bounds for tuple with head symbol NAME near point."
  (save-excursion
    (let ((current (point))
          (abs-line (line-number-at-pos))
          (cur-line nil)
          (cur-col (current-column))
          (start nil)
          (end nil))
      (save-excursion
        ;; find the start of the region
        ;; XXX
        ;;(forward-line -1)
        (beginning-of-line)
        (jee-top-level--start-of)
        (setq start (point))
        (setq cur-line
              (- abs-line (line-number-at-pos)))
        ;; find the end of the region
        (goto-char current)
        (jee-top-level--before-next)
        ;; XXX: edge case?
        ;;
        ;;      (import ...)
        ;;      (import ...)
        ;;
        ;;      starting on first line, first char
        (when (equal (point) start)
          (end-of-line))
        (setq end (point))
        ;; ask the helper to try to determine bounds
        (jee-wrap--helper start end cur-line cur-col name)))))

(defun jee-wrap--wrap-with (name)
  "Wrap with call to NAME."
  (interactive)
  (when-let ((result (jee-bounds--calculate)))
    (let ((offset (nth 0 result))
          (start-line (nth 1 result))
          (start-col (nth 2 result))
          (end-line (nth 3 result))
          (end-col (nth 4 result))
          (beg nil)
          (end nil))
      ;; start
      (goto-char (point-min))
      (forward-line offset)
      (forward-line start-line)
      (forward-char start-col)
      (setq beg (point))
      ;; end
      (goto-char (point-min))
      (forward-line offset)
      (forward-line end-line)
      (forward-char end-col)
      (setq end (point))
      ;; replace
      (kill-region beg end)
      (insert (concat "(" name " "))
      (yank)
      (insert ")"))))

(defun jee-wrap--unwrap (name)
  "Remove wrapping for NAME."
  (when-let ((result (jee-wrap--calculate name)))
    (let ((offset (nth 0 result))
          ;;
          (t-start-line (nth 1 result))
          (t-start-col (nth 2 result))
          (t-start nil)
          (t-end-line (nth 3 result))
          (t-end-col (nth 4 result))
          (t-end nil)
          ;;
          (i-start-line (nth 5 result))
          (i-start-col (nth 6 result))
          (i-start nil)
          (i-end-line (nth 7 result))
          (i-end-col (nth 8 result))
          (i-end nil))
      ;; find bounds of form
      ;; start
      (goto-char (point-min))
      (forward-line offset)
      (forward-line t-start-line)
      (forward-char t-start-col)
      (setq t-start (point))
      ;; end
      (goto-char (point-min))
      (forward-line offset)
      (forward-line t-end-line)
      (forward-char t-end-col)
      (setq t-end (point))
      ;; find bounds of contained form
      ;; start
      (goto-char (point-min))
      (forward-line offset)
      (forward-line i-start-line)
      (forward-char i-start-col)
      (setq i-start (point))
      ;; end
      (goto-char (point-min))
      (forward-line offset)
      (forward-line i-end-line)
      (forward-char i-end-col)
      (setq i-end (point))
      ;; copy
      (kill-ring-save i-start i-end)
      ;; delete
      (kill-region t-start t-end)
      ;; insert
      (yank 2))))

;; XXX: figure out
(defun jee-wrap--remove-from-top-level (name)
  "Unwrap each call to NAME from a top level construct."
  (interactive)
  )

;; clean up temp files
(add-hook 'kill-emacs-hook
          (lambda ()
            (dolist (file-path jee-wrap--temp-files)
              (delete-file file-path 'trash))))

(provide 'jee-wrap)
;;; jee-wrap.el ends here
