;;; jee-bounds.el --- Bounds of Janet things -*- lexical-binding: t; -*-

;;; Commentary:

;; Deducing bounds of and selecting Janet things

;; Prerequisites

;; 1. Some janet major mode
;;
;; 2. Janet (needed to execute indentation code in external process)

;;; Code:

(require 'jee-top-level)

(defvar jee-bounds--helper-path
  (expand-file-name
   (concat (expand-file-name
	    (file-name-directory (or load-file-name
				     buffer-file-name)))
	   "janet-editor-elf/"
           "janet-bounds/"
           "janet-bounds/"
           "bounds.janet"))
  "Path to helper program to calculate bounds.")

(defvar jee-bounds--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar jee-bounds--temp-files
  '()
  "List of files to delete when Emacs exits.")

(defvar jee-bounds--temp-buffers
  '()
  "List of buffers to clean up before executing `jee-bounds--helper'.")

(defun jee-bounds--helper (start end line col)
  "Determine bounds of thing at point by asking Janet.

A region bounded by START and END with position LINE, COL is sent to a
helper program."
  (interactive "r")
  (let ((send-buffer (generate-new-buffer "*jee-bounds-send*"))
        (recv-buffer (generate-new-buffer "*jee-bounds-recv*"))
        (err-file (if (not jee-bounds--debug-output)
                      nil
                    ;; arrangements for deletion are near end of this file
                    (let ((file-path (make-temp-file "jee-bounds-err")))
                      (add-to-list 'jee-bounds--temp-files file-path)
                      file-path))))
    ;; clean up any old buffers
    ;; XXX: assumes all previous calls have completed before this call
    (dolist (old-buffer jee-bounds--temp-buffers)
      (kill-buffer old-buffer))
    (add-to-list 'jee-bounds--temp-buffers send-buffer)
    (add-to-list 'jee-bounds--temp-buffers recv-buffer)
    ;;
    (condition-case err
        (let ((result nil)
              (region-string (buffer-substring-no-properties start end))
              (line-offset (1- (line-number-at-pos start))))
          (save-excursion
            (when jee-bounds--debug-output
              (message "error file: %S" err-file)
              (message "region: %S" region-string)
              (message "line-offset: %S" line-offset))
            ;; prepare content to send
            (set-buffer send-buffer)
            ;; XXX: extend to work with a region?
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
                                        jee-bounds--helper-path)))
              (if (not (zerop exit-code))
                  (progn
                    (message "exit-code: %s" exit-code)
                    (error "Non-zero exit"))
                ;; retrieve content
                (set-buffer recv-buffer)
                (setq result
                      (buffer-substring-no-properties (point-min) (point-max)))
                (when jee-bounds--debug-output
                  (message "jee-bounds: %S" result))
                ;; interpret result
                (cond ((string-match
                        ;;  start line   start col    end line     end col
                        "^\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$"
                        result)
                       ;; coordinate conversions
                       (list line-offset
                             (1- (string-to-number (match-string 1 result)))
                             (1- (string-to-number (match-string 2 result)))
                             (1- (string-to-number (match-string 3 result)))
                             (1- (string-to-number (match-string 4 result)))))
                      ;; failed to determine bounds
                      ((string-match "^-1\\|-2$" result)
                       (message "failed to determine bounds")
                       nil)
                      (t
                       (message "result: <<%s>>" result)
                       (error "Unexpected result")))))))
      (error
       (message "Error: %s %s" (car err) (cdr err))
       nil))))

(defun jee-bounds--calculate ()
  "Calculate bounds for Janet thing at point."
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
        (jee-bounds--helper start end cur-line cur-col)))))

(defun jee-bounds-select-around-point ()
  "Select appropriately around point."
  (interactive)
  (when-let ((result (jee-bounds--calculate)))
    (let ((offset (nth 0 result))
          (start-line (nth 1 result))
          (start-col (nth 2 result))
          (end-line (nth 3 result))
          (end-col (nth 4 result)))
      ;; start
      (goto-char (point-min))
      (forward-line offset)
      (forward-line start-line)
      (forward-char start-col)
      (set-mark (point))
      ;; end
      (goto-char (point-min))
      (forward-line offset)
      (forward-line end-line)
      (forward-char end-col)
      (activate-mark))))

;; clean up temp files
(add-hook 'kill-emacs-hook
          (lambda ()
            (dolist (file-path jee-bounds--temp-files)
              (delete-file file-path 'trash))))

(provide 'jee-bounds)
;;; jee-bounds.el ends here
