;;; jee-indent-line.el --- Janet line indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Indentation

;; Prerequisites

;; 1. Some janet major mode
;;
;; 2. Janet (needed to execute indentation code in external process)

;; Setup

;; 0. Clone the repository this file is in.  (Note: there should be
;;    a janet file in the same repository.  It is used for computing
;;    indentation, so the current elisp file is not sufficient on its
;;    own.)
;;
;; 1. Add directory containing this elisp file to `load-path'
;;
;; 2. Add following sort of thing to .emacs-equivalent:
;;
;;    (add-hook 'janet-mode-hook
;;              (lambda ()
;;                (require 'jee-indent-line)
;;                (setq-local indent-line-function
;;                            #'jee-indent-line)))

;;; Code:

(require 'jee-top-level)

(defvar jee-indent-line--helper-path
  (expand-file-name
   (concat (expand-file-name
	    (file-name-directory (or load-file-name
				     buffer-file-name)))
	   "janet-editor-elf/indent-line.janet"))
  "Path to helper program to calculate indentation for a line.")

(defvar jee-indent-line--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar jee-indent-line--temp-buffers
  '()
  "List of buffers to clean up before executing `jee-indent-line--helper'.")

(defun jee-indent-line--helper (start end)
  "Determine indentation of current line by asking Janet.

A region bounded by START and END is sent to a helper program."
  (let ((temp-buffer (generate-new-buffer "*jee-indent*")))
    ;; clean up any old buffers
    ;; XXX: assumes all previous calls have completed before this invocation
    (dolist (old-buffer jee-indent-line--temp-buffers)
      (kill-buffer old-buffer))
    (add-to-list 'jee-indent-line--temp-buffers temp-buffer)
    ;;
    (condition-case err
        (let ((result nil))
          (save-excursion
            (when jee-indent-line--debug-output
              (message "region: %S"
                       (buffer-substring-no-properties start end)))
            ;; https://emacs.stackexchange.com/a/54353
            (let ((exit-code
                   (call-process-region start end
                                        "janet"
                                        nil `(,temp-buffer nil) nil
                                        jee-indent-line--helper-path)))
              (if (not (zerop exit-code))
                  (progn
                    (message "exit-code: %s" exit-code)
                    (error "Non-zero exit"))
                (set-buffer temp-buffer)
                (setq result
                      (buffer-substring-no-properties (point-min) (point-max)))
                (when jee-indent-line--debug-output
                  (message "jee-indent: %S" result))
                (cond ((string-match "^[0-9]+$" result)
                       (string-to-number result))
                      ;; indenter says to leave things alone
                      ((string-match "^-1$" result)
                       nil)
                      (t
                       (message "result: <<%s>>" result)
                       (error "Unexpected result")))))))
      (error
       (message "Error: %s %s" (car err) (cdr err))
       nil))))

(defun jee-indent-line--calculate ()
  "Calculate indentation for the current line of Janet code."
  (save-excursion
    (let ((cur-indent nil)
          (start nil)
          (end nil))
      (save-excursion
        (beginning-of-line)
        (if (bobp)
            0
          ;; remember in case no indentation is calculated
          (setq cur-indent (current-indentation))
          ;; find the end of the region
          (end-of-line)
          (setq end (point))
          ;; find the start of the region
          (forward-line -1)
          (jee-top-level--start-of)
          (setq start (point))
          ;; ask the helper to try to determine an indentation
          (or (jee-indent-line--helper start end)
              cur-indent))))))

;; for lisp-indent-function
(defun jee-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

Ignores INDENT-POINT and STATE and just uses `jee-indent-line--calculate'."
  ;; quiet the linter
  (ignore state)
  (ignore indent-point)
  ;; now for the real work
  (jee-indent-line--calculate))

;; XXX: consider using `indent-line-to`
(defun jee-indent-line ()
  "Indent current line as Janet code."
  (interactive)
  (when-let ((result (jee-indent-line--calculate)))
    ;; remember the cursor position relative to the end of the buffer
    (let ((pos (- (point-max) (point)))
          (bol nil))
      ;; find the first non-whitespace character on the line
      (beginning-of-line)
      (setq bol (point))
      (skip-chars-forward " \t")
      ;; only adjust indentation if necessary
      (unless (= result (current-column))
        (delete-region bol (point))
        (indent-to result))
      ;; restore cursor sensibly
      (when (< (point) (- (point-max) pos))
        (goto-char (- (point-max) pos))))))

;; XXX: enumerate other indentation related functions and consider necessity

(provide 'jee-indent-line)
;;; jee-indent-line.el ends here
