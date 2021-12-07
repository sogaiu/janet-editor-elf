;;; jee-indent-region.el --- Janet region indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Region indentation

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
;;                (require 'jee-indent-region)
;;                (setq-local indent-region-function
;;                            #'jee-indent-region)))

;;; Code:

(require 'jee-top-level)
(require 'jee-utils)

(defvar jee-indent-region--helper-path
  (expand-file-name
   (concat (expand-file-name
	    (file-name-directory (or load-file-name
				     buffer-file-name)))
	   "janet-editor-elf/indent-region.janet"))
  "Path to helper program to calculate indentation for a region.")

(defvar jee-indent-region--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar jee-indent-region--temp-buffers
  '()
  "List of buffers to clean up before calling `jee-indent-region--helper'.")

;; 1                          \           for emacs, first line is 1
;; 2                           \
;; .                       outer offset
;; .                           /
;; .                          /
;; i    start of outer region             0
;; i+1                        \           1
;; i+2                         \          2
;; .                       start offset   .
;; .                           /          .
;; .                          /           .
;; j    start of inner region        start offset
;; j+1                                            \
;; j+2                                             \
;; .                                           n inner lines
;; .                                               /
;; .                                              /
;; k    end of inner region
(defun jee-indent-region--helper (outer-offset start-offset n-inner-lines
                                  outer-start outer-end)
  "Determine indentation change information of region via a helper.

Indentation change information is either nil or a list of pairs.

The first element in a pair is a line number and the second
element is its target indentation.

OUTER-OFFSET is the line number of the beginning of the outer
region.

START-OFFSET is the offset of the start line of the inner region
from OUTER-OFFSET.

N-INNER-LINES is the number of lines in the inner region.

The helper is also sent the text of the outer region that
completely contains the aforementiond inner region.  The outer
region is bound by OUTER-START and OUTER-END.  The beginning of
the outer region is expected to be the start of a top level
construct."
  (let ((temp-buffer (generate-new-buffer "*jee-indent*")))
    ;; clean up any old buffers
    ;; XXX: assumes all previous calls have completed before this invocation
    (dolist (old-buffer jee-indent-region--temp-buffers)
      (kill-buffer old-buffer))
    (add-to-list 'jee-indent-region--temp-buffers temp-buffer)
    ;;
    (condition-case err
        (let ((result nil))
          (save-excursion
            (when jee-indent-region--debug-output
              (message "region: %S"
                       (buffer-substring-no-properties outer-start
                                                       outer-end)))
            ;; https://emacs.stackexchange.com/a/54353
            (let ((exit-code
                   (call-process-region outer-start outer-end
                                        "janet" nil
                                        `(,temp-buffer nil)
                                        ;; XXX
                                        ;;`(,temp-buffer "/tmp/jee-ir.txt")
                                        nil
                                        jee-indent-region--helper-path
                                        (number-to-string outer-offset)
                                        (number-to-string start-offset)
                                        (number-to-string n-inner-lines))))
              (if (not (zerop exit-code))
                  (progn
                    (message "exit-code: %s" exit-code)
                    (error "Non-zero exit"))
                (set-buffer temp-buffer)
                (setq result
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))
                (when jee-indent-region--debug-output
                  (message "jee-indent: %S" result))
                ;; parse results
                (cond ;; indenter says to leave things alone
                 ((string-match "^-1$" result)
                  nil)
                 ((string-match "^-2$" result)
                  nil)
                 ((string-match (concat "^"
                                        "("
                                        "\\("
                                        "([0-9]+ [0-9]+) "
                                        "\\)*"
                                        ")"
                                        "$" )
                                result)
                  (read result))
                 (t
                  (message "result: <<%s>>" result)
                  (error "Unexpected result")))))))
      (error
       (message "Error: %s %s" (car err) (cdr err))
       nil))))

;; * inner region is the desired region to indent
;; * outer region begins with a top-level construct and
;;   completely contains the inner region
(defun jee-indent-region--calculate (start end)
  "Calculate indentation for a region of Janet code bound by START and END."
  ;; 1. determine helper arguments
  ;; 2. invoke helper to find out indentation info for each line
  ;;    within region that needs to be reindented
  (save-excursion
    (let ((start-line nil)
          (end-line nil)
          ;; helper arguments
          (outer-offset nil)  ; 1
          (start-offset nil)  ; 2
          (n-inner-lines nil) ; 3
          (outer-start nil)   ; 4
          (outer-end nil))    ; 5
      (save-excursion
        ;; determine arguments for `jee-indent-region--helper'
        (goto-char start)
        (setq start-line (line-number-at-pos))
        (forward-line -1)
        (jee-top-level--start-of)
        ;; 1
        (setq outer-offset
              (line-number-at-pos))
        ;; 2
        (setq start-offset
              (- start-line outer-offset))
        ;; 4
        (setq outer-start (point))
        ;;
        (goto-char end)
        (setq end-line (line-number-at-pos))
        ;; 3
        (setq n-inner-lines
              (1+ (- end-line start-line)))
        ;;
        (end-of-line)
        ;; 5
        (setq outer-end
              (point))
        ;; ask the helper to try to determine indentation
        (jee-indent-region--helper outer-offset start-offset n-inner-lines
                                   outer-start outer-end)))))

(defun jee-indent-region (start end)
  "Indent a region of Janet code bound by START and END.

In contrast to `jee-indent-line', the indentation of the interior of
multiline strings is left alone."
  (interactive "r")
  (save-excursion
    (when-let ((indents
                (jee-indent-region--calculate start end)))
      ;; reindent only as necessary
      (let ((cur-line-no 1))
        (goto-char (point-min))
        (dolist (indent-info indents)
          (let ((line (car indent-info))
                (indent (cadr indent-info)))
            (forward-line (- line cur-line-no))
            ;; this only indents if necessary
            (indent-line-to indent)
            (setq cur-line-no (line-number-at-pos))))))))

(jee-comment

 (add-hook 'janet-mode-hook
           (lambda ()
             (require 'jee-indent-region)
             (setq-local indent-region-function
                         #'jee-indent-region)))

 )

(provide 'jee-indent-region)
;;; jee-indent-region.el ends here
