;;; jee-comment.el --- Janet comment convenience -*- lexical-binding: t; -*-

;;; Commentary:

;; Conveniently using / dis-using comment

;; Prerequisites

;; 1. Some janet major mode
;;
;; 2. Janet (needed to execute indentation code in external process)

;;; Code:

(require 'jee-wrap)

(defun jee-comment-wrap ()
  "Wrap with comment call."
  (interactive)
  (jee-wrap--wrap-with "comment"))

(defun jee-comment-unwrap ()
  "Remove comment wrapping."
  (interactive)
  (jee-wrap--unwrap "comment"))

(defun jee-comment-wrap-region (beg end)
  "Wrap region bound by BEG and END with comment call."
  (interactive "r")
  (jee-wrap--wrap-region-with beg end "comment\n"))

(provide 'jee-comment)
;;; jee-comment.el ends here
