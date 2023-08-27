;;; jee-tracev.el --- Janet tracev convenience -*- lexical-binding: t; -*-

;;; Commentary:

;; Conveniently using / dis-using tracev

;; Prerequisites

;; 1. Some janet major mode
;;
;; 2. Janet (needed to execute indentation code in external process)

;;; Code:

(require 'jee-wrap)

(defun jee-tracev-wrap ()
  "Wrap with tracev call."
  (interactive)
  (jee-wrap-wrap-with "tracev"))

(defun jee-tracev-unwrap ()
  "Remove tracev wrapping."
  (interactive)
  (jee-wrap--unwrap "tracev"))

;; XXX: figure out
(defun jee-tracev-remove-from-top-level ()
  "Remove tracev use from top level construct."
  (interactive)
  )

(provide 'jee-tracev)
;;; jee-tracev.el ends here
