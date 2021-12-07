;;; janet-editor-elf.el --- Janet editor elf -*- lexical-binding: t; -*-

;;; Commentary:

;; Various bits to help working with Janet code

;; Prerequisites

;; 1. Some janet major mode
;;
;; 2. Janet (needed to execute support code in external process)

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
;;                (require 'janet-editor-elf)
;;                (setq-local indent-line-function
;;                            #'jee-indent-line)))

;;; Code:

(require 'jee-comment)
(require 'jee-indent-line)
(require 'jee-indent-region)
(require 'jee-tracev)

(provide 'janet-editor-elf)
;;; janet-editor-elf.el ends here
