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
;;
;;    (add-hook 'janet-mode-hook #'jee-mode)

;;; Code:

(require 'jee-comment)
(require 'jee-indent-line)
(require 'jee-indent-region)
(require 'jee-tracev)
(require 'jee-utils)

(defvar jee-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-t" 'jee-tracev-wrap)
    (define-key map "\C-c\C-y" 'jee-tracev-unwrap)
    (define-key map "\C-c\C-c" 'jee-comment-wrap)
    (define-key map "\C-c\C-x" 'jee-comment-wrap-region)
    (define-key map "\C-c\C-v" 'jee-comment-unwrap)
    (easy-menu-define jee-mode-map map
      "Janet Editor Elf Mode Menu"
      '("Jee"
        ["Wrap with tracev" jee-tracev-wrap t]
        ["Unwrap tracev" jee-tracev-unwrap t]
        "--"
        ["Wrap with comment" jee-comment-wrap t]
        ["Wrap region with comment" jee-comment-wrap-region t]
        ["Unwrap comment" jee-comment-unwrap t]))
    map)
  "Janet editor elf mode map.")

;;;###autoload
(define-minor-mode jee-mode
  "Minor mode for janet-editor-elf.

The following keys are available in `jee-mode`:

\\{jee-mode-mode}"

  nil " jee" jee-mode-map)

(jee-comment

 (add-hook 'janet-mode-hook
           (lambda ()
             (require 'janet-editor-elf)
             (setq-local indent-line-function
                         #'jee-indent-line)))

 (add-hook 'janet-mode-hook #'jee-mode)

 )

(provide 'janet-editor-elf)
;;; janet-editor-elf.el ends here
