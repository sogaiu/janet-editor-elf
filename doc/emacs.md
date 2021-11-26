# Emacs

## Setup

How to set things up varies a bit depending on how one manages one's
Emacs, e.g. straight.el, Doom, etc.  What's common to all situations
is likely:

* Ensure a janet-mode is installed and configured.

* Clone this repository.

### straight.el

* I add the following sort of thing to my `.emacs`-equivalent:
    ```
    (straight-use-package
      '(janet-editor-elf :host github
                         :repo "sogaiu/janet-editor-elf"
                         :files ("*.el" "janet-editor-elf")))

    (use-package janet-editor-elf
      :straight t
      :config
      (add-hook 'janet-mode-hook
                (lambda ()
                  (setq-local indent-line-function
                              #'jee-indent-line))))
    ```

### Doom

* Allegedly, the following is valid for Doom:
    ```
    (package! janet-editor-elf
      :recipe (:type git
               :host github
               :repo "sogaiu/janet-editor-elf"
               :files (:defaults ("janet-editor-elf/"
                                  "janet-editor-elf/*"))))

    (use-package! janet-editor-elf
      :config
      (add-hook 'janet-mode-hook
                (lambda ()
                  (setq-local indent-line-function
                              #'jee-indent-line))))
    ```

### Vanilla

* If you cloned to `~/src/janet-editor-elf`, add the following to your
  `.emacs`-equivalent:
    ```
    (add-to-list 'load-path
                 (expand-file-name "~/src/janet-editor-elf"))

    (add-hook 'janet-mode-hook
              (lambda ()
                (require 'janet-editor-elf)
                (setq-local indent-line-function
                            #'jee-indent-line)))
    ```

### package.el

* Sorry, no support for that.  The Vanilla instructions should work
  though.

## Usage

### Indentation

Indentation of lines should work via `Tab` and happen automatically
via `Enter` -- though these might depend on per-user Emacs settings.

Indenting of regions should work, but is likely to be slow for larger
regions.  There is hope this may be improved at some point.

At present it is not recommended to use indentation of regions
containing long strings as it may lead to undesirable modification of
leading spaces.

As an example, consider:
```
(def a
  ``
    hello
    there
  ``)
```
Use of `indent-region` on the whole form will currently produce:
```
(def a
  ``
  hello
  there
  ``)
```
This might not be what is desired.  The current plan is to implement
`indent-region` so that it leaves long string content alone, but this
is not done yet.

### Wrapping and Unwrapping

To wrap a form with `(tracev ...)`, put the cursor over the first
character of a form and use `M-x jee-tracev-wrap`.

For example, to wrap `(+ 1 1)`, put the cursor on `(` and then execute
the command.

To unwrap a form that is contained in `(tracev ...)`, put point on
some character of the `(tracev ...)` form and use `M-x
jee-tracev-unwrap`.

For example, to unwrap `(tracev (+ 1 1))`, put the cursor on any of
the characters in `(tracev (+ 1 1))` and then execute the command.

A similar pair of things is possible for use with `(comment ...)` via:

* `M-x jee-comment-wrap`
* `M-x jee-comment-unwrap`
