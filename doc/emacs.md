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
                              #'jee-indent-line)))
      (add-hook 'janet-mode-hook
                (lambda ()
                  (setq-local indent-region-function
                              #'jee-indent-region))))
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
      :after janet-mode
      :config
      (add-hook 'janet-mode-hook
                (lambda ()
                  (setq-local indent-line-function
                              #'jee-indent-line)))
      (add-hook 'janet-mode-hook
                (lambda ()
                  (setq-local indent-region-function
                              #'jee-indent-region))))
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

    (add-hook 'janet-mode-hook
              (lambda ()
                (setq-local indent-region-function
                            #'jee-indent-region)))
    ```

### package.el

* Sorry, no support for that.  The Vanilla instructions should work
  though.

## Usage

### Indentation

Indentation of lines should work via `Tab` and happen automatically
via `Enter` -- though these might depend on per-user Emacs settings.

There is a separate implementation for `indent-region`.  Its behavior
differs a bit from repeatedly applying `indent-line` to lines in a
region, but it should also be faster.

To illustrate, consider the following code:

```
(def a
  ``
    hello
    there
  ``)
```

If one were to apply `indent-line` manually to each line, the result
would be:

```
(def a
  ``
  hello
  there
  ``)
```

Note that the leading space from some lines within the long string
have been changed.  This is not likely to be what one wants when
indenting the region.  `indent-region` should preserve such whitespace,
leaving the long string unchanged.

Note that pressing `Enter` within a long string may lead to
indentation of the line the cursor was just on (which may change the
leading whitespace).  One way to prevent that from occuring is to use
`C-q C-j` (i.e. applying the Emacs command `quoted-insert` to `C-j`)
instead of `Enter` for such lines.

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

There is also a command to operate on wrapping a region:

* `M-x jee-comment-wrap-region`
