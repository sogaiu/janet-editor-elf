# Left Margin Convention

If your source code is compliant with the [Left Margin Convention](https://www.gnu.org/software/emacs/manual/html_node/emacs/Left-Margin-Paren.html)
janet-editor-elf may work better.

To follow that convention in Janet's case, the characters to avoid having
in column zero (unless they are being used to indicate the start of a
top-level form) include:

* ( - Open parenthesis
* ~ - Tilde
* ' - Single quote

Docstrings and long strings are some of the place these might be likely to
crop up, but if you use the provided indentation functionality, it should
help to prevent problems.

So far, the author's opinion is that following the convention is a
worthwhile trade-off.  What one gains in return is likelihood of tooling
working more reliably and in more contexts.  One reason for this is that
it becomes possible to rely on it as an indication of a programmer's intent
of where a top-level construct begins.  This can be an issue for
efficiency, correct operation, and robustness in the face of code that is
not-quite-right (e.g. missing delimiters).

If you don't have a particular preference, please consider following it.
