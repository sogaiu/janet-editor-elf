# janet-indent

Code for indenting a line or region of Janet source code.

## Demos

### Line Indentation

Given the file `data/indent-line-input.txt` with content [1]:

```
(defn my-fn
  [x]
  (+ x
(- 2 x)
```

Invoking:

```
cat data/indent-line-input.txt | janet janet-indent/indent-line.janet
```

should result in `5`, the amount by which the last line should be
indented.

The line indentation code above (i.e. what `main` does) was written
with some assumptions:

* an appropriate sequence of lines preceding the line-to-be-indented
  are provided (except when the line-to-be-indented is the first line
  of a top-level form)
* previous lines have been indented correctly
* an appropriate [left-margin
  convention](./doc/left-margin-convention.md) is being followed

The code was designed to be used in the context of an editor and was
successfully used in Emacs via
[janet-editor-elf](https://github.com/sogaiu/janet-editor-elf).

### Region Indentation

Given the file `data/indent-region-input.txt` with content [2]:

```
(defn my-fn
[x]
(+ x
(- 2 x)))
```

Invoking:

```
cat data/indent-region-input | janet janet-indent/indent-region.janet 1 0 4
```

should result in:

```
((2 2) (3 2) (4 5) )
```

The results describe which lines should be indented and by how much:

* line 2 by 2 spaces
* line 3 by 2 spaces
* line 4 by 5 spaces

Note that there is no pair of the form `(1 x)`, i.e. line 1 does not
need to be indented any differently than it already is.

See `main` (and the comment above it) in `indent-region.janet` for
information about what `1 0 4` means.

As with the line indentation, there are some assumptions about the
lines the code is expecting to see:

* an appropriate sequence of lines preceding the region-to-be-indented
  are provided.  the expected lines are formed from the
  region-to-be-indented along with an enclosing region which begins at
  a top-level construct.  (this information is provided to `main` in
  the form of the three numbers that appeared before (recall `1 0
  4`).)
* an appropriate [left-margin
  convention](./doc/left-margin-convention.md) is being followed

Again, as with the line indentation case, the code was designed to be
used in the context of an editor and was successfully used in Emacs
via [janet-editor-elf](https://github.com/sogaiu/janet-editor-elf).

Note that due to the way long-strings are processed in Janet, there is
a bit of a subtlty involved in indenting a region with long-strings:

> long-strings have two advantages over ordinary strings. First, new
> lines are preserved. This makes it simple to write readable strings
> in code. Second, Janet will automatically removed indentation
> (so-called "dedenting") for whitespace that appears before the
> column in which the long-string began.

Since Janet may perform some "dedenting", region-indentation code
could change an author's intent if it changed long-string lines.  The
only safe option is for the region-indentation code to leave
long-string lines alone and this is what the code does.

See [this section of Janet's
docs](https://janet-lang.org/docs/documentation.html#Using-Long-Strings)
for more details about long-strings (including an example).

## Usages

### calc-last-line-indent

```janet
(calc-last-line-indent
  # non-spork/fmt formatting
  (string " (defn a\n"
          "   1"))
# =>
-1

(calc-last-line-indent "(+ 2 8)")
# =>
0

(calc-last-line-indent ":a")
# =>
0

(calc-last-line-indent
  (string "(+ 2\n"
          "8)"))
# =>
3

(calc-last-line-indent
  (string "(defn my-fn\n"
          "  [x]\n"
          "(+ x"))
# =>
2

(calc-last-line-indent
  (string "{:a 1\n"
          ":b"))
# =>
1

(calc-last-line-indent
  (string "`\n"
          " hello"))
# =>
0

(calc-last-line-indent
  (string "``\n"
          "  hello"))
# =>
0

(calc-last-line-indent
  (string "(def a\n"
          "  ``\n"
          "hi"))
# =>
2

(calc-last-line-indent
  (string " @``\n"
          "hi"))
# =>
2

(calc-last-line-indent
  (string "(def b\n"
          `  "Beginning\n`
          "next"))
# =>
2

(calc-last-line-indent
  (string "{:a\n"
          `""`))
# =>
1

(calc-last-line-indent
  (string "{:a\n"
          "[]"))
# =>
1

(calc-last-line-indent
  (string "(def a\n"
          "(print 1))"))
# =>
2

(calc-last-line-indent "(def a")
# =>
0

(calc-last-line-indent "(def a\n1")
# =>
2

# XXX: whitespace before newline needs to be cleaned by editor?
(calc-last-line-indent "(def a \n1")
# =>
2

(calc-last-line-indent (string "(try\n"
                               "  1\n"
                               "  #\n"
                               "  ([err]\n"
                               "2))"))
# =>
4

(calc-last-line-indent (string "(defn my-fun\n"
                               "[x]"))
# =>
2
```

### calc-region-indent

```janet
# last line in region is blank
(calc-region-indent
  (string `(/ 12` "\n" # line 100
          `   1)` "\n"
          ""      "\n"
          "")          # 3 lines down, 1 line region
  100 3 1)
# =>
'@[(103 0)]

# last line has a top-level construct
(calc-region-indent
  (string `(/ 2`      "\n" # line 200
          `   1)`     "\n"
          ""          "\n"
          ""          "\n"
          `(def a 1)`)     # 4 lines down, 1 line region
  200 4 1)
# =>
'@[(204 0)]

# last line starts a top-level construct
(calc-region-indent
  (string `(/ 2`   "\n" # line 50
          `   1)`  "\n"
          ""       "\n"
          `(def a`)     # 3 lines down, 1 line region
  50 3 1)
# =>
'@[(53 0)]

# last line is in the middle of a string
(calc-region-indent
  (string `(+ 1 1)`     "\n" # line 10
          ""            "\n"
          `(defn my-fn` "\n" # 2 lines down, 4 line region
          `[x]`         "\n" # needs to be indented (line 13)
          "  ``"        "\n"
          `hello`)           # should be left alone
  10 2 4)
# =>
'@[(13 2)]

# last line is a comment
(calc-region-indent
  (string `(+ 1 1)`      "\n" # line 25
          ""             "\n"
          ` (defn a [x]` "\n" # 2 lines down, 2 line region
          ` # hello`)
  25 2 2)
# =>
'@[(27 0) (28 2)]

# last line does not end in a comment
(calc-region-indent
  (string `(+ 1 1)`     "\n" # line 300
          ""            "\n"
          ""            "\n"
          ` # hello`)        # 3 lines down, 1 line region
  300 3 1)
# =>
'@[(303 0)]

(calc-region-indent
  (string `(+ 1 1)`     "\n" # line 1
          ""            "\n"
          `  (def a`    "\n" # 2 lines down, 4 line region
          `1)`          "\n"
          ""            "\n"
          ` # hello`)
  1 2 4)
# =>
'@[(3 0) (4 2) (6 0)]

(calc-region-indent
  (string `(+ 1 1)`     "\n" # line 1
          ""            "\n"
          `  (def a`    "\n" # 2 lines down, 6 line region
          `1)`          "\n"
          ` (def b`     "\n"
          `    2)`      "\n"
          ""            "\n"
          ` (def c 3)`)
  1 2 6)
# =>
'@[(3 0) (4 2) (5 0) (6 2) (8 0)]
```

## Users

The code is currently used via Emacs Lisp code in
[janet-editor-elf](https://github.com/sogaiu/janet-editor-elf).

## Footnotes

[1] The file should not end with a newline.  This can be checked via a
    hex editor.  On Linux, the `hd` command might be a convenient way
    to observe this.  Also, `truncate -s -1 FILENAME` is one method of
    remove a trailing byte from a file named `FILENAME`.

[2] See footnote [1].

