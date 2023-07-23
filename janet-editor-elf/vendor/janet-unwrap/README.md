# janet-unwrap

Code for determining bounds for unwrapping forms in strings of Janet
source code.

## Demo

Given the file `data/unwrap-input.txt` with content:

```
Name: tracev
Line: 2
Column: 10

(def a
  (tracev (+ 3
             (- 1 2))))

```

To determine the bounds of a `(tracev ...)` form wrapping the location
line 2, column 10:

```
cat data/unwrap-input.txt | janet janet-unwrap/unwrap.janet
```

The result should be:

```
2 3 3 23 2 11 3 22
```

The first four numbers refer to the bounds of the whole `(tracev ...)`
form:

```
start: line 2, column 3
  end: line 3, column 23
```

while the next four numbers refer to the bounds of the "wrapped" form
`(+ 3 (- 1 2))`:

```
start: line 2, column 11
  end: line 3, column 22
```

## Usages

Below are some usages of a provided function named `find-wrap-info`.

```janet
(find-wrap-info
  ``
  (def a
    (tracev (+ 3
               (- 1 2))))
  ``
  2 10 "def")
# =>
[[1 1 3 24] [1 6 3 23]]

(find-wrap-info
  ``
  (comment

    (+ 1 1)
    # =>
    2

    )
  ``
  3 3 "comment")
# =>
[[1 1 7 4] [3 3 5 4]]
```

## Use

The code is currently used via Emacs Lisp code in
[janet-editor-elf](https://github.com/sogaiu/janet-editor-elf) via
process invocation to aid in unwrapping content within `comment` and
`tracev` forms.

