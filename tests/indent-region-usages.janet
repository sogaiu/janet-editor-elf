(import ../janet-editor-elf/indent-region :prefix "")

# region before last line has balanced delimiters
(comment

  # last line in region is blank
  (calc-region-indent
    (string `(/ 12` "\n" # line 100
            `   1)` "\n"
            ""      "\n"
            "")          # 3 lines down, 1 line region
    100 3 1)
  # => '@[(103 0)]

  # last line has a top-level construct
  (calc-region-indent
    (string `(/ 2`      "\n" # line 200
            `   1)`     "\n"
            ""          "\n"
            ""          "\n"
            `(def a 1)`)     # 4 lines down, 1 line region
    200 4 1)
  # => '@[(204 0)]

  # last line starts a top-level construct
  (calc-region-indent
    (string `(/ 2`   "\n" # line 50
            `   1)`  "\n"
            ""       "\n"
            `(def a`)     # 3 lines down, 1 line region
    50 3 1)
  # => '@[(53 0)]

  )

# region before last line doesn't have balanced delimiters
(comment

  # last line is in the middle of a string
  (calc-region-indent
    (string `(+ 1 1)`     "\n" # line 10
            ""            "\n"
            `(defn my-fn` "\n" # 2 lines down, 4 line region
            `[x]`         "\n" # needs to be indented (line 13)
            "  ``"        "\n"
            `hello`)           # should be left alone
    10 2 4)
  # => '@[(13 2)]

  # last line is a comment
  (calc-region-indent
    (string `(+ 1 1)`      "\n" # line 25
            ""             "\n"
            ` (defn a [x]` "\n" # 2 lines down, 2 line region
            ` # hello`)
    25 2 2)
  # => '@[(27 0) (28 2)]

  # last line does not end in a comment
  (calc-region-indent
    (string `(+ 1 1)`     "\n" # line 300
            ""            "\n"
            ""            "\n"
            ` # hello`)        # 3 lines down, 1 line region
    300 3 1)
  # => '@[(303 0)]

  )

# region spans multiple top-level constructs
(comment

  (calc-region-indent
    (string `(+ 1 1)`     "\n" # line 1
            ""            "\n"
            `  (def a`    "\n" # 2 lines down, 4 line region
            `1)`          "\n"
            ""            "\n"
            ` # hello`)
    1 2 4)
  # => '@[(3 0) (4 2) (6 0)]

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
  # => '@[(3 0) (4 2) (5 0) (6 2) (8 0)]

  )
