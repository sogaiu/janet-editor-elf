(import ../janet-editor-elf/last-expression :prefix "")

# expr-grammar
(comment

  (peg/match expr-grammar " ")
  # => '@[(:whitespace " " 0 1)]

  (peg/match expr-grammar "# hi there ")
  # => '@[(:comment " hi there " 0 11)]

  (peg/match expr-grammar "8")
  # => '@[(:number "8" 0 1)]

  (peg/match expr-grammar "true")
  # => '@[(:constant "true" 0 4)]

  (peg/match expr-grammar "~0")
  # => '@[(:quasiquote (:number "0" 1 2) 0 2)]

  (peg/match expr-grammar "':hi")
  # => '@[(:quote (:keyword ":hi" 1 4) 0 4)]

  (peg/match expr-grammar "@``snake?``")
  # => '@[(:long-buffer "``snake?``" 0 11)]

  (peg/match expr-grammar "print")
  # => '@[(:symbol "print" 0 5)]

  (peg/match expr-grammar "()")
  # => '@[(:tuple 0 2)]

  (deep=
    #
    (peg/match expr-grammar "[2 3]")
    #
    '@[(:bracket-tuple
         (:number "2" 1 2) (:whitespace " " 2 3)
         (:number "3" 3 4)
         0 5)])
  # => true

  (deep=
    #
    (peg/match expr-grammar ";[8 9]")
    #
    '@[(:splice
         (:bracket-tuple
           (:number "8" 2 3) (:whitespace " " 3 4)
           (:number "9" 4 5)
           1 6)
         0 6)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "@[:x :z]")
    #
    '@[(:bracket-array
         (:keyword ":x" 2 4) (:whitespace " " 4 5)
         (:keyword ":z" 5 7)
         0 8)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "~,1")
    #
    '@[(:quasiquote
         (:unquote
           (:number "1" 2 3)
           1 3)
         0 3)])
  # => true

  (deep=
    #
    (peg/match expr-grammar "@{:a 1}")
    #
    '@[(:table
         (:keyword ":a" 2 4) (:whitespace " " 4 5)
         (:number "1" 5 6)
         0 7)])
  # => true

  (deep=
    #
    (peg/match expr-grammar (string "{:alpha 1\n"
                                    " :beta 2}"))
    #
    '@[(:struct
         (:keyword ":alpha" 1 7) (:whitespace " " 7 8)
         (:number "1" 8 9) (:whitespace "\n" 9 10)
         (:whitespace " " 10 11)
         (:keyword ":beta" 11 16) (:whitespace " " 16 17)
         (:number "2" 17 18)
         0 19)])
  # => true

  )

# last-expr
(comment

  (let [maybe-code (string "a")]
    (last-expr maybe-code))
  # => "a"

  (let [maybe-code (string "[:a :b]")]
    (last-expr maybe-code))
  # => "[:a :b]"

  (let [maybe-code
        (string "~(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code
        (string "(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)\n"
                "      ")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  (let [maybe-code
        (string "~(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)\n"
                "      ")]
    (last-expr maybe-code))
  # => "(- 2 1)"

  )
