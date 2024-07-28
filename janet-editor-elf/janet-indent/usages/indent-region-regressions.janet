(import ../janet-indent/indent-region :prefix "")

(comment

  (calc-region-indent
    (string `  `         "\n" # line 1, 0 lines down, 2 line region
            `(def a 1)`)
    1 0 2)
  # =>
  '@[(2 0)]

  (calc-region-indent
    (string `(defn my-fn` "\n" # line 1, 0 lines down, 5 line region
            `  []`        "\n"
            ""            "\n"
            `  `          "\n"
            `  (+ 1 1))`)
    1 0 5)
  # =>
  '@[(5 2)]

 )

