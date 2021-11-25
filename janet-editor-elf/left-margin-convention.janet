# draft idea (flawed, but useful stepping stone)
#
# * some cases can be handled by considering the following procedure
#   * find all column zero character positions which have (, ', or ~
#   * for each such location, use the parser to parse to one position
#     before the location and determine the "status".
#     * if the status is :root, then the location represents the beginning
#       of a top-level construct.
#     * if the status is :pending and the current location is in a string
#       (in parser's state, last frame, :type :string), warn?

# implemented idea
#
# * loop over each line of source collecting info:
#   * remember parser status from last line (at start, no line -> :root)
#   * consume a line (without the trailing newline)
#   * check leading character on line
#     * (, ', or ~
#       * if remembered status is :root, col 0 is ok
#       * if remembered status is :pending, col 0 is NOT ok
#     * #
#       * in a string -> col 0 is NOT ok
#       * else, col 0 is ok
#     * space, tab, nul, and other ws, or nothing (empty line) -> col 0 is ok
#     * all other characters mean col 0 is NOT ok
#   * consume trailing new line

# questions
#
# * closing delimiters in col 0 seem harmless, still warn?
# * special handling for \r\n needed?
# * is all relevant whitespace addressed appropriately?
# * assuming source is well-formed reasonable?

# * are there any cases that this doesn't handle?

(comment

  # example parser/state based on `{:a 1`
  @{:delimiters "{"
    :frames @[@{:args @[]
                :column 0
                :line 1
                :type :root}
              @{:args @[:a 1]
                :column 1
                :line 1
                :type :struct}]}

  )

(defn in-string?
  [p]
  (let [frames (parser/state p :frames)]
    (= :string
       (-> (last frames)
           (get :type)))))

(comment

  (let [src-chunks
        @["(defn a\n"
          "  ``Nice" " docstring``\n"
          "  [x]\n"
          "  x)"]]
    (def p (parser/new))
    (parser/consume p (get src-chunks 0))
    (parser/consume p (get src-chunks 1))
    (in-string? p))
  # => true

  (let [src-chunks
        @["(defn a\n"
          "  ``Nice" " docstring``\n"
          "  [x]\n"
          "  x)"]]
    (def p (parser/new))
    (parser/consume p (get src-chunks 0))
    (in-string? p))
  # => false

  (let [src-chunks
        @["(defn a\n"
          "  ``Nice" " docstring``\n"
          "  [x]\n"
          "  x)"]]
    (def p (parser/new))
    (parser/consume p (get src-chunks 0))
    (parser/consume p (get src-chunks 1))
    (parser/consume p (get src-chunks 2))
    (in-string? p))
  # => false

  )

(defn in-comment?
  [p]
  (let [frames (parser/state p :frames)]
    (= :comment
       (-> (last frames)
           (get :type)))))

(comment

  (let [src-chunks
        @["# nice comment" " that is here\n"]]
    (def p (parser/new))
    (parser/consume p (get src-chunks 0))
    (in-comment? p))
  # => true

  (let [src-chunks
        @["# nice comment" " that is here\n"]]
    (def p (parser/new))
    (parser/consume p (get src-chunks 0))
    (parser/consume p (get src-chunks 1))
    (in-comment? p))
  # => false

  )

(defn make-set
  [elts]
  (let [uniq-elts (distinct elts)]
    (zipcoll uniq-elts
             (array/new-filled (length uniq-elts) true))))

(comment

  (make-set [1 2 3])
  # => @{1 true 2 true 3 true}

  )

(def non-nl-ws-chars
  (make-set [" "
             "\0"
             "\f"
             "\r"
             "\t"
             "\v"]))

(def col-0-ok-chars
  (make-set ["("
             "'"
             "~"]))

# XXX: assume source is "correct" syntactically
(defn analyze
  [lines]
  (def p (parser/new))
  # non-compliant lines
  (var nc-line-nos @[])
  # should be :root the first time through
  (var prev-status nil)
  (for i 0 (length lines)
    (set prev-status (parser/status p))
    (def line (get lines i))
    # XXX: check for errors? or should "correct" source be a
    #      pre-condition?
    (parser/consume p line)
    (when (pos? (length line))
      (def first-char-str
        (string/slice line 0 1))
      (cond
        # whitespace
        (get non-nl-ws-chars first-char-str)
        nil
        # possibly a top-level construct
        (get col-0-ok-chars first-char-str)
        (cond
          (= :root prev-status)
          nil
          #
          (= :pending prev-status)
          (array/push nc-line-nos i)
          #
          (errorf "Unexpected status: %p" prev-status))
        # possibly a comment
        (= "#" first-char-str)
        (when (in-string? p) # not a comment
          (array/push nc-line-nos i))
        # unexpected case
        (array/push nc-line-nos i)))
    (parser/consume p "\n"))
  nc-line-nos)

(comment

  (let [source " "]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source "\0"]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source "\f"]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source "\t"]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source "\v"]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        (defn my-fun
          [x y]
          (+ x y))
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        (def a 1)
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        (defn another-fn
          "My fun docstring"
          []
          8)
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        '(print "hi")
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        ~(+ 1 1)
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        # stuff after here should be "caught"
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => true

  (let [source
        ``
        1 # a number at the top-level just wastes cycles?
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        ``
        {:a :b} # a struct at the top-level just wastes cycles?
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        ``
        [1 2 3] # a square bracket tuple at the top-level just wastes cycles?
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        ``
        "hi" # a string at the top-level just wastes cycles?
        ``]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        ```
        ``smile!`` # a lone string at the top-level just wastes cycles?
        ```]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        ```
        (defn bad-docstring-0
        ``Docstring not indented properly.``
          []
          2)
        ```]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        ```
        (defn bad-docstring-1
        "Docstring not indented properly."
          []
          2)
        ```]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        `````
        (defn bad-docstring-2
          ```This is ok. But not the following line...
        (don't use a left paren in col 0 in a string)
          ```
          [x]
          3)
        `````]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        `````
        (defn bad-docstring-3
          ```This is ok. But not the following line...
        'don't use single quotes in col 0 in strings'
          ```
          [x]
          3)
        `````]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        `````
        (defn bad-docstring-4
          ```This is ok. But not the following line...
        ~tildes in col 0 are not good either
          ```
          [x]
          3)
        `````]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        `````
        (defn bad-docstring-5
          ```This is ok. But not the following line...
        In col 0, only put left paren, single quote, or tilde,
        and only if they start a top-level form
          ```
          [x]
          3)
        `````]
    (empty? (analyze (string/split "\n" source))))
  # => false

  (let [source
        `````
        (defn bad-docstring-6
          ``This part is ok.
        # Looks like a comment, but is not -- don't do this either
          ``
          []
          2)
        `````]
    (empty? (analyze (string/split "\n" source))))
  # => false

  )

(comment

  (def p (parser/new))

  (parser/consume p "(")

  (parser/status p)
  # => :pending

  (parser/consume p ")")

  (parser/status p)
  # => :root

  ###

  (parser/consume p "``\nhello\n")

  (parser/status p)
  # => :pending

  (in-string? p)
  # => true

  (parser/consume p "``")

  (parser/status p)
  # => :pending

  (parser/consume p " ")

  (parser/status p)
  # => :root

  )

(defn main
  [& args]
  (def f
    (file/open (get args 1)))
  (def file-content
    (file/read f :all))
  (def lines
    (string/split "\n" file-content))
  (def results
    (analyze lines))
  (when (not (empty? results))
    (each i results
      (eprintf "%d: %s" i (get lines i)))))
