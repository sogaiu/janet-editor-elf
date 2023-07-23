(import ./jandent/indent :as fmt)
(import ./janet-delims/delims)

(defn deprintf
  [fmt & args]
  (when (os/getenv "VERBOSE")
    (eprintf fmt ;args)))

(defn indentation-pos
  [line]
  (if-let [[pos]
           (peg/match ~(sequence (any :s)
                                 (if-not " " 1)
                                 (capture (position)))
                      line)]
    (dec pos)
    # when line only has whitespace or is empty, return 0
    0))

(comment

  (indentation-pos "    3")
  # =>
  4

  (indentation-pos ":a")
  # =>
  0

  (indentation-pos " ")
  # =>
  0

  (indentation-pos "")
  # =>
  0

  (indentation-pos " @``")
  # =>
  1

  )

# in order to peform the indentation algorithm, it is necessary
# to have a region that has balanced delimiters.  if the received
# text does not have balanced delimiters, an attempt will be made to
# add appropriate closing delimiters.
(defn calc-region-indent-helper!
  [input-lines outer-offset start-offset n-inner-lines]
  # remember the last line index
  (def last-line-index
    (dec (length input-lines)))
  # XXX
  (deprintf "last line index: %p" last-line-index)
  # remove the last line
  (def last-line
    (array/pop input-lines))
  # lines before last line
  (def preceding-region
    (string/join input-lines "\n"))
  # check whether the preceding region is "balanced" delimiter-wise
  (def result
    (delims/missing-delims preceding-region))
  # missing-delims had a problem (e.g. too many closing delimiters)
  (when (nil? result)
    (break -2))
  (def [delims delim-start-pos delim-type] result)
  # cases:
  #
  # 1. lines before last line have balanced delimiters
  #    (delims is empty)
  #
  #    a. last line is blank
  #
  #       => last line's indentation is 0
  #
  #    b. last line begins a top-level form
  #
  #       => replace line with empty one so region is "balanced"
  #       => last line's indentation is 0
  #
  #    => near the end of the "calculation" incorporate this info
  #
  # 2. lines before last line don't have balanced delimiters
  #    (delims is not empty)
  #
  #    a. last line is in the middle of a string
  #
  #       => construct new last line by
  #          * harmless to begin line with symbol / number
  #          * add proper closing delimiters
  #       => indentation of the line for this index must not change
  #
  #    b. last line is not in the middle of a string
  #
  #       => construct new last line by
  #          * important to begin line with symbol / number
  #          * add proper closing delimiters
  #       => indentation of the line for this index may change
  #
  #    note that the last line has been discarded so it's not necessary
  #    to be concerned with it being a comment (which could complicate
  #    closing delimiters).
  (def delims-str (string/join delims))
  (if (empty? delims-str)
    (array/push input-lines
                "")
    (array/push input-lines
                (string "11" delims-str)))
  (def input-region
    (string/join input-lines "\n"))
  # XXX
  (deprintf "input region: %p" input-region)
  (def indented-region
    (fmt/format input-region))
  # XXX
  (deprintf "output region: %p" indented-region)
  #
  (def in-lines
    (string/split "\n" input-region))
  # XXX
  (deprintf "in-lines")
  (for i 0 (length in-lines)
    (deprintf "%d: %p" i (get in-lines i)))
  (def out-lines
    (string/split "\n" indented-region))
  # XXX
  (deprintf "out-lines")
  (for i 0 (length out-lines)
    (deprintf "%d: %p" i (get out-lines i)))
  # XXX
  (deprintf "total lines: %p" (length in-lines))
  (def diffs @[])
  # first handle all lines except the last one
  (for i 0 (dec n-inner-lines)
    (def j (+ start-offset i))
    # XXX
    (deprintf "j: %p" j)
    (def in-line
      (get in-lines j))
    (def out-line
      (get out-lines j))
    (when (not= in-line out-line)
        (array/push diffs
                    [(+ outer-offset j) (indentation-pos out-line)])))
  # handle last line specially
  (def last-out-line
    (get out-lines last-line-index))
  (cond
    (empty? delims-str)
    (array/push diffs [(+ outer-offset last-line-index) 0])
    # middle of string, don't change indentation
    (let [first-char (first delims-str)]
      (or (= (chr "`") first-char)
          (= (chr `"`) first-char)))
    nil
    #
    (array/push diffs [(+ outer-offset last-line-index)
                       (indentation-pos last-out-line)]))
  diffs)

(defn calc-region-indent
  [fragment outer-offset start-offset n-inner-lines]
  # XXX
  (deprintf "original region: %p" fragment)
  (def input-lines
    (string/split "\n" fragment))
  (calc-region-indent-helper! input-lines
                              outer-offset start-offset n-inner-lines))

# 1                          \           for emacs, first line is 1
# 2                           \
# .                       outer offset
# .                           /
# .                          /
# i    start of outer region             0
# i+1                        \           1
# i+2                         \          2
# .                       start offset   .
# .                           /          .
# .                          /           .
# j    start of inner region        start offset
# j+1                                            \
# j+2                                             \
# .                                           n inner lines
# .                                               /
# .                                              /
# k    end of inner region
(defn main
  [& args]
  (def outer-offset (scan-number (get args 1)))
  (def start-offset (scan-number (get args 2)))
  (def n-inner-lines (scan-number (get args 3)))
  (def indents
    (calc-region-indent (file/read stdin :all)
                        outer-offset
                        start-offset
                        n-inner-lines))
  # working with emacs is easier this way
  (prin "(")
  (each [line indentation] indents
    (prinf "(%d %d) " line indentation))
  (prin ")")
  #(each [line indentation] indents
  #  (printf "%d: %d" line indentation))
  )

