(import ./janet-bounds/janet-bounds :as bounds)
(import ./janet-delims/janet-delims :as delims)

(defn deprintf
  [fmt & args]
  (when (os/getenv "VERBOSE")
    (eprintf fmt ;args)))

# find all, then find smallest
(defn find-bounds-and-span
  [tree target-span name]
  (var ctxt @[tree])
  (var seed-span @[nil])
  (defn helper
    [node]
    (when (and (= :tuple (type node))
               (= :tuple (bounds/node-type node)))
      (def span (bounds/span node))
      (deprintf "span: %p" span)
      (each item (bounds/content node)
        (deprintf "item: %p" item)
        (when (and (= :symbol (bounds/node-type item))
                   (= name (first (bounds/content item))))
          (deprintf "spans? %p %p" span target-span)
          (when (bounds/spans? span target-span)
            (deprintf "yes, spans: %p %p" span target-span)
            (array/push ctxt node)
            (def nodes
              (->> (tuple/slice (bounds/content node) 1)
                   (filter |(and (= :tuple (type $))
                                 (not= :whitespace
                                       (bounds/node-type $))))))
            (cond
              (one? (length nodes))
              (array/push seed-span (bounds/span (first nodes)))
              #
              (> (length nodes) 1)
              (let [first-node (first nodes)
                    last-node (last nodes)]
                (array/push seed-span
                            [;(slice (bounds/span first-node) 0 2)
                             ;(slice (bounds/span last-node) 2)])))))))
    (each item (bounds/content node)
      (deprintf "item: %p" item)
      (when (= :tuple (type item))
        (helper item))))
  #
  (helper tree)
  # XXX: is this sensible? if this always works, just keep
  #      overwriting instead of accumulating in array?
  (def result (last ctxt))
  (deprintf "seed-span: %p" seed-span)
  (def span-result (last seed-span))
  (deprintf "result: %p" result)
  (deprintf "span-result: %p" span-result)
  # XXX
  (deprintf "ctxt: %p" ctxt)
  (if (nil? span-result)
    [nil nil]
    [result span-result]))

(defn wrap-info-helper!
  [input-lines delims target-span name]
  # if there are any missing delimiters add them as a new line
  (when (not (empty? delims))
    (array/push input-lines delims))
  (def new-region
    (string/join input-lines "\n"))
  # XXX
  (deprintf "new region: %p" new-region)
  (def tree
    (bounds/ast new-region))
  #
  (def [node i-span]
    (find-bounds-and-span tree target-span name))
  (deprintf "bounds node: %p" node)
  (deprintf "contained span: %p" i-span)
  (if (indexed? node)
    [(bounds/span node) i-span]
    [nil nil]))

(defn find-wrap-info
  # XXX: pass in span
  [fragment line column name]
  (def [delims _ _]
    (delims/missing-delims fragment))
  # missing-delims had a problem (e.g. too many closing delimiters)
  (when (nil? delims)
    (break -2))
  #
  (def input-lines
    (string/split "\n" fragment))
  # XXX
  (def target-span
    [line column
     line column])
  # XXX
  (deprintf "target span: %p" target-span)
  (wrap-info-helper! input-lines delims target-span name))

(comment

  (find-wrap-info
    ``
    (def a
      (tracev (+ 3
                 (- 1 2))))
    ``
    2 10 "tracev")
  # =>
  [[2 3 3 23] [2 11 3 22]]

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

  )

(defn main
  [& args]
  (def input
    (file/read stdin :all))
  (def pieces
    (string/split "\n\n" input 0 2))
  #
  (def header-lines
    (string/split "\n" (first pieces)))
  (var line nil)
  (var col nil)
  (var name nil)
  # XXX: extend to work with region
  (each hl header-lines
    (when-let [[value]
               (peg/match ~(sequence "Name: "
                                     (capture (some :S))
                                     -1)
                          hl)]
      (set name value))
    (when-let [[num]
               (peg/match ~(sequence "Column: "
                                     (capture :d+)
                                     -1)
                          hl)]
      (set col (scan-number num)))
    (when-let [[num]
               (peg/match ~(sequence "Line: "
                                     (capture :d+)
                                     -1)
                          hl)]
      (set line (scan-number num))))
  (when (or (nil? line)
            (nil? col)
            (nil? name))
    (deprintf "failed to find name, line, and/or column in header lines")
    (print "-1")
    (os/exit 0))
  #
  (def fragment
    (get pieces 1))
  (def [bounds i-span]
    # XXX: pass in span
    (find-wrap-info fragment line col name))
  (deprintf "bounds: %p" bounds)
  (cond
    (indexed? bounds)
    (printf "%d %d %d %d %d %d %d %d" ;bounds ;i-span)
    #
    (neg? bounds)
    (print bounds)
    #
    (print -1)))

