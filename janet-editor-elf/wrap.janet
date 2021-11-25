(import ./bounds)
(import ./delims)

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
      (eprintf "span: %p" span)
      (each item (bounds/content node)
        (eprintf "item: %p" item)
        (when (and (= :symbol (bounds/node-type item))
                   (= name (first (bounds/content item))))
          (eprintf "spans? %p %p" span target-span)
          (when (bounds/spans? span target-span)
            (eprintf "yes, spans: %p %p" span target-span)
            (array/push ctxt node)
            (array/push seed-span
                        (->> (tuple/slice (bounds/content node) 1)
                             (filter |(and (= :tuple (type $))
                                           (not= :whitespace
                                                 (bounds/node-type $))
                                           (not= :comment
                                                 (bounds/node-type $))))
                             first
                             bounds/span))))))
    (each item (bounds/content node)
      (eprintf "item: %p" item)
      (when (= :tuple (type item))
        (helper item))))
  #
  (helper tree)
  # XXX: is this sensible? if this always works, just keep
  #      overwriting instead of accumulating in array?
  (def result (last ctxt))
  (def span-result (last seed-span))
  (eprintf "result: %p" result)
  (eprintf "span-result: %p" span-result)
  # XXX
  (eprintf "ctxt: %p" ctxt)
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
  (eprintf "new region: %p" new-region)
  (def tree
    (bounds/ast new-region))
  #
  (def [node i-span]
    (find-bounds-and-span tree target-span name))
  (eprintf "bounds node: %p" node)
  (eprintf "contained span: %p" i-span)
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
  (eprintf "target span: %p" target-span)
  (wrap-info-helper! input-lines delims target-span name))

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
    (eprint "failed to find name, line, and/or column in header lines")
    (print "-1")
    (os/exit 0))
  #
  (def fragment
    (get pieces 1))
  (def [bounds i-span]
    # XXX: pass in span
    (find-wrap-info fragment line col name))
  (eprintf "bounds: %p" bounds)
  (cond
    (indexed? bounds)
    (printf "%d %d %d %d %d %d %d %d" ;bounds ;i-span)
    #
    (neg? bounds)
    (print bounds)
    #
    (print -1)))

