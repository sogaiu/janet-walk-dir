#! /usr/bin/env janet

(use ./sh-dsl)

########################################################################

(defn copy-file
  [src dst]
  (spit dst (slurp src)))

########################################################################

(prin "* copying walk-dir/walk-dir.janet to walk-dir.janet...")
(copy-file "walk-dir/walk-dir.janet" "walk-dir.janet")
(print "done")

########################################################################

(print "* running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

