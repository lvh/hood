(ns hood.constraint
  [:require
   [clojure.math.numeric-tower :refer [expt]]
   [loco.core :refer [solution]]
   [loco.constraints :refer :all]])

(defn ^:private solve
  "Throw the constraint problem into loco."
  [apps budget target]
  (let [allocs (for [a apps] [:allocation a])
        grant-constraints (map #($in [:allocation %] 0 (:requested %)) apps)
        within-budget ($= (apply $+ allocs) budget)
        constraints (conj grant-constraints within-budget)]
    (solution constraints :maximize target)))

(defn alloc
  "Allocates using a constraint solver.

  apps is a seq of all applications.
  budget is the total budget.
  target is the term to optimize.
  "
  [apps budget target]
  (let [soln (solve apps budget target)]
    (into {} (for [[[tag application] grant] soln] [application grant]))))

(defn ^:private $expt
  [x n]
  (reduce $* (repeat n x)))

(defn poly-target
  "A polynomial target function of the shape:

  sum of si * (ai/ri)^n

  ... where si, ai and ri are an applications score, allocated amount
  and requested amount, respectively.

  Making n bigger weights towards giving fewer, larger grants. Making
  n smaller weights towards more, smaller grants. Suggested values for
  n are 1 (linear; estimated probability of attending is the fraction
  of requested amount received) and 2 (quadratic; estimated
  probability of attending is the square of the fraction of the
  requested amount received).
  "
  [n apps score]
  (let [ratios (map #(/ (score %) (expt (:requested %) n)) apps)
        scale (/ 100 (- (apply max ratios) (apply min ratios)))
        scaled-ratios (vec (map #(long (* scale %)) ratios))
        expd-allocs (for [a apps] ($expt [:allocation a] n))
        scaled-allocs (map $* expd-allocs scaled-ratios)]
    (apply $+ scaled-allocs)))

(def linear-target (partial poly-target 1))
(def quadratic-target (partial poly-target 2))
