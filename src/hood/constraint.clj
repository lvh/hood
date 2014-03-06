(ns hood.constraint
  [:require
   [loco.core :refer [solution]]
   [loco.constraints :refer :all]])

(defn ^:private alloc-vars
  "The allocation constraint variables for a seq of applications."
  [apps]
  (for [a apps] [:allocation a]))

(defn grant-constraint
  "An application will get between zero and the requested amount of
  dollars."
  [app]
  ($in [:allocation app] 0 (:requested app)))

(defn ^:private solve
  "Throw the constraint problem into loco."
  [apps budget target]
  (let [allocs (alloc-vars apps)
        grant-constraints (map grant-constraint apps)
        within-budget ($<= (apply $+ allocs) budget)
        constraints (conj grant-constraints within-budget)]
    (solution constraints :maximize target)))

(defn ^:private soln-to-map
  [soln]
  (into {} (for [[[tag application] grant] soln] [application grant])))

(def alloc
  "Allocates using a constraint solver.

  apps is a seq of all applications.
  budget is the total budget.
  target is the term to optimize.
  "
  (comp soln-to-map solve))

(defn linear-target
  [apps score]
  (let [ratios (map #(/ (score %) (:requested %)) apps)
        scale (/ 100 (- (apply max ratios) (apply min ratios)))
        scaled-ratios (vec (map #(long (* scale %)) ratios))
        scaled-allocs (map $* (alloc-vars apps) scaled-ratios)]
    (apply $+ scaled-allocs)))
