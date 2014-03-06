(ns hood.constraint
  [:require
   [loco.core :refer [solution]]
   [loco.constraints :refer :all]])

(defn alloc
  "Allocates using a constraint solver.

  apps is a seq of all applications.
  budget is the total budget.
  target is the term to optimize.
  "
  [apps budget target]
  (let [n (count apps)
        allocs (for [i (range (count apps))] [:allocation i])
        grant-constrs (for [i (range n)]
                        ($in [:allocation i]
                             0 (:requested (apps i))))
        budget-constr ($<= (apply $+ allocs) budget)
        constraints (conj grant-constrs budget-constr)]
    (solution constraints :maximize target)))

(defn linear-target
  [apps score]
  (let [ratios (map #(/ (score %) (:requested %)) apps)
        scale (/ 100 (- (apply max ratios) (apply min ratios)))
        scaled-ratios (vec (map #(long (* scale %)) ratios))
        scaled-allocs (for [i (range (count apps))]
                        ($* [:allocation i] (scaled-ratios i)))]
    (apply $+ scaled-allocs)))
