(ns hood.fitness
  [:require
   [clojure.math.numeric-tower :refer [expt]]])

(defn fitness
  "The fitness of the given grant allocation"
  [score prob allocations]
  (reduce (fn [acc [app grant]]
            (+ acc
               (* (score app)
                  (prob app grant))))
          0 allocations))

(defn lin-p
  "A probability function that assumes that the probability someone
  attends is equal to the ratio of the grant money that they wanted
  that they actually received."
  [application granted]
  (/ granted (:requested application)))

(defn exp-p
  [n application granted]
  (expt (lin-p application granted) n))

(def quad-p (partial exp-p 2))
(def sqrt-p (partial exp-p 0.5))
