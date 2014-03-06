(ns hood.score)

(def score-funcs
  [[1 (constantly 1)]
   [5 (fn [a] (if (:student a) 1 0))]])

(defn score*
  "Computes the score of an allocation."
  [a]
  (reduce (fn [acc [w f]]
            (+ acc (* w (f a))))
          0 score-funcs))

(def score
  (memoize score*))
