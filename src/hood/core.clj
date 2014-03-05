(ns hood.core
  (:require [clojure.set :refer [difference]]))

(def score-funcs
  [[5 (fn [a] (if (:student a) 1 0))]])

(defn score*
  "Computes the score of an allocation."
  [a]
  (reduce (fn [acc [w f]]
            (+ acc (* w (f a))))
          0 score-funcs))

(def score (memoize score))

(defn one-pass-alloc
  "Allocates funds to all people that are requesting less than their
  slice of the budget. Returns a map of applications to allocations.

  This should be called with the applications that still need to be
  allocated funds.

  "
  [applications budget accept-less]
  (let [total-score (apply + (map score applications))
        budget-slice (/ budget total-score)
        offer (fn [app]
                (min
                 (:requested-grant app)
                 (* (score app) budget-slice)))]
    (reduce (fn [allocs app]
              (if (or accept-less (== (:requested-grant app) (offer app)))
                (assoc allocs app (offer app))
                allocs))
            {} applications)))

(defn iter-alloc
  "Does one-pass allocs until they don't work no more.

  This has to be done iteratively (with a static budget per pass) in
  order to guarantee consistent results. Otherwise, suppose that you
  have two applications x1, x2 that request significantly fewer funds
  than their score would allow, and two applications y1, y2 that both
  request (perhaps only slightly) more. If we adjust the budget during
  processing, it's possible that in this processing order:

    y1 x1 x2 y2

  y1 doesn't get the increased grant but y2 does; because it happened
  to be processed after more funds became available in the budget.

  "
  [apps budget]
  (loop [rem-apps (set apps)
         rem-budget budget
         allocs #{}]
    (let [new-allocs (one-pass-alloc rem-apps rem-budget false)]
      (if (empty? new-allocs)
        [allocs rem-apps rem-budget]
        (recur [(difference rem-apps (set (keys new-allocs)))
                (- budget (apply + (values new-allocs)))
                (into allocs new-allocs)])))))

(defn needs-alloc?
  [app]
  (contains? #{:reviewed :requesting-more} (:state application)))

(defn alloc
  "Allocates funds.

  First does iter-alloc, then best-effort on the remaining ones.
  "
  [applications budget]
  (let [pool (set (filter needs-allocation? applications))
        [allocs rem-apps rem-budget] (iter-alloc pool budget)
        best-effort-allocs (one-pass-alloc rem-apps rem-budget true)]
    (into allocs best-effort-allocs)))
