(ns hood.constraint-test
  (:require [hood.constraint :refer :all]
            [clojure.test :refer :all]))

(def applications
  [{:name "Alice", :score 5, :requested 120}
   {:name "Bob", :score 4, :requested 100}
   {:name "Carol", :score 3, :requested 80}])

;; Keep in mind that most applications won't actually have a score
;; attribute. In this case, we're doing it so that the scoring
;; function is just :score, and that it's easy to tweak.

(defn sane-target?
  "Basic sanity checking for a target function.

  Checks that this target is a sum of products of allocation vectors
  times long weights.
  "
  [target]
  (and
   (= (:type target) :+)
   (let [subtargets (:args target)]
     (and
      (= (set (map :type subtargets)) #{:*})
      (= (map :arg1 subtargets) [[:allocation 0] [:allocation 1] [:allocation 2]])
      (= (set (map (comp type :arg2) subtargets)) #{Long})))))

(deftest target-tests
  (testing "linear target produces k * var constraints"
    (is (sane-target? (linear-target applications :score)))))

(deftest alloc-tests
  (testing "linear allocation of test applications"
    (is (= (alloc applications 200 (linear-target applications :score))
           {}))))
