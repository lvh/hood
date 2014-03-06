(ns hood.constraint-test
  (:require [hood.constraint :refer :all]
            [clojure.test :refer :all]))

(def alice {:name "Alice", :score 5, :requested 120})
(def bob {:name "Bob", :score 4, :requested 100})
(def carol {:name "Carol", :score 3, :requested 80})
(def applications [alice bob carol])

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
      (= (map :arg1 subtargets) (#'hood.constraint/alloc-vars applications))
      (= (set (map (comp type :arg2) subtargets)) #{Long})))))

(deftest target-tests
  (testing "linear target is sane"
    (is (sane-target? (linear-target applications :score))))
  (testing "quadratic target is sane"
    (is (sane-target? (quadratic-target applications :score)))))

(deftest alloc-tests
  (testing "linear allocation on restricted budget"
    (is (= (alloc applications 200 (linear-target applications :score))
           {alice 120
            bob 80
            carol 0})))
  (testing "linear allocation on complete budget"
    (is (= (alloc applications 300 (linear-target applications :score))
           {alice 120
            bob 100
            carol 80})))
  (testing "quadratic allocation on restricted budget"
    (is (= (alloc applications 200 (quadratic-target applications :score))
           {alice 120
            bob 0 ;; bob gets penalized vs linear: incomplete grant!
            carol 80})))
  (testing "quadratic allocation on complete budget"
    (is (= (alloc applications 300 (quadratic-target applications :score))
           {alice 120
            bob 100
            carol 80}))))
