(ns hood.fitness-test
  (:require [hood.fitness :refer :all]
            [clojure.test :refer :all]))

(defn application
  [name score requested]
  {:name name :score score :requested requested})

(def one (application "One" 1 100))
(def two (application "Two" 1 100))
(def three (application "Three" 1 100))
(def four (application "Four" 1 100))
(def five (application "Five" 1 100))

(def unequal-allocation {one 100
                         two 100
                         three 100
                         four 100
                         five 0})

(def equal-allocation {one 80
                       two 80
                       three 80
                       four 80
                       five 80})

(deftest several-identical-applications-test
  (testing "lin-p considers both allocations equal"
    (is (let [fit #(fitness :score lin-p %)]
          (= (fit unequal-allocation)
             (fit equal-allocation)))))
  (testing "quad-p prefers unequal allocation"
    (is (let [fit #(fitness :score quad-p %)]
          (> (fit unequal-allocation)
             (fit equal-allocation)))))
  (testing "sqrt-p prefers equal allocation"
    (is (let [fit #(fitness :score sqrt-p %)]
          (< (fit unequal-allocation)
             (fit equal-allocation))))))
