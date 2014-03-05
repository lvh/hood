(ns hood.core-test
  (:require [clojure.test :refer :all]
            [hood.core :refer :all]))

(deftest score-test
  (testing "scores don't fail on empty apps"
    (is (= (score* {}) 0)))
  (testing "a student"
    (is (= (score* {:student :true}) 5))))
