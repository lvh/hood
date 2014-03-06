(ns hood.score-test
  (:require [hood.score :refer :all]
            [clojure.test :refer :all]))

(deftest score-test
  (testing "scores don't fail on empty apps"
    (is (= (score* {}) 1)))
  (testing "a student"
    (is (= (score* {:student :true}) 6))))
