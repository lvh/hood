(ns hood.core-test
  (:require [clojure.test :refer :all]
            [hood.core :refer :all]))

(deftest score-test
  (testing "scores don't fail on empty apps"
    (is (= (score* {}) 1)))
  (testing "a student"
    (is (= (score* {:student :true}) 6))))

(deftest needs-alloc?-test
  (testing "is reviewed"
    (is (needs-alloc? {:state :reviewed})))
  (testing "is requesting more"
    (is (needs-alloc? {:state :requesting-more})))
  (testing "is something else"
    (is (not (needs-alloc? {:state :something-else})))))
