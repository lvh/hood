(ns hood.core-test
  (:require [clojure.test :refer :all]
            [hood.greedy :refer :all]))

(deftest needs-alloc?-test
  (testing "is reviewed"
    (is (needs-alloc? {:state :reviewed})))
  (testing "is requesting more"
    (is (needs-alloc? {:state :requesting-more})))
  (testing "is something else"
    (is (not (needs-alloc? {:state :something-else})))))

(deftest one-pass-alloc-tests
  (testing "one app, requests < offer"
    (let [app {:requested 100}]
      (is (= (one-pass-alloc [app] 200 true)
             {app 100}))
      (is (= (one-pass-alloc [app] 200 false)
             {app 100}))))
  (testing "one app, requests > offer, not accepting less"
    (is (= (one-pass-alloc [{:requested 100}] 1 false)
           {})))
  (testing "one app, requests > offer, accepting less"
    (is (let [app {:requested 100}]
          (= (one-pass-alloc [app] 1 true)
             {app 1})))))
