(ns tiler.trace-test
  "Tests for the trace export used by the replay page."
  (:require [clojure.test :refer [deftest is testing]]
            [tiler.trace :as trace]
            [tiler.agent :as agent]))

(deftest json-encoding
  (is (= "{\"a\":1,\"b\":[true,false,null],\"c\":\"q\\\"\\n\"}"
         (trace/->json (array-map :a 1 :b [true false nil] :c "q\"\n")))))

(deftest describing-a-change
  (testing "an answer-function edit"
    (is (= "answer: (* 2 x) → (* 3 x)"
           (trace/change agent/initial (#'trace/set-answer agent/initial '(* 3 x))))))
  (testing "an inserted statement"
    (is (= "insert: (if true (put :score 1000) nil)"
           (trace/change agent/initial (agent/insert-statement agent/initial '(if true (put :score 1000) nil) 2))))))

(deftest a-run-as-data
  (let [r (trace/run-data :same 11 400)]
    (is (= "same" (:guard r)))
    (is (= 400 (count (:correct r))))
    (is (pos? (:violations r)))
    (is (some #(re-find #"check \(get :main\)" (:answer-or-change %)) (:installs r))
        "the installed Löbian wirehead appears in the lineage")
    (is (every? #(contains? % :why) (:checks r)))))
