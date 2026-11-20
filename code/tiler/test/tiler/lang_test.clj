(ns tiler.lang-test
  "Tests for the machine's interpreter (the world's \"physics\").

  The physics executes whatever code it is given. It enforces nothing about
  safety: a `put` to any slot happens. These tests pin down that it is
  total and fuel-bounded, and that it never refuses a write."
  (:require [clojure.test :refer [deftest is testing]]
            [tiler.lang :as lang]))

(defn- run-form
  "Evaluate `form` against memory `mem` with `fuel` steps; return
  {:value v :mem m :out-of-fuel? b :puts [...]}."
  ([form mem] (run-form form mem 10000))
  ([form mem fuel]
   (lang/execute form {:mem mem :fuel fuel :mode :real
                       :check (constantly false)
                       :trial (constantly 0)
                       :mutate (fn [c _] c)})))

(deftest arithmetic-and-control
  (is (= 7 (:value (run-form '(+ 3 4) {}))))
  (is (= 12 (:value (run-form '(let [a 3 b (* a 4)] b) {}))))
  (is (= 1 (:value (run-form '(if (< 2 3) 1 2) {}))))
  (is (= 2 (:value (run-form '(if (< 3 2) 1 2) {}))))
  (is (= 4 (:value (run-form '(do 1 2 (inc 3)) {}))))
  (testing "total arithmetic: mod by zero and non-numbers give a value, not an error"
    (is (= 0 (:value (run-form '(mod 5 0) {}))))
    (is (nil? (:value (run-form '(+ 1 (quote a)) {}))))))

(deftest unknown-operators-evaluate-to-nil
  (is (nil? (:value (run-form '(launch-missiles 1) {}))))
  (is (= 3 (:value (run-form '(do (launch-missiles 1) 3) {})))))

(deftest answer-fn-is-transparent
  (is (= 6 (:value (run-form '(let [x 3] (answer-fn (* 2 x))) {})))))

(deftest memory-reads-and-writes
  (let [r (run-form '(do (put :answer (+ (get :x) 1)) (get :answer)) {:x 4})]
    (is (= 5 (:value r)))
    (is (= 5 (get-in r [:mem :answer])))
    (is (= [[:answer 5]] (:puts r)))))

(deftest physics-enforces-nothing
  (testing "a write to an environment-owned slot happens; the physics only records it"
    (let [r (run-form '(put :score 1000) {:score 3})]
      (is (= 1000 (get-in r [:mem :score])))
      (is (= [[:score 1000]] (:puts r)))))
  (testing "computed slot names are physically fine"
    (let [r (run-form '(put (first (list :score)) 9) {})]
      (is (= 9 (get-in r [:mem :score]))))))

(deftest fuel-bounds-every-run
  (let [r (run-form '(do (put :a 1) (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1)))))) {} 5)]
    (is (:out-of-fuel? r))
    (testing "writes made before the fuel ran out stay"
      (is (= 1 (get-in r [:mem :a]))))))

(deftest installed-code-runs-next-time-not-now
  (testing "a put to :main changes memory, but the running evaluation continues on the old form"
    (let [r (run-form '(do (put :main (quote (put :answer 2))) (put :answer 1)) {})]
      (is (= 1 (get-in r [:mem :answer])))
      (is (= '(put :answer 2) (get-in r [:mem :main]))))))

(deftest run-uses-strictly-less-fuel
  (testing "run executes code inline with at most half the remaining fuel"
    (let [r (lang/execute '(let [c (quote (put :answer 5))] (run c))
                          {:mem {} :fuel 1000 :mode :real
                           :check (constantly true) :trial (constantly 0)
                           :mutate (fn [c _] c)})]
      (is (= 5 (get-in r [:mem :answer])))))
  (testing "code run inline that loops out of fuel does not take the caller down"
    (let [deep (reduce (fn [f _] (list 'inc f)) 0 (range 400))
          r (lang/execute (list 'do (list 'let ['c (list 'quote deep)] (list 'run 'c)) '(put :answer 7))
                          {:mem {} :fuel 600 :mode :real
                           :check (constantly true) :trial (constantly 0)
                           :mutate (fn [c _] c)})]
      (is (= 7 (get-in r [:mem :answer]))))))

(deftest primitives-for-agents
  (is (= '((1 2) (3 4)) (:value (run-form '(push-example (list (list 1 2)) 3 4) {}))))
  (testing "push-example ignores a missing example and keeps at most 12"
    (is (= '((1 2)) (:value (run-form '(push-example (list (list 1 2)) nil 4) {}))))
    (is (= 12 (count (:value (run-form (reduce (fn [acc i] (list 'push-example acc i i)) '(list) (range 20)) {}))))))
  (is (integer? (:value (run-form '(next-seed 42) {})))))
