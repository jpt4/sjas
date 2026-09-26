(ns lcert.verified-test
  "Tests of the Ansatz-verified kernel (ansatz/lcert/verified.clj).

  Loading lcert.verified already ran the Ansatz elaborator and kernel on every
  definition and theorem; a failure there aborts the extended suite.  These
  tests check that the theorems are really in the kernel's environment, and
  that the compiled, verified measures agree with the plain ones."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.verified :as v]
            [lcert.kernel :as k]
            [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.check-test :as ct]
            [lcert.core :as lc]))

(deftest the-theorems-are-kernel-checked
  (doseq [nm v/theorem-names]
    (is (v/declared? nm) (str nm " is in the Ansatz environment"))))

(deftest the-drafts-counts-are-kernel-computed
  (testing "the §2.7 counts were proved by kernel computation (rfl) at load time"
    (is (every? v/declared? ["not_cert_nodes" "not_cert_budget"]))
    (is (= 35 (v/nodes (v/code->ct ct/c-not))))))

(defn- random-code
  "A random code of depth at most d over a few labels."
  [rng d]
  (if (or (zero? d) (< (.nextDouble ^java.util.Random rng) 0.3))
    [:sl (rand-nth [:a :b :nil :empty])]
    [:sn (rand-nth [:a :cons :extw :ext1 :has :Lam]) (random-code rng (dec d)) (random-code rng (dec d))]))

(deftest verified-and-plain-measures-agree
  (let [codes (concat [ct/c-not]
                      (map (fn [[n form]] (:code (lc/certify n form))) ct/corpus)
                      (let [rng (java.util.Random. 42)] (repeatedly 300 #(random-code rng 8))))]
    (doseq [c codes]
      (let [ct (v/code->ct c)]
        (is (= (k/nodes* c) (v/nodes ct)))
        (is (= (k/ctxlen* c) (v/ctxlen ct)))
        (is (= (k/budget* c) (v/budget ct)))))))

(deftest the-runtime-uses-the-verified-measures
  (testing "the extended runner switched lcert.kernel to the Ansatz-compiled functions"
    (is (= :verified (k/implementation)))))
