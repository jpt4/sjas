(ns lcert.f7-shell-test
  "The untrusted parser bridge must preserve the term checked by F7."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.extracted.runtime :as rt]
            [lcert.f7-shell :as f7]
            [lcert.typing :as typing]))

(def ^:private examples
  ;; Each expected constructor number comes from formal/syntax.clj's Exp
  ;; declaration, independently of the bridge's implementation.
  [[[:Empty] 0] [[:Unit] 1] [[:Bool] 2] [[:Nat] 3]
   [[:Lbl] 4] [[:Syn] 5] [[:Dia] 6] [[:R] 7]
   [[:T [:tt]] 8] [[:Pi 1 [:Bool] [:Bool]] 9]
   [[:Sigma :w [:Nat] [:Nat]] 10] [[:var 2] 11]
   [[:star] 12] [[:abort [:Nat] [:var 0]] 13]
   [[:tt] 14] [[:ff] 15] [[:if [:tt] [:zero] [:zero]] 16]
   [[:elimBool [:Nat] [:tt] [:zero] [:zero]] 17]
   [[:zero] 18] [[:succ [:zero]] 19]
   [[:recN [:Nat] [:zero] [:var 0] [:zero]] 20]
   [[:lbl :a] 21] [[:caseLbl [:Nat] [:lbl :a] [[:zero] [:zero]]] 22]
   [[:sleaf [:lbl :a]] 25]
   [[:snode [:lbl :a] [:sleaf [:lbl :a]] [:sleaf [:lbl :b]]] 26]
   [[:recSyn [:Nat] [:zero] [:zero] [:sleaf [:lbl :a]]] 27]
   [[:leaf [:lbl :a]] 28]
   [[:node [:var 0] [:lbl :a] [:leaf [:lbl :b]] [:leaf [:lbl :c]]] 29]
   [[:itR [:Nat] [:var 0] [:var 0] [:leaf [:lbl :a]]] 30]
   [[:print [:var 0]] 31] [[:lam 0 [:Unit] [:star]] 32]
   [[:app [:var 1] [:var 0]] 33]
   [[:pair [:Sigma 1 [:Unit] [:Unit]] [:star] [:star]] 34]
   [[:let [:Unit] [:var 0] [:star]] 35]
   [[:chk [:var 0] [:var 1]] 36]
   [[:h1 [:var 0] [:var 1] [:var 2] [:var 3] [:var 4]] 37]
   [[:reflect [:Unit] [:var 0] [:var 1]] 38]
   [[:inspect [:Nat] [:var 0] [:var 1] [:zero] [:zero]] 39]])

(deftest parsed-expressions-are-the-checked-expressions
  (doseq [[source tag] examples]
    (testing (pr-str source)
      (let [value (f7/exp source)]
        (is (= tag (first value)))
        (is (= [value] (rt/decE (rt/encE value)))))))
  (testing "usage and labels have their canonical indices"
    (is (= [32 [0] [1] [12]] (f7/exp [:lam 0 [:Unit] [:star]])))
    (is (= [32 [1] [1] [12]] (f7/exp [:lam 1 [:Unit] [:star]])))
    (is (= [32 [2] [1] [12]] (f7/exp [:lam :w [:Unit] [:star]])))
    (is (= [21 97] (f7/exp [:lbl :a]))))
  (testing "malformed expressions cannot be silently recast as a checked term"
    (is (thrown? clojure.lang.ExceptionInfo (f7/exp [:unknown])))
    (is (thrown? clojure.lang.ExceptionInfo (f7/exp [:lam 1 [:Unit]])))
    (is (thrown? clojure.lang.ExceptionInfo (f7/exp [:lbl :not-a-label])))))

(deftest closed-constant-derivations-produce-f7-certificates
  (doseq [[term type] [[[:star] [:Unit]] [[:tt] [:Bool]]
                       [[:ff] [:Bool]] [[:zero] [:Nat]]
                       [[:lbl :a] [:Lbl]]]]
    (testing (pr-str term)
      (let [d (typing/check-top 0 term)
            cert (f7/certificate 0 d)]
        (is (= type (:type d)))
        (is (= [31 nil nil (f7/exp term) (f7/exp type)] (f7/dt d)))
        (is (= [0 nil (f7/exp type)] (f7/dt (typing/tf [] type))))
        (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp type))))))))
  (let [d (typing/check-top 0 [:star])
        cert (f7/certificate 0 d)]
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))
    (is (false? (rt/Check rt/decCert (assoc cert 1 0)
                          (rt/encE (f7/exp [:Unit])))))))

(deftest lambda-derivations-preserve-context-and-usage
  (doseq [term [[:lam 0 [:Unit] [:star]]
                [:lam 1 [:Unit] [:var 0]]
                [:lam :w [:Unit] [:var 0]]]]
    (testing (pr-str term)
      (let [d (typing/check-top 0 term)
            cert (f7/certificate 0 d)
            type (:type d)]
        (is (= :Lam (:rule d)))
        (is (= [9 (case (second term) 0 [0] 1 [1] :w [2]) [1] [1]]
               (f7/exp type)))
        (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp type)))))
        (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Unit]))))))))
  (testing "the bound variable records the innermost context entry"
    (let [d (second (:prems (typing/check-top 0 [:lam 1 [:Unit] [:var 0]])))]
      (is (= [30 (list [1]) (list [1]) 0 [1] [1]] (f7/dt d)))))
  (testing "two binders keep the outer variable at de Bruijn index one"
    (let [term [:lam :w [:Unit] [:lam 0 [:Unit] [:var 1]]]
          d (typing/check-top 0 term)
          cert (f7/certificate 0 d)]
      (is (true? (rt/Check rt/decCert cert
                            (rt/encE (f7/exp (:type d)))))))))
