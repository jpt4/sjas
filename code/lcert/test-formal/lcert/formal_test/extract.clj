(ns lcert.formal-test.extract
  "Differential checks for ADR-0008's first extracted F7 encoder slice."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.encode]
            [lcert.formal.check-spec]
            [lcert.formal.extract :as extract]))

(deftest extracted-expression-encoder
  (let [fns (extract/compile-functions! '[uLbl encNat encE])
        encE (get fns 'encE)]
    (testing "the generated encoder follows the kernel's Code representation"
      (is (= [0 17] (encE [2])))
      (is (= [1 14 [0 2] [0 0]] (encE [11 0]))))
    (testing "the expected codes reduce in the kernel"
      (is (false? (b/rejects? []
                   '(Eq Code (encE Exp.tBool) (Code.sl 17)) '[(rfl)])))
      (is (false? (b/rejects? []
                   '(Eq Code (encE (Exp.var 0))
                     (Code.sn 14 (Code.sl 2) (Code.sl 0))) '[(rfl)]))))))

(deftest dependency-extraction-and-node-count
  (let [encE (extract/compile-closure! 'encE)
        cnodes (extract/compile-closure! 'cnodes)
        code (encE [11 0])]
    (is (= [1 14 [0 2] [0 0]] code))
    (is (= 1 (cnodes code)))
    (is (= 0 (cnodes [0 17])))))

(deftest recursive-checker-fuel-base
  ;; chkN uses a recursor inside its stored lambda. The direct-recursion
  ;; emitter cannot peel every binder, so this exercises generic lowering.
  (let [chkN (extract/compile-closure! 'chkN)]
    (is (false? (chkN (constantly nil) 0 [0 0] [0 0])))
    (is (false? (b/rejects? []
                 '(Eq Bool (chkN decCertPad 0 (Code.sl 0) (Code.sl 0)) Bool.false)
                 '[(rfl)])))))
