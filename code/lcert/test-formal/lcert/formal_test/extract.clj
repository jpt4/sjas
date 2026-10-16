(ns lcert.formal-test.extract
  "Differential checks for ADR-0008's first extracted F7 encoder slice."
  (:require [clojure.test :refer [deftest is testing]]
            [ansatz.core :as a]
            [ansatz.codegen :as cg]
            [ansatz.surface.elaborate :as el]
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

(def ^:private fixed-star-decoder
  '(fn [c :- Code]
     (Option.some (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))
       (Prod.mk Nat (Prod Exp (Prod Exp (Prod DT DT))) 0
         (Prod.mk Exp (Prod Exp (Prod DT DT)) Exp.star
           (Prod.mk Exp (Prod DT DT) Exp.tUnit
             (Prod.mk DT DT (DT.rConst (List.nil Exp) (List.nil U) Exp.star Exp.tUnit)
                            (DT.fBase (List.nil Exp) Exp.tUnit))))))))

(deftest extracted-checker-accepts-and-rejects
  (let [E @a/ansatz-env
        decoder (eval (cg/ansatz->clj E (el/elaborate E (b/lv fixed-star-decoder)) []))
        check (extract/compile-closure! 'Check)
        encode (extract/compile-closure! 'encE)
        code [1 0 [0 0] [0 0]]]
    (is (true? (check decoder code (encode [1]))))
    (is (false? (check decoder code (encode [2]))))
    (is (false? (check decoder [0 0] (encode [1]))))))

(deftest polymorphic-option-bind-from-checked-definition
  (let [ob (extract/compile-closure! 'ob)
        f (fn [x] [(inc x)])]
    (is (= [6] (ob [5] f)))
    (is (nil? (ob nil f)))
    (is (false? (b/rejects? []
                 '(Eq (Option Nat)
                   (ob Nat Nat (Option.some Nat 5)
                     (fn [x :- Nat] (Option.some Nat (Nat.succ x))))
                   (Option.some Nat 6)) '[(rfl)])))))

(deftest polymorphic-option-map-from-checked-definition
  (let [om (extract/compile-closure! 'om)]
    (is (= [6] (om inc [5])))
    (is (nil? (om inc nil)))
    (is (false? (b/rejects? []
                 '(Eq (Option Nat)
                   (om Nat Nat Nat.succ (Option.some Nat 5))
                   (Option.some Nat 6)) '[(rfl)])))))
