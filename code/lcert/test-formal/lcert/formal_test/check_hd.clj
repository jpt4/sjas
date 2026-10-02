(ns lcert.formal-test.check-hd
  "F7: recorded head steps and steps at paths (R4 §1.5–1.6)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-hd :as c]))

(def no-check '(fn [c :- Code, d :- Code] Bool.false))
(def yes-check '(fn [c :- Code, d :- Code] Bool.true))
(defn checked? [term value tactics]
  (not (b/rejects? [] (list 'Eq 'Bool term value) tactics)))

;; Each record exercises a different Hd constructor. The universal soundness
;; theorem checks correspondence with Hd; these computations ensure each
;; constructor actually accepts, including the two conditional rules.
(def head-records
  '[(HdDT.beta U.u1 Exp.tBool (Exp.var 0) Exp.tt)
    (HdDT.betaLet Exp.tUnit Exp.tUnit Exp.tt Exp.zero (Exp.var 1))
    (HdDT.iteT Exp.zero Exp.tt) (HdDT.iteF Exp.zero Exp.tt)
    (HdDT.elimT Exp.tNat Exp.zero Exp.tt) (HdDT.elimF Exp.tNat Exp.zero Exp.tt)
    (HdDT.recNZ Exp.tNat Exp.zero (Exp.var 1))
    (HdDT.recNS Exp.tNat Exp.zero (Exp.var 1) Exp.zero)
    (HdDT.caseLb Exp.tNat 0 (Exp.bcons Exp.zero Exp.bnil) Exp.zero)
    (HdDT.recSL Exp.tLbl (Exp.var 0) (Exp.var 4) (Exp.lbl 2))
    (HdDT.recSN Exp.tLbl (Exp.var 0) (Exp.var 4) (Exp.lbl 2)
      (Exp.sleaf (Exp.lbl 3)) (Exp.sleaf (Exp.lbl 4)))
    (HdDT.itRL Exp.tNat Exp.zero Exp.star (Exp.lbl 2))
    (HdDT.itRN Exp.tNat Exp.zero Exp.star (Exp.var 0) (Exp.lbl 2)
      (Exp.leaf (Exp.lbl 3)) (Exp.leaf (Exp.lbl 4)))
    (HdDT.prnL (Exp.lbl 2))
    (HdDT.prnN (Exp.var 0) (Exp.lbl 2) (Exp.leaf (Exp.lbl 3)) (Exp.leaf (Exp.lbl 4)))
    (HdDT.delta (Exp.sleaf (Exp.lbl 2)) (Exp.sleaf (Exp.lbl 3)) (Code.sl 2) (Code.sl 3))
    HdDT.tTT HdDT.tTF])

(deftest all-head-rules-compute
  (is (= 18 (count c/hd-rules) (count head-records)))
  (doseq [record head-records]
    (is (checked? (list 'hdCheck no-check record (list 'hdSource record)
                   (list 'hdTarget no-check record)) 'Bool.true '[(simp [hdCheck hdSide nthB])])
        (str record))))

(deftest head-and-path-soundness
  (doseq [c '[HdDT hdSource hdTarget hdSide hdCheck hdCheck_sound stepCheck stepCheck_sound
              f7OptExp_sound f7OptCode_sound f7Hd_transport]]
    (is (b/has? c) (str c)))
  (doseq [rule '[beta betaLet iteT iteF elimT elimF recNZ recNS caseLb
                recSL recSN itRL itRN prnL prnN delta tTT tTF]]
    (is (b/has? (symbol (str "hdCheck_" rule "_sound"))) (str rule)))
  (testing "a beta redex accepts its substituted body, rejects a false contractum"
    (is (not (b/rejects? '[]
      '(Eq Bool (hdCheck (fn [c :- Code, d :- Code] Bool.false)
                  (HdDT.beta U.u1 Exp.tBool (Exp.var 0) Exp.tt)
                  (Exp.app (Exp.lam U.u1 Exp.tBool (Exp.var 0)) Exp.tt) Exp.tt) Bool.true)
      '[(rfl)])))
    (is (b/rejects? '[]
      '(Eq Bool (hdCheck (fn [c :- Code, d :- Code] Bool.false)
                  (HdDT.beta U.u1 Exp.tBool (Exp.var 0) Exp.tt)
                  (Exp.app (Exp.lam U.u1 Exp.tBool (Exp.var 0)) Exp.tt) Exp.ff) Bool.true)
      '[(rfl)]))))

(deftest head-side-conditions-and-substitution
  (testing "delta reads both codes and obeys chkf's actual answer"
    (let [record (nth head-records 15), source '(Exp.chk (Exp.sleaf (Exp.lbl 2)) (Exp.sleaf (Exp.lbl 3)))]
      (is (checked? (list 'hdCheck yes-check record source 'Exp.tt) 'Bool.true '[(rfl)]))
      (is (checked? (list 'hdCheck no-check record source 'Exp.ff) 'Bool.true '[(rfl)]))
      (is (checked? (list 'hdCheck no-check record source 'Exp.tt) 'Bool.false '[(rfl)])))
    (doseq [record '[(HdDT.delta Exp.star (Exp.sleaf (Exp.lbl 3)) (Code.sl 2) (Code.sl 3))
                    (HdDT.delta (Exp.sleaf (Exp.lbl 2)) Exp.star (Code.sl 2) (Code.sl 3))
                    (HdDT.delta (Exp.sleaf (Exp.lbl 2)) (Exp.sleaf (Exp.lbl 3)) (Code.sl 9) (Code.sl 3))
                    (HdDT.delta (Exp.sleaf (Exp.lbl 2)) (Exp.sleaf (Exp.lbl 3)) (Code.sl 2) (Code.sl 9))]]
      (is (checked? (list 'hdCheck yes-check record (list 'hdSource record) 'Exp.tt) 'Bool.false '[(rfl)]))))
  (testing "caseLbl rejects an absent branch and a falsely recorded branch"
    (doseq [record '[(HdDT.caseLb Exp.tNat 0 Exp.bnil Exp.zero)
                    (HdDT.caseLb Exp.tNat 0 (Exp.bcons Exp.tt Exp.bnil) Exp.zero)]]
      (is (checked? (list 'hdCheck no-check record (list 'hdSource record) 'Exp.zero)
                    'Bool.false '[(simp [hdCheck hdSide nthB])]))))
  (testing "let substitution uses y at index 0 and x at index 1"
    (is (checked? (list 'expEq '(hdTarget (fn [c :- Code, d :- Code] Bool.false)
                                (HdDT.betaLet Exp.tUnit Exp.tUnit Exp.tt Exp.zero (Exp.var 1))) 'Exp.tt)
                  'Bool.true '[(simp [hdTarget substL instL subst substF])]))))

(deftest recorded-path-checks
  (let [record '(HdDT.iteT Exp.zero Exp.tt)
        source '(Exp.succ (Exp.ite Exp.tt Exp.zero Exp.tt))
        target '(Exp.succ Exp.zero), path '(List.cons Nat 0 (List.nil Nat))]
    (is (checked? (list 'stepCheck no-check path record source target) 'Bool.true '[(rfl)]))
    (testing "an invalid path cannot exploit setP's fallback"
      (is (checked? (list 'stepCheck no-check '(List.cons Nat 9 (List.nil Nat)) record source source)
                    'Bool.false '[(rfl)])))
    (testing "the entire replaced expression is compared"
      (is (checked? (list 'stepCheck no-check path record source 'Exp.zero) 'Bool.false '[(rfl)]))))
  (testing "a change in a sibling is rejected"
    (is (checked? (list 'stepCheck no-check '(List.cons Nat 0 (List.nil Nat)) '(HdDT.tTT)
                       '(Exp.tPi U.u1 (Exp.tT Exp.tt) Exp.tBool)
                       '(Exp.tPi U.u1 Exp.tUnit Exp.tNat)) 'Bool.false '[(rfl)]))))
