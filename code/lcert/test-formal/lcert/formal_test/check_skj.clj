(ns lcert.formal-test.check-skj
  "F7: skeleton derivation trees check all 40 rules, including hidden skeletons."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-skj :as c]))

(defn list-sk [xs] (reduce (fn [tail x] (list 'List.cons 'Sk x tail)) '(List.nil Sk) (reverse xs)))
(def method-sk '(Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr Sk.unit (Sk.arr Sk.unit Sk.unit)))))
(def fixture-context
  (list-sk ['Sk.unit 'Sk.bool 'Sk.nat 'Sk.lbl 'Sk.syn 'Sk.dia 'Sk.cert
            '(Sk.arr Sk.lbl Sk.unit) method-sk '(Sk.arr Sk.unit Sk.unit) '(Sk.prod Sk.unit Sk.unit)]))

;; Explicit successful certificates for all 40 rules. Expressions and
;; skeletons below are expectations written separately from the checker.
;; Variables in the fixture context expose wrong premise skeletons; binder
;; cases also exercise the extended contexts (recSyn has five new entries).
(def skeleton-fixtures
  (concat
    (map (fn [rule e] [(symbol (str "SkDT." rule)) 'Bool.true e 'Sk.unit])
      '[wEmpty wUnit wBool wNat wLbl wSyn wDia wR]
      '[Exp.tEmpty Exp.tUnit Exp.tBool Exp.tNat Exp.tLbl Exp.tSyn Exp.tDia Exp.tR])
    '[[ (SkDT.wT Exp.tt SkDT.sTT) Bool.true (Exp.tT Exp.tt) Sk.unit]
      [(SkDT.wPi U.u1 Exp.tUnit Exp.tUnit SkDT.wUnit SkDT.wUnit)
       Bool.true (Exp.tPi U.u1 Exp.tUnit Exp.tUnit) Sk.unit]
      [(SkDT.wSig U.u1 Exp.tUnit Exp.tUnit SkDT.wUnit SkDT.wUnit)
       Bool.true (Exp.tSig U.u1 Exp.tUnit Exp.tUnit) Sk.unit]
      [(SkDT.sVar 1 Sk.bool) Bool.false (Exp.var 1) Sk.bool]
      [SkDT.sStar Bool.false Exp.star Sk.unit]
      [(SkDT.sAbort Exp.tUnit (Exp.var 0) SkDT.wUnit (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.abort Exp.tUnit (Exp.var 0)) Sk.unit]
      [SkDT.sTT Bool.false Exp.tt Sk.bool] [SkDT.sFF Bool.false Exp.ff Sk.bool]
      [(SkDT.sIte (Exp.var 1) (Exp.var 0) Exp.star Sk.unit (SkDT.sVar 1 Sk.bool) (SkDT.sVar 0 Sk.unit) SkDT.sStar)
       Bool.false (Exp.ite (Exp.var 1) (Exp.var 0) Exp.star) Sk.unit]
      [(SkDT.sElimB Exp.tUnit (Exp.var 1) (Exp.var 0) Exp.star SkDT.wUnit (SkDT.sVar 1 Sk.bool) (SkDT.sVar 0 Sk.unit) SkDT.sStar)
       Bool.false (Exp.elimB Exp.tUnit (Exp.var 1) (Exp.var 0) Exp.star) Sk.unit]
      [SkDT.sZero Bool.false Exp.zero Sk.nat]
      [(SkDT.sSucc (Exp.var 2) (SkDT.sVar 2 Sk.nat)) Bool.false (Exp.succ (Exp.var 2)) Sk.nat]
      [(SkDT.sRecN Exp.tUnit (Exp.var 0) (Exp.var 0) (Exp.var 2) SkDT.wUnit (SkDT.sVar 0 Sk.unit) (SkDT.sVar 0 Sk.unit) (SkDT.sVar 2 Sk.nat))
       Bool.false (Exp.recN Exp.tUnit (Exp.var 0) (Exp.var 0) (Exp.var 2)) Sk.unit]
      [(SkDT.sLbl 7) Bool.false (Exp.lbl 7) Sk.lbl]
      [(SkDT.sCaseL Exp.tUnit (Exp.var 3) Exp.bnil SkDT.wUnit (SkDT.sVar 3 Sk.lbl) (SkDT.sBnil Sk.unit))
       Bool.false (Exp.caseL Exp.tUnit (Exp.var 3) Exp.bnil) Sk.unit]
      [(SkDT.sBnil Sk.unit) Bool.false Exp.bnil (Sk.arr Sk.lbl Sk.unit)]
      [(SkDT.sBcons (Exp.var 0) Exp.bnil Sk.unit (SkDT.sVar 0 Sk.unit) (SkDT.sBnil Sk.unit))
       Bool.false (Exp.bcons (Exp.var 0) Exp.bnil) (Sk.arr Sk.lbl Sk.unit)]
      [(SkDT.sSleaf (Exp.var 3) (SkDT.sVar 3 Sk.lbl)) Bool.false (Exp.sleaf (Exp.var 3)) Sk.syn]
      [(SkDT.sSnode (Exp.var 3) (Exp.var 4) (Exp.var 4) (SkDT.sVar 3 Sk.lbl) (SkDT.sVar 4 Sk.syn) (SkDT.sVar 4 Sk.syn))
       Bool.false (Exp.snode (Exp.var 3) (Exp.var 4) (Exp.var 4)) Sk.syn]
      [(SkDT.sRecS Exp.tUnit Exp.star (Exp.var 0) (Exp.var 4) SkDT.wUnit SkDT.sStar (SkDT.sVar 0 Sk.unit) (SkDT.sVar 4 Sk.syn))
       Bool.false (Exp.recS Exp.tUnit Exp.star (Exp.var 0) (Exp.var 4)) Sk.unit]
      [(SkDT.sLeaf (Exp.var 3) (SkDT.sVar 3 Sk.lbl)) Bool.false (Exp.leaf (Exp.var 3)) Sk.cert]
      [(SkDT.sNode (Exp.var 5) (Exp.var 3) (Exp.var 6) (Exp.var 6) (SkDT.sVar 5 Sk.dia) (SkDT.sVar 3 Sk.lbl) (SkDT.sVar 6 Sk.cert) (SkDT.sVar 6 Sk.cert))
       Bool.false (Exp.node (Exp.var 5) (Exp.var 3) (Exp.var 6) (Exp.var 6)) Sk.cert]]
    [[(list 'SkDT.sItR 'Exp.tUnit '(Exp.var 7) '(Exp.var 8) '(Exp.var 6) 'SkDT.wUnit
        '(SkDT.sVar 7 (Sk.arr Sk.lbl Sk.unit)) (list 'SkDT.sVar 8 method-sk) '(SkDT.sVar 6 Sk.cert))
      'Bool.false '(Exp.itR Exp.tUnit (Exp.var 7) (Exp.var 8) (Exp.var 6)) 'Sk.unit]]
    '[[ (SkDT.sPrn (Exp.var 6) (SkDT.sVar 6 Sk.cert)) Bool.false (Exp.prn (Exp.var 6)) Sk.syn]
      [(SkDT.sLam U.u1 Exp.tUnit (Exp.var 0) Sk.unit SkDT.wUnit (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.lam U.u1 Exp.tUnit (Exp.var 0)) (Sk.arr Sk.unit Sk.unit)]
      [(SkDT.sApp (Exp.var 9) (Exp.var 0) Sk.unit Sk.unit (SkDT.sVar 9 (Sk.arr Sk.unit Sk.unit)) (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.app (Exp.var 9) (Exp.var 0)) Sk.unit]
      [(SkDT.sPair U.u1 Exp.tUnit Exp.tUnit (Exp.var 0) (Exp.var 0) (SkDT.wSig U.u1 Exp.tUnit Exp.tUnit SkDT.wUnit SkDT.wUnit) (SkDT.sVar 0 Sk.unit) (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.pair (Exp.tSig U.u1 Exp.tUnit Exp.tUnit) (Exp.var 0) (Exp.var 0)) (Sk.prod Sk.unit Sk.unit)]
      [(SkDT.sLetp Exp.tUnit (Exp.var 10) (Exp.var 0) Sk.unit Sk.unit SkDT.wUnit (SkDT.sVar 10 (Sk.prod Sk.unit Sk.unit)) (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.letp Exp.tUnit (Exp.var 10) (Exp.var 0)) Sk.unit]
      [(SkDT.sChk (Exp.var 4) (Exp.var 4) (SkDT.sVar 4 Sk.syn) (SkDT.sVar 4 Sk.syn))
       Bool.false (Exp.chk (Exp.var 4) (Exp.var 4)) Sk.bool]
      [(SkDT.sH1 (Exp.var 6) (Exp.var 6) (Exp.var 4) (Exp.var 0) (Exp.var 0) (SkDT.sVar 6 Sk.cert) (SkDT.sVar 6 Sk.cert) (SkDT.sVar 4 Sk.syn) (SkDT.sVar 0 Sk.unit) (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.h1 (Exp.var 6) (Exp.var 6) (Exp.var 4) (Exp.var 0) (Exp.var 0)) Sk.unit]
      [(SkDT.sRefl Exp.tUnit (Exp.var 6) (Exp.var 0) SkDT.wUnit (SkDT.sVar 6 Sk.cert) (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.refl Exp.tUnit (Exp.var 6) (Exp.var 0)) Sk.unit]
      [(SkDT.sInsp Exp.tUnit (Exp.var 6) (Exp.var 4) (Exp.var 0) (Exp.var 0) SkDT.wUnit (SkDT.sVar 6 Sk.cert) (SkDT.sVar 4 Sk.syn) (SkDT.sVar 0 Sk.unit) (SkDT.sVar 0 Sk.unit))
       Bool.false (Exp.insp Exp.tUnit (Exp.var 6) (Exp.var 4) (Exp.var 0) (Exp.var 0)) Sk.unit]]))

(defn computes? [tree mode context e s expected]
  (not (b/rejects? [] (list 'Eq 'Bool (list 'skjCheck mode context e s tree) expected)
                  '[(simp [skjCheck skjCheckF nthS])])) )

(deftest every-skeleton-rule-computes
  (is (= 40 (count skeleton-fixtures)))
  (doseq [[tree mode e s] skeleton-fixtures]
    (is (computes? tree mode fixture-context e s 'Bool.true) (str tree))
    ;; Every constructor rejects a false mode and a false expression. This
    ;; guards the common conclusion checks as well as individual rule code.
    (is (computes? tree (if (= mode 'Bool.true) 'Bool.false 'Bool.true)
                    fixture-context e s 'Bool.false) (str :wrong-mode tree))
    (is (computes? tree mode fixture-context '(Exp.tBrs Exp.tUnit 0) s 'Bool.false)
        (str :wrong-expression tree))))

(deftest malformed-skeleton-premises
  (testing "a recorded domain handles an application whose function is a branch list"
    (is (computes? '(SkDT.sApp Exp.bnil (Exp.lbl 0) Sk.lbl Sk.nat
                      (SkDT.sBnil Sk.nat) (SkDT.sLbl 0))
                    'Bool.false '(List.nil Sk) '(Exp.app Exp.bnil (Exp.lbl 0)) 'Sk.nat 'Bool.true))
    ;; SkJ permits this term; Cv's separate nbr condition excludes it.
    ;; Skeleton checking must not silently import the conversion restriction.
    (is (not (b/rejects? '[] '(Eq Bool (nbr (Exp.app Exp.bnil (Exp.lbl 0))) Bool.false) '[(rfl)]))))
  (testing "lookup must exist and match the recorded skeleton"
    (is (computes? '(SkDT.sVar 0 Sk.nat) 'Bool.false '(List.nil Sk) '(Exp.var 0) 'Sk.nat 'Bool.false))
    (is (computes? '(SkDT.sVar 0 Sk.nat) 'Bool.false fixture-context '(Exp.var 0) 'Sk.nat 'Bool.false)))
  (testing "a valid child certificate for the wrong expression cannot be reused"
    (is (computes? '(SkDT.sSucc Exp.zero SkDT.sTT) 'Bool.false fixture-context
                    '(Exp.succ Exp.zero) 'Sk.nat 'Bool.false)))
  (testing "reflect checks the base-type restriction"
    (is (computes? '(SkDT.sRefl (Exp.tPi U.u1 Exp.tUnit Exp.tUnit) (Exp.var 6) Exp.star
                      (SkDT.wPi U.u1 Exp.tUnit Exp.tUnit SkDT.wUnit SkDT.wUnit)
                      (SkDT.sVar 6 Sk.cert) SkDT.sStar)
                    'Bool.false fixture-context '(Exp.refl (Exp.tPi U.u1 Exp.tUnit Exp.tUnit) (Exp.var 6) Exp.star)
                    '(Sk.arr Sk.unit Sk.unit) 'Bool.false))))

(deftest skeleton-checker-soundness
  (doseq [nm '[SkDT f7SkExp f7SkExp_skel f7SkEq f7SkEq_sound
               f7BoolEq f7BoolEq_sound f7OptSk f7OptSk_sound
               f7SkJ_transport skjCheckF skjCheck skjCheck_sound]]
    (is (b/has? nm) (str nm)))
  (is (= 40 (count c/skj-rules)))
  (doseq [rule c/skj-rules]
    (is (b/has? (symbol (str "skjCheck_" (first rule) "_sound"))) (str (first rule))))
  (testing "a branch list records its otherwise undetermined codomain"
    (is (not (b/rejects? '[]
      '(Eq Bool (skjCheck Bool.false (List.nil Sk) Exp.bnil
                  (Sk.arr Sk.lbl Sk.nat) (SkDT.sBnil Sk.nat)) Bool.true)
      '[(rfl)])))
    (is (b/rejects? '[]
      '(Eq Bool (skjCheck Bool.false (List.nil Sk) Exp.bnil
                  (Sk.arr Sk.lbl Sk.bool) (SkDT.sBnil Sk.nat)) Bool.true)
      '[(rfl)]))))
