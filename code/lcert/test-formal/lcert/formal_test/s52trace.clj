(ns lcert.formal-test.s52trace
  "Formal suite (ADR-0006), lcert.formal.s52trace: Theorem 5.2's safe traces
  (Trace52) are built by the evaluator's rules, one lemma per rule shared
  by evalₙ (Ok) and evalᴱₙ (OkE); a trace never roots at abort, H₁ or H."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52trace]))

(deftest f5-s52trace
  (testing "the shared rules, lifted to Trace52 for either evaluator"
    (doseq [c '[trace52_false trace52_true
                trace52_eVar trace52_eStar trace52_eTT trace52_eFF trace52_eZero trace52_eLbl
                trace52_eIteT trace52_eIteF trace52_eElimT trace52_eElimF
                trace52_eSucc trace52_eRecN trace52_eIterZ trace52_eIterS
                trace52_eCaseL trace52_eBnil trace52_eBcons
                trace52_eSleaf trace52_eSnode trace52_eRecS trace52_eRecSL trace52_eRecSN
                trace52_eLeaf trace52_eNode trace52_eItR trace52_eItL trace52_eItN trace52_ePrn
                trace52_eLam trace52_eApp trace52_ePair trace52_eLet trace52_eChk
                trace52_eReflNo trace52_eReflNone trace52_eInspT trace52_eInspF
                trace52_apClos trace52_apBnil trace52_apBconsZ trace52_apBconsS
                trace52_no_abort trace52_no_h1 trace52_no_H]]
      (is (b/has? c) (str c))))
  (testing "reflect at 0 (H) has no safe trace, even when its check succeeds"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code), n :- Nat, erasing :- Bool,
                      r :- Exp, e :- Exp, c :- Code,
                      hr :- (Trace52 chkf dec encTy n erasing (EvSrc.tm (List.nil RV) r) (RV.cert c)),
                      he :- (Trace52 chkf dec encTy n erasing (EvSrc.tm (List.nil RV) e) RV.star),
                      hb :- (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy Exp.tEmpty))) Bool.false)]
                    '(Trace52 chkf dec encTy n erasing
                       (EvSrc.tm (List.nil RV) (Exp.refl Exp.tEmpty r e)) (rdflt Sk.unit))
                    '[(exact (trace52_eReflNo chkf dec encTy erasing n (List.nil RV) Exp.tEmpty r e c RV.star
                               hr he hb rfl))]))
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code), n :- Nat, erasing :- Bool,
                           r :- Exp, e :- Exp, c :- Code,
                           hr :- (Trace52 chkf dec encTy n erasing (EvSrc.tm (List.nil RV) r) (RV.cert c)),
                           he :- (Trace52 chkf dec encTy n erasing (EvSrc.tm (List.nil RV) e) RV.star),
                           hb :- (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy Exp.tUnit))) Bool.false)]
                         '(Trace52 chkf dec encTy n erasing
                            (EvSrc.tm (List.nil RV) (Exp.refl Exp.tUnit r e)) (rdflt Sk.unit))
                         '[(exact (trace52_eReflNo chkf dec encTy erasing n (List.nil RV) Exp.tUnit r e c RV.star
                                    hr he hb rfl))])))))
