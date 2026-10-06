(ns lcert.formal.s52fund
  "Theorem 5.2: elementary cases of the fundamental property (R4 §5).

  Result52 keeps the source term t and evaluated term e separate: e=t for
  eval_n, while Er supplies e for evalE_n. The flag selects the matching
  safe trace and S relation. The cases below make no typing-mode or usage
  assumption, so apply equally to logical and runtime premises.

  Abort, H1, and H are impossible, using only the related evidence. The
  latter two use Corollary 3.7 at every code size, through CheckSpec; no
  footprint assumption or induction on the evidence's size is introduced."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.s52env]))

;; Public proof-form builders shared by the later case modules. They emit
;; ordinary kernel-checked declarations; no new trusted operation is used.
(def pars
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool])
(def ps '[chkf dec encTy cap erasing])
(def ctx '[G :- (List Sk), en :- (HEnv G), rho :- (List RV)])
(defn prove! [nm params goal tactics]
  (a/prove-theorem nm (lv (vec (concat pars params))) (lv goal) (lv tactics)))
(defn den [t s] (list 'den 'chkf 'dec 'encTy 'cap t 'G s 'en))
(defn rel [A v a] (apply list 'S52 (concat ps [A 'G 'en (list 'skel A) v a])))
(defn trace [e v] (apply list 'Trace52 (concat ps [(list 'EvSrc.tm 'rho e) v])))
(defn result [t A e]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And (trace e 'v) (rel A 'v (den t (list 'skel A)))))))

(kdef Result52
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)] (forall [cap Nat] (forall [erasing Bool]
        (forall [G (List Sk)] (=> (HEnv G) (List RV) Exp Exp Exp Prop)))))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool, G :- (List Sk),
       en :- (HEnv G), rho :- (List RV), t :- Exp, A :- Exp, e :- Exp]
    (Exists (fn [v :- RV]
      (And (Trace52 chkf dec encTy cap erasing (EvSrc.tm rho e) v)
        (S52 chkf dec encTy cap erasing A G en (skel A) v
          (den chkf dec encTy cap t G (skel A) en)))))))

;; Transport only the runtime result of an already safe trace.
(prove! 's52_trace_cast
  '[src :- EvSrc, v :- RV, w :- RV,
    h :- (Trace52 chkf dec encTy cap erasing src v), eq :- (Eq RV v w)]
  '(Trace52 chkf dec encTy cap erasing src w)
  '[(exact (Eq.mp (congrArg (fn [z :- RV] (Trace52 chkf dec encTy cap erasing src z)) eq) h))])

;; Constants stop immediately. The Boolean/numeral/label denotations are
;; unfolded only at their own carrier; Unit ignores its carrier value.
(doseq [[nm t A value rule eq] '[[star Exp.star Exp.tUnit RV.star eStar nil]
                                [tt Exp.tt Exp.tBool (RV.bool Bool.true) eTT den_tt_at]
                                [ff Exp.ff Exp.tBool (RV.bool Bool.false) eFF den_ff_at]
                                [zero Exp.zero Exp.tNat (RV.nat 0) eZero den_zero_at]]]
  (prove! (symbol (str "s52_" nm)) ctx (result t A t)
    (vec (concat
      (when eq [(list 'rw [(list eq 'chkf 'dec 'encTy 'cap 'G (list 'skel A) 'en)])])
      ['(constructor) (list 'exact value) '(constructor)
       (list 'exact (list (symbol (str "trace52_" rule)) 'chkf 'dec 'encTy 'erasing 'cap 'rho))
       '(rfl)]))))

(prove! 's52_lbl (concat ctx '[l :- Nat, hl :- (LT.lt l (NL))])
  (result '(Exp.lbl l) 'Exp.tLbl '(Exp.lbl l))
  '[(rw [(den_lbl_at chkf dec encTy cap l G Sk.lbl en)])
    (constructor) (exact (RV.lbl l)) (constructor)
    (exact (trace52_eLbl chkf dec encTy erasing cap rho l))
    (constructor) (rfl) (exact hl)])

;; Variable lookup is the environment theorem, with the lookup denotation
;; exposed. Non-erasing mode can read every usage; erasing mode reads 1/ω.
(prove! 's52_var
  '[D :- (List Exp), us :- (List U), i :- Nat, A :- Exp, r :- U,
    hw :- (WFS D), hA :- (Eq (Option Exp) (nthE D i) (Option.some Exp A)),
    hu :- (Eq (Option U) (nthU us i) (Option.some U r)),
    hn :- (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true)),
    rho :- (List RV), en :- (HEnv (skels D)),
    he :- (Env52 chkf dec encTy cap erasing D us rho en)]
  '(Result52 chkf dec encTy cap erasing (skels D) en rho
     (Exp.var i) (lift (+ i 1) 0 A) (Exp.var i))
  '[(refine' (exT RV _ _ (var52 chkf dec encTy cap erasing D hw i A hA us r rho en hu hn he) _))
    (intro v hv) (constructor) (exact v) (constructor)
    (exact (trace52_eVar chkf dec encTy erasing cap rho i v (And.left hv)))
    (exact (Eq.mpr (congrArg
      (fn [z :- (Car (skel (lift (+ i 1) 0 A)))]
        (S52 chkf dec encTy cap erasing (lift (+ i 1) 0 A) (skels D) en
          (skel (lift (+ i 1) 0 A)) v z))
      (den_var_at chkf dec encTy cap i (skels D) (skel (lift (+ i 1) 0 A)) en))
      (And.right hv)))])

;; Conv preserves the very same trace; the carrier family expresses the
;; skeleton transport already proved by s52_cv.
(prove! 's52_conv (concat ctx '[t :- Exp, e :- Exp, A :- Exp, B :- Exp,
                                hc :- (Cv chkf G A B)]
                          ['ih :- (result 't 'A 'e)])
  (result 't 'B 'e)
  '[(refine' (exT RV _ _ ih _)) (intro v hv)
    (constructor) (exact v) (constructor) (exact (And.left hv))
    (exact (Eq.mp (s52_cv chkf dec encTy cap erasing G A B hc
      (fn [s :- Sk] (den chkf dec encTy cap t G s en)) en v) (And.right hv)))])

;; Truth extraction from related checker evidence, with no size bound.
(prove! 's52_chk_true
  (concat ctx '[r :- Exp, c :- Exp, v :- RV, a :- Unit,
                h :- (S52 chkf dec encTy cap erasing (chkT r c) G en Sk.unit v a)])
  '(Eq Bool (chkf (den chkf dec encTy cap r G Sk.cert en)
                  (den chkf dec encTy cap c G Sk.syn en)) Bool.true)
  '[(have h1 (Eq Bool (den chkf dec encTy cap (Exp.chk (Exp.prn r) c) G Sk.bool en) Bool.true)
      (And.right h))
    (have h2 (Eq Bool (chkf (den chkf dec encTy cap (Exp.prn r) G Sk.syn en)
                           (den chkf dec encTy cap c G Sk.syn en)) Bool.true)
      (Eq.trans (Eq.symm (den_chk_val chkf dec encTy cap (Exp.prn r) c G en)) h1))
    (exact (Eq.mp (congrArg (fn [q :- Code]
      (Eq Bool (chkf q (den chkf dec encTy cap c G Sk.syn en)) Bool.true))
      (den_prn_val chkf dec encTy cap r G en)) h2))])

;; The three vacuous rule cases. Their premises yield False before any
;; forbidden evaluator constructor is entered. Q permits either evaluated
;; syntax (original or erased) and any result type in the rule assembly.
(prove! 's52_abort
  (concat ctx '[t :- Exp, e :- Exp, Q :- Prop] ['ih :- (result 't 'Exp.tEmpty 'e)])
  'Q
  '[(refine' (exT RV _ _ ih _)) (intro v hv)
    (exact (False.elim$0 (And.right hv)))])

(prove! 's52_h1
  (concat ctx '[hcs :- (CheckSpec chkf dec encTy), r :- Exp, s :- Exp, c :- Exp,
                e1 :- Exp, e2 :- Exp, q1 :- Exp, q2 :- Exp, Q :- Prop]
    ['ih1 :- (result 'e1 '(chkT r c) 'q1)
     'ih2 :- (result 'e2 '(chkT s (negT c)) 'q2)])
  'Q
  '[(refine' (exT RV _ _ ih1 _)) (intro v1 hv1)
    (refine' (exT RV _ _ ih2 _)) (intro v2 hv2)
    (have hc1 (Eq Bool (chkf (den chkf dec encTy cap r G Sk.cert en)
                           (den chkf dec encTy cap c G Sk.syn en)) Bool.true)
      (s52_chk_true chkf dec encTy cap erasing G en rho r c v1 _ (And.right hv1)))
    (have hc2 (Eq Bool (chkf (den chkf dec encTy cap s G Sk.cert en)
                           (den chkf dec encTy cap (negT c) G Sk.syn en)) Bool.true)
      (s52_chk_true chkf dec encTy cap erasing G en rho s (negT c) v2 _ (And.right hv2)))
    (have hc3 (Eq Bool (chkf (den chkf dec encTy cap s G Sk.cert en)
                    (Code.sn 25 (den chkf dec encTy cap c G Sk.syn en) (Code.sl 15))) Bool.true)
      (Eq.mp (congrArg (fn [q :- Code]
        (Eq Bool (chkf (den chkf dec encTy cap s G Sk.cert en) q) Bool.true))
        (den_negT chkf dec encTy cap c G en)) hc2))
    (exact (False.elim$0 (s52_no_contradiction chkf dec encTy hcs
      (den chkf dec encTy cap r G Sk.cert en) (den chkf dec encTy cap s G Sk.cert en)
      (den chkf dec encTy cap c G Sk.syn en) hc1 hc3)))])

(prove! 's52_H
  (concat ctx '[hcs :- (CheckSpec chkf dec encTy), r :- Exp, cd :- Exp, e :- Exp, q :- Exp,
                hb :- (Eq (Option Exp) (baseCode Exp.tEmpty) (Option.some Exp cd)), Q :- Prop]
    ['ih :- (result 'e '(chkT r cd) 'q)])
  'Q
  '[(refine' (exT RV _ _ ih _)) (intro v hv)
    (have hc (Eq Bool (chkf (den chkf dec encTy cap r G Sk.cert en)
                          (den chkf dec encTy cap cd G Sk.syn en)) Bool.true)
      (s52_chk_true chkf dec encTy cap erasing G en rho r cd v _ (And.right hv)))
    (have ht (Eq Bool (chkf (den chkf dec encTy cap r G Sk.cert en) (encTy Exp.tEmpty)) Bool.true)
      (Eq.mp (congrArg (fn [z :- Code]
        (Eq Bool (chkf (den chkf dec encTy cap r G Sk.cert en) z) Bool.true))
        (base_den chkf dec encTy cap (And.left (And.right hcs)) Exp.tEmpty cd hb G en)) hc))
    (exact (False.elim$0 (Bool.noConfusion
      (Eq.trans (Eq.symm (s52_no_refutation chkf dec encTy hcs
        (den chkf dec encTy cap r G Sk.cert en))) ht))))])
