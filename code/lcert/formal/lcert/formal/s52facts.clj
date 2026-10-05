(ns lcert.formal.s52facts
  "Theorem 5.2: conversion, substitution and weakening of S (R4 §5).

  Both settings of the erasing flag are covered. Safety is held fixed while
  a type is converted or substituted: its application traces do not inspect
  the type. Only T(b)'s denotation and the component relations change.

  Substitution carries the same SubOK premise as Lemma 3.1. Conversion uses
  the existing Cv (formed types, with nbr at every link). These are the
  formalization's established representation conditions, not new axioms."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.safety52]))

(def ^:private pars
  '[chkf :- (=> Code Code Bool),
    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool])
(def ^:private ps '[chkf dec encTy cap erasing])
(defn- prove! [nm params goal tactics]
  (a/prove-theorem nm (lv (vec (concat pars params))) (lv goal) (lv tactics)))
(defn- define! [nm params result body]
  (let [bs (vec (concat pars params))]
    (kdef! nm (reduce (fn [q [x _ ty]] (list 'forall [x ty] q)) result
                     (reverse (partition 3 bs))) (list 'fn bs body))))
(defn- sr [A G en s rv a] (apply list 'S52 (concat ps [A G en s rv a])))
(defn- ext [bindings proof]
  (reduce (fn [p [v ty]] (list 'funext (list 'fn [v :- ty] p)))
          proof (reverse bindings)))

;; Named presentations of the two clauses, with abstract component
;; predicates. They are definitionally the existing S52 clauses, and add
;; no restriction on runtime values or on the safe trace.
(def ^:private rel-params
  '[r :- U, x :- Sk, y :- Sk,
    RA :- (=> RV (Car x) Prop), RB :- (=> (Car x) RV (Car y) Prop),
    rv :- RV, a :- (Car (Sk.arr x y))])
(defn- result-at [arg]
  (list 'Exists (list 'fn '[w :- RV]
    (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.ap 'rv arg) 'w]))
               '(RB alpha w (a alpha))))))
(def ^:private normal-pi
  (list 'forall '[av RV] (list 'forall '[alpha (Car x)]
    (list '=> '(RA av alpha) (result-at 'av)))))
(def ^:private erased-pi (list 'forall '[alpha (Car x)] (result-at 'RV.star)))
(define! 'Pi52 rel-params 'Prop
  (list 'Bool.rec$1 '(fn [_ :- Bool] Prop) normal-pi
    (list 'U.rec$1 '(fn [_ :- U] Prop) erased-pi normal-pi normal-pi 'r) 'erasing))
(def ^:private sig-params
  (vec (concat (drop-last 3 rel-params) '[a :- (Car (Sk.prod x y))])))
(define! 'Sig52 sig-params 'Prop
  '(Exists (fn [av :- RV] (Exists (fn [bv :- RV]
     (And (Eq RV rv (RV.pair av bv))
       (And
         (Bool.rec$1 (fn [_ :- Bool] Prop) (RA av (Prod.fst a))
           (U.rec$1 (fn [_ :- U] Prop) True (RA av (Prod.fst a))
             (RA av (Prod.fst a)) r) erasing)
         (RB (Prod.fst a) bv (Prod.snd a)))))))))

;; Component extensionality, shared by conversion and substitution. The
;; whole function predicates are equated before applying congrArg, which
;; keeps the proof independent of the three usage cases.
(doseq [[nm shape] '[[Pi52 Sk.arr] [Sig52 Sk.prod]]]
  (let [params
        (vec (concat '[r :- U, x :- Sk, y :- Sk,
                       RA :- (=> RV (Car x) Prop), RA2 :- (=> RV (Car x) Prop),
                       RB :- (=> (Car x) RV (Car y) Prop), RB2 :- (=> (Car x) RV (Car y) Prop),
                       hA :- (forall [v RV] (forall [a (Car x)] (Eq Prop (RA v a) (RA2 v a)))),
                       hB :- (forall [a (Car x)] (forall [v RV] (forall [b (Car y)]
                               (Eq Prop (RB a v b) (RB2 a v b))))), rv :- RV]
                     ['a :- (list 'Car (list shape 'x 'y))]))
        at (fn [ra rb] (apply list nm (concat ps ['r 'x 'y ra rb 'rv 'a])))
        pf (list 'Eq.trans
             (list 'congrArg (list 'fn '[q :- (=> RV (Car x) Prop)] (at 'q 'RB))
               (ext '[[v RV] [a (Car x)]] '(hA v a)))
             (list 'congrArg (list 'fn '[q :- (=> (Car x) RV (Car y) Prop)] (at 'RA2 'q))
               (ext '[[a (Car x)] [v RV] [b (Car y)]] '(hB a v b))))]
    (prove! (symbol (str nm "_ext")) params
      (list 'Eq 'Prop (at 'RA 'RB) (at 'RA2 'RB2)) [(list 'exact pf)])))

;; S at a Pi/Sigma is the abstract clause applied to its two relations.
;; Crucially the carrier skeletons x,y are explicit, so changing an Exp
;; does not silently transport a carrier or its environment.
(doseq [[ctor shape clause] '[[tPi Sk.arr Pi52] [tSig Sk.prod Sig52]]]
  (let [a-rel '(fn [av :- RV, alpha :- (Car x)]
                (S52 chkf dec encTy cap erasing A G en x av alpha))
        b-rel '(fn [alpha :- (Car x), bv :- RV, beta :- (Car y)]
                (S52 chkf dec encTy cap erasing B (List.cons Sk x G)
                  (Prod.mk alpha en) y bv beta))]
    (prove! (symbol (str "s52_" (if (= ctor 'tPi) "pi" "sig") "_view"))
      (vec (concat '[r :- U, A :- Exp, B :- Exp, G :- (List Sk), en :- (HEnv G),
                     x :- Sk, y :- Sk, rv :- RV]
                   ['a :- (list 'Car (list shape 'x 'y))]))
      (list 'Eq 'Prop (sr (list (symbol (str "Exp." ctor)) 'r 'A 'B) 'G 'en
                             (list shape 'x 'y) 'rv 'a)
            (apply list clause (concat ps ['r 'x 'y a-rel b-rel 'rv 'a])))
      '[(rfl)])))

;; A component equality may compare different environments: that is what
;; substitution and weakening need. At binders, the same carrier argument
;; extends each side. All safe traces are unchanged.
(doseq [[ctor shape clause] '[[tPi Sk.arr Pi52] [tSig Sk.prod Sig52]]]
  (let [left-a '(fn [av :- RV, alpha :- (Car x)]
                 (S52 chkf dec encTy cap erasing A G en x av alpha))
        right-a '(fn [av :- RV, alpha :- (Car x)]
                  (S52 chkf dec encTy cap erasing A2 G2 en2 x av alpha))
        left-b '(fn [alpha :- (Car x), bv :- RV, beta :- (Car y)]
                 (S52 chkf dec encTy cap erasing B (List.cons Sk x G) (Prod.mk alpha en) y bv beta))
        right-b '(fn [alpha :- (Car x), bv :- RV, beta :- (Car y)]
                  (S52 chkf dec encTy cap erasing B2 (List.cons Sk x G2) (Prod.mk alpha en2) y bv beta))
        c (symbol (str "Exp." ctor))]
    (prove! (symbol (str "s52_" (if (= ctor 'tPi) "pi" "sig") "_congr"))
      (vec (concat
        '[r :- U, A :- Exp, A2 :- Exp, B :- Exp, B2 :- Exp,
          G :- (List Sk), G2 :- (List Sk), en :- (HEnv G), en2 :- (HEnv G2), x :- Sk, y :- Sk,
          hA :- (forall [av RV] (forall [alpha (Car x)]
                  (Eq Prop (S52 chkf dec encTy cap erasing A G en x av alpha)
                           (S52 chkf dec encTy cap erasing A2 G2 en2 x av alpha)))),
          hB :- (forall [alpha (Car x)] (forall [bv RV] (forall [beta (Car y)]
                  (Eq Prop
                    (S52 chkf dec encTy cap erasing B (List.cons Sk x G) (Prod.mk alpha en) y bv beta)
                    (S52 chkf dec encTy cap erasing B2 (List.cons Sk x G2) (Prod.mk alpha en2) y bv beta))))),
          rv :- RV] ['a :- (list 'Car (list shape 'x 'y))]))
      (list 'Eq 'Prop (sr (list c 'r 'A 'B) 'G 'en (list shape 'x 'y) 'rv 'a)
                      (sr (list c 'r 'A2 'B2) 'G2 'en2 (list shape 'x 'y) 'rv 'a))
      [(list 'exact (apply list (symbol (str clause "_ext"))
                           (concat ps ['r 'x 'y left-a right-a left-b right-b 'hA 'hB 'rv 'a])))])))

;; --- Substitution (Theorem 5.2, the second invariance fact) -------------------
(defn- sub-motive [Gp A]
  (list 'forall '[G (List Sk)] (list 'forall '[sg (=> Nat Exp)]
    (list 'forall '[en (HEnv G)] (list 'forall '[rv RV]
      (list 'forall ['a (list 'Car (list 'skel A))]
        (list '=> (list 'SubOK Gp 'sg 'G)
          (list 'Eq 'Prop (sr (list 'subst 'sg A) 'G 'en (list 'skel A) 'rv 'a)
            (sr A Gp (list 'envOf 'chkf 'dec 'encTy 'cap Gp 'sg 'G 'en) (list 'skel A) 'rv 'a)))))))))
(prove! 's52_subst_T '[Gp :- (List Sk), b :- Exp, hb :- (SkJ Bool.false Gp b Sk.bool)]
  (sub-motive 'Gp '(Exp.tT b))
  '[(intro G sg en rv a hs)
    (exact (congrArg (fn [q :- Bool] (And (Eq RV rv RV.star) (Eq Bool q Bool.true)))
             (lemma31 chkf dec encTy cap Bool.false Gp b Sk.bool hb G sg en hs)))])
(doseq [[ctor nm] '[[tPi pi] [tSig sig]]]
  (prove! (symbol (str "s52_subst_" nm))
    (vec (concat '[Gp :- (List Sk), r :- U, A :- Exp, B :- Exp]
      ['ih_hA :- (sub-motive 'Gp 'A),
       'ih_hB :- (sub-motive '(List.cons Sk (skel A) Gp) 'B)]))
    (sub-motive 'Gp (list (symbol (str "Exp." ctor)) 'r 'A 'B))
    ['(intro G sg en rv a hs)
     (list 'exact (apply list (symbol (str "s52_" nm "_congr"))
       (concat ps
         ['r '(subst sg A) 'A '(subst (upn 1 sg) B) 'B 'G 'Gp 'en
          '(envOf chkf dec encTy cap Gp sg G en) '(skel A) '(skel B)
          '(fn [av :- RV, alpha :- (Car (skel A))] (ih_hA G sg en av alpha hs))
          '(fn [alpha :- (Car (skel A)), bv :- RV, beta :- (Car (skel B))]
             (Eq.trans
               (ih_hB (List.cons Sk (skel A) G) (upn 1 sg) (Prod.mk alpha en)
                 bv beta (subOK_up (skel A) Gp sg G hs))
               (congrArg (fn [qe :- (HEnv (List.cons Sk (skel A) Gp))]
                 (S52 chkf dec encTy cap erasing B (List.cons Sk (skel A) Gp) qe (skel B) bv beta))
                 (envOf_up chkf dec encTy cap (skel A) Gp sg G en alpha hs)))) 'rv 'a])))]))

(def ^:private skj-rules @#'lcert.formal.substitution/skj-rules)
(prove! 's52_subst_gen
  '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (list '=> '(Eq Bool w0 Bool.true) (sub-motive 'G0 'e0))
  (vec (cons '(induction der)
    (mapcat
      (fn [rule]
        (case rule
          (wEmpty wUnit wBool wNat wLbl wSyn wDia wR) '[(intro hw G sg en rv a hs) (rfl)]
          wT [(list 'intro 'hw) (list 'exact (apply list 's52_subst_T (concat ps '[G b hb])))]
          wPi ['(intro hw) (list 'exact (apply list 's52_subst_pi (concat ps
                     '[G r A B (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))])))]
          wSig ['(intro hw) (list 'exact (apply list 's52_subst_sig (concat ps
                     '[G r A B (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))])))]
          '[(intro hw) (cases hw)])) skj-rules))))

(prove! 's52_subst
  '[Gp :- (List Sk), A :- Exp, der :- (SkJ Bool.true Gp A Sk.unit),
    G :- (List Sk), sg :- (=> Nat Exp), en :- (HEnv G), hs :- (SubOK Gp sg G),
    rv :- RV, a :- (Car (skel A))]
  '(Eq Prop (S52 chkf dec encTy cap erasing (subst sg A) G en (skel A) rv a)
            (S52 chkf dec encTy cap erasing A Gp (envOf chkf dec encTy cap Gp sg G en) (skel A) rv a))
  '[(exact (s52_subst_gen chkf dec encTy cap erasing Bool.true Gp A Sk.unit der
             (Eq.refl$1 Bool.true) G sg en rv a hs))])

;; Single substitution states the paper's S(B[u/x]) eta = S(B)(eta,den u).
;; The term premise and skOf premise are precisely den_subst1's hypotheses.
(prove! 's52_subst1
  '[G :- (List Sk), s :- Sk, A :- Exp, der :- (SkJ Bool.true (List.cons Sk s G) A Sk.unit),
    u :- Exp, hu :- (SkJ Bool.false G u s), hk :- (Eq (Option Sk) (skOf G u) (Option.some Sk s)),
    en :- (HEnv G), rv :- RV, a :- (Car (skel A))]
  '(Eq Prop (S52 chkf dec encTy cap erasing (subst1 u A) G en (skel A) rv a)
            (S52 chkf dec encTy cap erasing A (List.cons Sk s G)
              (Prod.mk (den chkf dec encTy cap u G s en) en) (skel A) rv a))
  '[(rw [(subst1_consSub u A)])
    (exact (Eq.trans
      (s52_subst chkf dec encTy cap erasing (List.cons Sk s G) A der G
        (consSub u (fn [j :- Nat] (Exp.var j))) en
        (subOK_cons u s G (fn [j :- Nat] (Exp.var j)) G hu hk (subOK_id G)) rv a)
      (congrArg (fn [q :- (HEnv G)]
        (S52 chkf dec encTy cap erasing A (List.cons Sk s G)
          (Prod.mk (den chkf dec encTy cap u G s en) q) (skel A) rv a))
        (envOf_id chkf dec encTy cap G en))))])

;; --- Weakening: the renaming half of substitution ----------------------------
;; Inserting one variable at an arbitrary cutoff is needed by Var and by
;; the motives of the recursors. den_weaken supplies the T(b) case.
(defn- lift-motive [G A]
  (list 'forall '[c Nat] (list 'forall '[x Sk] (list 'forall ['en (list 'HEnv G)]
    (list 'forall '[vx (Car x)] (list 'forall '[rv RV]
      (list 'forall ['a (list 'Car (list 'skel A))]
        (list 'Eq 'Prop
          (sr (list 'lift 1 'c A) (list 'insS 'c 'x G) (list 'insE 'c 'x G 'en 'vx) (list 'skel A) 'rv 'a)
          (sr A G 'en (list 'skel A) 'rv 'a)))))))))
(prove! 's52_lift_T '[G :- (List Sk), b :- Exp, hb :- (SkJ Bool.false G b Sk.bool)]
  (lift-motive 'G '(Exp.tT b))
  '[(intro c x en vx rv a)
    (exact (congrArg (fn [q :- Bool] (And (Eq RV rv RV.star) (Eq Bool q Bool.true)))
             (den_weaken chkf dec encTy cap Bool.false G b Sk.bool hb c x en vx)))])
(doseq [[ctor nm] '[[tPi pi] [tSig sig]]]
  (prove! (symbol (str "s52_lift_" nm))
    (vec (concat '[G :- (List Sk), r :- U, A :- Exp, B :- Exp]
      ['ih_hA :- (lift-motive 'G 'A), 'ih_hB :- (lift-motive '(List.cons Sk (skel A) G) 'B)]))
    (lift-motive 'G (list (symbol (str "Exp." ctor)) 'r 'A 'B))
    ['(intro c x en vx rv a)
     (list 'exact (apply list (symbol (str "s52_" nm "_congr"))
       (concat ps ['r '(lift 1 c A) 'A '(lift 1 (+ c 1) B) 'B '(insS c x G) 'G
          '(insE c x G en vx) 'en '(skel A) '(skel B)
          '(fn [av :- RV, alpha :- (Car (skel A))] (ih_hA c x en vx av alpha))
          '(fn [alpha :- (Car (skel A)), bv :- RV, beta :- (Car (skel B))]
             (ih_hB (+ c 1) x (Prod.mk alpha en) vx bv beta)) 'rv 'a])))]))
(prove! 's52_lift_gen
  '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (list '=> '(Eq Bool w0 Bool.true) (lift-motive 'G0 'e0))
  (vec (cons '(induction der)
    (mapcat
      (fn [rule]
        (case rule
          (wEmpty wUnit wBool wNat wLbl wSyn wDia wR) '[(intro hw c x en vx rv a) (rfl)]
          wT ['(intro hw) (list 'exact (apply list 's52_lift_T (concat ps '[G b hb])))]
          wPi ['(intro hw) (list 'exact (apply list 's52_lift_pi (concat ps
                     '[G r A B (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))])))]
          wSig ['(intro hw) (list 'exact (apply list 's52_lift_sig (concat ps
                     '[G r A B (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))])))]
          '[(intro hw) (cases hw)])) skj-rules))))
(prove! 's52_lift
  '[G :- (List Sk), A :- Exp, der :- (SkJ Bool.true G A Sk.unit),
    c :- Nat, x :- Sk, en :- (HEnv G), vx :- (Car x), rv :- RV, a :- (Car (skel A))]
  '(Eq Prop
     (S52 chkf dec encTy cap erasing (lift 1 c A) (insS c x G) (insE c x G en vx) (skel A) rv a)
     (S52 chkf dec encTy cap erasing A G en (skel A) rv a))
  '[(exact (s52_lift_gen chkf dec encTy cap erasing Bool.true G A Sk.unit der
             (Eq.refl$1 Bool.true) c x en vx rv a))])

;; --- Conversion (Theorem 5.2, the first invariance fact) ----------------------
;; First one step at a recorded path, then an arbitrary Cv chain. The
;; result is stated at the old skeleton during a step. The final chain
;; theorem transports along the already proved step_skel equality.
(define! 'StepS52 '[e :- Exp] 'Prop
  '(forall [G (List Sk)]
     (=> (SkJ Bool.true G e Sk.unit) (Eq Bool (nbr e) Bool.true)
       (forall [pth (List Nat)] (forall [re Exp] (forall [co Exp]
         (=> (Eq (Option Exp) (getP pth e) (Option.some Exp re)) (Hd chkf re co)
           (forall [en (HEnv G)] (forall [rv RV] (forall [a (Car (skel e))]
             (Eq Prop
               (S52 chkf dec encTy cap erasing (setP pth e co) G en (skel e) rv a)
               (S52 chkf dec encTy cap erasing e G en (skel e) rv a))))))))))))

(prove! 's52_T_den
  '[b :- Exp, b2 :- Exp, G :- (List Sk), en :- (HEnv G), s :- Sk, rv :- RV, a :- (Car s),
    h :- (Eq Bool (den chkf dec encTy cap b2 G Sk.bool en) (den chkf dec encTy cap b G Sk.bool en))]
  '(Eq Prop (S52 chkf dec encTy cap erasing (Exp.tT b2) G en s rv a)
            (S52 chkf dec encTy cap erasing (Exp.tT b) G en s rv a))
  '[(exact (congrArg (fn [q :- Bool] (And (Eq RV rv RV.star) (Eq Bool q Bool.true))) h))])
(prove! 's52_T_tt
  '[G :- (List Sk), en :- (HEnv G), s :- Sk, rv :- RV, a :- (Car s)]
  '(Eq Prop (S52 chkf dec encTy cap erasing Exp.tUnit G en s rv a)
            (S52 chkf dec encTy cap erasing (Exp.tT Exp.tt) G en s rv a))
  '[(rw [(s52_T chkf dec encTy cap erasing Exp.tt G en s rv a)])
    (rw [(den_tt_bool chkf dec encTy cap G en)])
    (exact (propext (Iff.intro
      (fn [h :- (Eq RV rv RV.star)] (And.intro h (Eq.refl$1 Bool.true)))
      (fn [h :- (And (Eq RV rv RV.star) (Eq Bool Bool.true Bool.true))] (And.left h)))))])
(prove! 's52_T_ff
  '[G :- (List Sk), en :- (HEnv G), s :- Sk, rv :- RV, a :- (Car s)]
  '(Eq Prop (S52 chkf dec encTy cap erasing Exp.tEmpty G en s rv a)
            (S52 chkf dec encTy cap erasing (Exp.tT Exp.ff) G en s rv a))
  '[(rw [(s52_T chkf dec encTy cap erasing Exp.ff G en s rv a)])
    (rw [(den_ff_bool chkf dec encTy cap G en)])
    (exact (propext (Iff.intro
      (fn [h :- False] (False.elim$0 h))
      (fn [h :- (And (Eq RV rv RV.star) (Eq Bool Bool.false Bool.true))]
        (Bool.noConfusion (And.right h))))))])

(prove! 's52_step_T_nil
  '[b :- Exp, G :- (List Sk), re :- Exp, co :- Exp,
    hg :- (Eq (Option Exp) (getP (List.nil Nat) (Exp.tT b)) (Option.some Exp re)),
    hd :- (Hd chkf re co), en :- (HEnv G), rv :- RV, a :- (Car Sk.unit)]
  '(Eq Prop
     (S52 chkf dec encTy cap erasing (setP (List.nil Nat) (Exp.tT b) co) G en Sk.unit rv a)
     (S52 chkf dec encTy cap erasing (Exp.tT b) G en Sk.unit rv a))
  '[(have he (Eq Exp (Exp.tT b) re)
      (some_inj (Exp.tT b) re (Eq.trans (Eq.symm (getP_nil (Exp.tT b))) hg)))
    (subst he) (cases hd)
    (exact (Eq.trans
      (congrArg (fn [e :- Exp] (S52 chkf dec encTy cap erasing e G en Sk.unit rv a))
        (setP_nil (Exp.tT Exp.ff) Exp.tEmpty))
      (s52_T_ff chkf dec encTy cap erasing G en Sk.unit rv a)))
    (exact (Eq.trans
      (congrArg (fn [e :- Exp] (S52 chkf dec encTy cap erasing e G en Sk.unit rv a))
        (setP_nil (Exp.tT Exp.tt) Exp.tUnit))
      (s52_T_tt chkf dec encTy cap erasing G en Sk.unit rv a)))])
(prove! 's52_step_tT '[b :- Exp]
  '(StepS52 chkf dec encTy cap erasing (Exp.tT b))
  '[(intro G hj hn pth re co hg hd en rv a)
    (cases pth)
    (exact (s52_step_T_nil chkf dec encTy cap erasing b G re co hg hd en rv a))
    (cases head)
    (exact (s52_T_den chkf dec encTy cap erasing b (setP tail b co) G en Sk.unit rv a
      ((And.right (step_pack chkf dec encTy b Bool.false G Sk.bool
        (And.right (And.right (inv_tT Bool.true G b Sk.unit hj)))
        (nbr_tT_b b Bool.false hn) tail re co hg hd)) cap en)))
    (exact (False.elim$0 (none_ne_someE re hg)))])

;; Pi and Sigma steps can occur only in their two children. The child IHs
;; are equated pointwise; s52_*_congr composes them without changing any
;; evaluation trace or quantifier over related arguments.
(doseq [[ctor nm] '[[tPi pi] [tSig sig]]]
  (let [ty (list (symbol (str "Exp." ctor)) 'r 'A 'B)
        inv (list (symbol (str "inv_" ctor)) 'Bool.true 'G 'r 'A 'B 'Sk.unit 'hj)
        invA (list 'And.left (list 'And.right (list 'And.right inv)))
        invB (list 'And.right (list 'And.right (list 'And.right inv)))
        nbrA (list (symbol (str "nbr_" ctor "_A")) 'r 'A 'B 'Bool.false 'hn)
        nbrB (list (symbol (str "nbr_" ctor "_B")) 'r 'A 'B 'Bool.false 'hn)
        call (fn [A1 B1 hA hB]
               (apply list (symbol (str "s52_" nm "_congr"))
                 (concat ps ['r A1 'A B1 'B 'G 'G 'en 'en '(skel A) '(skel B) hA hB 'rv 'a])))]
    (prove! (symbol (str "s52_step_" ctor))
      '[r :- U, A :- Exp, B :- Exp,
        ih_A :- (StepS52 chkf dec encTy cap erasing A),
        ih_B :- (StepS52 chkf dec encTy cap erasing B)]
      (apply list 'StepS52 (concat ps [ty]))
      ['(intro G hj hn pth re co hg hd)
       '(cases pth)
       (list 'have 'he (list 'Eq 'Exp ty 're)
         (list 'some_inj ty 're (list 'Eq.trans (list 'Eq.symm (list 'getP_nil ty)) 'hg)))
       '(subst he) '(cases hd) '(cases head)
       '(intro en rv a)
       (list 'have 'hA '(SkJ Bool.true G A Sk.unit) invA)
       (list 'exact
         (call '(setP tail A co) 'B
           (list 'fn '[av :- RV, alpha :- (Car (skel A))]
             (list 'ih_A 'G 'hA nbrA 'tail 're 'co 'hg 'hd 'en 'av 'alpha))
           '(fn [alpha :- (Car (skel A)), bv :- RV, beta :- (Car (skel B))]
              (Eq.refl$1 (S52 chkf dec encTy cap erasing B (List.cons Sk (skel A) G)
                            (Prod.mk alpha en) (skel B) bv beta)))))
       '(cases n) '(intro en rv a)
       (list 'have 'hB '(SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit) invB)
       (list 'exact
         (call 'A '(setP tail B co)
           '(fn [av :- RV, alpha :- (Car (skel A))]
              (Eq.refl$1 (S52 chkf dec encTy cap erasing A G en (skel A) av alpha)))
           (list 'fn '[alpha :- (Car (skel A)), bv :- RV, beta :- (Car (skel B))]
             (list 'ih_B '(List.cons Sk (skel A) G) 'hB nbrB 'tail 're 'co 'hg 'hd
               '(Prod.mk alpha en) 'bv 'beta))))
       '(exact (False.elim$0 (none_ne_someE re hg)))])))

;; Base types have no head step and no child; non-types cannot inhabit a
;; formation judgment. The ordinary Exp induction therefore has just the
;; three substantive cases above.
(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(doseq [[ctor fs] exp-fields :when (not (#{'tT 'tPi 'tSig} ctor))]
  (let [ty (if (empty? fs) (symbol (str "Exp." ctor))
               (apply list (symbol (str "Exp." ctor)) (map first fs)))]
    (prove! (symbol (str "s52_step_" ctor))
      (vec (mapcat (fn [[f t _]] [f :- t]) fs))
      (apply list 'StepS52 (concat ps [ty]))
      (cond
        (#{'tEmpty 'tUnit 'tBool 'tNat 'tLbl 'tSyn 'tDia 'tR} ctor)
        ['(intro G hj hn pth re co hg hd) '(cases pth)
         (list 'have 'he (list 'Eq 'Exp ty 're)
           (list 'some_inj ty 're (list 'Eq.trans (list 'Eq.symm (list 'getP_nil ty)) 'hg)))
         '(subst he) '(cases hd) '(exact (False.elim$0 (none_ne_someE re hg)))]
        (= ctor 'tBrs)
        ['(intro G hj) (list 'exact (list 'False.elim$0 (list 'skj_inv 'Bool.true 'G ty 'Sk.unit 'hj)))]
        :else
        ['(intro G hj) (list 'exact (list 'False.elim$0
          (list 'Bool.noConfusion (list 'And.left (list 'skj_inv 'Bool.true 'G ty 'Sk.unit 'hj)))))]))))
(prove! 's52_step '[e :- Exp] '(StepS52 chkf dec encTy cap erasing e)
  (vec (cons '(induction e)
    (for [[ctor fs] exp-fields]
      (list 'exact (apply list (symbol (str "s52_step_" ctor))
        (concat ps (map first fs) (when (#{'tPi 'tSig} ctor) '[ih_A ih_B]))))))))

(prove! 's52_step_of_step
  '[B :- Exp, C :- Exp, G :- (List Sk), hj :- (SkJ Bool.true G B Sk.unit),
    hn :- (Eq Bool (nbr B) Bool.true), hs :- (Step chkf B C),
    en :- (HEnv G), rv :- RV, a :- (Car (skel B))]
  '(Eq Prop (S52 chkf dec encTy cap erasing C G en (skel B) rv a)
            (S52 chkf dec encTy cap erasing B G en (skel B) rv a))
  '[(refine' (exT (List Nat) _ _ hs _)) (intro p hp)
    (refine' (exT Exp _ _ hp _)) (intro re hre)
    (refine' (exT Exp _ _ hre _)) (intro co hco)
    (exact (Eq.mp
      (congrArg (fn [E :- Exp]
        (Eq Prop (S52 chkf dec encTy cap erasing E G en (skel B) rv a)
                 (S52 chkf dec encTy cap erasing B G en (skel B) rv a)))
        (Eq.symm (And.right (And.right hco))))
      (s52_step chkf dec encTy cap erasing B G hj hn p re co
        (And.left hco) (And.left (And.right hco)) en rv a)))])

;; The family g exposes the dependent carrier transport explicitly. In the
;; fundamental property it is s |-> den t G s eta, exactly as in cv_V.
(prove! 's52_cv
  '[G :- (List Sk), A :- Exp, B :- Exp, der :- (Cv chkf G A B)]
  '(forall [g (forall [s Sk] (Car s))] (forall [en (HEnv G)] (forall [rv RV]
     (Eq Prop
       (S52 chkf dec encTy cap erasing A G en (skel A) rv (g (skel A)))
       (S52 chkf dec encTy cap erasing B G en (skel B) rv (g (skel B)))))))
  '[(induction der)
    (intro g en rv) (rfl)
    (intro g en rv)
    (have hJB (SkJ Bool.true G B Sk.unit) (And.right (cv_skel_wf chkf G A B hab)))
    (have hsk (Eq Sk (skel C) (skel B))
      (step_skel chkf B C (skj_isTy Bool.true G B Sk.unit hJB (Eq.refl$1 Bool.true)) hs))
    (have hAC
      (Eq Prop (S52 chkf dec encTy cap erasing A G en (skel A) rv (g (skel A)))
               (S52 chkf dec encTy cap erasing C G en (skel B) rv (g (skel B))))
      (Eq.trans (ih_hab g en rv)
        (Eq.symm (s52_step_of_step chkf dec encTy cap erasing B C G hJB
          (cv_nbr_right chkf G A B hab) hs en rv (g (skel B))))))
    (exact (sk_transport
      (fn [s :- Sk, w :- (Car s)]
        (Eq Prop (S52 chkf dec encTy cap erasing A G en (skel A) rv (g (skel A)))
                 (S52 chkf dec encTy cap erasing C G en s rv w)))
      g (skel B) (skel C) (Eq.symm hsk) hAC))
    (intro g en rv)
    (have hsk (Eq Sk (skel B) (skel C))
      (step_skel chkf C B (skj_isTy Bool.true G C Sk.unit hc (Eq.refl$1 Bool.true)) hs))
    (have hABatC
      (Eq Prop (S52 chkf dec encTy cap erasing A G en (skel A) rv (g (skel A)))
               (S52 chkf dec encTy cap erasing B G en (skel C) rv (g (skel C))))
      (sk_transport
        (fn [s :- Sk, w :- (Car s)]
          (Eq Prop (S52 chkf dec encTy cap erasing A G en (skel A) rv (g (skel A)))
                   (S52 chkf dec encTy cap erasing B G en s rv w)))
        g (skel B) (skel C) hsk (ih_hab g en rv)))
    (exact (Eq.trans hABatC
      (s52_step_of_step chkf dec encTy cap erasing C B G hc hn hs en rv (g (skel C)))))])
