(ns lcert.formal.eval
  "F5 — the budgeted evaluator and Theorem 4 (R4-metatheory.md §5).

  evalₙ is call-by-value and does not erase: every term position is evaluated,
  usage-0 arguments included, and type annotations are not evaluated.  It is a
  big-step relation, partial on values of the wrong shape, and it never
  consults a typing derivation.  It mirrors den_gen.clj clause by clause, so
  Theorem 4 is Tait's argument over simple types (SkJ at Bool.false).

  Adaptations forced by the embedding (ADR-0006, this namespace):

  1. One inductive family.  Application of a closure evaluates the body, and
     recN / recSyn / itR recurse on a value, so evaluation and application
     are mutually recursive.  Ansatz builds one recursor per inductive, so
     both are constructors of Ev, indexed by EvSrc.  Eval n ρ t v abbreviates
     Ev n (tm ρ t) v, and AppV n f a w abbreviates Ev n (ap f a) w.  The
     iterators Iter, RecRun and ItRun are the other three forms of EvSrc.

  2. Tokens are not told apart.  The carrier of ◇ is Unit and the carrier of
     R is Code (carrier.clj): a token carries no information, and print
     forgets tokens.  There is one runtime token, RV.token.  A runtime
     certificate is RV.cert c for the same Code the model uses, and print
     sends it to RV.code c.  chk′ and inspect therefore pass that Code to
     chkf, which is Check(print v, …) of the paper.  Node still evaluates its
     token argument (call-by-value) and then drops it, as ⟦node⟧ does.

  3. Branch lists are a spine, not a bare List.  bnil does not record its
     result skeleton, but both dflt and an application of the empty list need
     the default of that skeleton.  RV.bnil s tags the value; eBnil may pick
     any s, and Theorem 4 picks the one SkJ assigned.  bcons stores the head
     and the tail *value*, because a tail of skeleton Lbl → σ may be a λ, not
     a concrete list.  AppV of bnil returns rdflt s; of bcons h t at label 0
     returns h, and at label k+1 applies t to label k — the clause of
     den_bcons.

  4. The default of an arrow is a closure whose body is abort at the reified
     codomain (reifySk), so applying it evaluates to the codomain default.
     At a product the default is the pair of defaults.  Base defaults match
     dflt: ⋆, ff, 0, label 0, a leaf, the token, a leaf certificate.

  5. ⟦·⟧ reads skOf at an application's argument and a let's scrutinee, and
     returns the default when that is none.  Branch lists are exactly the
     terms for which skOf is none (skeletons.clj).  nbr, the conversion
     branch's hypothesis, is not on this branch.  argsOK G t is the
     replacement: at every such position skOf is defined, and it is defined
     in the binder's context for the subterms.  Theorem 4 assumes argsOK.
     An application of a branch list is excluded, because there ⟦f u⟧ is the
     default rather than ⟦f⟧(⟦u⟧).

  H₁ returns ⋆: its only SkJ skeleton is Unit.  abort and a failed reflect
  return rdflt of the skeleton of the type annotation (the annotation is not
  evaluated).  reflect, when the cap test and chkf succeed and dec returns
  (m, t′, A), evaluates t′ at budget m in m tokens; the proof that m < n and
  that t′ is the program ⟦·⟧ runs is CheckSpec, as in outer.clj."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.model :refer :all]
            ;; Loading these defines the kernel constants the adequacy proofs
            ;; name (den_*_at, coe_self, none_ne_someS, exT).  Theorems are
            ;; not Clojure vars, so they are not referred.  None of these
            ;; namespaces depends on eval.
            [lcert.formal.unfold]
            [lcert.formal.subst]
            [lcert.formal.substitution]
            [lcert.formal.outer]))

;; --- runtime values ----------------------------------------------------------

;; RV: a value of the budgeted evaluator.  No type is stored, except the
;; result skeleton on an empty branch list (adaptation 3).
(a/inductive RV []
  (token)
  (star)
  (bool [b Bool])
  (nat [k Nat])
  (lbl [l Nat])
  (code [c Code])
  (cert [c Code])
  (pair [x RV] [y RV])
  (clos [rho (List RV)] [body Exp])
  (bnil [s Sk])
  (bcons [h RV] [t RV]))

;; reifySk s: a closed type whose skeleton is s.  The default closure of an
;; arrow aborts at the reified codomain, and abort returns rdflt of that
;; skeleton (skel_reify, below).
(a/defn reifySk [s :- Sk] Exp
  (match s
    [unit Exp.tUnit]
    [bool Exp.tBool]
    [nat Exp.tNat]
    [lbl Exp.tLbl]
    [syn Exp.tSyn]
    [dia Exp.tDia]
    [cert Exp.tR]
    [(arr x y) (Exp.tPi U.u0 (reifySk x) (reifySk y))]
    [(prod x y) (Exp.tSig U.u0 (reifySk x) (reifySk y))]))

;; rdflt s: the runtime default of skeleton s, mirroring dflt.
(a/defn rdflt [s :- Sk] RV
  (match s
    [unit RV.star]
    [bool (RV.bool Bool.false)]
    [nat (RV.nat 0)]
    [lbl (RV.lbl 0)]
    [syn (RV.code (Code.sl 0))]
    [dia RV.token]
    [cert (RV.cert (Code.sl 0))]
    [(arr x y) (RV.clos (List.nil RV) (Exp.abort (reifySk y) Exp.star))]
    [(prod x y) (RV.pair (rdflt x) (rdflt y))]))

;; rlookup ρ i: de Bruijn 0 is the head of ρ (the innermost binder).  Recurses
;; on ρ only, returning a function of the index, so the equations reduce
;; (a recursion on both arguments would be well-founded and would not).
(a/defn rlookupF [rho :- (List RV)] (=> Nat (Option RV))
  (match rho
    [nil (fn [i :- Nat] (Option.none RV))]
    [(cons h t)
     (fn [i :- Nat]
       (match i
         [zero (Option.some RV h)]
         [(succ j) ((rlookupF t) j)]))]))

(a/defn rlookup [rho :- (List RV), i :- Nat] (Option RV)
  ((rlookupF rho) i))

;; rtokens m: the m runtime tokens reflect gives a decoded program.
(a/defn rtokens [m :- Nat] (List RV)
  (match m
    [zero (List.nil RV)]
    [(succ k) (List.cons RV RV.token (rtokens k))]))

;; --- the big-step relation ---------------------------------------------------

;; What is being run: a term in an environment, an application of a value,
;; or one step of recN / recSyn / itR.
(a/inductive EvSrc []
  (tm [rho (List RV)] [t Exp])
  (ap [f RV] [arg RV])
  (iter [rho (List RV)] [step Exp] [k Nat] [acc RV])
  (recs [rho (List RV)] [tl Exp] [tn Exp] [c Code])
  (itr [g RV] [h RV] [c Code]))

;; Ev chkf dec encTy n src v: under budget n, src evaluates to v.
;; chkf, dec and encTy are the same parameters as denotation.  The budget is
;; an index, so a reflect premise may use a smaller one.
;; One parameter vector: a/inductive binds only the first binder list as the
;; parameters (the rest of the form is constructors).
(a/inductive Ev [chkf (=> Code Code Bool),
                 dec (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                 encTy (=> Exp Code)]
  :in Prop :indices [n Nat, src EvSrc, v RV]

  ;; variables and constants
  (eVar [n Nat] [rho (List RV)] [i Nat] [v RV]
    [h (Eq (Option RV) (rlookup rho i) (Option.some RV v))]
    :where [n (EvSrc.tm rho (Exp.var i)) v])
  (eStar [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.star) RV.star])
  (eTT [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.tt) (RV.bool Bool.true)])
  (eFF [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.ff) (RV.bool Bool.false)])
  (eZero [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.zero) (RV.nat 0)])
  (eLbl [n Nat] [rho (List RV)] [l Nat]
    :where [n (EvSrc.tm rho (Exp.lbl l)) (RV.lbl l)])

  ;; abort and H₁ evaluate their arguments, then return a default
  (eAbort [n Nat] [rho (List RV)] [A Exp] [t Exp] [vt RV]
    [ht (Ev chkf dec encTy n (EvSrc.tm rho t) vt)]
    :where [n (EvSrc.tm rho (Exp.abort A t)) (rdflt (skel A))])
  (eH1 [n Nat] [rho (List RV)] [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp]
       [vr RV] [vs RV] [vc RV] [v1 RV] [v2 RV]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) vr)]
    [hs (Ev chkf dec encTy n (EvSrc.tm rho s) vs)]
    [hc (Ev chkf dec encTy n (EvSrc.tm rho c) vc)]
    [h1 (Ev chkf dec encTy n (EvSrc.tm rho e1) v1)]
    [h2 (Ev chkf dec encTy n (EvSrc.tm rho e2) v2)]
    :where [n (EvSrc.tm rho (Exp.h1 r s c e1 e2)) RV.star])

  ;; eliminators: the scrutinee selects one branch (both are not run)
  (eIteT [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ev chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (Ev chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eIteF [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ev chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (Ev chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eElimT [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ev chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (Ev chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])
  (eElimF [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ev chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (Ev chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])

  ;; numerals
  (eSucc [n Nat] [rho (List RV)] [m Exp] [k Nat]
    [hm (Ev chkf dec encTy n (EvSrc.tm rho m) (RV.nat k))]
    :where [n (EvSrc.tm rho (Exp.succ m)) (RV.nat (Nat.succ k))])
  ;; recN: the scrutinee is a numeral k; the step runs k times from the base.
  ;; y is variable 0 (the accumulator), x is variable 1 (the predecessor).
  (eRecN [n Nat] [rho (List RV)] [P Exp] [z Exp] [step Exp] [nv Exp]
         [k Nat] [z0 RV] [v RV]
    [hn (Ev chkf dec encTy n (EvSrc.tm rho nv) (RV.nat k))]
    [hz (Ev chkf dec encTy n (EvSrc.tm rho z) z0)]
    [hi (Ev chkf dec encTy n (EvSrc.iter rho step k z0) v)]
    :where [n (EvSrc.tm rho (Exp.recN P z step nv)) v])
  (eIterZ [n Nat] [rho (List RV)] [step Exp] [acc RV]
    :where [n (EvSrc.iter rho step 0 acc) acc])
  (eIterS [n Nat] [rho (List RV)] [step Exp] [k Nat] [acc RV] [mid RV] [out RV]
    [hp (Ev chkf dec encTy n (EvSrc.iter rho step k acc) mid)]
    [hs (Ev chkf dec encTy n
           (EvSrc.tm (List.cons RV mid (List.cons RV (RV.nat k) rho)) step) out)]
    :where [n (EvSrc.iter rho step (Nat.succ k) acc) out])

  ;; labels and branch lists
  (eCaseL [n Nat] [rho (List RV)] [P Exp] [a Exp] [bs Exp] [va RV] [vf RV] [w RV]
    [ha (Ev chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (Ev chkf dec encTy n (EvSrc.tm rho bs) vf)]
    [hp (Ev chkf dec encTy n (EvSrc.ap vf va) w)]
    :where [n (EvSrc.tm rho (Exp.caseL P a bs)) w])
  (eBnil [n Nat] [rho (List RV)] [s Sk]
    :where [n (EvSrc.tm rho Exp.bnil) (RV.bnil s)])
  (eBcons [n Nat] [rho (List RV)] [h Exp] [t Exp] [vh RV] [vt RV]
    [hh (Ev chkf dec encTy n (EvSrc.tm rho h) vh)]
    [ht (Ev chkf dec encTy n (EvSrc.tm rho t) vt)]
    :where [n (EvSrc.tm rho (Exp.bcons h t)) (RV.bcons vh vt)])

  ;; codes
  (eSleaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (Ev chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.sleaf a)) (RV.code (Code.sl l))])
  (eSnode [n Nat] [rho (List RV)] [a Exp] [c1 Exp] [c2 Exp] [l Nat] [x Code] [y Code]
    [ha (Ev chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (Ev chkf dec encTy n (EvSrc.tm rho c1) (RV.code x))]
    [h2 (Ev chkf dec encTy n (EvSrc.tm rho c2) (RV.code y))]
    :where [n (EvSrc.tm rho (Exp.snode a c1 c2)) (RV.code (Code.sn l x y))])
  (eRecS [n Nat] [rho (List RV)] [P Exp] [tl Exp] [tn Exp] [c Exp] [cv Code] [v RV]
    [hc (Ev chkf dec encTy n (EvSrc.tm rho c) (RV.code cv))]
    [hr (Ev chkf dec encTy n (EvSrc.recs rho tl tn cv) v)]
    :where [n (EvSrc.tm rho (Exp.recS P tl tn c)) v])
  ;; leaf method: variable 0 is the label.  Node method, innermost first:
  ;; yb, ya, the right code, the left code, the label.
  (eRecSL [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [v RV]
    [h (Ev chkf dec encTy n (EvSrc.tm (List.cons RV (RV.lbl l) rho) tl) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sl l)) v])
  (eRecSN [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [a Code] [b Code]
          [ya RV] [yb RV] [v RV]
    [ha (Ev chkf dec encTy n (EvSrc.recs rho tl tn a) ya)]
    [hb (Ev chkf dec encTy n (EvSrc.recs rho tl tn b) yb)]
    [hs (Ev chkf dec encTy n
           (EvSrc.tm (List.cons RV yb (List.cons RV ya
              (List.cons RV (RV.code b) (List.cons RV (RV.code a)
                (List.cons RV (RV.lbl l) rho))))) tn) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sn l a b)) v])

  ;; certificates.  The token of a node is evaluated and dropped.
  (eLeaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (Ev chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.leaf a)) (RV.cert (Code.sl l))])
  (eNode [n Nat] [rho (List RV)] [d Exp] [a Exp] [r1 Exp] [r2 Exp]
         [vd RV] [l Nat] [x Code] [y Code]
    [hd (Ev chkf dec encTy n (EvSrc.tm rho d) vd)]
    [ha (Ev chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (Ev chkf dec encTy n (EvSrc.tm rho r1) (RV.cert x))]
    [h2 (Ev chkf dec encTy n (EvSrc.tm rho r2) (RV.cert y))]
    :where [n (EvSrc.tm rho (Exp.node d a r1 r2)) (RV.cert (Code.sn l x y))])
  (eItR [n Nat] [rho (List RV)] [X Exp] [g Exp] [h Exp] [r Exp]
        [vg RV] [vh RV] [c Code] [w RV]
    [hg (Ev chkf dec encTy n (EvSrc.tm rho g) vg)]
    [hh (Ev chkf dec encTy n (EvSrc.tm rho h) vh)]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [hi (Ev chkf dec encTy n (EvSrc.itr vg vh c) w)]
    :where [n (EvSrc.tm rho (Exp.itR X g h r)) w])
  (eItL [n Nat] [g RV] [h RV] [l Nat] [w RV]
    [ha (Ev chkf dec encTy n (EvSrc.ap g (RV.lbl l)) w)]
    :where [n (EvSrc.itr g h (Code.sl l)) w])
  ;; h is applied to the token, the label, and the two recursive results.
  (eItN [n Nat] [g RV] [h RV] [l Nat] [a Code] [b Code] [ya RV] [yb RV]
        [v1 RV] [v2 RV] [v3 RV] [w RV]
    [ha (Ev chkf dec encTy n (EvSrc.itr g h a) ya)]
    [hb (Ev chkf dec encTy n (EvSrc.itr g h b) yb)]
    [h1 (Ev chkf dec encTy n (EvSrc.ap h RV.token) v1)]
    [h2 (Ev chkf dec encTy n (EvSrc.ap v1 (RV.lbl l)) v2)]
    [h3 (Ev chkf dec encTy n (EvSrc.ap v2 ya) v3)]
    [h4 (Ev chkf dec encTy n (EvSrc.ap v3 yb) w)]
    :where [n (EvSrc.itr g h (Code.sn l a b)) w])
  (ePrn [n Nat] [rho (List RV)] [r Exp] [c Code]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    :where [n (EvSrc.tm rho (Exp.prn r)) (RV.code c)])

  ;; functions and pairs.  λ stops at a closure; application is AppV.
  (eLam [n Nat] [rho (List RV)] [r U] [A Exp] [t Exp]
    :where [n (EvSrc.tm rho (Exp.lam r A t)) (RV.clos rho t)])
  (eApp [n Nat] [rho (List RV)] [f Exp] [u Exp] [vf RV] [vu RV] [w RV]
    [hf (Ev chkf dec encTy n (EvSrc.tm rho f) vf)]
    [hu (Ev chkf dec encTy n (EvSrc.tm rho u) vu)]
    [ha (Ev chkf dec encTy n (EvSrc.ap vf vu) w)]
    :where [n (EvSrc.tm rho (Exp.app f u)) w])
  (ePair [n Nat] [rho (List RV)] [S Exp] [a Exp] [b Exp] [va RV] [vb RV]
    [ha (Ev chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (Ev chkf dec encTy n (EvSrc.tm rho b) vb)]
    :where [n (EvSrc.tm rho (Exp.pair S a b)) (RV.pair va vb)])
  ;; y is variable 0 (the second component), x is variable 1.
  (eLet [n Nat] [rho (List RV)] [C Exp] [p Exp] [t Exp] [va RV] [vb RV] [w RV]
    [hp (Ev chkf dec encTy n (EvSrc.tm rho p) (RV.pair va vb))]
    [ht (Ev chkf dec encTy n
           (EvSrc.tm (List.cons RV vb (List.cons RV va rho)) t) w)]
    :where [n (EvSrc.tm rho (Exp.letp C p t)) w])

  ;; chk′ is chkf on the two codes
  (eChk [n Nat] [rho (List RV)] [c Exp] [d Exp] [cc Code] [dc Code]
    [hc (Ev chkf dec encTy n (EvSrc.tm rho c) (RV.code cc))]
    [hd (Ev chkf dec encTy n (EvSrc.tm rho d) (RV.code dc))]
    :where [n (EvSrc.tm rho (Exp.chk c d)) (RV.bool (chkf cc dc))])

  ;; reflect.  The success rule runs t′ at budget m on m tokens.  The two
  ;; failure rules are the model's Bool.rec / Option.rec defaults: the
  ;; conjunction of the cap test and chkf is false, or it is true and dec
  ;; returns none.  Both evaluate r and e first.
  (eReflOk [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code]
           [m Nat] [t2 Exp] [A Exp] [ve RV] [w RV]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (Ev chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.some (Prod Nat (Prod Exp Exp))
              (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t2 A))))]
    [ht (Ev chkf dec encTy m (EvSrc.tm (rtokens m) t2) w)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) w])
  (eReflNo [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (Ev chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])
  (eReflNone [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (Ev chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.none (Prod Nat (Prod Exp Exp))))]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])

  ;; inspect branches on chkf and rebinds the certificate and ⋆
  (eInspT [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (Ev chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.true)]
    [ht (Ev chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t1) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])
  (eInspF [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (Ev chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (Ev chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.false)]
    [ht (Ev chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t2) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])

  ;; application of a runtime value (the clause at an arrow)
  (apClos [n Nat] [rho (List RV)] [body Exp] [arg RV] [w RV]
    [h (Ev chkf dec encTy n (EvSrc.tm (List.cons RV arg rho) body) w)]
    :where [n (EvSrc.ap (RV.clos rho body) arg) w])
  (apBnil [n Nat] [s Sk] [arg RV]
    :where [n (EvSrc.ap (RV.bnil s) arg) (rdflt s)])
  (apBconsZ [n Nat] [h RV] [t RV]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl 0)) h])
  (apBconsS [n Nat] [h RV] [t RV] [k Nat] [w RV]
    [hp (Ev chkf dec encTy n (EvSrc.ap t (RV.lbl k)) w)]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl (Nat.succ k))) w]))

;; Eval n ρ t v and AppV n f a w, the two readings of Ev used in the paper.
(kdef Eval
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (=> Nat (List RV) Exp RV Prop))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, rho :- (List RV), t :- Exp, v :- RV]
    (Ev chkf dec encTy n (EvSrc.tm rho t) v)))

(kdef AppV
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (=> Nat RV RV RV Prop))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, f :- RV, arg :- RV, w :- RV]
    (Ev chkf dec encTy n (EvSrc.ap f arg) w)))

;; --- the logical relation ----------------------------------------------------

;; rel n s v α: v is related to the carrier value α at skeleton s (§5,
;; Theorem 4).  Equality at the base skeletons (a runtime token at ◇, and
;; the same Code at Syn and at R); componentwise at a product; at an arrow,
;; every related argument is sent to a terminating, related result.
;; Large elimination into Prop, as V's clauses do.
(kdef rel
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [s Sk] (=> RV (Car s) Prop))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, s :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (=> RV (Car t) Prop))
      (fn [v :- RV, _a :- Unit] (Eq RV v RV.star))
      (fn [v :- RV, a :- Bool] (Eq RV v (RV.bool a)))
      (fn [v :- RV, a :- Nat] (Eq RV v (RV.nat a)))
      (fn [v :- RV, a :- Nat] (Eq RV v (RV.lbl a)))
      (fn [v :- RV, a :- Code] (Eq RV v (RV.code a)))
      (fn [v :- RV, _a :- Unit] (Eq RV v RV.token))
      (fn [v :- RV, a :- Code] (Eq RV v (RV.cert a)))
      (fn [x :- Sk, y :- Sk, rx :- (=> RV (Car x) Prop), ry :- (=> RV (Car y) Prop)]
        (fn [v :- RV, f :- (Car (Sk.arr x y))]
          (forall [arg RV] (forall [alpha (Car x)]
            (=> (rx arg alpha)
              (Exists (fn [w :- RV]
                (And (Ev chkf dec encTy n (EvSrc.ap v arg) w)
                     (ry w (f alpha))))))))))
      (fn [x :- Sk, y :- Sk, rx :- (=> RV (Car x) Prop), ry :- (=> RV (Car y) Prop)]
        (fn [v :- RV, p :- (Car (Sk.prod x y))]
          (Exists (fn [a :- RV] (Exists (fn [b :- RV]
            (And (Eq RV v (RV.pair a b))
              (And (rx a (Prod.fst p)) (ry b (Prod.snd p))))))))))
      s)))

;; Environments, pointwise along the skeleton context (innermost first).
(kdef envRel
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [G (List Sk)] (=> (List RV) (HEnv G) Prop))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, G :- (List Sk)]
    (List.rec$1$0 Sk (fn [G :- (List Sk)] (=> (List RV) (HEnv G) Prop))
      (fn [rho :- (List RV), _e :- Unit] (Eq (List RV) rho (List.nil RV)))
      (fn [s :- Sk, rest :- (List Sk), ih :- (=> (List RV) (HEnv rest) Prop)]
        (fn [rho :- (List RV), e :- (HEnv (List.cons Sk s rest))]
          (Exists (fn [v :- RV] (Exists (fn [rho2 :- (List RV)]
            (And (Eq (List RV) rho (List.cons RV v rho2))
              (And (rel chkf dec encTy n s v (Prod.fst e))
                   (ih rho2 (Prod.snd e))))))))))
      G)))

;; --- where den reads skOf ----------------------------------------------------

;; argsOK G t: every application argument and every let scrutinee under t has
;; a defined skOf, in that subterm's binder context.  This is the hypothesis
;; Theorem 4 takes in place of nbr (adaptation 5).  Recurses on the term
;; alone, as skOfF does.
(a/defn argsOKF [e :- Exp] (=> (List Sk) Bool)
  (match e
    [(abort A t) (fn [G :- (List Sk)] ((argsOKF t) G))]
    [(ite b t x) (fn [G :- (List Sk)]
                   (Bool.and ((argsOKF b) G) (Bool.and ((argsOKF t) G) ((argsOKF x) G))))]
    [(elimB P b t x) (fn [G :- (List Sk)]
                       (Bool.and ((argsOKF b) G) (Bool.and ((argsOKF t) G) ((argsOKF x) G))))]
    [(succ m) (fn [G :- (List Sk)] ((argsOKF m) G))]
    [(recN P z st nv) (fn [G :- (List Sk)]
                        (Bool.and ((argsOKF z) G)
                          (Bool.and ((argsOKF nv) G)
                            ((argsOKF st) (sk2 (skel P) Sk.nat G)))))]
    [(caseL P a bs) (fn [G :- (List Sk)] (Bool.and ((argsOKF a) G) ((argsOKF bs) G)))]
    [(bcons h t) (fn [G :- (List Sk)] (Bool.and ((argsOKF h) G) ((argsOKF t) G)))]
    [(sleaf a) (fn [G :- (List Sk)] ((argsOKF a) G))]
    [(snode a c1 c2) (fn [G :- (List Sk)]
                       (Bool.and ((argsOKF a) G) (Bool.and ((argsOKF c1) G) ((argsOKF c2) G))))]
    [(recS P tl tn c) (fn [G :- (List Sk)]
                        (Bool.and ((argsOKF c) G)
                          (Bool.and ((argsOKF tl) (List.cons Sk Sk.lbl G))
                            ((argsOKF tn) (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G)))))))]
    [(leaf a) (fn [G :- (List Sk)] ((argsOKF a) G))]
    [(node d a r1 r2) (fn [G :- (List Sk)]
                        (Bool.and ((argsOKF d) G)
                          (Bool.and ((argsOKF a) G)
                            (Bool.and ((argsOKF r1) G) ((argsOKF r2) G)))))]
    [(itR X g h r) (fn [G :- (List Sk)]
                     (Bool.and ((argsOKF g) G) (Bool.and ((argsOKF h) G) ((argsOKF r) G))))]
    [(prn r) (fn [G :- (List Sk)] ((argsOKF r) G))]
    [(lam r A t) (fn [G :- (List Sk)] ((argsOKF t) (List.cons Sk (skel A) G)))]
    [(app f u) (fn [G :- (List Sk)]
                 (Bool.and ((argsOKF f) G)
                   (Bool.and ((argsOKF u) G)
                     (match (skOf G u) [none Bool.false] [(some _) Bool.true]))))]
    [(pair S a b) (fn [G :- (List Sk)] (Bool.and ((argsOKF a) G) ((argsOKF b) G)))]
    [(letp C p t) (fn [G :- (List Sk)]
                    (Bool.and ((argsOKF p) G)
                      (match (skOf G p)
                        [none Bool.false]
                        [(some sp) (match sp
                                     [(prod a b) ((argsOKF t) (sk2 b a G))]
                                     [_ Bool.false])])))]
    [(chk c d) (fn [G :- (List Sk)] (Bool.and ((argsOKF c) G) ((argsOKF d) G)))]
    [(h1 r s c e1 e2) (fn [G :- (List Sk)]
                        (Bool.and ((argsOKF r) G)
                          (Bool.and ((argsOKF s) G)
                            (Bool.and ((argsOKF c) G)
                              (Bool.and ((argsOKF e1) G) ((argsOKF e2) G))))))]
    [(refl D r e) (fn [G :- (List Sk)] (Bool.and ((argsOKF r) G) ((argsOKF e) G)))]
    [(insp X r c t1 t2) (fn [G :- (List Sk)]
                          (Bool.and ((argsOKF r) G)
                            (Bool.and ((argsOKF c) G)
                              (Bool.and ((argsOKF t1) (sk2 Sk.unit Sk.cert G))
                                ((argsOKF t2) (sk2 Sk.unit Sk.cert G))))))]
    [_ (fn [G :- (List Sk)] Bool.true)]))

(a/defn argsOK [G :- (List Sk), e :- Exp] Bool ((argsOKF e) G))

;; --- Theorem 4, the statement ------------------------------------------------

;; Adeq n: every simply typed term with argsOK, in a related environment,
;; evaluates to a value related to its denotation.  Theorem 4 is Adeq at
;; every budget, for every checker satisfying CheckSpec (reflect's outer
;; appeal).  The proof is strong induction on the budget and, within it,
;; induction on the SkJ derivation.
(kdef Adeq
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (=> Nat Prop))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat]
    (forall [G (List Sk)] (forall [t Exp] (forall [s Sk]
      (=> (SkJ Bool.false G t s)
        (=> (Eq Bool (argsOK G t) Bool.true)
          (forall [rho (List RV)] (forall [eta (HEnv G)]
            (=> (envRel chkf dec encTy n G rho eta)
              (Exists (fn [v :- RV]
                (And (Eval chkf dec encTy n rho t v)
                     (rel chkf dec encTy n s v
                       (den chkf dec encTy n t G s eta)))))))))))))))

;; Theorem 4 (R4-metatheory.md §5): termination and adequacy of evalₙ.
(kdef Theorem_4 Prop
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (=> (CheckSpec chkf dec encTy)
          (forall [n Nat] (Adeq chkf dec encTy n)))))))

;; --- a program evaluates -----------------------------------------------------

;; tt evaluates to the Boolean true, at any budget and in any environment.
(thm eval_tt
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, rho :- (List RV)]
  (Eval chkf dec encTy n rho Exp.tt (RV.bool Bool.true))
  (exact (Ev.eTT chkf dec encTy n rho)))

;; The de Bruijn head is the value at index 0.  rlookup returns a function of
;; the index, so this is definitional (the same shape as vadd).
(thm rlookup_head [h :- RV]
  (Eq (Option RV) (rlookup (List.cons RV h (List.nil RV)) 0) (Option.some RV h))
  (rfl))

(thm rlookup_zero [h :- RV, t :- (List RV)]
  (Eq (Option RV) (rlookup (List.cons RV h t) 0) (Option.some RV h))
  (rfl))

(thm rlookup_succ [h :- RV, t :- (List RV), j :- Nat]
  (Eq (Option RV) (rlookup (List.cons RV h t) (Nat.succ j)) (rlookup t j))
  (rfl))

;; Eval is a proposition indexed by the result, so an equality of runtime
;; values transports a derivation.  The base of rel is such an equality, and
;; the clauses that build on a numeral or a Boolean need the canonical one.
(thm eval_cast
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, rho :- (List RV), t :- Exp,
   v :- RV, w :- RV,
   h :- (Eval chkf dec encTy n rho t v),
   e :- (Eq RV v w)]
  (Eval chkf dec encTy n rho t w)
  (exact (Eq.mp (congrArg (fn [x :- RV] (Eval chkf dec encTy n rho t x)) e) h)))

;; Option.some is injective at Sk.  cases on the equality is the same proof
;; as some_injU; the kernel accepts it.
(thm some_injSk [x :- Sk, y :- Sk,
                 h :- (Eq (Option Sk) (Option.some Sk x) (Option.some Sk y))]
  (Eq Sk x y)
  (cases h) (rfl))

;; §5's running example, the one den_not_tt computes: (λ_. ite b ff tt) tt
;; evaluates to ff.  The scrutinee is variable 0, bound to tt, so the true
;; branch of ite is ff.
(thm eval_not_tt
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code)]
  (Eval chkf dec encTy 0 (List.nil RV)
    (Exp.app (Exp.lam U.uw Exp.tBool (Exp.ite (Exp.var 0) Exp.ff Exp.tt)) Exp.tt)
    (RV.bool Bool.false))
  (exact (Ev.eApp chkf dec encTy 0 (List.nil RV)
           (Exp.lam U.uw Exp.tBool (Exp.ite (Exp.var 0) Exp.ff Exp.tt))
           Exp.tt
           (RV.clos (List.nil RV) (Exp.ite (Exp.var 0) Exp.ff Exp.tt))
           (RV.bool Bool.true)
           (RV.bool Bool.false)
           (Ev.eLam chkf dec encTy 0 (List.nil RV) U.uw Exp.tBool
             (Exp.ite (Exp.var 0) Exp.ff Exp.tt))
           (Ev.eTT chkf dec encTy 0 (List.nil RV))
           (Ev.apClos chkf dec encTy 0 (List.nil RV)
             (Exp.ite (Exp.var 0) Exp.ff Exp.tt)
             (RV.bool Bool.true)
             (RV.bool Bool.false)
             (Ev.eIteT chkf dec encTy 0
               (List.cons RV (RV.bool Bool.true) (List.nil RV))
               (Exp.var 0) Exp.ff Exp.tt
               (RV.bool Bool.false)
               (Ev.eVar chkf dec encTy 0
                 (List.cons RV (RV.bool Bool.true) (List.nil RV))
                 0 (RV.bool Bool.true)
                 (Eq.refl (Option RV) (Option.some RV (RV.bool Bool.true))))
               (Ev.eFF chkf dec encTy 0
                 (List.cons RV (RV.bool Bool.true) (List.nil RV))))))))

;; --- the relation computes at bases; defaults are related -------------------

;; At a base skeleton rel is equality with the runtime constructor (◇ ignores
;; the Unit carrier and asks only for the token).  Both reduce by rfl.
(thm rel_bool_rfl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, b :- Bool]
  (rel chkf dec encTy n Sk.bool (RV.bool b) b)
  (rfl))

(thm rel_unit_rfl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat]
  (rel chkf dec encTy n Sk.unit RV.star Unit.unit)
  (rfl))

;; reifySk is a left inverse of skel.  Bases are rfl.  At an arrow or a
;; product, reifySk reduces to tPi / tSig (even though the match equation
;; lemmas for those arms were not emitted) and skel_pi / the product clause
;; bring out the two induction hypotheses.
(thm skel_reify [sk :- Sk]
  (Eq Sk (skel (reifySk sk)) sk)
  (induction sk)
  (all_goals (try rfl))
  (exact (Eq.trans
           (congrArg (fn [x :- Sk] (Sk.prod x (skel (reifySk t)))) ih_s)
           (congrArg (fn [y :- Sk] (Sk.prod s y)) ih_t)))
  (exact (Eq.trans
           (congrArg (fn [x :- Sk] (Sk.arr x (skel (reifySk t)))) ih_s)
           (congrArg (fn [y :- Sk] (Sk.arr s y)) ih_t))))

;; The paper's "defaults are related", by induction on the skeleton.  At an
;; arrow the default closure aborts at the reified codomain, so applying it
;; returns rdflt of that skeleton, which skel_reify identifies with the
;; codomain default.  dflt_arr drops the ignored argument.
(thm rdflt_rel
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, sk :- Sk]
  (rel chkf dec encTy n sk (rdflt sk) (dflt sk))
  (induction sk)
  (all_goals (try rfl))
  (constructor) (exact (rdflt s))
  (constructor) (exact (rdflt t))
  (constructor) (rfl)
  (constructor) (exact ih_s) (exact ih_t)
  (intro arg) (intro alpha) (intro harg)
  (constructor)
  (exact (rdflt (skel (reifySk t))))
  (constructor)
  (exact (Ev.apClos chkf dec encTy n (List.nil RV)
           (Exp.abort (reifySk t) Exp.star) arg
           (rdflt (skel (reifySk t)))
           (Ev.eAbort chkf dec encTy n
             (List.cons RV arg (List.nil RV))
             (reifySk t) Exp.star RV.star
             (Ev.eStar chkf dec encTy n
               (List.cons RV arg (List.nil RV))))))
  (rw [(dflt_arr s t alpha)])
  (rw [(congrArg rdflt (skel_reify t))])
  (exact ih_t))

;; --- adequacy of the constants, abort and H₁ --------------------------------
;; Each is the corresponding case of Theorem 4, before the induction is
;; assembled: the value exists, the evaluation derives it, and it is related
;; to the denotation.  den_*_at plus coe_self turn ⟦c⟧ into the carrier
;; constant; rel at that base is then rfl.  abort returns rdflt of its
;; annotation, related to dflt by rdflt_rel.  H₁ returns ⋆, and rel at Unit
;; does not read the denotation (which is dflt Unit).

(thm adeq_star
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho Exp.star v)
         (rel chkf dec encTy n Sk.unit v (den chkf dec encTy n Exp.star G Sk.unit eta)))))
  (constructor) (exact RV.star)
  (constructor) (exact (Ev.eStar chkf dec encTy n rho)) (rfl))

(thm adeq_tt
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho Exp.tt v)
         (rel chkf dec encTy n Sk.bool v (den chkf dec encTy n Exp.tt G Sk.bool eta)))))
  (rw [(den_tt_at chkf dec encTy n G Sk.bool eta)])
  (rw [(coe_self Sk.bool Bool.true)])
  (constructor) (exact (RV.bool Bool.true))
  (constructor) (exact (Ev.eTT chkf dec encTy n rho)) (rfl))

(thm adeq_ff
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho Exp.ff v)
         (rel chkf dec encTy n Sk.bool v (den chkf dec encTy n Exp.ff G Sk.bool eta)))))
  (rw [(den_ff_at chkf dec encTy n G Sk.bool eta)])
  (rw [(coe_self Sk.bool Bool.false)])
  (constructor) (exact (RV.bool Bool.false))
  (constructor) (exact (Ev.eFF chkf dec encTy n rho)) (rfl))

(thm adeq_zero
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho Exp.zero v)
         (rel chkf dec encTy n Sk.nat v (den chkf dec encTy n Exp.zero G Sk.nat eta)))))
  (rw [(den_zero_at chkf dec encTy n G Sk.nat eta)])
  (rw [(coe_self Sk.nat 0)])
  (constructor) (exact (RV.nat 0))
  (constructor) (exact (Ev.eZero chkf dec encTy n rho)) (rfl))

(thm adeq_lbl
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, l :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.lbl l) v)
         (rel chkf dec encTy n Sk.lbl v (den chkf dec encTy n (Exp.lbl l) G Sk.lbl eta)))))
  (rw [(den_lbl_at chkf dec encTy n l G Sk.lbl eta)])
  (rw [(coe_self Sk.lbl l)])
  (constructor) (exact (RV.lbl l))
  (constructor) (exact (Ev.eLbl chkf dec encTy n rho l)) (rfl))

;; The argument is evaluated (ht) and then discarded.  The induction supplies
;; ht from the hypothesis for t; this lemma does not re-prove it.
(thm adeq_abort
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), A :- Exp, t :- Exp,
   rho :- (List RV), eta :- (HEnv G), vt :- RV,
   ht :- (Eval chkf dec encTy n rho t vt)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.abort A t) v)
         (rel chkf dec encTy n (skel A) v
           (den chkf dec encTy n (Exp.abort A t) G (skel A) eta)))))
  (rw [(den_abort_at chkf dec encTy n A t G (skel A) eta)])
  (constructor) (exact (rdflt (skel A)))
  (constructor) (exact (Ev.eAbort chkf dec encTy n rho A t vt ht))
  (exact (rdflt_rel chkf dec encTy n (skel A))))

(thm adeq_h1
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   vr :- RV, vs :- RV, vc :- RV, v1 :- RV, v2 :- RV,
   hr :- (Eval chkf dec encTy n rho r vr),
   hs :- (Eval chkf dec encTy n rho s vs),
   hc :- (Eval chkf dec encTy n rho c vc),
   h1 :- (Eval chkf dec encTy n rho e1 v1),
   h2 :- (Eval chkf dec encTy n rho e2 v2)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.h1 r s c e1 e2) v)
         (rel chkf dec encTy n Sk.unit v
           (den chkf dec encTy n (Exp.h1 r s c e1 e2) G Sk.unit eta)))))
  (constructor) (exact RV.star)
  (constructor)
  (exact (Ev.eH1 chkf dec encTy n rho r s c e1 e2 vr vs vc v1 v2 hr hs hc h1 h2))
  (rfl))

;; A related environment agrees with lookup at every index nthS defines.
;; The budget is named cap: cases on the index would otherwise rename the
;; predecessor, because the budget already occupies the name n.  At 0 the
;; head is coe of itself, hence the first component (coe_self).  At a
;; successor, nthS.eq_3 and rlookup_succ step into the tail.
(thm env_at
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk)]
  (forall [rho (List RV)] (forall [eta (HEnv G)]
    (=> (envRel chkf dec encTy cap G rho eta)
      (forall [i Nat] (forall [s Sk]
        (=> (Eq (Option Sk) (nthS G i) (Option.some Sk s))
          (Exists (fn [v :- RV]
            (And (Eq (Option RV) (rlookup rho i) (Option.some RV v))
                 (rel chkf dec encTy cap s v (lookup G i s eta)))))))))))
  (induction G)
  (intro rho) (intro eta) (intro hr) (intro i) (intro s) (intro hs)
  (exact (False.elim (none_ne_someS s (Eq.trans (Eq.symm (nthS.eq_1 i)) hs))))
  (intro rho) (intro eta) (intro hr) (intro i) (intro s) (intro hs)
  (cases i)
  (have hs2 (Eq (Option Sk) (Option.some Sk head) (Option.some Sk s))
    (Eq.trans (Eq.symm (nthS.eq_2 head tail)) hs))
  (have hhd (Eq Sk head s) (some_injSk head s hs2))
  (subst hhd)
  (refine' (exT RV _ _ hr _)) (intro v hex)
  (refine' (exT (List RV) _ _ hex _)) (intro rho2 hp)
  (have hl (Eq (List RV) rho (List.cons RV v rho2)) (And.left hp))
  (have hv (rel chkf dec encTy cap s v (Prod.fst eta)) (And.left (And.right hp)))
  (have hr0 (Eq (Option RV) (rlookup rho 0) (Option.some RV v))
    (Eq.trans (congrArg (fn [r :- (List RV)] (rlookup r 0)) hl) (rlookup_zero v rho2)))
  (have hv2 (rel chkf dec encTy cap s v (coe s s (Prod.fst eta)))
    (Eq.mp (congrArg (fn [a :- (Car s)] (rel chkf dec encTy cap s v a))
                     (Eq.symm (coe_self s (Prod.fst eta)))) hv))
  (constructor) (exact v) (constructor) (exact hr0) (exact hv2)
  (refine' (exT RV _ _ hr _)) (intro v0 hex)
  (refine' (exT (List RV) _ _ hex _)) (intro rho2 hp)
  (have hl (Eq (List RV) rho (List.cons RV v0 rho2)) (And.left hp))
  (have hrest (envRel chkf dec encTy cap tail rho2 (Prod.snd eta)) (And.right (And.right hp)))
  (have hs3 (Eq (Option Sk) (nthS tail n) (Option.some Sk s))
    (Eq.trans (Eq.symm (nthS.eq_3 head tail n)) hs))
  (refine' (exT RV _ _ (ih_tail rho2 (Prod.snd eta) hrest n s hs3) _))
  (intro w hand)
  (have hrn (Eq (Option RV) (rlookup rho2 n) (Option.some RV w)) (And.left hand))
  (have hrel (rel chkf dec encTy cap s w (lookup tail n s (Prod.snd eta))) (And.right hand))
  (have hr1 (Eq (Option RV) (rlookup rho (Nat.succ n)) (Option.some RV w))
    (Eq.trans (congrArg (fn [r :- (List RV)] (rlookup r (Nat.succ n))) hl)
              (Eq.trans (rlookup_succ v0 rho2 n) hrn)))
  (constructor) (exact w) (constructor) (exact hr1) (exact hrel))

;; Successor: the premise denotes a numeral k, so it evaluates at RV.nat k
;; (eval_cast along rel), and the successor value is related to succ k.
(thm adeq_succ
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), m :- Exp,
   rho :- (List RV), eta :- (HEnv G), vt :- RV,
   ht :- (Eval chkf dec encTy n rho m vt),
   he :- (rel chkf dec encTy n Sk.nat vt (den chkf dec encTy n m G Sk.nat eta))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.succ m) v)
         (rel chkf dec encTy n Sk.nat v
           (den chkf dec encTy n (Exp.succ m) G Sk.nat eta)))))
  (rw [(den_succ_at chkf dec encTy n m G Sk.nat eta)])
  (rw [(coe_self Sk.nat (Nat.succ (den chkf dec encTy n m G Sk.nat eta)))])
  (constructor)
  (exact (RV.nat (Nat.succ (den chkf dec encTy n m G Sk.nat eta))))
  (constructor)
  (exact (Ev.eSucc chkf dec encTy n rho m
           (den chkf dec encTy n m G Sk.nat eta)
           (eval_cast chkf dec encTy n rho m vt
             (RV.nat (den chkf dec encTy n m G Sk.nat eta)) ht he)))
  (rfl))

;; Variables (Theorem 4): the value env_at finds is the denotation, which is
;; lookup, and eVar records the rlookup equation.
(thm adeq_var
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), i :- Nat, s :- Sk,
   hs :- (Eq (Option Sk) (nthS G i) (Option.some Sk s)),
   rho :- (List RV), eta :- (HEnv G),
   hr :- (envRel chkf dec encTy n G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.var i) v)
         (rel chkf dec encTy n s v (den chkf dec encTy n (Exp.var i) G s eta)))))
  (rw [(den_var_at chkf dec encTy n i G s eta)])
  (refine' (exT RV _ _ (env_at chkf dec encTy n G rho eta hr i s hs) _))
  (intro v hand)
  (have hl (Eq (Option RV) (rlookup rho i) (Option.some RV v)) (And.left hand))
  (have hv (rel chkf dec encTy n s v (lookup G i s eta)) (And.right hand))
  (constructor) (exact v)
  (constructor)
  (exact (Ev.eVar chkf dec encTy n rho i v hl))
  (exact hv))

;; Extending a related environment by one related value.  The head of a
;; de Bruijn environment is the new binding.
(thm envRel_cons
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, s0 :- Sk, G :- (List Sk),
   v :- RV, rho :- (List RV), alpha :- (Car s0), eta :- (HEnv G),
   hv :- (rel chkf dec encTy n s0 v alpha),
   hr :- (envRel chkf dec encTy n G rho eta)]
  (envRel chkf dec encTy n (List.cons Sk s0 G) (List.cons RV v rho) (Prod.mk alpha eta))
  (constructor) (exact v)
  (constructor) (exact rho)
  (constructor) (rfl)
  (constructor) (exact hv) (exact hr))

(thm arrCase_arr [x :- Sk, y :- Sk, f :- (forall [a Sk] (forall [b Sk] (Car (Sk.arr a b))))]
  (= (arrCase (Sk.arr x y) f) (f x y))
  (rfl))

;; λ (Theorem 4): the value is the closure of the current environment.  At an
;; arrow, a related argument extends the environment (envRel_cons) and the
;; hypothesis for the body supplies the result.  den_lam at that arrow is the
;; denotation of the body, definitionally, so the relation matches.
(thm adeq_lam
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   r :- U, A :- Exp, t :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   hr :- (envRel chkf dec encTy n G rho eta),
   ih :- (forall [rho2 (List RV)] (forall [eta2 (HEnv (List.cons Sk (skel A) G))]
           (=> (envRel chkf dec encTy n (List.cons Sk (skel A) G) rho2 eta2)
             (Exists (fn [w :- RV]
               (And (Eval chkf dec encTy n rho2 t w)
                    (rel chkf dec encTy n s w
                      (den chkf dec encTy n t (List.cons Sk (skel A) G) s eta2))))))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.lam r A t) v)
         (rel chkf dec encTy n (Sk.arr (skel A) s) v
           (den chkf dec encTy n (Exp.lam r A t) G (Sk.arr (skel A) s) eta)))))
  (rw [(den_lam_at chkf dec encTy n r A t G (Sk.arr (skel A) s) eta)])
  (constructor) (exact (RV.clos rho t))
  (constructor) (exact (Ev.eLam chkf dec encTy n rho r A t))
  (intro arg) (intro alpha) (intro harg)
  (have henv (envRel chkf dec encTy n (List.cons Sk (skel A) G)
               (List.cons RV arg rho) (Prod.mk alpha eta))
    (envRel_cons chkf dec encTy n (skel A) G arg rho alpha eta harg hr))
  (refine' (exT RV _ _ (ih (List.cons RV arg rho) (Prod.mk alpha eta) henv) _))
  (intro w hand)
  (have hev (Eval chkf dec encTy n (List.cons RV arg rho) t w) (And.left hand))
  (have hrel (rel chkf dec encTy n s w
               (den chkf dec encTy n t (List.cons Sk (skel A) G) s (Prod.mk alpha eta)))
    (And.right hand))
  (constructor) (exact w)
  (constructor)
  (exact (Ev.apClos chkf dec encTy n rho t arg w hev))
  (exact hrel))

;; Application (Theorem 4), when skOf of the argument is the skeleton SkJ
;; gave it.  den_app_some is then ⟦f⟧(⟦u⟧).  The arrow clause of rel on f,
;; at the value of u, is the application.  argsOK is what guarantees skOf is
;; some; that the some is this skeleton is skOf_agree, not yet assembled
;; (bnil is SkJ-typed at every arrow and skOf of it is none, so the equation
;; is not true for every SkJ term).
(thm adeq_app
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   f :- Exp, u :- Exp, s :- Sk, t :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   hsu :- (Eq (Option Sk) (skOf G u) (Option.some Sk s)),
   ihf :- (Exists (fn [vf :- RV]
            (And (Eval chkf dec encTy n rho f vf)
                 (rel chkf dec encTy n (Sk.arr s t) vf
                   (den chkf dec encTy n f G (Sk.arr s t) eta))))),
   ihu :- (Exists (fn [vu :- RV]
            (And (Eval chkf dec encTy n rho u vu)
                 (rel chkf dec encTy n s vu
                   (den chkf dec encTy n u G s eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.app f u) v)
         (rel chkf dec encTy n t v
           (den chkf dec encTy n (Exp.app f u) G t eta)))))
  (rw [(den_app_some chkf dec encTy n f u G s t eta hsu)])
  (refine' (exT RV _ _ ihf _)) (intro vf hf)
  (have hef (Eval chkf dec encTy n rho f vf) (And.left hf))
  (have hrf (rel chkf dec encTy n (Sk.arr s t) vf
              (den chkf dec encTy n f G (Sk.arr s t) eta)) (And.right hf))
  (refine' (exT RV _ _ ihu _)) (intro vu hu)
  (have heu (Eval chkf dec encTy n rho u vu) (And.left hu))
  (have hru (rel chkf dec encTy n s vu (den chkf dec encTy n u G s eta)) (And.right hu))
  (refine' (exT RV _ _ (hrf vu (den chkf dec encTy n u G s eta) hru) _))
  (intro w hw)
  (have hap (Ev chkf dec encTy n (EvSrc.ap vf vu) w) (And.left hw))
  (have hrel (rel chkf dec encTy n t w
               ((den chkf dec encTy n f G (Sk.arr s t) eta)
                (den chkf dec encTy n u G s eta))) (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (Ev.eApp chkf dec encTy n rho f u vf vu w hef heu hap))
  (exact hrel))

;; ite (Theorem 4).  The scrutinee denotes a Boolean c, so it evaluates at
;; RV.bool c (eval_cast).  The false case runs the else branch and the true
;; case the then branch; Bool.rec in the denotation does the same.  adeq_ite
;; is the case split; adeq_ite_den is the denotation.
(thm adeq_ite
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   b :- Exp, t :- Exp, e :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G), c :- Bool,
   hb :- (Eval chkf dec encTy n rho b (RV.bool c)),
   iht :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho t v)
                 (rel chkf dec encTy n s v (den chkf dec encTy n t G s eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho e v)
                 (rel chkf dec encTy n s v (den chkf dec encTy n e G s eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.ite b t e) v)
         (rel chkf dec encTy n s v
           (Bool.rec$1 (fn [_ :- Bool] (Car s))
             (den chkf dec encTy n e G s eta)
             (den chkf dec encTy n t G s eta) c)))))
  (cases c)
  (refine' (exT RV _ _ ihe _)) (intro ve he)
  (have hee (Eval chkf dec encTy n rho e ve) (And.left he))
  (have hre (rel chkf dec encTy n s ve (den chkf dec encTy n e G s eta)) (And.right he))
  (constructor) (exact ve)
  (constructor)
  (exact (Ev.eIteF chkf dec encTy n rho b t e ve hb hee))
  (exact hre)
  (refine' (exT RV _ _ iht _)) (intro vt ht)
  (have het (Eval chkf dec encTy n rho t vt) (And.left ht))
  (have hrt (rel chkf dec encTy n s vt (den chkf dec encTy n t G s eta)) (And.right ht))
  (constructor) (exact vt)
  (constructor)
  (exact (Ev.eIteT chkf dec encTy n rho b t e vt hb het))
  (exact hrt))

(thm adeq_ite_den
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   b :- Exp, t :- Exp, e :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   ihb :- (Exists (fn [vb :- RV]
            (And (Eval chkf dec encTy n rho b vb)
                 (rel chkf dec encTy n Sk.bool vb (den chkf dec encTy n b G Sk.bool eta))))),
   iht :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho t v)
                 (rel chkf dec encTy n s v (den chkf dec encTy n t G s eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho e v)
                 (rel chkf dec encTy n s v (den chkf dec encTy n e G s eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.ite b t e) v)
         (rel chkf dec encTy n s v (den chkf dec encTy n (Exp.ite b t e) G s eta)))))
  (rw [(den_ite_at chkf dec encTy n b t e G s eta)])
  (refine' (exT RV _ _ ihb _)) (intro vb hb)
  (have hev (Eval chkf dec encTy n rho b vb) (And.left hb))
  (have hr (rel chkf dec encTy n Sk.bool vb (den chkf dec encTy n b G Sk.bool eta)) (And.right hb))
  (exact (adeq_ite chkf dec encTy n G b t e s rho eta
           (den chkf dec encTy n b G Sk.bool eta)
           (eval_cast chkf dec encTy n rho b vb
             (RV.bool (den chkf dec encTy n b G Sk.bool eta)) hev hr)
           iht ihe)))

(thm prodCase_prod [x :- Sk, y :- Sk, f :- (forall [a Sk] (forall [b Sk] (Car (Sk.prod a b))))]
  (= (prodCase (Sk.prod x y) f) (f x y))
  (rfl))

;; Pair (Theorem 4).  The value is the pair of the component values, and the
;; product clause of rel is componentwise.  prodCase at a product skeleton is
;; the pair of the denotations.
(thm adeq_pair
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   r :- U, A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ihx :- (Exists (fn [vx :- RV]
            (And (Eval chkf dec encTy n rho x vx)
                 (rel chkf dec encTy n (skel A) vx (den chkf dec encTy n x G (skel A) eta))))),
   ihy :- (Exists (fn [vy :- RV]
            (And (Eval chkf dec encTy n rho y vy)
                 (rel chkf dec encTy n (skel B) vy (den chkf dec encTy n y G (skel B) eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.pair (Exp.tSig r A B) x y) v)
         (rel chkf dec encTy n (Sk.prod (skel A) (skel B)) v
           (den chkf dec encTy n (Exp.pair (Exp.tSig r A B) x y) G (Sk.prod (skel A) (skel B)) eta)))))
  (rw [(den_pair_at chkf dec encTy n (Exp.tSig r A B) x y G (Sk.prod (skel A) (skel B)) eta)])
  (rw [(prodCase_prod (skel A) (skel B)
        (fn [xa :- Sk, yb :- Sk]
          (Prod.mk (den chkf dec encTy n x G xa eta) (den chkf dec encTy n y G yb eta))))])
  (refine' (exT RV _ _ ihx _)) (intro vx hx)
  (have hex (Eval chkf dec encTy n rho x vx) (And.left hx))
  (have hrx (rel chkf dec encTy n (skel A) vx (den chkf dec encTy n x G (skel A) eta)) (And.right hx))
  (refine' (exT RV _ _ ihy _)) (intro vy hy)
  (have hey (Eval chkf dec encTy n rho y vy) (And.left hy))
  (have hry (rel chkf dec encTy n (skel B) vy (den chkf dec encTy n y G (skel B) eta)) (And.right hy))
  (constructor) (exact (RV.pair vx vy))
  (constructor)
  (exact (Ev.ePair chkf dec encTy n rho (Exp.tSig r A B) x y vx vy hex hey))
  (constructor) (exact vx)
  (constructor) (exact vy)
  (constructor) (rfl)
  (constructor) (exact hrx) (exact hry))

;; chk′ (Theorem 4).  Both arguments are codes.  The result is the Boolean
;; chkf returns on those codes, which is exactly the denotation at Bool.
(thm adeq_chk
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), c :- Exp, d :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ihc :- (Exists (fn [vc :- RV]
            (And (Eval chkf dec encTy n rho c vc)
                 (rel chkf dec encTy n Sk.syn vc (den chkf dec encTy n c G Sk.syn eta))))),
   ihd :- (Exists (fn [vd :- RV]
            (And (Eval chkf dec encTy n rho d vd)
                 (rel chkf dec encTy n Sk.syn vd (den chkf dec encTy n d G Sk.syn eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.chk c d) v)
         (rel chkf dec encTy n Sk.bool v (den chkf dec encTy n (Exp.chk c d) G Sk.bool eta)))))
  (rw [(den_chk_at chkf dec encTy n c d G Sk.bool eta)])
  (rw [(coe_self Sk.bool (chkf (den chkf dec encTy n c G Sk.syn eta) (den chkf dec encTy n d G Sk.syn eta)))])
  (refine' (exT RV _ _ ihc _)) (intro vc hc)
  (have hec (Eval chkf dec encTy n rho c vc) (And.left hc))
  (have hrc (rel chkf dec encTy n Sk.syn vc (den chkf dec encTy n c G Sk.syn eta)) (And.right hc))
  (refine' (exT RV _ _ ihd _)) (intro vd hd)
  (have hed (Eval chkf dec encTy n rho d vd) (And.left hd))
  (have hrd (rel chkf dec encTy n Sk.syn vd (den chkf dec encTy n d G Sk.syn eta)) (And.right hd))
  (constructor)
  (exact (RV.bool (chkf (den chkf dec encTy n c G Sk.syn eta) (den chkf dec encTy n d G Sk.syn eta))))
  (constructor)
  (exact (Ev.eChk chkf dec encTy n rho c d
           (den chkf dec encTy n c G Sk.syn eta)
           (den chkf dec encTy n d G Sk.syn eta)
           (eval_cast chkf dec encTy n rho c vc (RV.code (den chkf dec encTy n c G Sk.syn eta)) hec hrc)
           (eval_cast chkf dec encTy n rho d vd (RV.code (den chkf dec encTy n d G Sk.syn eta)) hed hrd)))
  (rfl))

;; An empty branch list denotes the default function at Lbl → s: every
;; application returns the default of s (apBnil), and that default is related
;; to the denotational default (rdflt_rel).
(thm adeq_bnil
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), s :- Sk,
   rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho Exp.bnil v)
         (rel chkf dec encTy n (Sk.arr Sk.lbl s) v
           (den chkf dec encTy n Exp.bnil G (Sk.arr Sk.lbl s) eta)))))
  (rw [(den_bnil_at chkf dec encTy n G (Sk.arr Sk.lbl s) eta)])
  (constructor) (exact (RV.bnil s))
  (constructor) (exact (Ev.eBnil chkf dec encTy n rho s))
  (intro arg) (intro alpha) (intro harg)
  (constructor) (exact (rdflt s))
  (constructor) (exact (Ev.apBnil chkf dec encTy n s arg))
  (rw [(dflt_arr Sk.lbl s alpha)])
  (exact (rdflt_rel chkf dec encTy n s)))

;; elimBool (Theorem 4), the same Boolean split as ite, at the skeleton of
;; the motive.  adeq_elim is the split; adeq_elim_den is the denotation.
(thm adeq_elim
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   rho :- (List RV), eta :- (HEnv G), c :- Bool,
   hb :- (Eval chkf dec encTy n rho b (RV.bool c)),
   iht :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho t v)
                 (rel chkf dec encTy n (skel P) v (den chkf dec encTy n t G (skel P) eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho e v)
                 (rel chkf dec encTy n (skel P) v (den chkf dec encTy n e G (skel P) eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.elimB P b t e) v)
         (rel chkf dec encTy n (skel P) v
           (Bool.rec$1 (fn [_ :- Bool] (Car (skel P)))
             (den chkf dec encTy n e G (skel P) eta)
             (den chkf dec encTy n t G (skel P) eta) c)))))
  (cases c)
  (refine' (exT RV _ _ ihe _)) (intro ve he)
  (have hee (Eval chkf dec encTy n rho e ve) (And.left he))
  (have hre (rel chkf dec encTy n (skel P) ve (den chkf dec encTy n e G (skel P) eta)) (And.right he))
  (constructor) (exact ve)
  (constructor)
  (exact (Ev.eElimF chkf dec encTy n rho P b t e ve hb hee))
  (exact hre)
  (refine' (exT RV _ _ iht _)) (intro vt ht)
  (have het (Eval chkf dec encTy n rho t vt) (And.left ht))
  (have hrt (rel chkf dec encTy n (skel P) vt (den chkf dec encTy n t G (skel P) eta)) (And.right ht))
  (constructor) (exact vt)
  (constructor)
  (exact (Ev.eElimT chkf dec encTy n rho P b t e vt hb het))
  (exact hrt))

(thm adeq_elim_den
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ihb :- (Exists (fn [vb :- RV]
            (And (Eval chkf dec encTy n rho b vb)
                 (rel chkf dec encTy n Sk.bool vb (den chkf dec encTy n b G Sk.bool eta))))),
   iht :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho t v)
                 (rel chkf dec encTy n (skel P) v (den chkf dec encTy n t G (skel P) eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy n rho e v)
                 (rel chkf dec encTy n (skel P) v (den chkf dec encTy n e G (skel P) eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.elimB P b t e) v)
         (rel chkf dec encTy n (skel P) v (den chkf dec encTy n (Exp.elimB P b t e) G (skel P) eta)))))
  (rw [(den_elimB_at chkf dec encTy n P b t e G (skel P) eta)])
  (refine' (exT RV _ _ ihb _)) (intro vb hb)
  (have hev (Eval chkf dec encTy n rho b vb) (And.left hb))
  (have hr (rel chkf dec encTy n Sk.bool vb (den chkf dec encTy n b G Sk.bool eta)) (And.right hb))
  (exact (adeq_elim chkf dec encTy n G P b t e rho eta
           (den chkf dec encTy n b G Sk.bool eta)
           (eval_cast chkf dec encTy n rho b vb
             (RV.bool (den chkf dec encTy n b G Sk.bool eta)) hev hr)
           iht ihe)))

;; print (Theorem 4).  A certificate and its printed code are the same Code;
;; print forgets nothing because the model has no token payload.
(thm adeq_prn
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), c :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ih :- (Exists (fn [vc :- RV]
           (And (Eval chkf dec encTy n rho c vc)
                (rel chkf dec encTy n Sk.cert vc (den chkf dec encTy n c G Sk.cert eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.prn c) v)
         (rel chkf dec encTy n Sk.syn v (den chkf dec encTy n (Exp.prn c) G Sk.syn eta)))))
  (rw [(den_prn_at chkf dec encTy n c G Sk.syn eta)])
  (rw [(coe_self Sk.syn (den chkf dec encTy n c G Sk.cert eta))])
  (refine' (exT RV _ _ ih _)) (intro vc hc)
  (have he (Eval chkf dec encTy n rho c vc) (And.left hc))
  (have hr (rel chkf dec encTy n Sk.cert vc (den chkf dec encTy n c G Sk.cert eta)) (And.right hc))
  (constructor) (exact (RV.code (den chkf dec encTy n c G Sk.cert eta)))
  (constructor)
  (exact (Ev.ePrn chkf dec encTy n rho c (den chkf dec encTy n c G Sk.cert eta)
           (eval_cast chkf dec encTy n rho c vc
             (RV.cert (den chkf dec encTy n c G Sk.cert eta)) he hr)))
  (rfl))

;; A code leaf is the leaf of the label it denotes (Theorem 4).
(thm adeq_sleaf
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), a :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ih :- (Exists (fn [va :- RV]
           (And (Eval chkf dec encTy n rho a va)
                (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.sleaf a) v)
         (rel chkf dec encTy n Sk.syn v (den chkf dec encTy n (Exp.sleaf a) G Sk.syn eta)))))
  (rw [(den_sleaf_at chkf dec encTy n a G Sk.syn eta)])
  (rw [(coe_self Sk.syn (Code.sl (den chkf dec encTy n a G Sk.lbl eta)))])
  (refine' (exT RV _ _ ih _)) (intro va ha)
  (have he (Eval chkf dec encTy n rho a va) (And.left ha))
  (have hr (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)) (And.right ha))
  (constructor) (exact (RV.code (Code.sl (den chkf dec encTy n a G Sk.lbl eta))))
  (constructor)
  (exact (Ev.eSleaf chkf dec encTy n rho a (den chkf dec encTy n a G Sk.lbl eta)
           (eval_cast chkf dec encTy n rho a va
             (RV.lbl (den chkf dec encTy n a G Sk.lbl eta)) he hr)))
  (rfl))

;; A certificate leaf is the same construction at the certificate skeleton.
(thm adeq_leaf
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), a :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ih :- (Exists (fn [va :- RV]
           (And (Eval chkf dec encTy n rho a va)
                (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.leaf a) v)
         (rel chkf dec encTy n Sk.cert v (den chkf dec encTy n (Exp.leaf a) G Sk.cert eta)))))
  (rw [(den_leaf_at chkf dec encTy n a G Sk.cert eta)])
  (rw [(coe_self Sk.cert (Code.sl (den chkf dec encTy n a G Sk.lbl eta)))])
  (refine' (exT RV _ _ ih _)) (intro va ha)
  (have he (Eval chkf dec encTy n rho a va) (And.left ha))
  (have hr (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)) (And.right ha))
  (constructor) (exact (RV.cert (Code.sl (den chkf dec encTy n a G Sk.lbl eta))))
  (constructor)
  (exact (Ev.eLeaf chkf dec encTy n rho a (den chkf dec encTy n a G Sk.lbl eta)
           (eval_cast chkf dec encTy n rho a va
             (RV.lbl (den chkf dec encTy n a G Sk.lbl eta)) he hr)))
  (rfl))

;; A code node is the node of the label and the two codes (Theorem 4).
(thm adeq_snode
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   a :- Exp, c1 :- Exp, c2 :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   iha :- (Exists (fn [va :- RV]
            (And (Eval chkf dec encTy n rho a va)
                 (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta))))),
   ih1 :- (Exists (fn [v1 :- RV]
            (And (Eval chkf dec encTy n rho c1 v1)
                 (rel chkf dec encTy n Sk.syn v1 (den chkf dec encTy n c1 G Sk.syn eta))))),
   ih2 :- (Exists (fn [v2 :- RV]
            (And (Eval chkf dec encTy n rho c2 v2)
                 (rel chkf dec encTy n Sk.syn v2 (den chkf dec encTy n c2 G Sk.syn eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.snode a c1 c2) v)
         (rel chkf dec encTy n Sk.syn v (den chkf dec encTy n (Exp.snode a c1 c2) G Sk.syn eta)))))
  (rw [(den_snode_at chkf dec encTy n a c1 c2 G Sk.syn eta)])
  (rw [(coe_self Sk.syn (Code.sn (den chkf dec encTy n a G Sk.lbl eta)
                           (den chkf dec encTy n c1 G Sk.syn eta)
                           (den chkf dec encTy n c2 G Sk.syn eta)))])
  (refine' (exT RV _ _ iha _)) (intro va ha)
  (have hea (Eval chkf dec encTy n rho a va) (And.left ha))
  (have hra (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)) (And.right ha))
  (refine' (exT RV _ _ ih1 _)) (intro v1 h1)
  (have he1 (Eval chkf dec encTy n rho c1 v1) (And.left h1))
  (have hr1 (rel chkf dec encTy n Sk.syn v1 (den chkf dec encTy n c1 G Sk.syn eta)) (And.right h1))
  (refine' (exT RV _ _ ih2 _)) (intro v2 h2)
  (have he2 (Eval chkf dec encTy n rho c2 v2) (And.left h2))
  (have hr2 (rel chkf dec encTy n Sk.syn v2 (den chkf dec encTy n c2 G Sk.syn eta)) (And.right h2))
  (constructor)
  (exact (RV.code (Code.sn (den chkf dec encTy n a G Sk.lbl eta)
                    (den chkf dec encTy n c1 G Sk.syn eta)
                    (den chkf dec encTy n c2 G Sk.syn eta))))
  (constructor)
  (exact (Ev.eSnode chkf dec encTy n rho a c1 c2
           (den chkf dec encTy n a G Sk.lbl eta)
           (den chkf dec encTy n c1 G Sk.syn eta)
           (den chkf dec encTy n c2 G Sk.syn eta)
           (eval_cast chkf dec encTy n rho a va (RV.lbl (den chkf dec encTy n a G Sk.lbl eta)) hea hra)
           (eval_cast chkf dec encTy n rho c1 v1 (RV.code (den chkf dec encTy n c1 G Sk.syn eta)) he1 hr1)
           (eval_cast chkf dec encTy n rho c2 v2 (RV.code (den chkf dec encTy n c2 G Sk.syn eta)) he2 hr2)))
  (rfl))

;; A certificate node evaluates its token and drops it.  The denoted code is
;; built from the label and the two subtrees; the token does not appear.
(thm adeq_node
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   d :- Exp, a :- Exp, r1 :- Exp, r2 :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   ihd :- (Exists (fn [vd :- RV] (Eval chkf dec encTy n rho d vd))),
   iha :- (Exists (fn [va :- RV]
            (And (Eval chkf dec encTy n rho a va)
                 (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta))))),
   ih1 :- (Exists (fn [v1 :- RV]
            (And (Eval chkf dec encTy n rho r1 v1)
                 (rel chkf dec encTy n Sk.cert v1 (den chkf dec encTy n r1 G Sk.cert eta))))),
   ih2 :- (Exists (fn [v2 :- RV]
            (And (Eval chkf dec encTy n rho r2 v2)
                 (rel chkf dec encTy n Sk.cert v2 (den chkf dec encTy n r2 G Sk.cert eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.node d a r1 r2) v)
         (rel chkf dec encTy n Sk.cert v (den chkf dec encTy n (Exp.node d a r1 r2) G Sk.cert eta)))))
  (rw [(den_node_at chkf dec encTy n d a r1 r2 G Sk.cert eta)])
  (rw [(coe_self Sk.cert (Code.sn (den chkf dec encTy n a G Sk.lbl eta)
                            (den chkf dec encTy n r1 G Sk.cert eta)
                            (den chkf dec encTy n r2 G Sk.cert eta)))])
  (refine' (exT RV _ _ ihd _)) (intro vd hd)
  (refine' (exT RV _ _ iha _)) (intro va ha)
  (have hea (Eval chkf dec encTy n rho a va) (And.left ha))
  (have hra (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)) (And.right ha))
  (refine' (exT RV _ _ ih1 _)) (intro v1 h1)
  (have he1 (Eval chkf dec encTy n rho r1 v1) (And.left h1))
  (have hr1 (rel chkf dec encTy n Sk.cert v1 (den chkf dec encTy n r1 G Sk.cert eta)) (And.right h1))
  (refine' (exT RV _ _ ih2 _)) (intro v2 h2)
  (have he2 (Eval chkf dec encTy n rho r2 v2) (And.left h2))
  (have hr2 (rel chkf dec encTy n Sk.cert v2 (den chkf dec encTy n r2 G Sk.cert eta)) (And.right h2))
  (constructor)
  (exact (RV.cert (Code.sn (den chkf dec encTy n a G Sk.lbl eta)
                    (den chkf dec encTy n r1 G Sk.cert eta)
                    (den chkf dec encTy n r2 G Sk.cert eta))))
  (constructor)
  (exact (Ev.eNode chkf dec encTy n rho d a r1 r2 vd
           (den chkf dec encTy n a G Sk.lbl eta)
           (den chkf dec encTy n r1 G Sk.cert eta)
           (den chkf dec encTy n r2 G Sk.cert eta)
           hd
           (eval_cast chkf dec encTy n rho a va (RV.lbl (den chkf dec encTy n a G Sk.lbl eta)) hea hra)
           (eval_cast chkf dec encTy n rho r1 v1 (RV.cert (den chkf dec encTy n r1 G Sk.cert eta)) he1 hr1)
           (eval_cast chkf dec encTy n rho r2 v2 (RV.cert (den chkf dec encTy n r2 G Sk.cert eta)) he2 hr2)))
  (rfl))

;; caseLbl (Theorem 4) is application of the branch list to the label.
(thm adeq_caseL
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   P :- Exp, a :- Exp, bs :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   iha :- (Exists (fn [va :- RV]
            (And (Eval chkf dec encTy n rho a va)
                 (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta))))),
   ihb :- (Exists (fn [vf :- RV]
            (And (Eval chkf dec encTy n rho bs vf)
                 (rel chkf dec encTy n (Sk.arr Sk.lbl s) vf
                   (den chkf dec encTy n bs G (Sk.arr Sk.lbl s) eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.caseL P a bs) v)
         (rel chkf dec encTy n s v (den chkf dec encTy n (Exp.caseL P a bs) G s eta)))))
  (rw [(den_caseL_at chkf dec encTy n P a bs G s eta)])
  (refine' (exT RV _ _ iha _)) (intro va ha)
  (have hea (Eval chkf dec encTy n rho a va) (And.left ha))
  (have hra (rel chkf dec encTy n Sk.lbl va (den chkf dec encTy n a G Sk.lbl eta)) (And.right ha))
  (refine' (exT RV _ _ ihb _)) (intro vf hb)
  (have heb (Eval chkf dec encTy n rho bs vf) (And.left hb))
  (have hrb (rel chkf dec encTy n (Sk.arr Sk.lbl s) vf
              (den chkf dec encTy n bs G (Sk.arr Sk.lbl s) eta)) (And.right hb))
  (refine' (exT RV _ _ (hrb va (den chkf dec encTy n a G Sk.lbl eta) hra) _))
  (intro w hw)
  (have hap (Ev chkf dec encTy n (EvSrc.ap vf va) w) (And.left hw))
  (have hrel (rel chkf dec encTy n s w
               ((den chkf dec encTy n bs G (Sk.arr Sk.lbl s) eta)
                (den chkf dec encTy n a G Sk.lbl eta))) (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (Ev.eCaseL chkf dec encTy n rho P a bs va vf w hea heb hap))
  (exact hrel))

;; A cons branch list at label 0 is the head (bcons_zero).  The argument is
;; related at Lbl, so it is that label; the application is transported from
;; the literal 0 that apBconsZ uses.
(thm bcons_apply_zero
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   h :- Exp, t :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   vh :- RV, vt :- RV, arg :- RV,
   hrh :- (rel chkf dec encTy cap s vh (den chkf dec encTy cap h G s eta)),
   harg :- (rel chkf dec encTy cap Sk.lbl arg 0)]
  (Exists (fn [w :- RV]
    (And (Ev chkf dec encTy cap (EvSrc.ap (RV.bcons vh vt) arg) w)
         (rel chkf dec encTy cap s w
           ((den chkf dec encTy cap (Exp.bcons h t) G (Sk.arr Sk.lbl s) eta) 0)))))
  (constructor) (exact vh)
  (constructor)
  (exact (Eq.mp (congrArg (fn [a :- RV] (Ev chkf dec encTy cap (EvSrc.ap (RV.bcons vh vt) a) vh)) (Eq.symm harg))
                (Ev.apBconsZ chkf dec encTy cap vh vt)))
  (rw [(bcons_zero chkf dec encTy cap h t G s eta)])
  (exact hrh))

;; At label k + 1 the tail is applied to k (bcons_succ, apBconsS).
(thm bcons_apply_succ
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   h :- Exp, t :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   vh :- RV, vt :- RV, arg :- RV, k :- Nat,
   hrt :- (rel chkf dec encTy cap (Sk.arr Sk.lbl s) vt
            (den chkf dec encTy cap t G (Sk.arr Sk.lbl s) eta)),
   harg :- (rel chkf dec encTy cap Sk.lbl arg (Nat.succ k))]
  (Exists (fn [w :- RV]
    (And (Ev chkf dec encTy cap (EvSrc.ap (RV.bcons vh vt) arg) w)
         (rel chkf dec encTy cap s w
           ((den chkf dec encTy cap (Exp.bcons h t) G (Sk.arr Sk.lbl s) eta) (Nat.succ k))))))
  (refine' (exT RV _ _ (hrt (RV.lbl k) k rfl) _))
  (intro w hw)
  (have hap (Ev chkf dec encTy cap (EvSrc.ap vt (RV.lbl k)) w) (And.left hw))
  (have hrel (rel chkf dec encTy cap s w
               ((den chkf dec encTy cap t G (Sk.arr Sk.lbl s) eta) k)) (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (Eq.mp (congrArg (fn [a :- RV] (Ev chkf dec encTy cap (EvSrc.ap (RV.bcons vh vt) a) w)) (Eq.symm harg))
                (Ev.apBconsS chkf dec encTy cap vh vt k w hap)))
  (rw [(bcons_succ chkf dec encTy cap h t G s eta k)])
  (exact hrel))

;; bcons (Theorem 4).  The value is the spine of the two values.  cases on
;; the label comes before the relatedness hypothesis is introduced, so the
;; hypothesis sees 0 or succ k rather than an unreduced variable.
(thm adeq_bcons
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   h :- Exp, t :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   ihh :- (Exists (fn [vh :- RV]
            (And (Eval chkf dec encTy cap rho h vh)
                 (rel chkf dec encTy cap s vh (den chkf dec encTy cap h G s eta))))),
   iht :- (Exists (fn [vt :- RV]
            (And (Eval chkf dec encTy cap rho t vt)
                 (rel chkf dec encTy cap (Sk.arr Sk.lbl s) vt
                   (den chkf dec encTy cap t G (Sk.arr Sk.lbl s) eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.bcons h t) v)
         (rel chkf dec encTy cap (Sk.arr Sk.lbl s) v
           (den chkf dec encTy cap (Exp.bcons h t) G (Sk.arr Sk.lbl s) eta)))))
  (refine' (exT RV _ _ ihh _)) (intro vh hh)
  (have heh (Eval chkf dec encTy cap rho h vh) (And.left hh))
  (have hrh (rel chkf dec encTy cap s vh (den chkf dec encTy cap h G s eta)) (And.right hh))
  (refine' (exT RV _ _ iht _)) (intro vt ht)
  (have het (Eval chkf dec encTy cap rho t vt) (And.left ht))
  (have hrt (rel chkf dec encTy cap (Sk.arr Sk.lbl s) vt
              (den chkf dec encTy cap t G (Sk.arr Sk.lbl s) eta)) (And.right ht))
  (constructor) (exact (RV.bcons vh vt))
  (constructor)
  (exact (Ev.eBcons chkf dec encTy cap rho h t vh vt heh het))
  (intro arg) (intro alpha) (cases alpha)
  (intro harg)
  (exact (bcons_apply_zero chkf dec encTy cap G h t s rho eta vh vt arg hrh harg))
  (intro harg)
  (exact (bcons_apply_succ chkf dec encTy cap G h t s rho eta vh vt arg n hrt harg)))

;; inspect (Theorem 4).  chkf of the certificate and the code selects the
;; branch.  Both branches run in the environment (⋆, certificate, ρ), which
;; is related once the certificate denotes the code it evaluates to.  The
;; split is adeq_insp_pick: cases on the Boolean comes before the equality
;; hypothesis, so eInspT sees tt and eInspF sees ff.
(thm adeq_insp_pick
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   cr :- Code, cd :- Code,
   her :- (Eval chkf dec encTy n rho r (RV.cert cr)),
   hec :- (Eval chkf dec encTy n rho c (RV.code cd)),
   w1 :- RV,
   he1 :- (Eval chkf dec encTy n (List.cons RV RV.star (List.cons RV (RV.cert cr) rho)) t1 w1),
   hr1 :- (rel chkf dec encTy n s w1
            (den chkf dec encTy n t1 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s
              (Prod.mk Unit.unit (Prod.mk cr eta)))),
   w2 :- RV,
   he2 :- (Eval chkf dec encTy n (List.cons RV RV.star (List.cons RV (RV.cert cr) rho)) t2 w2),
   hr2 :- (rel chkf dec encTy n s w2
            (den chkf dec encTy n t2 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s
              (Prod.mk Unit.unit (Prod.mk cr eta)))),
   b :- Bool]
  (forall [_u Unit]
    (=> (Eq Bool (chkf cr cd) b)
      (Exists (fn [v :- RV]
        (And (Eval chkf dec encTy n rho (Exp.insp X r c t1 t2) v)
             (rel chkf dec encTy n s v
               (Bool.rec$1 (fn [_ :- Bool] (Car s))
                 (den chkf dec encTy n t2 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s
                   (Prod.mk Unit.unit (Prod.mk cr eta)))
                 (den chkf dec encTy n t1 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s
                   (Prod.mk Unit.unit (Prod.mk cr eta)))
                 b)))))))
  (cases b)
  (intro u hb)
  (constructor) (exact w2)
  (constructor)
  (exact (Ev.eInspF chkf dec encTy n rho X r c t1 t2 cr cd w2 her hec hb he2))
  (exact hr2)
  (intro u hb)
  (constructor) (exact w1)
  (constructor)
  (exact (Ev.eInspT chkf dec encTy n rho X r c t1 t2 cr cd w1 her hec hb he1))
  (exact hr1))

(thm adeq_insp
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   hr0 :- (envRel chkf dec encTy n G rho eta),
   ihr :- (Exists (fn [vr :- RV]
            (And (Eval chkf dec encTy n rho r vr)
                 (rel chkf dec encTy n Sk.cert vr (den chkf dec encTy n r G Sk.cert eta))))),
   ihc :- (Exists (fn [vc :- RV]
            (And (Eval chkf dec encTy n rho c vc)
                 (rel chkf dec encTy n Sk.syn vc (den chkf dec encTy n c G Sk.syn eta))))),
   ih1 :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)))]
              (=> (envRel chkf dec encTy n (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (Eval chkf dec encTy n rho2 t1 w)
                       (rel chkf dec encTy n s w
                         (den chkf dec encTy n t1 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s eta2)))))))),
   ih2 :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)))]
              (=> (envRel chkf dec encTy n (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (Eval chkf dec encTy n rho2 t2 w)
                       (rel chkf dec encTy n s w
                         (den chkf dec encTy n t2 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s eta2))))))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.insp X r c t1 t2) v)
         (rel chkf dec encTy n s v (den chkf dec encTy n (Exp.insp X r c t1 t2) G s eta)))))
  (rw [(den_insp_at chkf dec encTy n X r c t1 t2 G s eta)])
  (refine' (exT RV _ _ ihr _)) (intro vr hr)
  (have her0 (Eval chkf dec encTy n rho r vr) (And.left hr))
  (have hrr (rel chkf dec encTy n Sk.cert vr (den chkf dec encTy n r G Sk.cert eta)) (And.right hr))
  (have her (Eval chkf dec encTy n rho r (RV.cert (den chkf dec encTy n r G Sk.cert eta)))
    (eval_cast chkf dec encTy n rho r vr (RV.cert (den chkf dec encTy n r G Sk.cert eta)) her0 hrr))
  (refine' (exT RV _ _ ihc _)) (intro vc hc)
  (have hec0 (Eval chkf dec encTy n rho c vc) (And.left hc))
  (have hrc (rel chkf dec encTy n Sk.syn vc (den chkf dec encTy n c G Sk.syn eta)) (And.right hc))
  (have hec (Eval chkf dec encTy n rho c (RV.code (den chkf dec encTy n c G Sk.syn eta)))
    (eval_cast chkf dec encTy n rho c vc (RV.code (den chkf dec encTy n c G Sk.syn eta)) hec0 hrc))
  (have hmid (envRel chkf dec encTy n (List.cons Sk Sk.cert G)
               (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho)
               (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta))
    (envRel_cons chkf dec encTy n Sk.cert G
      (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho
      (den chkf dec encTy n r G Sk.cert eta) eta rfl hr0))
  (have henv (envRel chkf dec encTy n (List.cons Sk Sk.unit (List.cons Sk Sk.cert G))
               (List.cons RV RV.star (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho))
               (Prod.mk Unit.unit (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta)))
    (envRel_cons chkf dec encTy n Sk.unit (List.cons Sk Sk.cert G)
      RV.star (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho)
      Unit.unit (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta)
      (rel_unit_rfl chkf dec encTy n) hmid))
  (refine' (exT RV _ _
    (ih1 (List.cons RV RV.star (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho))
         (Prod.mk Unit.unit (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta)) henv) _))
  (intro w1 h1)
  (have he1 (Eval chkf dec encTy n (List.cons RV RV.star (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho)) t1 w1) (And.left h1))
  (have hr1 (rel chkf dec encTy n s w1
              (den chkf dec encTy n t1 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s
                (Prod.mk Unit.unit (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta)))) (And.right h1))
  (refine' (exT RV _ _
    (ih2 (List.cons RV RV.star (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho))
         (Prod.mk Unit.unit (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta)) henv) _))
  (intro w2 h2)
  (have he2 (Eval chkf dec encTy n (List.cons RV RV.star (List.cons RV (RV.cert (den chkf dec encTy n r G Sk.cert eta)) rho)) t2 w2) (And.left h2))
  (have hr2 (rel chkf dec encTy n s w2
              (den chkf dec encTy n t2 (List.cons Sk Sk.unit (List.cons Sk Sk.cert G)) s
                (Prod.mk Unit.unit (Prod.mk (den chkf dec encTy n r G Sk.cert eta) eta)))) (And.right h2))
  (exact (adeq_insp_pick chkf dec encTy n G X r c t1 t2 s rho eta
           (den chkf dec encTy n r G Sk.cert eta)
           (den chkf dec encTy n c G Sk.syn eta)
           her hec w1 he1 hr1 w2 he2 hr2
           (chkf (den chkf dec encTy n r G Sk.cert eta) (den chkf dec encTy n c G Sk.syn eta))
           Unit.unit rfl)))

;; let (Theorem 4), when skOf of the scrutinee is the product the typing
;; assigned.  den_letp_some is the body under (second, (first, η)).  The
;; product clause of rel supplies the two components; envRel_cons extends
;; the environment in that order (variable 0 is the second component).
(thm adeq_let
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   C :- Exp, p :- Exp, t :- Exp, sa :- Sk, sb :- Sk, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   hr :- (envRel chkf dec encTy n G rho eta),
   hsu :- (Eq (Option Sk) (skOf G p) (Option.some Sk (Sk.prod sa sb))),
   ihp :- (Exists (fn [vp :- RV]
            (And (Eval chkf dec encTy n rho p vp)
                 (rel chkf dec encTy n (Sk.prod sa sb) vp
                   (den chkf dec encTy n p G (Sk.prod sa sb) eta))))),
   iht :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (List.cons Sk sb (List.cons Sk sa G)))]
              (=> (envRel chkf dec encTy n (List.cons Sk sb (List.cons Sk sa G)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (Eval chkf dec encTy n rho2 t w)
                       (rel chkf dec encTy n s w
                         (den chkf dec encTy n t (List.cons Sk sb (List.cons Sk sa G)) s eta2))))))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n rho (Exp.letp C p t) v)
         (rel chkf dec encTy n s v (den chkf dec encTy n (Exp.letp C p t) G s eta)))))
  (rw [(den_letp_some chkf dec encTy n C p t G sa sb s eta hsu)])
  (refine' (exT RV _ _ ihp _)) (intro vp hp)
  (have hep (Eval chkf dec encTy n rho p vp) (And.left hp))
  (have hrp (rel chkf dec encTy n (Sk.prod sa sb) vp (den chkf dec encTy n p G (Sk.prod sa sb) eta)) (And.right hp))
  (refine' (exT RV _ _ hrp _)) (intro va ha)
  (refine' (exT RV _ _ ha _)) (intro vb hb)
  (have heq (Eq RV vp (RV.pair va vb)) (And.left hb))
  (have hra (rel chkf dec encTy n sa va (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta))) (And.left (And.right hb)))
  (have hrb (rel chkf dec encTy n sb vb (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) eta))) (And.right (And.right hb)))
  (have hep2 (Eval chkf dec encTy n rho p (RV.pair va vb))
    (eval_cast chkf dec encTy n rho p vp (RV.pair va vb) hep heq))
  (have henv1 (envRel chkf dec encTy n (List.cons Sk sa G) (List.cons RV va rho)
                (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta)) eta))
    (envRel_cons chkf dec encTy n sa G va rho
      (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta)) eta hra hr))
  (have henv2 (envRel chkf dec encTy n (List.cons Sk sb (List.cons Sk sa G))
                (List.cons RV vb (List.cons RV va rho))
                (Prod.mk (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) eta))
                  (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta)) eta)))
    (envRel_cons chkf dec encTy n sb (List.cons Sk sa G) vb (List.cons RV va rho)
      (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) eta))
      (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta)) eta)
      hrb henv1))
  (refine' (exT RV _ _
    (iht (List.cons RV vb (List.cons RV va rho))
         (Prod.mk (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) eta))
           (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta)) eta))
         henv2) _))
  (intro w hw)
  (have het (Eval chkf dec encTy n (List.cons RV vb (List.cons RV va rho)) t w) (And.left hw))
  (have hrel (rel chkf dec encTy n s w
               (den chkf dec encTy n t (List.cons Sk sb (List.cons Sk sa G)) s
                 (Prod.mk (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) eta))
                   (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) eta)) eta))))
    (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (Ev.eLet chkf dec encTy n rho C p t va vb w hep2 het))
  (exact hrel))

;; The recN iterator (Theorem 4's inner induction on the numeral).  At 0 the
;; accumulator is the base.  At k + 1 the step runs in (mid, (k, ρ)), and that
;; environment is related because mid is related to the denotational Nat.rec
;; at k.  Nat.rec at a successor is that step, definitionally.
(thm adeq_iter
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   step :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   z0 :- RV, alpha :- (Car s),
   hz :- (rel chkf dec encTy cap s z0 alpha),
   hr :- (envRel chkf dec encTy cap G rho eta),
   ihs :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (List.cons Sk s (List.cons Sk Sk.nat G)))]
              (=> (envRel chkf dec encTy cap (List.cons Sk s (List.cons Sk Sk.nat G)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (Eval chkf dec encTy cap rho2 step w)
                       (rel chkf dec encTy cap s w
                         (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s eta2))))))))]
  (forall [k Nat]
    (Exists (fn [v :- RV]
      (And (Ev chkf dec encTy cap (EvSrc.iter rho step k z0) v)
           (rel chkf dec encTy cap s v
             (Nat.rec$1 (fn [_ :- Nat] (Car s)) alpha
               (fn [j :- Nat, acc :- (Car s)]
                 (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                   (Prod.mk acc (Prod.mk j eta))))
               k))))))
  (intro k) (induction k)
  (constructor) (exact z0)
  (constructor) (exact (Ev.eIterZ chkf dec encTy cap rho step z0))
  (exact hz)
  (refine' (exT RV _ _ ih_n _)) (intro mid hm)
  (have hem (Ev chkf dec encTy cap (EvSrc.iter rho step n z0) mid) (And.left hm))
  (have hrm (rel chkf dec encTy cap s mid
              (Nat.rec$1 (fn [_ :- Nat] (Car s)) alpha
                (fn [j :- Nat, acc :- (Car s)]
                  (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                    (Prod.mk acc (Prod.mk j eta))))
                n)) (And.right hm))
  (have henv1 (envRel chkf dec encTy cap (List.cons Sk Sk.nat G)
                (List.cons RV (RV.nat n) rho) (Prod.mk n eta))
    (envRel_cons chkf dec encTy cap Sk.nat G (RV.nat n) rho n eta rfl hr))
  (have henv2 (envRel chkf dec encTy cap (List.cons Sk s (List.cons Sk Sk.nat G))
                (List.cons RV mid (List.cons RV (RV.nat n) rho))
                (Prod.mk (Nat.rec$1 (fn [_ :- Nat] (Car s)) alpha
                           (fn [j :- Nat, acc :- (Car s)]
                             (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                               (Prod.mk acc (Prod.mk j eta))))
                           n)
                  (Prod.mk n eta)))
    (envRel_cons chkf dec encTy cap s (List.cons Sk Sk.nat G) mid (List.cons RV (RV.nat n) rho)
      (Nat.rec$1 (fn [_ :- Nat] (Car s)) alpha
        (fn [j :- Nat, acc :- (Car s)]
          (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
            (Prod.mk acc (Prod.mk j eta))))
        n)
      (Prod.mk n eta) hrm henv1))
  (refine' (exT RV _ _
    (ihs (List.cons RV mid (List.cons RV (RV.nat n) rho))
         (Prod.mk (Nat.rec$1 (fn [_ :- Nat] (Car s)) alpha
                    (fn [j :- Nat, acc :- (Car s)]
                      (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                        (Prod.mk acc (Prod.mk j eta))))
                    n)
           (Prod.mk n eta))
         henv2) _))
  (intro out ho)
  (have heo (Eval chkf dec encTy cap (List.cons RV mid (List.cons RV (RV.nat n) rho)) step out) (And.left ho))
  (have hro (rel chkf dec encTy cap s out
              (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                (Prod.mk (Nat.rec$1 (fn [_ :- Nat] (Car s)) alpha
                           (fn [j :- Nat, acc :- (Car s)]
                             (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                               (Prod.mk acc (Prod.mk j eta))))
                           n)
                  (Prod.mk n eta)))) (And.right ho))
  (constructor) (exact out)
  (constructor)
  (exact (Ev.eIterS chkf dec encTy cap rho step n z0 mid out hem heo))
  (exact hro))

;; recN (Theorem 4).  The scrutinee denotes a numeral k, the base is related
;; at the result skeleton, and adeq_iter runs the step k times.
(thm adeq_recN
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   P :- Exp, z :- Exp, step :- Exp, nv :- Exp, s :- Sk,
   rho :- (List RV), eta :- (HEnv G),
   hr :- (envRel chkf dec encTy cap G rho eta),
   ihn :- (Exists (fn [vn :- RV]
            (And (Eval chkf dec encTy cap rho nv vn)
                 (rel chkf dec encTy cap Sk.nat vn (den chkf dec encTy cap nv G Sk.nat eta))))),
   ihz :- (Exists (fn [z0 :- RV]
            (And (Eval chkf dec encTy cap rho z z0)
                 (rel chkf dec encTy cap s z0 (den chkf dec encTy cap z G s eta))))),
   ihs :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (List.cons Sk s (List.cons Sk Sk.nat G)))]
              (=> (envRel chkf dec encTy cap (List.cons Sk s (List.cons Sk Sk.nat G)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (Eval chkf dec encTy cap rho2 step w)
                       (rel chkf dec encTy cap s w
                         (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s eta2))))))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.recN P z step nv) v)
         (rel chkf dec encTy cap s v (den chkf dec encTy cap (Exp.recN P z step nv) G s eta)))))
  (rw [(den_recN_at chkf dec encTy cap P z step nv G s eta)])
  (refine' (exT RV _ _ ihn _)) (intro vn hn)
  (have hen (Eval chkf dec encTy cap rho nv vn) (And.left hn))
  (have hrn (rel chkf dec encTy cap Sk.nat vn (den chkf dec encTy cap nv G Sk.nat eta)) (And.right hn))
  (have hek (Eval chkf dec encTy cap rho nv (RV.nat (den chkf dec encTy cap nv G Sk.nat eta)))
    (eval_cast chkf dec encTy cap rho nv vn (RV.nat (den chkf dec encTy cap nv G Sk.nat eta)) hen hrn))
  (refine' (exT RV _ _ ihz _)) (intro z0 hz0)
  (have hez (Eval chkf dec encTy cap rho z z0) (And.left hz0))
  (have hrz (rel chkf dec encTy cap s z0 (den chkf dec encTy cap z G s eta)) (And.right hz0))
  (refine' (exT RV _ _
    (adeq_iter chkf dec encTy cap G step s rho eta z0
      (den chkf dec encTy cap z G s eta) hrz hr ihs
      (den chkf dec encTy cap nv G Sk.nat eta)) _))
  (intro v hv)
  (have hi (Ev chkf dec encTy cap (EvSrc.iter rho step (den chkf dec encTy cap nv G Sk.nat eta) z0) v) (And.left hv))
  (have hrel (rel chkf dec encTy cap s v
               (Nat.rec$1 (fn [_ :- Nat] (Car s)) (den chkf dec encTy cap z G s eta)
                 (fn [j :- Nat, acc :- (Car s)]
                   (den chkf dec encTy cap step (List.cons Sk s (List.cons Sk Sk.nat G)) s
                     (Prod.mk acc (Prod.mk j eta))))
                 (den chkf dec encTy cap nv G Sk.nat eta))) (And.right hv))
  (constructor) (exact v)
  (constructor)
  (exact (Ev.eRecN chkf dec encTy cap rho P z step nv
           (den chkf dec encTy cap nv G Sk.nat eta) z0 v hek hez hi))
  (exact hrel))
