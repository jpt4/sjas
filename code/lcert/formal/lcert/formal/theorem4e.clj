(ns lcert.formal.theorem4e
  "F5 — Theorem 4′ assembled (R4-metatheory.md §5, the erasing evaluator).

  funde.clj proves one case lemma per rule of the fundamental property of
  evalᴱ (adeqE_*).  This namespace supplies what the assembly needs and
  then assembles it.

  Restriction (the paper's \"Restriction\" bullet, review T52-04).  envE
  is stated for one usage vector.  A premise is judged at a part of the
  conclusion's vector: a summand of vadd, or a vector scaled by a nonzero
  usage.  SubU a b says a and b have one length and every entry nonzero in
  a is nonzero in b.  envE_sub: an environment related at b is related at
  a.  Entries of usage 0 in a are unconstrained (entryE at 0 is True), and
  an entry nonzero in a is nonzero in b, where it is already E-related.
  vadd truncates to the shorter vector, so restriction needs equal lengths;
  er_len gives them (every Er premise is judged in the same context).

  er_rt forgets the erased term: an Er derivation is an Rt derivation.  The
  case lemmas read skeletons off Rt (skOf_rt, lemma25_rt), as the App and
  Let cases do.

  The remaining cases.  abort and H₁ evaluate their runtime premises and
  return the default, related by erdflt_ty (abort) or by E at 1, which does
  not read the carrier (H₁).  inspect evaluates the certificate and the
  code, takes the branch chkf selects on both sides, and runs it in
  (⋆, certificate, ρ); the two new entries have usage 1 and are E-related.
  reflect uses the outer hypothesis: AdeqE at the decoded budget m < n,
  applied to the erasure (er_total) of the derivation CheckSpec decodes,
  in the token environment (tokU, tokE).  At a base data type E is
  equality, the same at every budget (erel_base_budget).

  adeqE_step is the induction on Er at one budget; adeqE_all is the strong
  induction on the budget.  theorem4e is Theorem 4′'s agreement at Θₙ."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.erase]
            [lcert.formal.uskel]
            [lcert.formal.funde]
            [lcert.formal.theorem4]))

;; --- restriction of related environments (Theorem 4′) ------------------------

;; SubU a b: a premise's usage vector a sits inside the conclusion's b.
;; Same length, and nonzero entries of a are nonzero in b.
(kdef SubU (=> (List U) (List U) Prop)
  (fn [a :- (List U), b :- (List U)]
    (And (Eq Nat (lenU a) (lenU b))
         (forall [i Nat] (=> (Eq Bool (nzAt a i) Bool.true)
                             (Eq Bool (nzAt b i) Bool.true))))))

(thm subU_refl [a :- (List U)]
  (SubU a a)
  (constructor) (rfl) (intro i h) (exact h))

(thm subU_trans [a :- (List U), b :- (List U), c :- (List U),
                 h1 :- (SubU a b), h2 :- (SubU b c)]
  (SubU a c)
  (constructor)
  (exact (Eq.trans (And.left h1) (And.left h2)))
  (intro i h)
  (exact ((And.right h2) i ((And.right h1) i h))))

;; vadd of two vectors of one length has that length.  The cons step is its
;; own lemma, so that cases on the second vector names its fields cleanly.
(thm len_vadd_cc [a :- U, xs :- (List U),
                  ih :- (forall [b (List U)] (=> (Eq Nat (lenU xs) (lenU b)) (Eq Nat (lenU (vadd xs b)) (lenU xs))))]
  (forall [b (List U)] (=> (Eq Nat (lenU (List.cons U a xs)) (lenU b))
                           (Eq Nat (lenU (vadd (List.cons U a xs) b)) (lenU (List.cons U a xs)))))
  (intro b) (cases b)
  (intro h)
  (exact (absurd h (Nat.succ_ne_zero (lenU xs))))
  (intro h)
  (exact (congrArg Nat.succ (ih tail (succ_eq (lenU xs) (lenU tail) h)))))

(thm len_vadd [a :- (List U)]
  (forall [b (List U)] (=> (Eq Nat (lenU a) (lenU b)) (Eq Nat (lenU (vadd a b)) (lenU a))))
  (induction a)
  (intro b h) (rfl)
  (exact (len_vadd_cc head tail ih_tail)))

(thm len_vscale [r :- U, a :- (List U)]
  (Eq Nat (lenU (vscale r a)) (lenU a))
  (induction a)
  (rfl)
  (exact (congrArg Nat.succ ih_tail)))

;; vadd is commutative (both orders truncate to the shorter vector; uadd_comm
;; is usage.clj's).  The cons step is its own lemma, as for len_vadd.
(thm vadd_comm_cc [a :- U, xs :- (List U),
                   ih :- (forall [b (List U)] (Eq (List U) (vadd xs b) (vadd b xs)))]
  (forall [b (List U)] (Eq (List U) (vadd (List.cons U a xs) b) (vadd b (List.cons U a xs))))
  (intro b) (cases b)
  (rfl)
  (exact (Eq.trans
           (congrArg (fn [z :- (List U)] (List.cons U (uadd a head) z)) (ih tail))
           (congrArg (fn [x :- U] (List.cons U x (vadd tail xs))) (uadd_comm a head)))))

(thm vadd_comm [a :- (List U)]
  (forall [b (List U)] (Eq (List U) (vadd a b) (vadd b a)))
  (induction a)
  (intro b) (cases b) (rfl) (rfl)
  (exact (vadd_comm_cc head tail ih_tail)))

(thm subU_vadd_l [a :- (List U), b :- (List U), h :- (Eq Nat (lenU a) (lenU b))]
  (SubU a (vadd a b))
  (constructor)
  (exact (Eq.symm (len_vadd a b h)))
  (intro i hi)
  (exact (nz_vadd a b i h hi)))

(thm subU_vadd_r [a :- (List U), b :- (List U), h :- (Eq Nat (lenU a) (lenU b))]
  (SubU b (vadd a b))
  (exact (Eq.mp (congrArg (fn [z :- (List U)] (SubU b z)) (vadd_comm b a))
           (subU_vadd_l b a (Eq.symm h)))))

(thm subU_vscale [r :- U, hr :- (Eq Bool (nonzero r) Bool.true), a :- (List U)]
  (SubU a (vscale r a))
  (constructor)
  (exact (Eq.symm (len_vscale r a)))
  (intro i hi)
  (exact (nz_vscale r hr a i hi)))

(thm subU_cons [r :- U, a :- (List U), b :- (List U), h :- (SubU a b)]
  (SubU (List.cons U r a) (List.cons U r b))
  (constructor)
  (exact (congrArg Nat.succ (And.left h)))
  (intro i) (cases i)
  (intro hi) (exact hi)
  (intro hi) (exact ((And.right h) n hi)))

;; One entry: entryE at a usage nonzero only where r is nonzero.
(thm entryE_mono
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, r :- U, u :- USk, v :- RV, a :- (Car (uskSk u)),
   h :- (entryE chkf dec encTy n r u v a)]
  (forall [r2 U]
    (=> (=> (Eq Bool (nonzero r2) Bool.true) (Eq Bool (nonzero r) Bool.true))
        (entryE chkf dec encTy n r2 u v a)))
  (intro r2) (cases r2)
  (intro hle) (exact True.intro)
  (intro hle)
  (exact (Eq.mp (entryE_nz chkf dec encTy n r u v a (hle rfl)) h))
  (intro hle)
  (exact (Eq.mp (entryE_nz chkf dec encTy n r u v a (hle rfl)) h)))

;; The cons step of envE_sub, with the head destructured.
(thm envE_sub_cons
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, rest :- (List USk),
   ih :- (forall [rs (List U)] (forall [rs2 (List U)] (forall [rho (List RV)] (forall [eta (HEnv (usks rest))]
          (=> (envE chkf dec encTy n rest rs rho eta)
            (=> (SubU rs2 rs) (envE chkf dec encTy n rest rs2 rho eta))))))),
   r :- U, rs :- (List U), v :- RV, rho :- (List RV),
   eta :- (HEnv (usks (List.cons USk u rest))),
   hrest :- (envE chkf dec encTy n rest rs rho (Prod.snd eta)),
   hent :- (entryE chkf dec encTy n r u v (Prod.fst eta))]
  (forall [ws (List U)]
    (=> (SubU ws (List.cons U r rs))
        (envE chkf dec encTy n (List.cons USk u rest) ws (List.cons RV v rho) eta)))
  (intro ws) (cases ws)
  (intro hs)
  (exact (absurd (Eq.symm (And.left hs)) (Nat.succ_ne_zero (lenU rs))))
  (intro hs)
  (have hl (Eq Nat (lenU tail) (lenU rs))
    (succ_eq (lenU tail) (lenU rs) (And.left hs)))
  (have ht (SubU tail rs)
    (And.intro hl (fn [i :- Nat, hi :- (Eq Bool (nzAt tail i) Bool.true)] ((And.right hs) (Nat.succ i) hi))))
  (constructor) (exact head)
  (constructor) (exact tail)
  (constructor) (exact v)
  (constructor) (exact rho)
  (constructor) (rfl)
  (constructor) (rfl)
  (constructor)
  (exact (ih rs tail rho (Prod.snd eta) hrest ht))
  (exact (entryE_mono chkf dec encTy n r u v (Prod.fst eta) hent head ((And.right hs) 0))))

;; The empty context: both vectors are empty (SubU keeps the length).
(thm envE_sub_nil
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, rs :- (List U), rho :- (List RV), eta :- (HEnv (usks (List.nil USk)))]
  (forall [ws (List U)]
    (=> (envE chkf dec encTy n (List.nil USk) rs rho eta)
      (=> (SubU ws rs) (envE chkf dec encTy n (List.nil USk) ws rho eta))))
  (intro ws) (cases ws)
  (intro h hs)
  (constructor) (rfl) (exact (And.right h))
  (intro h hs)
  (exact (absurd (Eq.trans (And.left hs) (congrArg lenU (And.left h)))
                 (Nat.succ_ne_zero (lenU tail)))))

;; Restriction: an environment related at the conclusion's vector is related
;; at any vector inside it.
(thm envE_sub
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, G :- (List USk)]
  (forall [rs (List U)] (forall [rs2 (List U)] (forall [rho (List RV)] (forall [eta (HEnv (usks G))]
    (=> (envE chkf dec encTy n G rs rho eta)
      (=> (SubU rs2 rs) (envE chkf dec encTy n G rs2 rho eta)))))))
  (induction G)
  (intro rs rs2 rho eta)
  (exact (envE_sub_nil chkf dec encTy n rs rho eta rs2))
  (intro rs rs2 rho eta h hs)
  (refine' (exT U _ _ h _)) (intro r0 hex)
  (refine' (exT (List U) _ _ hex _)) (intro rs0 hex2)
  (refine' (exT RV _ _ hex2 _)) (intro v0 hex3)
  (refine' (exT (List RV) _ _ hex3 _)) (intro rho0 hp)
  (have hrs (Eq (List U) rs (List.cons U r0 rs0)) (And.left hp))
  (have hl (Eq (List RV) rho (List.cons RV v0 rho0)) (And.left (And.right hp)))
  (have hrest (envE chkf dec encTy n tail rs0 rho0 (Prod.snd eta))
    (And.left (And.right (And.right hp))))
  (have he (entryE chkf dec encTy n r0 head v0 (Prod.fst eta))
    (And.right (And.right (And.right hp))))
  (have hs2 (SubU rs2 (List.cons U r0 rs0))
    (Eq.mp (congrArg (fn [z :- (List U)] (SubU rs2 z)) hrs) hs))
  (exact (Eq.mpr (congrArg (fn [z :- (List RV)] (envE chkf dec encTy n (List.cons USk head tail) rs2 z eta)) hl)
           (envE_sub_cons chkf dec encTy n head tail ih_tail r0 rs0 v0 rho0 eta hrest he rs2 hs2))))

;; Lengths through vadd and vscale, stated against a common length k (the
;; context's), which is how the assembly uses them.
(thm len_vadd2 [a :- (List U), b :- (List U), k :- Nat,
                ha :- (Eq Nat (lenU a) k), hb :- (Eq Nat (lenU b) k)]
  (Eq Nat (lenU (vadd a b)) k)
  (exact (Eq.trans (len_vadd a b (Eq.trans ha (Eq.symm hb))) ha)))

(thm len_vscale2 [r :- U, a :- (List U), k :- Nat, ha :- (Eq Nat (lenU a) k)]
  (Eq Nat (lenU (vscale r a)) k)
  (exact (Eq.trans (len_vscale r a) ha)))

;; --- Er forgets to Rt, and every Er premise has the context's length ----------

;; The constructors of Er, in order, with their fields: -x is an erased term
;; (Rt has no such field), *h is an Er premise (its induction hypothesis is
;; ih_h).  The Rt constructor has the remaining fields in the same order.
(def ^:private er-ctors
  '[[eVar rVar [D us i A r hl hA hu hr]]
    [eConst rConst [D us t A hl h]]
    [eLam rLam [D us r A t B -te hA *ht]]
    [eApp0 rApp0 [D us f u A B -fe *hf hu hA hB]]
    [eApp rApp [D us1 us2 r f u A B -fe -ue hr *hf *hu hA hB]]
    [ePair0 rPair0 [D us A B x y -ye hA hB hx *hy]]
    [ePair rPair [D us1 us2 r A B x y -xe -ye hr hA hB *hx *hy]]
    [eLet rLet [D us1 us2 r A B C p t -pe -te *hp hC hA hB *ht]]
    [eAbort rAbort [D us A t -te *ht hA]]
    [eConv rConv [D us t A B -te *ht hB hc]]
    [eIte rIte [D us1 us2 b t e C -be -te -ee *hb *ht *he]]
    [eElimB rElimB [D us1 us2 P b t e -be -te -ee *hb hP *ht *he]]
    [eSucc rSucc [D us n -ne *h]]
    [eRecN rRecN [D us1 us2 us3 P z s n -ze -se -ne *hn hP *hz *hs]]
    [eCaseL rCaseL [D us1 us2 P x bs -xe -bse *hx hP *hb]]
    [eBnil rBnil [D us P hl]]
    [eBcons rBcons [D us P k h t -he -te *hh *ht hP]]
    [eSleaf rSleaf [D us x -xe *h]]
    [eSnode rSnode [D us1 us2 us3 x c1 c2 -xe -c1e -c2e *hx *h1 *h2]]
    [eRecS rRecS [D us1 us2 us3 P tl tn c -tle -tne -ce *hc hP *hl *hn hY1 hY2]]
    [eLeaf rLeaf [D us x -xe *h]]
    [eNode rNode [D us1 us2 us3 us4 d x r1 r2 -de -xe -r1e -r2e *hd *hx *h1 *h2]]
    [eItR rItR [D us1 us2 us3 X g h r -ge -he -re hX *hg *hh *hr]]
    [ePrn rPrn [D us r -re *h]]
    [eChk rChk [D us1 us2 c d -ce -de *hc *hd]]
    [eH1 rH1 [D us1 us2 us3 us4 us5 r s c e1 e2 -re -se -ce -e1e -e2e *hr *hs *hc *h1 *h2]]
    [eRefl rRefl [D us1 us2 X cd r e -re -ee hb *hr *he]]
    [eInsp rInsp [D us1 us0 us2 X r c t1 t2 -re -ce -t1e -t2e *hr *hc hX hF1 hF2 *h1 *h2]]])

(defn- fname [f] (let [s (name f)] (symbol (if (#{\- \*} (first s)) (subs s 1) s))))
(defn- erased? [f] (= \- (first (name f))))
(defn- prem? [f] (= \* (first (name f))))

;; er_rt: an erasure derivation is a typing derivation of the same judgment.
(a/prove-theorem 'er_rt
  (lv '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, e0 :- Exp,
        der :- (Er chkf D0 us0 t0 A0 e0)])
  '(Rt chkf D0 us0 t0 A0)
  (lv (into ['(induction der)]
            (for [[_ rc fs] er-ctors]
              (list 'exact (apply list (symbol (str "Rt." rc)) 'chkf
                                  (for [f fs :when (not (erased? f))]
                                    (if (prem? f) (symbol (str "ih_" (fname f))) f))))))))

;; The usage vector of a judgment as a tree of vadd / vscale over the
;; premises' vectors, and where each leaf's length comes from: a premise,
;; peeled of the binders it is judged under ([a b] is one succ_eq step,
;; lenU a = lenE b, outermost binder first).
(def ^:private vtrees
  '{eApp   [(vadd us1 (vscale r us2)) {us1 [hf] us2 [hu]}]
    ePair  [(vadd (vscale r us1) us2) {us1 [hx] us2 [hy]}]
    eLet   [(vadd us1 us2) {us1 [hp] us2 [ht [(consU r us2) (consE A D)] [us2 D]]}]
    eIte   [(vadd us1 us2) {us1 [hb] us2 [ht]}]
    eElimB [(vadd us1 us2) {us1 [hb] us2 [ht]}]
    eRecN  [(vadd us1 (vadd us2 (vscale U.uw us3)))
            {us1 [hn] us2 [hz]
             (vscale U.uw us3) [hs [(consU U.uw (vscale U.uw us3)) (consE Exp.tNat D)] [(vscale U.uw us3) D]]}]
    eCaseL [(vadd us1 us2) {us1 [hx] us2 [hb]}]
    eSnode [(vadd us1 (vadd us2 us3)) {us1 [hx] us2 [h1] us3 [h2]}]
    eRecS  [(vadd us1 (vadd (vscale U.uw us2) (vscale U.uw us3)))
            {us1 [hc]
             (vscale U.uw us2) [hl [(vscale U.uw us2) D]]
             (vscale U.uw us3) [hn [(consU U.u1 (consU U.uw (consU U.uw (consU U.uw (vscale U.uw us3)))))
                                    (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D))))]
                                   [(consU U.uw (consU U.uw (consU U.uw (vscale U.uw us3))))
                                    (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))]
                                   [(consU U.uw (consU U.uw (vscale U.uw us3))) (consE Exp.tSyn (consE Exp.tLbl D))]
                                   [(consU U.uw (vscale U.uw us3)) (consE Exp.tLbl D)]
                                   [(vscale U.uw us3) D]]}]
    eNode  [(vadd us1 (vadd us2 (vadd us3 us4))) {us1 [hd] us2 [hx] us3 [h1] us4 [h2]}]
    eItR   [(vadd (vscale U.uw us1) (vadd (vscale U.uw us2) us3))
            {(vscale U.uw us1) [hg] (vscale U.uw us2) [hh] us3 [hr]}]
    eChk   [(vadd us1 us2) {us1 [hc] us2 [hd]}]
    eH1    [(vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5))))
            {us1 [hr] us2 [hs] (vscale U.uw us3) [hc] us4 [h1] us5 [h2]}]
    eRefl  [(vadd us1 us2) {us1 [hr] us2 [he]}]
    eInsp  [(vadd us1 (vadd (vscale U.uw us0) us2))
            {us1 [hr] (vscale U.uw us0) [hc] us2 [h1 [(consU U.u1 us2) (consE Exp.tR D)] [us2 D]]}]})

;; A leaf's length proof: L of the premise, then one succ_eq per binder.
(defn- leaf-len [L [h & peels]]
  (reduce (fn [acc [a b]] (list 'succ_eq (list 'lenU a) (list 'lenE b) acc)) (L h) peels))

;; lenU tree = lenE D, from the leaves' proofs.
(defn- len-pf [t lp]
  (or (lp t)
      (case (first t)
        vadd (list 'len_vadd2 (nth t 1) (nth t 2) '(lenE D) (len-pf (nth t 1) lp) (len-pf (nth t 2) lp))
        vscale (list 'len_vscale2 (nth t 1) (nth t 2) '(lenE D) (len-pf (nth t 2) lp)))))

;; er_len: the usage vector of an erasure derivation has the context's length.
(def ^:private er-len-simple
  '{eVar hl eConst hl eLam (succ_eq (lenU us) (lenE D) ih_ht) eApp0 ih_hf ePair0 ih_hy
    eAbort ih_ht eConv ih_ht eSucc ih_h eBnil hl eBcons ih_hh eSleaf ih_h eLeaf ih_h ePrn ih_h})

(a/prove-theorem 'er_len
  (lv '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, e0 :- Exp,
        der :- (Er chkf D0 us0 t0 A0 e0)])
  '(Eq Nat (lenU us0) (lenE D0))
  (lv (into ['(induction der)]
            (for [[c _ _] er-ctors]
              (list 'exact
                    (or (er-len-simple c)
                        (let [[t leaves] (vtrees c)
                              L (fn [h] (symbol (str "ih_" h)))
                              lp (into {} (for [[k v] leaves] [k (leaf-len L v)]))]
                          (len-pf t lp))))))))

;; --- statement builders ---------------------------------------------------------
;; The case lemmas below share long hypotheses; these build them as data.
;; Budget cap, context D, carrier environment eta.

(def ^:private P3 '[chkf :- (=> Code Code Bool),
                    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                    encTy :- (=> Exp Code)])

(defn- thm* [nm params prop tactics]
  (a/prove-theorem nm (lv (vec params)) (lv prop) (lv (vec tactics))))

(defn- sub [m form] (clojure.walk/postwalk-replace m form))

;; ∃ v. evalᴱ of the erased term ee in rho is v, and v is E-related at A to
;; denU of the source term tt.
(defn- adq [rho ee A tt]
  (sub {'RHO rho 'EE ee 'AA A 'TT tt}
    '(Exists (fn [v :- RV]
       (And (EvalE chkf dec encTy cap RHO EE v)
            (Erel chkf dec encTy cap (usk AA) v (denU chkf dec encTy cap D TT AA eta)))))))

;; The same for a premise judged in the extended context Dx at usages usx,
;; for every related environment of that context.
(defn- adqB [Dx usx ee A tt]
  (sub {'DX Dx 'USX usx 'EE ee 'AA A 'TT tt}
    '(forall [rho2 (List RV)]
       (forall [eta2 (HEnv (usks (uskCtx DX)))]
         (=> (envE chkf dec encTy cap (uskCtx DX) USX rho2 eta2)
             (Exists (fn [w :- RV]
               (And (EvalE chkf dec encTy cap rho2 EE w)
                    (Erel chkf dec encTy cap (usk AA) w (denU chkf dec encTy cap DX TT AA eta2))))))))))

(defn- envE-at [us] (list 'envE 'chkf 'dec 'encTy 'cap '(uskCtx D) us 'rho 'eta))

(def ^:private CTX '[cap :- Nat, D :- (List Exp)])
(def ^:private ENV '[rho :- (List RV), eta :- (HEnv (usks (uskCtx D)))])

;; --- constants (Theorem 4′) -------------------------------------------------------

;; A constant judged by constTyped is one of ⋆ : 1, tt, ff : Bool, zero : Nat,
;; lbl l : Lbl; every other pair has constTyped false and is refuted.
(thm* 'adeqE_const (concat P3 CTX ENV)
  (list 'forall '[t Exp] (list 'forall '[A Exp]
    (list '=> '(Eq Bool (constTyped t A) Bool.true) (adq 'rho 't 'A 't))))
  '[(intro t) (cases t) (all_goals (intro A))
    (all_goals (first (and_then (intro hct) (exact (Bool.noConfusion hct))) (skip)))
    (all_goals (cases A))
    (all_goals (first (and_then (intro hct) (exact (Bool.noConfusion hct))) (skip)))
    (all_goals (intro hct))
    (all_goals (first (exact (adeqE_star chkf dec encTy cap D rho eta))
                      (exact (adeqE_tt chkf dec encTy cap D rho eta))
                      (exact (adeqE_ff chkf dec encTy cap D rho eta))
                      (exact (adeqE_zero chkf dec encTy cap D rho eta))
                      (skip)))
    (exact (adeqE_lbl chkf dec encTy cap l D rho eta))])

;; --- abort and H₁ (Theorem 4′, the defaults) ----------------------------------------

;; ⟦abort A t⟧ is the carrier default, read at usk A.
(thm* 'denU_abort (concat P3 CTX '[A :- Exp, t :- Exp, eta :- (HEnv (usks (uskCtx D)))])
  '(Eq (Car (uskSk (usk A)))
     (denU chkf dec encTy cap D (Exp.abort A t) A eta)
     (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) (dflt (skel A))))
  '[(exact (congrArg (fn [x :- (Car (skel A))] (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) x))
             (den_abort_at chkf dec encTy cap A t (skels D) (skel A) (henv_of_usk D eta))))])

;; abort evaluates its argument and returns the runtime default, related to
;; the carrier default (erdflt_ty).  Not vacuous: E(0) relates ⋆ to ⋆.
(thm* 'adeqE_abort (concat P3 CTX '[A :- Exp, t :- Exp, te :- Exp] ENV
                    [(symbol "ih") :- (adq 'rho 'te 'Exp.tEmpty 't)])
  (adq 'rho '(Exp.abort A te) 'A '(Exp.abort A t))
  '[(refine' (exT RV _ _ ih _)) (intro vt hv)
    (constructor) (exact (rdflt (skel A)))
    (constructor) (exact (EvE.eAbort chkf dec encTy cap rho A te vt (And.left hv)))
    (exact (erel_car chkf dec encTy cap (usk A) (rdflt (skel A))
             (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) (dflt (skel A)))
             (denU chkf dec encTy cap D (Exp.abort A t) A eta)
             (erdflt_ty chkf dec encTy cap A)
             (Eq.symm (denU_abort chkf dec encTy cap D A t eta))))])

;; H₁ evaluates its five runtime premises and returns ⋆; E at 0 is the unit
;; clause, which does not read the carrier value.
(thm* 'adeqE_h1 (concat P3 CTX '[r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
                                re :- Exp, se :- Exp, ce :- Exp, e1e :- Exp, e2e :- Exp] ENV
                 ['ihr :- (adq 'rho 're 'Exp.tR 'r)
                  'ihs :- (adq 'rho 'se 'Exp.tR 's)
                  'ihc :- (adq 'rho 'ce 'Exp.tSyn 'c)
                  'ih1 :- (adq 'rho 'e1e '(chkT r c) 'e1)
                  'ih2 :- (adq 'rho 'e2e '(chkT s (negT c)) 'e2)])
  (adq 'rho '(Exp.h1 re se ce e1e e2e) 'Exp.tEmpty '(Exp.h1 r s c e1 e2))
  '[(refine' (exT RV _ _ ihr _)) (intro vr pr)
    (refine' (exT RV _ _ ihs _)) (intro vs ps)
    (refine' (exT RV _ _ ihc _)) (intro vc pc)
    (refine' (exT RV _ _ ih1 _)) (intro v1 p1)
    (refine' (exT RV _ _ ih2 _)) (intro v2 p2)
    (constructor) (exact RV.star)
    (constructor)
    (exact (EvE.eH1 chkf dec encTy cap rho re se ce e1e e2e vr vs vc v1 v2
             (And.left pr) (And.left ps) (And.left pc) (And.left p1) (And.left p2)))
    (rfl)])

;; --- inspect (Theorem 4′) -------------------------------------------------------------
;; Both sides branch on chkf of the certificate and the code; the runtime
;; values are those carrier values (E at R and Syn is equality).  The branch
;; runs in (⋆, certificate, ρ); both entries have usage 1 and are related:
;; ⋆ at T(…) (the unit clause) and the certificate at R.  The branch's type
;; is lift 2 0 X, brought back to X by usk_lift / denU_lift as in let.

(def ^:private D1 '(List.cons Exp (chkT (Exp.var 0) (lift 1 0 c)) (List.cons Exp Exp.tR D)))
(def ^:private D2 '(List.cons Exp (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))))
                     (List.cons Exp Exp.tR D)))
(def ^:private cX '(congrArg Car (Eq.symm (usk_skel X))))
(defn- body-den [t]
  (sub {'TT t}
    '(den chkf dec encTy cap TT (sk2 Sk.unit Sk.cert (skels D)) (skel X)
       (Prod.mk Unit.unit (Prod.mk cr (henv_of_usk D eta))))))

;; The branch selected by the Boolean b = chkf cr cd, on both sides.  cases
;; on b precedes the equation, so eInspT sees tt and eInspF sees ff.
(thm* 'adeqE_insp_pick
  (concat P3 CTX '[X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp,
                   re :- Exp, ce :- Exp, t1e :- Exp, t2e :- Exp] ENV
          '[cr :- Code, cd :- Code,
            her :- (EvalE chkf dec encTy cap rho re (RV.cert cr)),
            hec :- (EvalE chkf dec encTy cap rho ce (RV.code cd)),
            w1 :- RV,
            he1 :- (EvalE chkf dec encTy cap (List.cons RV RV.star (List.cons RV (RV.cert cr) rho)) t1e w1)]
          ['hr1 :- (list 'Erel 'chkf 'dec 'encTy 'cap '(usk X) 'w1 (list 'Eq.mp cX (body-den 't1)))]
          '[w2 :- RV,
            he2 :- (EvalE chkf dec encTy cap (List.cons RV RV.star (List.cons RV (RV.cert cr) rho)) t2e w2)]
          ['hr2 :- (list 'Erel 'chkf 'dec 'encTy 'cap '(usk X) 'w2 (list 'Eq.mp cX (body-den 't2)))
           'b :- 'Bool])
  (sub {'CX cX 'B1 (body-den 't1) 'B2 (body-den 't2)}
    '(forall [_u Unit]
       (=> (Eq Bool (chkf cr cd) b)
           (Exists (fn [v :- RV]
             (And (EvalE chkf dec encTy cap rho (Exp.insp X re ce t1e t2e) v)
                  (Erel chkf dec encTy cap (usk X) v
                    (Eq.mp CX (Bool.rec$1 (fn [_ :- Bool] (Car (skel X))) B2 B1 b)))))))))
  '[(cases b)
    (intro u hb)
    (constructor) (exact w2)
    (constructor)
    (exact (EvE.eInspF chkf dec encTy cap rho X re ce t1e t2e cr cd w2 her hec hb he2))
    (exact hr2)
    (intro u hb)
    (constructor) (exact w1)
    (constructor)
    (exact (EvE.eInspT chkf dec encTy cap rho X re ce t1e t2e cr cd w1 her hec hb he1))
    (exact hr1)])

;; A branch's hypothesis, from E at lift 2 0 X in the extended environment
;; to E at X of den in (⋆, cr, η) — the form den_insp_at unfolds to.
(defn- branch-rel [Dx Bh t w ih-res]
  (sub {'BH Bh 'DX Dx 'TT t 'W w 'IHR ih-res 'BD (body-den t) 'CX cX
        'ETA2 '(Prod.mk Unit.unit (Prod.mk cr eta))}
    '(erel_car chkf dec encTy cap (usk X) W
       (denU chkf dec encTy cap DX TT X ETA2)
       (Eq.mp CX BD)
       (erel_car chkf dec encTy cap (usk X) W
         (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 2 0))
           (denU chkf dec encTy cap DX TT (lift 2 0 X) ETA2))
         (denU chkf dec encTy cap DX TT X ETA2)
         (erel_at chkf dec encTy cap (usk (lift 2 0 X)) (usk X) W
           (denU chkf dec encTy cap DX TT (lift 2 0 X) ETA2) IHR (usk_lift X 2 0))
         (denU_lift chkf dec encTy cap DX TT X ETA2))
       (congrArg (fn [en1 :- (HEnv (skels DX))] (Eq.mp CX (den chkf dec encTy cap TT (skels DX) (skel X) en1)))
         (henv_ext2 Exp.tR BH D cr Unit.unit eta)))))

(thm* 'adeqE_retarget_eq (concat P3 CTX '[t :- Exp, te :- Exp, A :- Exp] ENV
                          '[a :- (Car (uskSk (usk A))),
                            e :- (Eq (Car (uskSk (usk A))) a (denU chkf dec encTy cap D t A eta)),
                            h :- (Exists (fn [v :- RV]
                                   (And (EvalE chkf dec encTy cap rho te v)
                                        (Erel chkf dec encTy cap (usk A) v a))))])
  (adq 'rho 'te 'A 't)
  '[(refine' (exT RV _ _ h _)) (intro v hv)
    (constructor) (exact v)
    (constructor) (exact (And.left hv))
    (exact (erel_car chkf dec encTy cap (usk A) v a (denU chkf dec encTy cap D t A eta) (And.right hv) e))])

(def ^:private cr-def '(denU chkf dec encTy cap D r Exp.tR eta))
(def ^:private cd-def '(denU chkf dec encTy cap D c Exp.tSyn eta))

(defn- insp-env [Bh]
  (sub {'BH Bh 'CR cr-def}
    '(envE_cons1 chkf dec encTy cap (usk BH) (uskCtx (List.cons Exp Exp.tR D))
       (List.cons U U.u1 us2) RV.star (List.cons RV (RV.cert CR) rho) Unit.unit (Prod.mk CR eta)
       (Eq.refl$1 RV.star)
       (envE_cons1 chkf dec encTy cap (usk Exp.tR) (uskCtx D) us2 (RV.cert CR) rho CR eta
         (Eq.refl$1 (RV.cert CR)) henv))))

(thm* 'adeqE_insp
  (concat P3 CTX '[us2 :- (List U), X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp,
                   re :- Exp, ce :- Exp, t1e :- Exp, t2e :- Exp] ENV
          ['henv :- (envE-at 'us2)
           'ihr :- (adq 'rho 're 'Exp.tR 'r)
           'ihc :- (adq 'rho 'ce 'Exp.tSyn 'c)
           'ih1 :- (adqB D1 '(List.cons U U.u1 (List.cons U U.u1 us2)) 't1e '(lift 2 0 X) 't1)
           'ih2 :- (adqB D2 '(List.cons U U.u1 (List.cons U U.u1 us2)) 't2e '(lift 2 0 X) 't2)])
  (adq 'rho '(Exp.insp X re ce t1e t2e) 'X '(Exp.insp X r c t1 t2))
  (sub {'CR cr-def 'CD cd-def 'CX cX 'D1 D1 'D2 D2
        'HEN1 (insp-env (nth D1 2)) 'HEN2 (insp-env (nth D2 2))
        'BR1 (sub {'cr cr-def} (branch-rel D1 (nth D1 2) 't1 'w1 '(And.right p1)))
        'BR2 (sub {'cr cr-def} (branch-rel D2 (nth D2 2) 't2 'w2 '(And.right p2)))}
   '[(refine' (exT RV _ _ ihr _)) (intro vr pr)
     (have her (EvalE chkf dec encTy cap rho re (RV.cert CR))
       (evalE_cast chkf dec encTy cap rho re vr (RV.cert CR) (And.left pr) (And.right pr)))
     (refine' (exT RV _ _ ihc _)) (intro vc pc)
     (have hec (EvalE chkf dec encTy cap rho ce (RV.code CD))
       (evalE_cast chkf dec encTy cap rho ce vc (RV.code CD) (And.left pc) (And.right pc)))
     (refine' (exT RV _ _ (ih1 (List.cons RV RV.star (List.cons RV (RV.cert CR) rho))
                               (Prod.mk Unit.unit (Prod.mk CR eta)) HEN1) _))
     (intro w1 p1)
     (refine' (exT RV _ _ (ih2 (List.cons RV RV.star (List.cons RV (RV.cert CR) rho))
                               (Prod.mk Unit.unit (Prod.mk CR eta)) HEN2) _))
     (intro w2 p2)
     (exact (adeqE_retarget_eq chkf dec encTy cap D (Exp.insp X r c t1 t2) (Exp.insp X re ce t1e t2e) X rho eta
              (Eq.mp CX (Bool.rec$1 (fn [_ :- Bool] (Car (skel X)))
                          (den chkf dec encTy cap t2 (sk2 Sk.unit Sk.cert (skels D)) (skel X)
                            (Prod.mk Unit.unit (Prod.mk CR (henv_of_usk D eta))))
                          (den chkf dec encTy cap t1 (sk2 Sk.unit Sk.cert (skels D)) (skel X)
                            (Prod.mk Unit.unit (Prod.mk CR (henv_of_usk D eta))))
                          (chkf CR CD)))
              (congrArg (fn [x :- (Car (skel X))] (Eq.mp CX x))
                (Eq.symm (den_insp_at chkf dec encTy cap X r c t1 t2 (skels D) (skel X) (henv_of_usk D eta))))
              (adeqE_insp_pick chkf dec encTy cap D X r c t1 t2 re ce t1e t2e rho eta CR CD her hec
                w1 (And.left p1) BR1 w2 (And.left p2) BR2 (chkf CR CD) Unit.unit (Eq.refl$1 (chkf CR CD)))))]))

;; --- the token environment of Θₘ, for E (reflect, Theorem 4′) --------------------

;; m tokens, as a carrier environment for usks (uskCtx Θₘ).  Each entry is ◇.
(kdef tokU (forall [m Nat] (HEnv (usks (uskCtx (thetaD m)))))
  (fn [m :- Nat]
    (Nat.rec$1 (fn [k :- Nat] (HEnv (usks (uskCtx (thetaD k))))) Unit.unit
      (fn [k :- Nat, e :- (HEnv (usks (uskCtx (thetaD k))))] (Prod.mk Unit.unit e))
      m)))

;; The m runtime tokens are related to it, every entry at usage 1.
(thm tokE [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
           encTy :- (=> Exp Code), cap :- Nat, m :- Nat]
  (envE chkf dec encTy cap (uskCtx (thetaD m)) (thetaU m) (rtokens m) (tokU m))
  (induction m)
  (exact (envE_nil chkf dec encTy cap))
  (exact (envE_cons1 chkf dec encTy cap (usk Exp.tDia) (uskCtx (thetaD n)) (thetaU n)
           RV.token (rtokens n) Unit.unit (tokU n) (Eq.refl$1 RV.token) ih_n)))

;; A cast of an environment along a context equation is the same dependent
;; pair (cases on the equation, as dflt_cast).
(thm psig_cast [G1 :- (List Sk), G2 :- (List Sk), h :- (Eq (List Sk) G1 G2), x :- (HEnv G1)]
  (= (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) G2 (Eq.mp (congrArg HEnv h) x))
     (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) G1 x))
  (cases h)
  (rfl))

(thm tokU_pair [m :- Nat]
  (= (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) (usks (uskCtx (thetaD m))) (tokU m))
     (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) (thetaSk m) (tokenEnv m)))
  (induction m)
  (rfl)
  (exact (congrArg (fn [p :- (PSigma (fn [G :- (List Sk)] (HEnv G)))]
                     (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G))
                       (List.cons Sk Sk.dia (PSigma.fst p)) (Prod.mk Unit.unit (PSigma.snd p))))
                   ih_n)))

;; Any denotation in the token environment of reflect (thetaSk, tokenEnv)
;; is the same in tokU, transported to the context's skeletons.
(thm tokU_transfer [m :- Nat, f :- DenBody, s :- Sk]
  (= (f (thetaSk m) s (tokenEnv m))
     (f (skels (thetaD m)) s (henv_of_usk (thetaD m) (tokU m))))
  (exact (congrArg (fn [p :- (PSigma (fn [G :- (List Sk)] (HEnv G)))] (f (PSigma.fst p) s (PSigma.snd p)))
           (Eq.trans (Eq.symm (tokU_pair m))
             (Eq.symm (psig_cast (usks (uskCtx (thetaD m))) (skels (thetaD m))
                        (usk_ctx (thetaD m)) (tokU m)))))))

;; --- base data types ------------------------------------------------------------

;; reflect's annotation has a base code exactly at the base data types.
(thm isBase_of_code [X :- Exp, cd :- Exp]
  (=> (Eq (Option Exp) (baseCode X) (Option.some Exp cd)) (Eq Bool (isBaseTy X) Bool.true))
  (cases X)
  (all_goals (intro hb))
  (all_goals (first (rfl) (exact (False.elim$0 (none_ne_someE cd hb))))))

;; At a base data type E is equality with the canonical runtime value, the
;; same at every budget.
(thm erel_base_budget [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                       encTy :- (=> Exp Code), n :- Nat, m :- Nat, X :- Exp]
  (=> (Eq Bool (isBaseTy X) Bool.true)
    (forall [w RV] (forall [a (Car (uskSk (usk X)))]
      (=> (Erel chkf dec encTy n (usk X) w a) (Erel chkf dec encTy m (usk X) w a)))))
  (cases X)
  (all_goals (intro hb w a h))
  (all_goals (first (exact h) (exact (Bool.noConfusion hb)))))

;; At a base data type, E at the carrier value and Theorem 4's relation at
;; the same value (read at skel X) pin the runtime value: both are equality
;; with its canonical form.
(thm erel_rel_base [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                    encTy :- (=> Exp Code), n :- Nat, X :- Exp]
  (=> (Eq Bool (isBaseTy X) Bool.true)
    (forall [w RV] (forall [v RV] (forall [a (Car (skel X))]
      (=> (Erel chkf dec encTy n (usk X) w (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) a))
        (=> (rel chkf dec encTy n (skel X) v a) (Eq RV w v)))))))
  (cases X)
  (all_goals (intro hb w v a h1 h2))
  (all_goals (first (exact (Eq.trans h1 (Eq.symm h2))) (exact (Bool.noConfusion hb)))))

;; --- reflect (Theorem 4′) -----------------------------------------------------------
;; r and e are evaluated (inductive hypothesis); the runtime certificate is
;; the carrier tree, so both sides take the same cap test and chkf.  On
;; failure both sides return the default (erdflt_ty).  On success CheckSpec
;; decodes Θₘ ⊢ t₂ : A with m < cnodes ≤ n and A = X (E1, both closed);
;; er_total erases that derivation and the outer hypothesis AdeqE m runs it
;; on the m tokens.  E at a base type does not depend on the budget, and
;; den_refl_ok_eq with tokU_transfer identifies the two denotations.

(def ^:private guard
  '(Bool.and (Nat.ble (cnodes (denU chkf dec encTy cap D r Exp.tR eta)) cap)
             (chkf (denU chkf dec encTy cap D r Exp.tR eta) (encTy X))))

(thm* 'adeqE_refl_no
  (concat P3 CTX '[X :- Exp, r :- Exp, e :- Exp, re :- Exp, ee :- Exp] ENV
          ['ve :- 'RV
           'her :- '(EvalE chkf dec encTy cap rho re (RV.cert (denU chkf dec encTy cap D r Exp.tR eta)))
           'hee :- '(EvalE chkf dec encTy cap rho ee ve)
           'hc :- (list 'Eq 'Bool guard 'Bool.false)])
  (adq 'rho '(Exp.refl X re ee) 'X '(Exp.refl X r e))
  '[(constructor) (exact (rdflt (skel X)))
    (constructor)
    (exact (EvE.eReflNo chkf dec encTy cap rho X re ee (denU chkf dec encTy cap D r Exp.tR eta) ve her hee hc))
    (exact (erel_car chkf dec encTy cap (usk X) (rdflt (skel X))
             (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) (dflt (skel X)))
             (denU chkf dec encTy cap D (Exp.refl X r e) X eta)
             (erdflt_ty chkf dec encTy cap X)
             (congrArg (fn [x :- (Car (skel X))] (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) x))
               (Eq.symm (den_refl_false chkf dec encTy cap X r e (skels D) (henv_of_usk D eta) hc)))))])

(def ^:private CR '(denU chkf dec encTy cap D r Exp.tR eta))

(thm* 'adeqE_refl_yes
  (concat P3 '[cap :- Nat, hcs :- (CheckSpec chkf dec encTy),
               below :- (forall [m Nat] (=> (LT.lt m cap) (AdeqE chkf dec encTy m))),
               D :- (List Exp)]
          '[X :- Exp, cd :- Exp, r :- Exp, e :- Exp, re :- Exp, ee :- Exp] ENV
          ['hb :- '(Eq (Option Exp) (baseCode X) (Option.some Exp cd))
           've :- 'RV
           'her :- '(EvalE chkf dec encTy cap rho re (RV.cert (denU chkf dec encTy cap D r Exp.tR eta)))
           'hee :- '(EvalE chkf dec encTy cap rho ee ve)
           'hc :- (list 'Eq 'Bool guard 'Bool.true)])
  (adq 'rho '(Exp.refl X re ee) 'X '(Exp.refl X r e))
  (sub {'CR CR}
   '[(have hbase (Eq Bool (isBaseTy X) Bool.true) (isBase_of_code X cd hb))
     (have hchk (Eq Bool (chkf CR (encTy X)) Bool.true)
       (andb_right (Nat.ble (cnodes CR) cap) (chkf CR (encTy X)) hc))
     (refine' (exT Nat _ _ ((And.left hcs) CR (encTy X) hchk) _))
     (intro m hm)
     (refine' (exT Exp _ _ hm _))
     (intro t2 ht2)
     (refine' (exT Exp _ _ ht2 _))
     (intro A hA)
     (have hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec CR)
               (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t2 A))))
       (And.left hA))
     (have hrt (Rt chkf (thetaD m) (thetaU m) t2 A) (And.left (And.right hA)))
     (have hclA (Eq Bool (closedTy A) Bool.true) (And.left (And.right (And.right (And.right hA)))))
     (have henc (Eq Code (encTy A) (encTy X)) (And.left (And.right (And.right (And.right (And.right hA))))))
     (have hlt0 (LT.lt m (cnodes CR)) (And.right (And.right (And.right (And.right (And.right hA))))))
     (have hble (Eq Bool (Nat.ble (cnodes CR) cap) Bool.true)
       (andb_left (Nat.ble (cnodes CR) cap) (chkf CR (encTy X)) hc))
     (have hle (Nat.le (cnodes CR) cap) (Nat.le_of_ble_eq_true hble))
     (have hlt (LT.lt m cap) (lt_le_omega m (cnodes CR) cap hlt0 hle))
     (have eAX (Eq Exp A X)
       ((And.right (And.right (And.right hcs))) A X hclA (closed_of_base X hbase) henc))
     (refine' (exT Exp _ _ (er_total chkf (thetaD m) (thetaU m) t2 A hrt) _))
     (intro e2 her2)
     (refine' (exT RV _ _ (below m hlt (thetaD m) (thetaU m) t2 A e2 her2 (rtokens m) (tokU m)
                             (tokE chkf dec encTy m m)) _))
     (intro w pw)
     (have hrelX (Erel chkf dec encTy m (usk X) w (denU chkf dec encTy m (thetaD m) t2 X (tokU m)))
       (Eq.mp (congrArg (fn [Z :- Exp] (Erel chkf dec encTy m (usk Z) w (denU chkf dec encTy m (thetaD m) t2 Z (tokU m))))
                eAX)
         (And.right pw)))
     (have hrelN (Erel chkf dec encTy cap (usk X) w (denU chkf dec encTy m (thetaD m) t2 X (tokU m)))
       (erel_base_budget chkf dec encTy m cap X hbase w (denU chkf dec encTy m (thetaD m) t2 X (tokU m)) hrelX))
     (constructor) (exact w)
     (constructor)
     (exact (EvE.eReflOk chkf dec encTy cap rho X re ee CR m t2 A e2 ve w her hee hc hd her2 (And.left pw)))
     (exact (erel_car chkf dec encTy cap (usk X) w
              (denU chkf dec encTy m (thetaD m) t2 X (tokU m))
              (denU chkf dec encTy cap D (Exp.refl X r e) X eta)
              hrelN
              (congrArg (fn [x :- (Car (skel X))] (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) x))
                (Eq.trans (Eq.symm (tokU_transfer m (den chkf dec encTy m t2) (skel X)))
                  (Eq.symm (den_refl_ok_eq chkf dec encTy cap (skels D) X r e (henv_of_usk D eta)
                             m t2 A hc hd hlt))))))]))

(thm* 'adeqE_refl
  (concat P3 '[cap :- Nat, hcs :- (CheckSpec chkf dec encTy),
               below :- (forall [m Nat] (=> (LT.lt m cap) (AdeqE chkf dec encTy m))),
               D :- (List Exp)]
          '[X :- Exp, cd :- Exp, r :- Exp, e :- Exp, re :- Exp, ee :- Exp] ENV
          ['hb :- '(Eq (Option Exp) (baseCode X) (Option.some Exp cd))
           'ihr :- (adq 'rho 're 'Exp.tR 'r)
           'ihe :- (adq 'rho 'ee '(chkT r cd) 'e)])
  (adq 'rho '(Exp.refl X re ee) 'X '(Exp.refl X r e))
  (sub {'CR CR}
   '[(refine' (exT RV _ _ ihr _)) (intro vr pr)
     (have her (EvalE chkf dec encTy cap rho re (RV.cert CR))
       (evalE_cast chkf dec encTy cap rho re vr (RV.cert CR) (And.left pr) (And.right pr)))
     (refine' (exT RV _ _ ihe _)) (intro ve pe)
     (by_cases (Bool.and (Nat.ble (cnodes CR) cap) (chkf CR (encTy X))))
     (exact (adeqE_refl_no chkf dec encTy cap D X r e re ee rho eta ve her (And.left pe) hc))
     (exact (adeqE_refl_yes chkf dec encTy cap hcs below D X cd r e re ee rho eta hb ve her (And.left pe) hc))]))

;; --- the usage-indexed rules: one dispatcher each (Theorem 4′) ---------------------
;; The case lemmas of funde.clj are stated per usage (0, 1, ω), because the
;; usage skeleton of Π/Σ does not reduce until the usage is a constructor.
;; cases on the usage selects the lemma.

(thm* 'cE_lam (concat P3 CTX '[us :- (List U), A :- Exp, B :- Exp, t :- Exp, te :- Exp] ENV
               ['henv :- (envE-at 'us)])
  (list 'forall '[r U]
    (list '=> (adqB '(List.cons Exp A D) '(List.cons U r us) 'te 'B 't)
              (adq 'rho '(Exp.lam r A te) '(Exp.tPi r A B) '(Exp.lam r A t))))
  '[(intro r) (cases r)
    (intro ih) (exact (adeqE_lam0 chkf dec encTy cap D us A B t te rho eta henv ih))
    (intro ih) (exact (adeqE_lam1 chkf dec encTy cap D us A B t te rho eta henv ih))
    (intro ih) (exact (adeqE_lamw chkf dec encTy cap D us A B t te rho eta henv ih))])

(thm* 'cE_app (concat P3 CTX '[us2 :- (List U), f :- Exp, u :- Exp, A :- Exp, B :- Exp, fe :- Exp, ue :- Exp] ENV
               '[hA :- (Tl chkf Bool.true D A Exp.tUnit),
                 hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)])
  (list 'forall '[r U]
    (list '=> '(Eq Bool (nonzero r) Bool.true) '(Rt chkf D us2 u A)
              (adq 'rho 'fe '(Exp.tPi r A B) 'f) (adq 'rho 'ue 'A 'u)
              (adq 'rho '(Exp.app fe ue) '(subst1 u B) '(Exp.app f u))))
  '[(intro r) (cases r)
    (intro hr hu ihf ihu) (exact (Bool.noConfusion hr))
    (intro hr hu ihf ihu) (exact (adeqE_app1 chkf dec encTy cap D us2 f u A B fe ue rho eta hu hA hB ihf ihu))
    (intro hr hu ihf ihu) (exact (adeqE_appw chkf dec encTy cap D us2 f u A B fe ue rho eta hu hA hB ihf ihu))])

(thm* 'cE_pair (concat P3 CTX '[us1 :- (List U), A :- Exp, B :- Exp, x :- Exp, y :- Exp, xe :- Exp, ye :- Exp] ENV
                '[hA :- (Tl chkf Bool.true D A Exp.tUnit)])
  (list 'forall '[r U]
    (list '=> '(Eq Bool (nonzero r) Bool.true) '(Rt chkf D us1 x A)
              (adq 'rho 'xe 'A 'x) (adq 'rho 'ye '(subst1 x B) 'y)
              (adq 'rho '(Exp.pair (Exp.tSig r A B) xe ye) '(Exp.tSig r A B) '(Exp.pair (Exp.tSig r A B) x y))))
  '[(intro r) (cases r)
    (intro hr hx ihx ihy) (exact (Bool.noConfusion hr))
    (intro hr hx ihx ihy) (exact (adeqE_pair1 chkf dec encTy cap D us1 A B x y xe ye rho eta hx hA ihx ihy))
    (intro hr hx ihx ihy) (exact (adeqE_pairw chkf dec encTy cap D us1 A B x y xe ye rho eta hx hA ihx ihy))])

(thm* 'cE_let (concat P3 CTX '[us1 :- (List U), us2 :- (List U), A :- Exp, B :- Exp, C :- Exp,
                               p :- Exp, t :- Exp, pe :- Exp, te :- Exp] ENV
               ['henv :- (envE-at 'us2)
                'hA :- '(Tl chkf Bool.true D A Exp.tUnit)
                'hB :- '(Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)])
  (list 'forall '[r U]
    (list '=> '(Rt chkf D us1 p (Exp.tSig r A B))
              (adq 'rho 'pe '(Exp.tSig r A B) 'p)
              (adqB '(List.cons Exp B (List.cons Exp A D)) '(List.cons U U.u1 (List.cons U r us2))
                    'te '(lift 2 0 C) 't)
              (adq 'rho '(Exp.letp C pe te) 'C '(Exp.letp C p t))))
  '[(intro r) (cases r)
    (intro hp ihp iht) (exact (adeqE_let0 chkf dec encTy cap D us1 us2 A B C p t pe te rho eta henv hp hA hB ihp iht))
    (intro hp ihp iht) (exact (adeqE_let1 chkf dec encTy cap D us1 us2 A B C p t pe te rho eta henv hp hA hB ihp iht))
    (intro hp ihp iht) (exact (adeqE_letw chkf dec encTy cap D us1 us2 A B C p t pe te rho eta henv hp hA hB ihp iht))])

;; --- the induction on Er (Theorem 4′, the fundamental property at budget cap) -------
;;
;; The motive: for every environment related at the judgment's usages, the
;; erasure evaluates and its value is E-related to the denotation.  Each
;; premise's environment is the conclusion's, restricted (envE_sub) along
;; SubU, whose proof sub-pf builds from the judgment's vadd/vscale tree and
;; the premises' lengths (er_len).  A premise under binders is restricted
;; in the extended environment (subU_cons on the binders).  The usage
;; parameter of a case lemma whose premise is under binders is instantiated
;; with the conclusion's vector, so its environment hypothesis is henv.

(defn- contains-expr? [tree x] (boolean (some #(= % x) (tree-seq seq? seq tree))))

;; SubU w t, for a premise vector w inside the tree t.
(defn- sub-pf [w t lp nz]
  (cond
    (= w t) (list 'subU_refl w)
    (and (seq? t) (= 'vadd (first t)))
    (let [[_ x y] t
          eq (list 'Eq.trans (len-pf x lp) (list 'Eq.symm (len-pf y lp)))]
      (if (contains-expr? x w)
        (list 'subU_trans w x t (sub-pf w x lp nz) (list 'subU_vadd_l x y eq))
        (list 'subU_trans w y t (sub-pf w y lp nz) (list 'subU_vadd_r x y eq))))
    (and (seq? t) (= 'vscale (first t)))
    (let [[_ r x] t]
      (list 'subU_trans w x t (sub-pf w x lp nz) (list 'subU_vscale r (nz r) x)))
    :else (throw (ex-info "not a subvector" {:w w :t t}))))

(defn- nz [r] (if (= r 'U.uw) '(Eq.refl$1 Bool.true) 'hr))

;; The premise h at vector w, in the conclusion's environment.
(defn- ihA [c h w]
  (let [[t leaves] (vtrees c)
        L (fn [h] (list 'er_len 'chkf '_ '_ '_ '_ '_ h))
        lp (into {} (for [[k v] leaves] [k (leaf-len L v)]))]
    (if (= w t)
      (list (symbol (str "ih_" h)) 'rho 'eta 'henv)
      (list (symbol (str "ih_" h)) 'rho 'eta
            (list 'envE_sub 'chkf 'dec 'encTy 'cap '(uskCtx D) t w 'rho 'eta 'henv (sub-pf w t lp nz))))))

(defn- cons-us [pre v] (reduce (fn [acc p] (list 'List.cons 'U p acc)) v (reverse pre)))
(defn- cons-sub [pre w v pf]
  (if (empty? pre) pf
      (list 'subU_cons (first pre) (cons-us (rest pre) w) (cons-us (rest pre) v)
            (cons-sub (rest pre) w v pf))))

;; The premise h under binders: context Dx, binder usages pre, premise
;; vector w; the case lemma's vector is the conclusion's.
(defn- ihB [c h Dx pre w]
  (let [[t leaves] (vtrees c)
        L (fn [h] (list 'er_len 'chkf '_ '_ '_ '_ '_ h))
        lp (into {} (for [[k v] leaves] [k (leaf-len L v)]))]
    (list 'fn ['rho2 :- '(List RV), 'eta2 :- (list 'HEnv (list 'usks (list 'uskCtx Dx))),
               'h2 :- (list 'envE 'chkf 'dec 'encTy 'cap (list 'uskCtx Dx) (cons-us pre t) 'rho2 'eta2)]
          (list (symbol (str "ih_" h)) 'rho2 'eta2
                (list 'envE_sub 'chkf 'dec 'encTy 'cap (list 'uskCtx Dx) (cons-us pre t) (cons-us pre w)
                      'rho2 'eta2 'h2 (cons-sub pre w t (sub-pf w t lp nz)))))))

(defn- V [c] (first (vtrees c)))
(defn- rt [D us t A e h] (list 'er_rt 'chkf D us t A e h))
(def ^:private hsn-recN
  '(skj_term_unit Bool.false (skels D) n (skel Exp.tNat)
     (lemma25_rt chkf D us1 n Exp.tNat (er_rt chkf D us1 n Exp.tNat ne hn)) rfl))
(def ^:private hsc-recS
  '(skj_term_unit Bool.false (skels D) c (skel Exp.tSyn)
     (lemma25_rt chkf D us1 c Exp.tSyn (er_rt chkf D us1 c Exp.tSyn ce hc)) rfl))

(def ^:private case-terms
  (let [PS '[chkf dec encTy cap]
        mk (fn [f & args] (apply list f (concat PS args)))]
    [(mk 'adeqE_var 'D 'us 'i 'A 'r 'hA 'hu 'hr 'rho 'eta 'henv)
     (list (mk 'adeqE_const 'D 'rho 'eta) 't 'A 'h)
     (list (mk 'cE_lam 'D 'us 'A 'B 't 'te 'rho 'eta 'henv) 'r 'ih_ht)
     (mk 'adeqE_app0 'D 'us 'f 'u 'A 'B 'fe 'rho 'eta 'hu 'hA 'hB '(ih_hf rho eta henv))
     (list (mk 'cE_app 'D 'us2 'f 'u 'A 'B 'fe 'ue 'rho 'eta 'hA 'hB) 'r 'hr
           (rt 'D 'us2 'u 'A 'ue 'hu) (ihA 'eApp 'hf 'us1) (ihA 'eApp 'hu 'us2))
     (mk 'adeqE_pair0 'D 'A 'B 'x 'y 'ye 'rho 'eta 'hx 'hA 'hB '(ih_hy rho eta henv))
     (list (mk 'cE_pair 'D 'us1 'A 'B 'x 'y 'xe 'ye 'rho 'eta 'hA) 'r 'hr
           (rt 'D 'us1 'x 'A 'xe 'hx) (ihA 'ePair 'hx 'us1) (ihA 'ePair 'hy 'us2))
     (list (mk 'cE_let 'D 'us1 (V 'eLet) 'A 'B 'C 'p 't 'pe 'te 'rho 'eta 'henv 'hA 'hB) 'r
           (rt 'D 'us1 'p '(Exp.tSig r A B) 'pe 'hp) (ihA 'eLet 'hp 'us1)
           (ihB 'eLet 'ht '(List.cons Exp B (List.cons Exp A D)) '[U.u1 r] 'us2))
     (mk 'adeqE_abort 'D 'A 't 'te 'rho 'eta '(ih_ht rho eta henv))
     (mk 'adeqE_conv 'D 't 'A 'B 'te 'rho 'eta 'hc '(ih_ht rho eta henv))
     (mk 'adeqE_ite 'D 'b 't 'e 'C 'be 'te 'ee 'rho 'eta
         (ihA 'eIte 'hb 'us1) (ihA 'eIte 'ht 'us2) (ihA 'eIte 'he 'us2))
     (mk 'adeqE_elimB 'D 'us1 'P 'b 't 'e 'be 'te 'ee 'rho 'eta (rt 'D 'us1 'b 'Exp.tBool 'be 'hb)
         (ihA 'eElimB 'hb 'us1) (ihA 'eElimB 'ht 'us2) (ihA 'eElimB 'he 'us2))
     (mk 'adeqE_succ 'D 'n 'ne 'rho 'eta '(ih_h rho eta henv))
     (mk 'adeqE_recN 'D 'P 'z 's 'n 'ze 'se 'ne (V 'eRecN) 'rho 'eta
         (list 'usk_of_unit 'n hsn-recN) hsn-recN 'henv
         (ihA 'eRecN 'hn 'us1) (ihA 'eRecN 'hz 'us2)
         (ihB 'eRecN 'hs '(List.cons Exp P (List.cons Exp Exp.tNat D)) '[U.u1 U.uw] '(vscale U.uw us3)))
     (mk 'adeqE_case 'D 'us1 'P 'x 'bs 'xe 'bse 'rho 'eta (rt 'D 'us1 'x 'Exp.tLbl 'xe 'hx)
         (ihA 'eCaseL 'hx 'us1) (ihA 'eCaseL 'hb 'us2))
     (mk 'adeqE_bnil 'D 'P '(NL) 'rho 'eta)
     (mk 'adeqE_bcons 'D 'P 'k 'h 't 'he 'te 'rho 'eta '(ih_hh rho eta henv) '(ih_ht rho eta henv))
     (mk 'adeqE_sleaf 'D 'x 'xe 'rho 'eta '(ih_h rho eta henv))
     (mk 'adeqE_snode 'D 'x 'c1 'c2 'xe 'c1e 'c2e 'rho 'eta
         (ihA 'eSnode 'hx 'us1) (ihA 'eSnode 'h1 'us2) (ihA 'eSnode 'h2 'us3))
     (mk 'adeqE_recS 'D 'P 'tl 'tn 'c 'tle 'tne 'ce (V 'eRecS) 'rho 'eta
         (list 'usk_of_unit 'c hsc-recS) hsc-recS 'henv
         (ihA 'eRecS 'hc 'us1)
         (ihB 'eRecS 'hl '(List.cons Exp Exp.tLbl D) '[U.uw] '(vscale U.uw us2))
         (ihB 'eRecS 'hn '(nodeCtx P D) '[U.u1 U.u1 U.uw U.uw U.uw] '(vscale U.uw us3)))
     (mk 'adeqE_leaf 'D 'x 'xe 'rho 'eta '(ih_h rho eta henv))
     (mk 'adeqE_node 'D 'd 'x 'r1 'r2 'de 'xe 'r1e 'r2e 'rho 'eta
         (ihA 'eNode 'hd 'us1) (ihA 'eNode 'hx 'us2) (ihA 'eNode 'h1 'us3) (ihA 'eNode 'h2 'us4))
     (mk 'adeqE_itR 'D 'X 'g 'h 'r 'ge 'he 're 'rho 'eta
         (ihA 'eItR 'hg '(vscale U.uw us1)) (ihA 'eItR 'hh '(vscale U.uw us2)) (ihA 'eItR 'hr 'us3))
     (mk 'adeqE_prn 'D 'r 're 'rho 'eta '(ih_h rho eta henv))
     (mk 'adeqE_chk 'D 'c 'd 'ce 'de 'rho 'eta (ihA 'eChk 'hc 'us1) (ihA 'eChk 'hd 'us2))
     (mk 'adeqE_h1 'D 'r 's 'c 'e1 'e2 're 'se 'ce 'e1e 'e2e 'rho 'eta
         (ihA 'eH1 'hr 'us1) (ihA 'eH1 'hs 'us2) (ihA 'eH1 'hc '(vscale U.uw us3))
         (ihA 'eH1 'h1 'us4) (ihA 'eH1 'h2 'us5))
     (apply list 'adeqE_refl (concat PS '[hcs below D X cd r e re ee rho eta hb]
                                     [(ihA 'eRefl 'hr 'us1) (ihA 'eRefl 'he 'us2)]))
     (mk 'adeqE_insp 'D (V 'eInsp) 'X 'r 'c 't1 't2 're 'ce 't1e 't2e 'rho 'eta 'henv
         (ihA 'eInsp 'hr 'us1) (ihA 'eInsp 'hc '(vscale U.uw us0))
         (ihB 'eInsp 'h1 D1 '[U.u1 U.u1] 'us2)
         (ihB 'eInsp 'h2 D2 '[U.u1 U.u1] 'us2))]))

(def ^:private STEP-PARAMS
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
    cap :- Nat, hcs :- (CheckSpec chkf dec encTy),
    below :- (forall [m Nat] (=> (LT.lt m cap) (AdeqE chkf dec encTy m))),
    D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, e0 :- Exp, der :- (Er chkf D0 us0 t0 A0 e0)])
(def ^:private STEP-GOAL
  '(forall [rho (List RV)] (forall [eta (HEnv (usks (uskCtx D0)))]
     (=> (envE chkf dec encTy cap (uskCtx D0) us0 rho eta)
       (Exists (fn [v :- RV]
         (And (EvalE chkf dec encTy cap rho e0 v)
              (Erel chkf dec encTy cap (usk A0) v (denU chkf dec encTy cap D0 t0 A0 eta)))))))))

;; adeqE_step: the fundamental property at budget cap, given it below cap
;; (reflect's outer hypothesis) and CheckSpec.  Induction on Er.
(thm* 'adeqE_step STEP-PARAMS STEP-GOAL
  (into ['(induction der)]
        (mapcat (fn [t] ['(intro rho eta henv) (list 'exact t)]) case-terms)))

;; --- the strong induction on the budget, and the fundamental property -------------

(thm adeqE_from_below
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, hcs :- (CheckSpec chkf dec encTy),
   below :- (forall [m Nat] (=> (LT.lt m cap) (AdeqE chkf dec encTy m)))]
  (AdeqE chkf dec encTy cap)
  (intro D us t A e her rho eta henv)
  (exact (adeqE_step chkf dec encTy cap hcs below D us t A e her rho eta henv)))

(thm adeqE_below_succ
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat]
    (=> (forall [m Nat] (=> (LT.lt m n) (AdeqE chkf dec encTy m)))
        (forall [m Nat] (=> (LT.lt m (Nat.succ n)) (AdeqE chkf dec encTy m)))))
  (intro n ih m hm)
  (have hor (Or (Eq Nat m n) (LT.lt m n)) (Nat.eq_or_lt_of_le (Nat.le_of_lt_succ hm)))
  (cases hor)
  (subst h)
  (exact (adeqE_from_below chkf dec encTy n hcs ih))
  (exact (ih m h)))

(thm adeqE_below_all
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat] (forall [m Nat] (=> (LT.lt m n) (AdeqE chkf dec encTy m))))
  (intro n)
  (induction n)
  (intro m hm) (exact (False.elim (Nat.not_lt_zero m hm)))
  (exact (adeqE_below_succ chkf dec encTy hcs n ih_n)))

;; Theorem 4′, the fundamental property of evalᴱ: under CheckSpec, at every
;; budget n, for every erasure Er of a runtime derivation Γ ⊢ t :¹ A and
;; every runtime environment E-related to η, the erasure evaluates under
;; evalᴱₙ and its value is E-related to ⟦t⟧ⁿη.
(thm adeqE_all
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat] (AdeqE chkf dec encTy n))
  (intro n)
  (exact (adeqE_from_below chkf dec encTy n hcs (adeqE_below_all chkf dec encTy hcs n))))

;; --- Theorem 4′ (R4-metatheory §5) ---------------------------------------------------
;;
;; For derivable Θₙ ⊢ t :¹ X with X a base data type, every erasure of the
;; derivation evaluates under evalᴱₙ, in the n runtime tokens, to the value
;; evalₙ(t) returns, and that value is the denotation's canonical form.
;; Theorem 4 (theorem4.clj) runs t; adeqE_all runs the erasure; at a base
;; type both relations are equality with the canonical form of ⟦t⟧ⁿ, read
;; in two token environments that denote the same element (tokU_transfer).
(thm theorem4e
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp,
   hd :- (Rt chkf (thetaD n) (thetaU n) t X),
   hb :- (Eq Bool (isBaseTy X) Bool.true)]
  (forall [e Exp]
    (=> (Er chkf (thetaD n) (thetaU n) t X e)
      (Exists (fn [v :- RV]
        (And (EvalE chkf dec encTy n (rtokens n) e v)
          (And (Eval chkf dec encTy n (rtokens n) t v)
               (rel chkf dec encTy n (skel X) v
                 (den chkf dec encTy n t (thetaSk n) (skel X) (tokenEnv n)))))))))
  (intro e her)
  (have hsk (SkJ Bool.false (thetaSk n) t (skel X))
    (skj_along Bool.false t (skel X) (skels (thetaD n)) (thetaSk n) (skels_theta n)
      (lemma25_rt chkf (thetaD n) (thetaU n) t X hd)))
  (have hok (Eq Bool (argsOK (thetaSk n) t) Bool.true)
    (Eq.trans (Eq.symm (argsOK_ctx t (skels (thetaD n)) (thetaSk n) (skels_theta n)))
      (rt_argsOK chkf (thetaD n) (thetaU n) t X hd)))
  (have hrun := (theorem4 chkf dec encTy hcs n (thetaSk n) t (skel X) hsk hok
                  (rtokens n) (tokenEnv n) (tokens_rel chkf dec encTy n n)))
  (have hE := (adeqE_all chkf dec encTy hcs n (thetaD n) (thetaU n) t X e her
                (rtokens n) (tokU n) (tokE chkf dec encTy n n)))
  (refine' (exT RV _ _ hrun _)) (intro v pv)
  (refine' (exT RV _ _ hE _)) (intro w pw)
  (have hrelw (Erel chkf dec encTy n (usk X) w
                (Eq.mp (congrArg Car (Eq.symm (usk_skel X)))
                  (den chkf dec encTy n t (thetaSk n) (skel X) (tokenEnv n))))
    (erel_car chkf dec encTy n (usk X) w
      (denU chkf dec encTy n (thetaD n) t X (tokU n))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel X)))
        (den chkf dec encTy n t (thetaSk n) (skel X) (tokenEnv n)))
      (And.right pw)
      (congrArg (fn [x :- (Car (skel X))] (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) x))
        (Eq.symm (tokU_transfer n (den chkf dec encTy n t) (skel X))))))
  (have hwv (Eq RV w v)
    (erel_rel_base chkf dec encTy n X hb w v
      (den chkf dec encTy n t (thetaSk n) (skel X) (tokenEnv n)) hrelw (And.right pv)))
  (constructor) (exact v)
  (constructor) (exact (evalE_cast chkf dec encTy n (rtokens n) e w v (And.left pw) hwv))
  (constructor) (exact (And.left pv))
  (exact (And.right pv)))

;; Theorem 4′ as the paper states it: an erasure exists (er_total), and
;; evalᴱₙ of it and evalₙ(t) terminate with the same result.
(thm theorem4e_agree
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp,
   hd :- (Rt chkf (thetaD n) (thetaU n) t X),
   hb :- (Eq Bool (isBaseTy X) Bool.true)]
  (Exists (fn [e :- Exp]
    (And (Er chkf (thetaD n) (thetaU n) t X e)
      (Exists (fn [v :- RV]
        (And (EvalE chkf dec encTy n (rtokens n) e v)
             (Eval chkf dec encTy n (rtokens n) t v)))))))
  (refine' (exT Exp _ _ (er_total chkf (thetaD n) (thetaU n) t X hd) _))
  (intro e her)
  (refine' (exT RV _ _ (theorem4e chkf dec encTy hcs n t X hd hb e her) _))
  (intro v pv)
  (constructor) (exact e)
  (constructor) (exact her)
  (constructor) (exact v)
  (constructor) (exact (And.left pv))
  (exact (And.left (And.right pv))))

;; Theorem 4′, \"in particular\": an R result of evalᴱₙ is a certificate of at
;; most n internal nodes.  It is the carrier tree ⟦t⟧ⁿ (Theorem 4′), which
;; Theorem 3 bounds (corollary51_nodes), in the other token environment
;; (tok_transfer).
(thm theorem4e_nodes
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp,
   hd :- (Rt chkf (thetaD n) (thetaU n) t Exp.tR)]
  (forall [e Exp]
    (=> (Er chkf (thetaD n) (thetaU n) t Exp.tR e)
      (Exists (fn [c :- Code]
        (And (EvalE chkf dec encTy n (rtokens n) e (RV.cert c))
             (LE.le (cnodes c) n))))))
  (intro e her)
  (refine' (exT RV _ _ (theorem4e chkf dec encTy hcs n t Exp.tR hd rfl e her) _))
  (intro v pv)
  (constructor) (exact (den chkf dec encTy n t (thetaSk n) Sk.cert (tokenEnv n)))
  (constructor)
  (exact (evalE_cast chkf dec encTy n (rtokens n) e v
           (RV.cert (den chkf dec encTy n t (thetaSk n) Sk.cert (tokenEnv n)))
           (And.left pv) (And.right (And.right pv))))
  (exact (Eq.mpr (congrArg (fn [z :- Code] (LE.le (cnodes z) n))
                   (tok_transfer n (den chkf dec encTy n t) Sk.cert))
           (corollary51_nodes chkf dec encTy hcs n t hd))))
