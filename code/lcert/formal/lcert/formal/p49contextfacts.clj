(ns lcert.formal.p49contextfacts
  "Typing and code-readback facts for bounded Proposition 4.9 contexts."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.p49context]
            [lcert.formal.judgment]
            [lcert.formal.skel]))

(thm p49facts_band_left [x :- Bool, y :- Bool,
                         h :- (Eq Bool (Bool.and x y) Bool.true)]
  (Eq Bool x Bool.true)
  (cases x) (exact (Bool.noConfusion h)) (rfl))

(thm p49facts_band_right [x :- Bool, y :- Bool,
                          h :- (Eq Bool (Bool.and x y) Bool.true)]
  (Eq Bool y Bool.true)
  (cases x) (exact (Bool.noConfusion h)) (exact h))

(thm p49facts_andb_intro [x :- Bool, y :- Bool,
                          hx :- (Eq Bool x Bool.true),
                          hy :- (Eq Bool y Bool.true)]
  (Eq Bool (Bool.and x y) Bool.true)
  (cases x) (exact hx) (cases y) (exact hy) (rfl))

;; Hole identifiers name environment entries; only labels decorating
;; rendered leaves and nodes must lie in the finite label range.
(a/defn p49c_lblok [K :- P49C] Bool
  (match K
    [(hole i) Bool.true]
    [(leaf l) (Nat.blt l 100)]
    [(node l a b)
     (Bool.and (Nat.blt l 100)
       (Bool.and (p49c_lblok a) (p49c_lblok b)))]))

;; If every hole is a syntactic code and every constructor label is below
;; 100, rendering the context is syntactic at the same context D.
(thm p49plug_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), rho :- (=> Nat Exp),
   hrho :- (forall [i Nat] (Tl chkf Bool.false D (rho i) Exp.tSyn))]
  (forall [K P49C]
    (=> (Eq Bool (p49c_lblok K) Bool.true)
        (Tl chkf Bool.false D (p49plug K rho) Exp.tSyn)))
  (intro K)
  (induction K)
  (intro hvalid)
  (exact (hrho i))
  (intro hvalid)
  (exact (Tl.zSleaf chkf D (Exp.lbl l)
    (Tl.zConst chkf D (Exp.lbl l) Exp.tLbl hvalid)))
  (intro hvalid)
  (have hl (Eq Bool (Nat.blt l 100) Bool.true)
    (p49facts_band_left (Nat.blt l 100)
      (Bool.and (p49c_lblok a) (p49c_lblok b)) hvalid))
  (have hab (Eq Bool (Bool.and (p49c_lblok a) (p49c_lblok b)) Bool.true)
    (p49facts_band_right (Nat.blt l 100)
      (Bool.and (p49c_lblok a) (p49c_lblok b)) hvalid))
  (have ha (Eq Bool (p49c_lblok a) Bool.true)
    (p49facts_band_left (p49c_lblok a) (p49c_lblok b) hab))
  (have hb (Eq Bool (p49c_lblok b) Bool.true)
    (p49facts_band_right (p49c_lblok a) (p49c_lblok b) hab))
  (exact (Tl.zSnode chkf D (Exp.lbl l)
    (p49plug a rho) (p49plug b rho)
    (Tl.zConst chkf D (Exp.lbl l) Exp.tLbl hl)
    (ih_a ha) (ih_b hb))))

;; Replacing any named hole by another valid context preserves the
;; structural label bound. This is the validity invariant needed as a
;; context is refined through the all-depth recursion.
(thm p49facts_lblok_choose [flag :- Bool, X :- P49C, Y :- P49C]
  (Eq Bool (p49c_lblok (if flag X Y))
    (if flag (p49c_lblok X) (p49c_lblok Y)))
  (cases flag) (rfl) (rfl))

(thm p49facts_true_if [flag :- Bool]
  (Eq Bool (if flag Bool.true Bool.true) Bool.true)
  (cases flag) (rfl) (rfl))

(thm p49facts_lblok_hole_repl
  [i :- Nat, j :- Nat, R :- P49C,
   hR :- (Eq Bool (p49c_lblok R) Bool.true)]
  (Eq Bool
    (p49c_lblok (if (Nat.beq i j) R (P49C.hole i))) Bool.true)
  (exact (Eq.mpr
    (congrArg (fn [v :- Bool] (Eq Bool v Bool.true))
      (p49facts_lblok_choose (Nat.beq i j) R (P49C.hole i)))
    (Eq.mpr
      (congrArg (fn [v :- Bool]
        (Eq Bool (if (Nat.beq i j) v Bool.true) Bool.true)) hR)
      (p49facts_true_if (Nat.beq i j))))))

(thm p49c_lblok_repl [K :- P49C]
  (forall [j Nat] (forall [R P49C]
    (=> (Eq Bool (p49c_lblok K) Bool.true)
        (=> (Eq Bool (p49c_lblok R) Bool.true)
            (Eq Bool (p49c_lblok ((p49crepl K) j R)) Bool.true)))))
  (induction K)
  (intro j R hK hR)
  (exact (p49facts_lblok_hole_repl i j R hR))
  (intro j R hK hR)
  (exact hK)
  (intro j R hK hR)
  (have hl (Eq Bool (Nat.blt l 100) Bool.true)
    (p49facts_band_left (Nat.blt l 100)
      (Bool.and (p49c_lblok a) (p49c_lblok b)) hK))
  (have hab (Eq Bool (Bool.and (p49c_lblok a) (p49c_lblok b)) Bool.true)
    (p49facts_band_right (Nat.blt l 100)
      (Bool.and (p49c_lblok a) (p49c_lblok b)) hK))
  (have ha (Eq Bool (p49c_lblok a) Bool.true)
    (p49facts_band_left (p49c_lblok a) (p49c_lblok b) hab))
  (have hb (Eq Bool (p49c_lblok b) Bool.true)
    (p49facts_band_right (p49c_lblok a) (p49c_lblok b) hab))
  (have hna (Eq Bool (p49c_lblok ((p49crepl a) j R)) Bool.true)
    (ih_a j R ha hR))
  (have hnb (Eq Bool (p49c_lblok ((p49crepl b) j R)) Bool.true)
    (ih_b j R hb hR))
  (have hnab (Eq Bool
    (Bool.and (p49c_lblok ((p49crepl a) j R))
              (p49c_lblok ((p49crepl b) j R))) Bool.true)
    (p49facts_andb_intro _ _ hna hnb))
  (exact (p49facts_andb_intro _ _ hl hnab)))

;; Canonical code expressions are generated structurally from Code.
(a/defn p49ccanon [c :- Code] Exp
  (match c
    [(sl l) (Exp.sleaf (Exp.lbl l))]
    [(sn l a b) (Exp.snode (Exp.lbl l) (p49ccanon a) (p49ccanon b))]))

(thm p49ccanon_sl [l :- Nat]
  (Eq Exp (p49ccanon (Code.sl l)) (Exp.sleaf (Exp.lbl l)))
  (rfl))

(thm p49ccanon_sn [l :- Nat, a :- Code, b :- Code]
  (Eq Exp (p49ccanon (Code.sn l a b))
    (Exp.snode (Exp.lbl l) (p49ccanon a) (p49ccanon b)))
  (rfl))

;; This local equation is just codeOf's defining node case.
(thm p49facts_codeOf_node [l :- Nat, x :- Exp, y :- Exp]
  (Eq (Option Code) (codeOf (Exp.snode (Exp.lbl l) x y))
    (match (codeOf x)
      [none (Option.none Code)]
      [(some a) (match (codeOf y)
        [none (Option.none Code)]
        [(some b) (Option.some Code (Code.sn l a b))])]))
  (rfl))

(thm p49ccanon_of [c :- Code]
  (Eq (Option Code) (codeOf (p49ccanon c)) (Option.some Code c))
  (induction c)
  (rfl)
  (rw [p49ccanon_sn])
  (rw [p49facts_codeOf_node])
  (rw [ih_a])
  (rw [ih_b]))

;; The corresponding code tree substitutes a Code at each hole and uses
;; Code.sl / Code.sn at context leaves / nodes.
(a/defn p49ccode [K :- P49C] (=> (=> Nat Code) Code)
  (match K
    [(hole i) (fn [rho :- (=> Nat Code)] (rho i))]
    [(leaf l) (fn [_ :- (=> Nat Code)] (Code.sl l))]
    [(node l a b)
     (fn [rho :- (=> Nat Code)]
       (Code.sn l ((p49ccode a) rho) ((p49ccode b) rho)))]))

(thm p49facts_ccode_choose
  [flag :- Bool, X :- P49C, Y :- P49C, rho :- (=> Nat Code)]
  (Eq Code (p49ccode (if flag X Y) rho)
    (if flag (p49ccode X rho) (p49ccode Y rho)))
  (cases flag) (rfl) (rfl))

;; Code-level replacement is the same pointwise environment update as the
;; expression-level equation p49plug_repl proved in the parent namespace.
(thm p49ccode_repl [K :- P49C]
  (forall [j Nat] (forall [R P49C] (forall [rho (=> Nat Code)]
    (Eq Code (p49ccode ((p49crepl K) j R) rho)
      (p49ccode K (fn [i :- Nat]
        (if (Nat.beq i j) (p49ccode R rho) (rho i))))))))
  (induction K)
  (intro j R rho)
  (exact (p49facts_ccode_choose
    (Nat.beq i j) R (P49C.hole i) rho))
  (intro j R rho)
  (rfl)
  (intro j R rho)
  (change (Eq Code
    (Code.sn l
      (p49ccode ((p49crepl a) j R) rho)
      (p49ccode ((p49crepl b) j R) rho))
    (Code.sn l
      (p49ccode a (fn [i :- Nat]
        (if (Nat.beq i j) (p49ccode R rho) (rho i))))
      (p49ccode b (fn [i :- Nat]
        (if (Nat.beq i j) (p49ccode R rho) (rho i)))))))
  (exact (Eq.trans
    (congrArg (fn [x :- Code]
      (Code.sn l x (p49ccode ((p49crepl b) j R) rho)))
      (ih_a j R rho))
    (congrArg (fn [y :- Code]
      (Code.sn l
        (p49ccode a (fn [i :- Nat]
          (if (Nat.beq i j) (p49ccode R rho) (rho i)))) y))
      (ih_b j R rho)))))

;; Every hole is filled by a structurally generated canonical expression.
;; Readback follows from p49ccanon_of and p49facts_codeOf_node; no
;; canonicity/readback property is assumed for the rendered result.
(thm p49plug_codeOf
  [K :- P49C]
  (forall [rho (=> Nat Code)]
    (Eq (Option Code)
      (codeOf (p49plug K (fn [i :- Nat] (p49ccanon (rho i)))))
      (Option.some Code (p49ccode K rho))))
  (induction K)
  (intro rho)
  (exact (p49ccanon_of (rho i)))
  (intro rho)
  (exact (p49ccanon_of (Code.sl l)))
  (intro rho)
  (change (Eq (Option Code)
    (codeOf (Exp.snode (Exp.lbl l)
      (p49plug a (fn [i :- Nat] (p49ccanon (rho i))))
      (p49plug b (fn [i :- Nat] (p49ccanon (rho i))))))
    (Option.some Code (Code.sn l (p49ccode a rho) (p49ccode b rho)))))
  (rw [p49facts_codeOf_node])
  (rw [ih_a])
  (rw [ih_b]))
