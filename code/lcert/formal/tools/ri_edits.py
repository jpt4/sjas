"""The hand edits of the generic chain (Theorem 4.6 design note §3), applied
after tools/gen_ri.py: the proof steps that read the five clauses the
R-interpretation changes (leaf, itR, print, inspect, reflect) or V(R).

    python3 tools/ri_edits.py DIR

Each edit is (file, old, new, count): `old` must occur exactly `count` times
(0 = at least once, all replaced), so a regenerated file that no longer
matches fails loudly.  Every new text carries a `;; RI:` comment or sits
inside a form that does."""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gen_ri import top_forms, DECL

EDITS = []
# Forms replaced whole: (file, declared name, new text).  For the case lemmas
# whose proof changes shape (the phantom branches of Reflect and H₁).
FORMS = []


def F(f, name, new):
    FORMS.append((f, name, new))


def E(f, old, new, count=1):
    EDITS.append((f, old, new, count))


# --- ri_den, ri_sem: the clauses over ri ------------------------------------------
E("ri_den.clj", '(load "den_gen")',
  ';; RI: the clauses over ri (formal/tools/gen_den.py --ri).\n(load "ri_den_gen")')
E("ri_sem.clj", '(load "sem_gen")',
  ';; RI: the clauses over ri (formal/tools/gen_sem.py --ri).\n(load "ri_sem_gen")')

# --- ri_model: Lemma 3.6 over ri needs the laws and PhCons; H1 inhabitant is std ---
E("ri_model.clj",
  "(kdef Lemma_3_6_ri Prop\n  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code) ri RInt]\n    (=> (CheckSpec chkf dec encTy)",
  ";; RI: over an R-interpretation the lemma assumes its laws (RLaws) and\n"
  ";; Corollary 3.7 for trees with a phantom (PhCons), both trivial at stdRI.\n"
  "(kdef Lemma_3_6_ri Prop\n  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code) ri RInt]\n    (=> (CheckSpec chkf dec encTy) (RLaws ri) (PhCons chkf encTy ri)")

# --- ri_unfold: generated names --------------------------------------------------
E("ri_unfold.clj", '(symbol (str "den_" ctor "_eq"))', '(symbol (str "den_" ctor "_eq_ri"))', 2)
E("ri_unfold.clj", '(symbol (str "den_" ctor "_at"))', '(symbol (str "den_" ctor "_at_ri"))', 1)
E("ri_unfold.clj", "(list (symbol (str \"den_\" ctor)) 'chkf", "(list (symbol (str \"den_\" ctor \"_ri\")) 'chkf", 2)

# --- ri_mono: V(R) reads the printed tree's labels --------------------------------
E("ri_mono.clj", "'(have h1 (And (LE.le (cnodes vv) fk) (Eq Bool (lblOk vv) Bool.true)) hv)",
  "'(have h1 (And (LE.le (cnodes vv) fk) (Eq Bool (lblOk (riPr ri vv)) Bool.true)) hv) ;; RI: V(R) over ri")
E("ri_mono.clj", "(have h2 (And (Nat.le (cnodes v) m) (Eq Bool (lblOk v) Bool.true)) hv)",
  "(have h2 (And (Nat.le (cnodes v) m) (Eq Bool (lblOk (riPr ri v)) Bool.true)) hv) ;; RI: V(R) over ri")

# --- ri_substitution: the clause templates of weakening and substitution ---------
E("ri_substitution.clj", "'(coe Sk.cert Sk.cert (Code.sl q1))",
  "'(coe Sk.cert Sk.cert (Code.sl (riLf ri q1)))", 2)  # RI: leaf
E("ri_substitution.clj", "(fn [l :- Nat] (q1 l))", "(fn [l :- Nat] (q1 (riIt ri l))) ;; RI: itR on a leaf\n     ", 2)
E("ri_substitution.clj", "'(coe Sk.syn Sk.syn q1)", "'(coe Sk.syn Sk.syn (riPr ri q1))", 2)  # RI: print
E("ri_substitution.clj", "       (dec q1))\n     (Bool.and (Nat.ble (cnodes q1) cp) (chkf q1 (encTy D)))))",
  "       (dec (riPr ri q1)))\n     ;; RI: reflect decodes the printed tree, only when it is phantom-free\n"
  "     (Bool.and (Nat.ble (cnodes q1) cp) (Bool.and (riPf ri q1) (chkf (riPr ri q1) (encTy D))))))")
E("ri_substitution.clj",
  "(chkf (%D (lift 1 cu r) (insS cu xs G) Sk.cert (insE cu xs G en vx)) (%D (lift 1 cu c) (insS cu xs G) Sk.syn (insE cu xs G en vx))))",
  "(chkf (riPr ri (%D (lift 1 cu r) (insS cu xs G) Sk.cert (insE cu xs G en vx))) (%D (lift 1 cu c) (insS cu xs G) Sk.syn (insE cu xs G en vx)))) ;; RI: inspect")
E("ri_substitution.clj", "               (chkf q1 q2))\n        (%H r G Sk.cert ih_hr)",
  "               (chkf (riPr ri q1) q2)) ;; RI: inspect\n        (%H r G Sk.cert ih_hr)")
E("ri_substitution.clj", "(chkf (%D (subst sg r) G Sk.cert en) (%D (subst sg c) G Sk.syn en)))",
  "(chkf (riPr ri (%D (subst sg r) G Sk.cert en)) (%D (subst sg c) G Sk.syn en))) ;; RI: inspect")
E("ri_substitution.clj", "               (chkf q1 q2))\n        (%H2 r Sk.cert ih_hr)",
  "               (chkf (riPr ri q1) q2)) ;; RI: inspect\n        (%H2 r Sk.cert ih_hr)")
# the counterexample to Lemma 3.1 without SubOK holds at stdRI
E("ri_substitution.clj", "(fn [e :- Exp] (Code.sl 0)) 0", "(fn [e :- Exp] (Code.sl 0)) stdRI 0", 0)
E("ri_substitution.clj", "(cex31_lhs_ri (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0))",
  "(cex31_lhs_ri (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0)) stdRI")
E("ri_substitution.clj", "(cex31_rhs_ri (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0))",
  "(cex31_rhs_ri (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0)) stdRI")


# Cuts: (file, marker, note) — drop everything from marker to the end of the
# file (a block of standard results the generic chain does not need, which
# drop_forms cannot see because a let wraps it).
CUTS = [("ri_model.clj", "(let [D0 '(List.nil Exp)",
         ";; RI: Theorem 2 (H₁) does not involve the model: the standard Theorem_2_H1 is used.\n")]


# Top-level forms dropped whole: (file, opening text, note).  For declarations
# of model-independent constants that gen_ri.py cannot see, because their
# names are built by a generator (doseq over the constructors) or declared
# through (eval (list 'kdef …)): the generic chain uses the standard ones.
DROPS = []


def DROP(f, opening, note):
    DROPS.append((f, opening, note))


def apply(d, files=None):
    for f, opening, note in DROPS:
        if files and f not in files:
            continue
        p = d + "/" + f
        s = open(p).read()
        hits = [(a, b) for a, b in top_forms(s) if s[a:b].startswith(opening)]
        if len(hits) != 1:
            raise SystemExit("%s: form %r found %d times" % (f, opening[:60], len(hits)))
        a, b = hits[0]
        open(p, "w").write(s[:a] + note.strip() + s[b:])
    for f, marker, note in CUTS:
        if files and f not in files:
            continue
        p = d + "/" + f
        s = open(p).read()
        if s.count(marker) != 1:
            raise SystemExit("%s: cut marker %r found %d times" % (f, marker, s.count(marker)))
        open(p, "w").write(s[:s.index(marker)] + note)
    for f, name, new in FORMS:
        if files and f not in files:
            continue
        p = d + "/" + f
        s = open(p).read()
        hits = [(a, b) for a, b in top_forms(s)
                if (lambda m: m and next(g for g in m.groups() if g) == name)(DECL.match(s[a:b]))]
        if len(hits) != 1:
            raise SystemExit("%s: form %s found %d times" % (f, name, len(hits)))
        a, b = hits[0]
        open(p, "w").write(s[:a] + new.strip() + s[b:])
    for f, old, new, count in EDITS:
        if files and f not in files:
            continue
        p = d + "/" + f
        s = open(p).read()
        k = s.count(old)
        if (count and k != count) or (not count and k == 0):
            raise SystemExit("%s: expected %s of %r, found %d" % (f, count or ">0", old[:80], k))
        s = s.replace(old, new)
        open(p, "w").write(s)



# --- ri_fundamental: Leaf, Node, ItR read the R-interpretation --------------------
F("ri_fundamental.clj", "F_leaf_ri", r"""
;; RI: Leaf.  ⟦leaf x⟧ = sl (riLf ri ⟦x⟧) has no node, and prints as sl ⟦x⟧ (L1),
;; whose label is ⟦x⟧ < NL.
(eval (list 'lcert.formal.base/thm 'F_leaf_ri (into P6 (into '[hrl :- (RLaws ri), x :- Exp] (into ['ihx :- (SND 'us 'x 'Exp.tLbl)] (conj ENV 'hs :- (ES 'us 'k)))))
  (concl 'Exp.tR '(Exp.leaf x))
  '(rw [(den_leaf_at_ri chkf dec encTy ri n x (skels D) (skel Exp.tR) en)])
  (list 'have 'hx (list 'Nat.lt dxl 100) '(ihx en k hk hs))
  '(constructor) '(exact (Nat.zero_le k))
  (list 'exact (list 'Eq.trans (list 'congrArg 'lblOk (list 'rl_leaf 'ri 'hrl dxl)) (list 'Nat.ble_eq_true_of_le 'hx)))))
""")
E("ri_fundamental.clj",
  "(case! 'F_node_ri\n  (into '[us1 :- (List U),",
  ";; RI: Node takes the laws: its tree prints node by node (L2).\n(case! 'F_node_ri\n  (into '[hrl :- (RLaws ri), us1 :- (List U),")
E("ri_fundamental.clj",
  "(Eq Bool (lblOk (den_ri chkf dec encTy ri n r1 (skels D) Sk.cert en)) Bool.true)",
  "(Eq Bool (lblOk (riPr ri (den_ri chkf dec encTy ri n r1 (skels D) Sk.cert en))) Bool.true)")
E("ri_fundamental.clj",
  "(Eq Bool (lblOk (den_ri chkf dec encTy ri n r2 (skels D) Sk.cert en)) Bool.true)",
  "(Eq Bool (lblOk (riPr ri (den_ri chkf dec encTy ri n r2 (skels D) Sk.cert en))) Bool.true)")
E("ri_fundamental.clj",
  """     '(exact (band3 (Nat.blt (den_ri chkf dec encTy ri n x (skels D) Sk.lbl en) 100) (lblOk (den_ri chkf dec encTy ri n r1 (skels D) Sk.cert en))
               (lblOk (den_ri chkf dec encTy ri n r2 (skels D) Sk.cert en)) (Nat.ble_eq_true_of_le vx) (And.right v1) (And.right v2)))]))""",
  """     ;; RI: print the node (L2), then the labels as before
     '(exact (Eq.trans (congrArg lblOk (rl_node ri hrl (den_ri chkf dec encTy ri n x (skels D) Sk.lbl en)
                                         (den_ri chkf dec encTy ri n r1 (skels D) Sk.cert en) (den_ri chkf dec encTy ri n r2 (skels D) Sk.cert en)))
               (band3 (Nat.blt (den_ri chkf dec encTy ri n x (skels D) Sk.lbl en) 100) (lblOk (riPr ri (den_ri chkf dec encTy ri n r1 (skels D) Sk.cert en)))
                 (lblOk (riPr ri (den_ri chkf dec encTy ri n r2 (skels D) Sk.cert en))) (Nat.ble_eq_true_of_le vx) (And.right v1) (And.right v2))))]))""")
# ItR: the leaf case passes riIt ri l; the invariant is lblOk of the printed tree
E("ri_fundamental.clj", "(def ^:private fl (list 'fn '[l :- Nat] (list gden 'l)))",
  ";; RI: itR passes riIt ri l for the leaf sl l\n(def ^:private fl (list 'fn '[l :- Nat] (list gden '(riIt ri l))))")
E("ri_fundamental.clj", ";; (coderec_inv: the standard constant is used)", r""";; RI: coderec_inv over ri.  The invariant is that the printed tree's labels
;; lie in L.  A leaf sl l passes riIt ri l, the label of its print or ℓ₀; a
;; node prints node by node (L2), so its label and both subtrees' prints
;; satisfy the invariant.
(thm coderec_inv_ri [ri :- RInt, hrl :- (RLaws ri), α :- Type, Q :- (=> Nat α Prop), fl :- (=> Nat α), fnd :- (=> Nat Code Code α α α), n :- Nat,
                    hl :- (forall [l Nat] (=> (Eq Bool (lblOk (riPr ri (Code.sl l))) Bool.true) (Q 0 (fl l)))),
                    hn :- (forall [l Nat] (forall [a Code] (forall [b Code] (forall [ya α] (forall [yb α]
                            (=> (LT.lt l 100) (Q (cnodes a) ya) (Q (cnodes b) yb) (LE.le (+ 1 (+ (cnodes a) (cnodes b))) n)
                                (Q (+ 1 (+ (cnodes a) (cnodes b))) (fnd l a b ya yb))))))))]
  (forall [w Code] (=> (Eq Bool (lblOk (riPr ri w)) Bool.true) (LE.le (cnodes w) n) (Q (cnodes w) (Code.rec$1 (fn [_ :- Code] α) fl fnd w))))
  (intro w) (induction w)
  (intro h1 h2) (exact (hl l h1))
  (intro h1 h2)
  (have hok (Eq Bool (Bool.and (Nat.blt l 100) (Bool.and (lblOk (riPr ri a)) (lblOk (riPr ri b)))) Bool.true)
    (Eq.trans (Eq.symm (Eq.trans (congrArg lblOk (rl_node ri hrl l a b)) (lblOk_sn l (riPr ri a) (riPr ri b)))) h1))
  (have hc (LE.le (+ 1 (+ (cnodes a) (cnodes b))) n) (Eq.mp (congrArg (fn [q :- Nat] (LE.le q n)) (cnodes_sn l a b)) h2))
  (have hab (And (LE.le (cnodes a) n) (LE.le (cnodes b) n)) (cnodes_split (cnodes a) (cnodes b) n hc))
  (have hr (Eq Bool (Bool.and (lblOk (riPr ri a)) (lblOk (riPr ri b))) Bool.true)
    (band_right (Nat.blt l 100) (Bool.and (lblOk (riPr ri a)) (lblOk (riPr ri b))) hok))
  (rw [(cnodes_sn l a b)])
  (exact (hn l a b (Code.rec$1 (fn [_ :- Code] α) fl fnd a) (Code.rec$1 (fn [_ :- Code] α) fl fnd b)
             (lbl_lt l (band_left (Nat.blt l 100) (Bool.and (lblOk (riPr ri a)) (lblOk (riPr ri b))) hok))
             (ih_a (band_left (lblOk (riPr ri a)) (lblOk (riPr ri b)) hr) (And.left hab))
             (ih_b (band_right (lblOk (riPr ri a)) (lblOk (riPr ri b)) hr) (And.right hab))
             hc)))""")
E("ri_fundamental.clj",
  "(eval (concat (list 'lcert.formal.base/thm 'F_itR_ri\n  (into P6 (into '[us1 :- (List U),",
  ";; RI: ItR takes the laws (coderec_inv_ri).\n(eval (concat (list 'lcert.formal.base/thm 'F_itR_ri\n  (into P6 (into '[hrl :- (RLaws ri), us1 :- (List U),")
E("ri_fundamental.clj",
  """     (list 'have 'HL (list 'forall '[l Nat] (list '=> '(LT.lt l 100) (VX 0 (list gden 'l))))
        '(fn [l :- Nat, hl :- (LT.lt l 100)]
           (Iff.mp (V_lift_fam_ri chkf dec encTy ri n (skels D) X hXS 0 Sk.lbl en l 0 (fn [s :- Sk] ((den_ri chkf dec encTy ri n g (skels D) (Sk.arr Sk.lbl s) en) l)))
                   (hgF l hl))))""",
  """     ;; RI: a leaf whose print's labels lie in L passes a label in L (riIt_lt)
     (list 'have 'HL (list 'forall '[l Nat] (list '=> '(Eq Bool (lblOk (riPr ri (Code.sl l))) Bool.true) (VX 0 (list gden '(riIt ri l)))))
        '(fn [l :- Nat, hl :- (Eq Bool (lblOk (riPr ri (Code.sl l))) Bool.true)]
           (Iff.mp (V_lift_fam_ri chkf dec encTy ri n (skels D) X hXS 0 Sk.lbl en (riIt ri l) 0 (fn [s :- Sk] ((den_ri chkf dec encTy ri n g (skels D) (Sk.arr Sk.lbl s) en) (riIt ri l))))
                   (hgF (riIt ri l) (riIt_lt ri l hl)))))""")
E("ri_fundamental.clj",
  """     (list 'have 'hinv (list 'forall '[w Code] (list '=> '(Eq Bool (lblOk w) Bool.true) '(LE.le (cnodes w) n)
                          (VX '(cnodes w) (list 'Code.rec$1 '(fn [_ :- Code] (Car (skel X))) fl fnd 'w))))
        (list 'coderec_inv '(Car (skel X)) Qf fl fnd 'n 'HL 'HN))
     (list 'have 'vr (list 'And (list 'Nat.le (list 'cnodes rv) 'k3) (list 'Eq 'Bool (list 'lblOk rv) 'Bool.true))""",
  """     (list 'have 'hinv (list 'forall '[w Code] (list '=> '(Eq Bool (lblOk (riPr ri w)) Bool.true) '(LE.le (cnodes w) n)
                          (VX '(cnodes w) (list 'Code.rec$1 '(fn [_ :- Code] (Car (skel X))) fl fnd 'w))))
        (list 'coderec_inv_ri 'ri 'hrl '(Car (skel X)) Qf fl fnd 'n 'HL 'HN))
     (list 'have 'vr (list 'And (list 'Nat.le (list 'cnodes rv) 'k3) (list 'Eq 'Bool (list 'lblOk (list 'riPr 'ri rv)) 'Bool.true))""")

# --- ri_outer: print, inspect, reflect and H₁ read the R-interpretation -----------
E("ri_outer.clj",
  "  (Eq Code (den_ri chkf dec encTy ri n (Exp.prn r) G Sk.syn en) (den_ri chkf dec encTy ri n r G Sk.cert en))",
  "  ;; RI: ⟦print r⟧ is riPr ri ⟦r⟧\n  (Eq Code (den_ri chkf dec encTy ri n (Exp.prn r) G Sk.syn en) (riPr ri (den_ri chkf dec encTy ri n r G Sk.cert en)))")
E("ri_outer.clj",
  "  (Eq Bool (chkf (den_ri chkf dec encTy ri n r G Sk.cert en) (den_ri chkf dec encTy ri n cd G Sk.syn en)) Bool.true)",
  "  ;; RI: the check is on the print of ⟦r⟧\n  (Eq Bool (chkf (riPr ri (den_ri chkf dec encTy ri n r G Sk.cert en)) (den_ri chkf dec encTy ri n cd G Sk.syn en)) Bool.true)")
F("ri_outer.clj", "den_refl_some_ri", r"""
;; RI: reflect decodes the print of ⟦r⟧ when ⟦r⟧ fits the cap, is phantom-free
;; and its print is accepted; den_refl_none_ri: a tree with a phantom gives the
;; default.
(eval (list 'lcert.formal.base/thm 'den_refl_some_ri
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, n :- Nat,
    X :- Exp, r :- Exp, e :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G), m :- Nat, t :- Exp, A :- Exp,
    hc :- (Eq Bool (Bool.and (Nat.ble (cnodes (den_ri chkf dec encTy ri n r G Sk.cert en)) n)
                             (Bool.and (riPf ri (den_ri chkf dec encTy ri n r G Sk.cert en)) (chkf (riPr ri (den_ri chkf dec encTy ri n r G Sk.cert en)) (encTy X)))) Bool.true),
    hd :- (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (riPr ri (den_ri chkf dec encTy ri n r G Sk.cert en))) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))]
  '(Eq (Car sk) (den_ri chkf dec encTy ri n (Exp.refl X r e) G sk en)
       (coe (skel X) sk (denPrev_ri chkf dec encTy ri n m t (thetaSk m) (skel X) (tokenEnv m))))
  '(rw [(den_refl_at_ri chkf dec encTy ri n X r e G sk en)])
  (list 'change (list 'Eq '(Car sk)
     (list 'Bool.rec$1 '(fn [_ :- Bool] (Car sk)) '(dflt sk)
       (list 'Option.rec$1$0 '(Prod Nat (Prod Exp Exp)) '(fn [_ :- (Option (Prod Nat (Prod Exp Exp)))] (Car sk)) '(dflt sk)
             '(fn [tr :- (Prod Nat (Prod Exp Exp))] (coe (skel X) sk (denPrev_ri chkf dec encTy ri n (Prod.fst tr) (Prod.fst (Prod.snd tr)) (thetaSk (Prod.fst tr)) (skel X) (tokenEnv (Prod.fst tr)))))
             (list 'dec (list 'riPr 'ri rv)))
       (list 'Bool.and (list 'Nat.ble (list 'cnodes rv) 'n) (list 'Bool.and (list 'riPf 'ri rv) (list 'chkf (list 'riPr 'ri rv) '(encTy X)))))
     '(coe (skel X) sk (denPrev_ri chkf dec encTy ri n m t (thetaSk m) (skel X) (tokenEnv m)))))
  '(rw [hc hd])))
(eval (list 'lcert.formal.base/thm 'den_refl_none_ri
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, n :- Nat,
    X :- Exp, r :- Exp, e :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
    hf :- (Eq Bool (riPf ri (den_ri chkf dec encTy ri n r G Sk.cert en)) Bool.false)]
  '(Eq (Car sk) (den_ri chkf dec encTy ri n (Exp.refl X r e) G sk en) (dflt sk))
  '(rw [(den_refl_at_ri chkf dec encTy ri n X r e G sk en)])
  (list 'change (list 'Eq '(Car sk)
     (list 'Bool.rec$1 '(fn [_ :- Bool] (Car sk)) '(dflt sk)
       (list 'Option.rec$1$0 '(Prod Nat (Prod Exp Exp)) '(fn [_ :- (Option (Prod Nat (Prod Exp Exp)))] (Car sk)) '(dflt sk)
             '(fn [tr :- (Prod Nat (Prod Exp Exp))] (coe (skel X) sk (denPrev_ri chkf dec encTy ri n (Prod.fst tr) (Prod.fst (Prod.snd tr)) (thetaSk (Prod.fst tr)) (skel X) (tokenEnv (Prod.fst tr)))))
             (list 'dec (list 'riPr 'ri rv)))
       (list 'Bool.and (list 'Nat.ble (list 'cnodes rv) 'n) (list 'Bool.and (list 'riPf 'ri rv) (list 'chkf (list 'riPr 'ri rv) '(encTy X)))))
     '(dflt sk)))
  '(rw [hf])
  (list 'rw [(list 'Bool.false_and (list 'chkf (list 'riPr 'ri rv) '(encTy X)))])
  (list 'rw [(list 'Bool.and_false (list 'Nat.ble (list 'cnodes rv) 'n))])))
""")
# Reflect: CheckSpec decodes the print
E("ri_outer.clj",
  "(def ^:private v1 '(den_ri chkf dec encTy ri n r (skels D) Sk.cert en))",
  "(def ^:private v1 '(den_ri chkf dec encTy ri n r (skels D) Sk.cert en))\n;; RI: the print of ⟦r⟧, which the checker reads\n(def ^:private pv1 (list 'riPr 'ri v1))")
E("ri_outer.clj", "(list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec v1)", "(list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec pv1)")
E("ri_outer.clj", "(list 'Nat.lt mm (list 'cnodes v1)))))))))", "(list 'Nat.lt mm (list 'cnodes pv1)))))))))")
F("ri_outer.clj", "F_refl_ri", r"""
;; RI: Reflect over ri.  e's IH forces chk′ (print ⟦r⟧) ⌜X⌝, i.e. the checker
;; accepts riPr ri ⟦r⟧ at encTy X.  If ⟦r⟧ is phantom-free, its print has its
;; node count (L3), and the standard argument runs on the print: CheckSpec
;; decodes it at a budget m < ‖⟦r⟧‖ ≤ k₁, the outer hypothesis types the
;; program at m, and Lemma 3.4 lifts it to n, k.  If ⟦r⟧ has a phantom,
;; ⟦reflect⟧ is the default (den_refl_none_ri): in V(X) for a base type X ≠ 0
;; (V_dflt_ri), and X = 0 is excluded by PhCons (an accepted refutation).
(a/prove-theorem 'V_dflt_ri
  (lv '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, hrl :- (RLaws ri),
        n :- Nat, G :- (List Sk), en :- (HEnv G), k :- Nat, X :- Exp, cd :- Exp])
  (lv '(=> (Eq (Option Exp) (baseCode X) (Option.some Exp cd)) (=> (Eq Exp X Exp.tEmpty) False)
           (V_ri chkf dec encTy ri n X G en k (skel X) (dflt (skel X)))))
  (lv (into ['(cases X)]
        (mapcat (fn [[ctor _]]
                  (into '[(intro hb hne)]
                        (case ctor
                          tEmpty '[(exact (False.elim (hne (Eq.refl$1 Exp.tEmpty))))]
                          (tUnit tBool tNat) '[(exact True.intro)]
                          tLbl '[(exact (Nat.zero_lt_succ 99))]
                          tSyn '[(exact (Eq.refl$1 Bool.true))]
                          tR '[(constructor) (exact (Nat.zero_le k)) (exact (rl_dflt ri hrl))]
                          '[(exact (False.elim$0 (none_ne_someE cd hb)))])))
                exp-fields))))
(thm bool_and3_true [a :- Bool, b :- Bool, c :- Bool, ha :- (Eq Bool a Bool.true), hb :- (Eq Bool b Bool.true), hc :- (Eq Bool c Bool.true)]
  (Eq Bool (Bool.and a (Bool.and b c)) Bool.true) (rw [ha hb hc]))
(thm lt_of_lt_eq [m :- Nat, a :- Nat, b :- Nat, h :- (LT.lt m a), e :- (Eq Nat a b)] (LT.lt m b) (rw [(Eq.symm e)]) (exact h))
;; The phantom-free branch of Reflect: the standard argument, on the print.
(eval (concat (list 'lcert.formal.base/thm 'refl_pf_ri
  (into P6 (into '[X :- Exp, cd :- Exp, r :- Exp, e :- Exp,
                   hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
                   hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH_ri chkf dec encTy ri n), hrl :- (RLaws ri)]
                 (into ENV ['k1 :- 'Nat 'k2 :- 'Nat 'hkk :- '(LE.le (+ k1 k2) k)
                            'vr :- (list 'LE.le (list 'cnodes v1) 'k1)
                            'hchk :- (list 'Eq 'Bool (list 'chkf pv1 '(encTy X)) 'Bool.true)
                            'hpf :- (list 'Eq 'Bool (list 'riPf 'ri v1) 'Bool.true)])))
  (concl 'X '(Exp.refl X r e)))
  ['(have hbase (And (Eq Bool (isBaseTy X) Bool.true) (Eq Bool (closedTy X) Bool.true)) (base_is_base X cd hb))
   (list 'have 'hsz (list 'Eq 'Nat (list 'cnodes pv1) (list 'cnodes v1)) (list 'rl_nodes 'ri 'hrl v1 'hpf))
   (list 'have 'hC1 (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (C1body 'mm 'tt 'AA)))))))
      (list '(And.left hcs) pv1 '(encTy X) 'hchk))
   '(refine' (exT Nat _ _ hC1 _)) '(intro mm hm) '(refine' (exT Exp _ _ hm _)) '(intro tt htt) '(refine' (exT Exp _ _ htt _)) '(intro AA hAA)
   (list 'have 'q (C1body 'mm 'tt 'AA) 'hAA)
   '(have eA (Eq Exp AA X) ((And.right (And.right (And.right hcs))) AA X (And.left (And.right (And.right (And.right q)))) (And.right hbase)
                            (And.left (And.right (And.right (And.right (And.right q)))))))
   '(have hRtX (Rt chkf (thetaD mm) (thetaU mm) tt X) (Eq.mp (congrArg (fn [Z :- Exp] (Rt chkf (thetaD mm) (thetaU mm) tt Z)) eA) (And.left (And.right q))))
   (list 'have 'hmv (list 'LT.lt 'mm (list 'cnodes v1))
      (list 'lt_of_lt_eq 'mm (list 'cnodes pv1) (list 'cnodes v1) '(And.right (And.right (And.right (And.right (And.right q))))) 'hsz))
   (list 'have 'har (list 'And '(LE.le mm k) (list 'And '(LT.lt mm n) (list 'LE.le (list 'cnodes v1) 'n)))
      (list 'refl_arith 'mm (list 'cnodes v1) 'k1 'k2 'k 'n 'hmv 'vr 'hkk 'hk))
   (list 'have 'hc (list 'Eq 'Bool (list 'Bool.and (list 'Nat.ble (list 'cnodes v1) 'n) (list 'Bool.and (list 'riPf 'ri v1) (list 'chkf pv1 '(encTy X)))) 'Bool.true)
      (list 'bool_and3_true (list 'Nat.ble (list 'cnodes v1) 'n) (list 'riPf 'ri v1) (list 'chkf pv1 '(encTy X))
            (list 'Nat.ble_eq_true_of_le '(And.right (And.right har))) 'hpf 'hchk))
   '(rw [(den_refl_some_ri chkf dec encTy ri n X r e (skels D) (skel X) en mm tt AA hc (And.left q))])
   '(rw [(coe_self (skel X) (denPrev_ri chkf dec encTy ri n mm tt (thetaSk mm) (skel X) (tokenEnv mm)))])
   '(rw [(congrArg (fn [f :- DenBody] (f (thetaSk mm) (skel X) (tokenEnv mm))) (denPrev_stable_ri chkf dec encTy ri n mm tt (And.left (And.right har))))])
   '(rw [(tok_transfer mm (den_ri chkf dec encTy ri mm tt) (skel X))])
   (list 'have 'vo (list 'V_ri 'chkf 'dec 'encTy 'ri 'mm 'X '(skels (thetaD mm)) '(tokEnvD mm) 'mm '(skel X) run)
      '(hout mm (And.left (And.right har)) tt X hRtX))
   (list 'exact (list 'Lemma_3_4_ri 'chkf 'dec 'encTy 'ri 'X 'mm 'n 'k '(skels (thetaD mm)) '(skels D) '(tokEnvD mm) 'en '(skel X) run
                      '(And.left hbase) '(And.left har) 'vo))]))
;; The branch with a phantom: ⟦reflect⟧ is the default, in V(X) for X ≠ 0;
;; X = 0 would make the print of ⟦r⟧ an accepted refutation, against PhCons.
(eval (list 'lcert.formal.base/thm 'refl_ph_ri
  (into P6 (into '[X :- Exp, cd :- Exp, r :- Exp, e :- Exp,
                   hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
                   hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri)]
                 (into ENV ['hchk :- (list 'Eq 'Bool (list 'chkf pv1 '(encTy X)) 'Bool.true)
                            'hpf :- (list 'Eq 'Bool (list 'riPf 'ri v1) 'Bool.false)])))
  (concl 'X '(Exp.refl X r e))
  '(rw [(den_refl_none_ri chkf dec encTy ri n X r e (skels D) (skel X) en hpf)])
  (list 'exact (list 'V_dflt_ri 'chkf 'dec 'encTy 'ri 'hrl 'n '(skels D) 'en 'k 'X 'cd 'hb
                     (list 'fn '[hX :- (Eq Exp X Exp.tEmpty)]
                           (list '(And.left hph) v1 'hpf
                                 (list 'Eq.mp (list 'congrArg (list 'fn '[Z :- Exp] (list 'Eq 'Bool (list 'chkf pv1 '(encTy Z)) 'Bool.true)) 'hX) 'hchk)))))))
(eval (concat (list 'lcert.formal.base/thm 'F_refl_ri
  (into P6 (into '[us1 :- (List U), us2 :- (List U), X :- Exp, cd :- Exp, r :- Exp, e :- Exp,
                   hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
                   hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH_ri chkf dec encTy ri n),
                   hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri)]
                 (into ['ihr :- (SND 'us1 'r 'Exp.tR) 'ihe :- (SND 'us2 'e '(chkT r cd))]
                       (conj ENV 'hs :- (ES '(vadd us1 us2) 'k)))))
  (concl 'X '(Exp.refl X r e)))
  (concat
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'p)
    ['(have hkn (Nat.le (+ k1 k2) n) (Nat.le_trans (And.left p) hk))
     '(have hk1 (Nat.le k1 n) (Nat.le_trans (Nat.le_add_right k1 k2) hkn))
     '(have hk2 (Nat.le k2 n) (Nat.le_trans (Nat.le_add_left k2 k1) hkn))
     (list 'have 'vr (list 'LE.le (list 'cnodes v1) 'k1) '(And.left (ihr en k1 hk1 (And.left (And.right p)))))
     '(have ve (V_ri chkf dec encTy ri n (chkT r cd) (skels D) en k2 Sk.unit (den_ri chkf dec encTy ri n e (skels D) Sk.unit en))
        (ihe en k2 hk2 (And.right (And.right p))))
     (list 'have 'hchk0 (list 'Eq 'Bool (list 'chkf pv1 '(den_ri chkf dec encTy ri n cd (skels D) Sk.syn en)) 'Bool.true)
        '(chk_true_ri chkf dec encTy ri n (skels D) en k2 r cd (den_ri chkf dec encTy ri n e (skels D) Sk.unit en) ve))
     '(have hcdv (Eq Code (den_ri chkf dec encTy ri n cd (skels D) Sk.syn en) (encTy X))
        (base_den_ri chkf dec encTy ri n (And.left (And.right hcs)) X cd hb (skels D) en))
     (list 'have 'hchk (list 'Eq 'Bool (list 'chkf pv1 '(encTy X)) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf pv1 'q) 'Bool.true)) 'hcdv) 'hchk0))
     (list 'exact (list 'bool_case (list 'fn '[b :- Bool] (concl 'X '(Exp.refl X r e))) (list 'riPf 'ri v1)
                        (list 'fn ['hpf :- (list 'Eq 'Bool (list 'riPf 'ri v1) 'Bool.true)]
                              '(refl_pf_ri chkf dec encTy ri n D X cd r e hb hcs hout hrl en k hk k1 k2 (And.left p) vr hchk hpf))
                        (list 'fn ['hpf :- (list 'Eq 'Bool (list 'riPf 'ri v1) 'Bool.false)]
                              '(refl_ph_ri chkf dec encTy ri n D X cd r e hb hrl hph en k hk hchk hpf))))])))
""")

F("ri_outer.clj", "F_h1_ri", r"""
;; RI: H₁ over ri.  The IHs force the check on the prints of ⟦r⟧ and ⟦s⟧, at
;; ⌜c⌝ and neg ⌜c⌝.  If both trees are phantom-free, their prints have their
;; node counts (L3), and h1_core_ri is §3.5's argument: decode both, compose
;; by Lemma 2.4 at m₁ + m₂ < k₁ + k₂ ≤ n, and the outer hypothesis puts a
;; value in V(0) = ∅.  If one has a phantom, PhCons excludes the pair.
(thm h1_split_arith [k1 :- Nat, m1 :- Nat, k2 :- Nat, m2 :- Nat, k :- Nat, n :- Nat,
                     o1 :- (LE.le (+ k1 m1) k), o2 :- (LE.le (+ k2 m2) m1), hk :- (LE.le k n)]
  (LE.le (+ k1 k2) n) (omega))
(thm h1_lt_arith [ma :- Nat, c1 :- Nat, k1 :- Nat, mb :- Nat, c2 :- Nat, k2 :- Nat, n :- Nat,
                  a1 :- (LT.lt ma c1), a2 :- (LE.le c1 k1), b1 :- (LT.lt mb c2), b2 :- (LE.le c2 k2), hb :- (LE.le (+ k1 k2) n)]
  (LT.lt (+ ma mb) n) (omega))
(thm band_false_or [a :- Bool, b :- Bool] (=> (Eq Bool (Bool.and a b) Bool.false) (Or (Eq Bool a Bool.false) (Eq Bool b Bool.false)))
  (cases a)
  (intro h) (exact (Or.inl (Eq.refl$1 Bool.false)))
  (intro h) (exact (Or.inr h)))
(eval (list 'lcert.formal.base/thm 'h1_core_ri
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, n :- Nat,
    hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH_ri chkf dec encTy ri n),
    c1 :- Code, c2 :- Code, dc :- Code, k1 :- Nat, k2 :- Nat,
    hc1 :- (Eq Bool (chkf c1 dc) Bool.true), hc2 :- (Eq Bool (chkf c2 (Code.sn 25 dc (Code.sl 15))) Bool.true),
    b1 :- (LE.le (cnodes c1) k1), b2 :- (LE.le (cnodes c2) k2), hb :- (LE.le (+ k1 k2) n)]
  'False
  (list 'have 'X1 (Cex 'c1 'dc) '((And.left hcs) c1 dc hc1))
  '(refine' (exT Nat _ _ X1 _)) '(intro ma hma) '(refine' (exT Exp _ _ hma _)) '(intro ta hta) '(refine' (exT Exp _ _ hta _)) '(intro Aa hAa)
  (list 'have 'qa (Cb 'c1 'dc 'ma 'ta 'Aa) 'hAa)
  (list 'have 'X2 (Cex 'c2 '(Code.sn 25 dc (Code.sl 15))) '((And.left hcs) c2 (Code.sn 25 dc (Code.sl 15)) hc2))
  '(refine' (exT Nat _ _ X2 _)) '(intro mb hmb) '(refine' (exT Exp _ _ hmb _)) '(intro tb htb) '(refine' (exT Exp _ _ htb _)) '(intro Ab hAb)
  (list 'have 'qb (Cb 'c2 '(Code.sn 25 dc (Code.sl 15)) 'mb 'tb 'Ab) 'hAb)
  (list 'have 'hcl (list 'Eq 'Bool '(closedTy (Exp.tPi U.u1 Aa Exp.tEmpty)) 'Bool.true) (list 'andb_intro '(closedTy Aa) 'true (gets 'qa 3) '(Eq.refl$1 Bool.true)))
  (list 'have 'henc (list 'Eq 'Code '(encTy (Exp.tPi U.u1 Aa Exp.tEmpty)) '(encTy Ab))
     (list 'Eq.trans (list '(And.left (And.right (And.right hcs))) 'Aa (gets 'qa 3))
           (list 'Eq.trans (list 'congrArg '(fn [q :- Code] (Code.sn 25 q (Code.sl 15))) (gets 'qa 4)) (list 'Eq.symm (gets 'qb 4)))))
  (list 'have 'eB '(Eq Exp (Exp.tPi U.u1 Aa Exp.tEmpty) Ab) (list '(And.right (And.right (And.right hcs))) '(Exp.tPi U.u1 Aa Exp.tEmpty) 'Ab 'hcl (gets 'qb 3) 'henc))
  (list 'have 'hRtb '(Rt chkf (thetaD mb) (thetaU mb) tb (Exp.tPi U.u1 Aa Exp.tEmpty))
     (list 'Eq.mpr (list 'congrArg '(fn [Z :- Exp] (Rt chkf (thetaD mb) (thetaU mb) tb Z)) 'eB) (gets 'qb 1)))
  (list 'have 'hcomp '(Rt chkf (thetaD (+ ma mb)) (thetaU (+ ma mb)) (Exp.app (lift ma 0 tb) (lift mb ma ta)) Exp.tEmpty)
     (list 'lemma24 'chkf 'ma 'mb 'ta 'tb 'Aa (gets 'qa 3) (gets 'qa 2) (gets 'qa 1) 'hRtb))
  (list 'have 'hlt '(LT.lt (+ ma mb) n) (list 'h1_lt_arith 'ma '(cnodes c1) 'k1 'mb '(cnodes c2) 'k2 'n (gets 'qa 5) 'b1 (gets 'qb 5) 'b2 'hb))
  '(exact (hout (+ ma mb) hlt (Exp.app (lift ma 0 tb) (lift mb ma ta)) Exp.tEmpty hcomp))))
(def ^:private pvv (list 'riPr 'ri vv))
(def ^:private pww (list 'riPr 'ri ww))
(eval (concat (list 'lcert.formal.base/thm 'F_h1_ri
  (into P6 (into '[us1 :- (List U), us2 :- (List U), us3 :- (List U), us4 :- (List U), us5 :- (List U),
                   r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
                   hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH_ri chkf dec encTy ri n),
                   hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri)]
                 (into ['ihr :- (SND 'us1 'r 'Exp.tR) 'ihs :- (SND 'us2 's 'Exp.tR)
                        'ih1 :- (SND 'us4 'e1 '(chkT r c)) 'ih2 :- (SND 'us5 'e2 '(chkT s (negT c)))]
                       (conj ENV 'hs :- (ES '(vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5)))) 'k)))))
  (concl 'Exp.tEmpty '(Exp.h1 r s c e1 e2)))
  (concat
    (split-steps 'hs 'us1 '(vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5))) 'k 'k1 'm1 'p1)
    (split-steps '(And.right (And.right p1)) 'us2 '(vadd (vscale U.uw us3) (vadd us4 us5)) 'm1 'k2 'm2 'p2)
    (split-steps '(And.right (And.right p2)) '(vscale U.uw us3) '(vadd us4 us5) 'm2 'k3 'm3 'p3)
    (split-steps '(And.right (And.right p3)) 'us4 'us5 'm3 'k4 'k5 'p4)
    ['(have o1 (LE.le (+ k1 m1) k) (And.left p1)) '(have o2 (LE.le (+ k2 m2) m1) (And.left p2))
     '(have o3 (LE.le (+ k3 m3) m2) (And.left p3)) '(have o4 (LE.le (+ k4 k5) m3) (And.left p4))
     '(have hk1 (Nat.le k1 n) (le_chain1 k1 m1 k n o1 hk))
     '(have hk2 (Nat.le k2 n) (le_chain2 k1 m1 k2 m2 k n o1 o2 hk))
     '(have hk4 (Nat.le k4 n) (le_chain4 k1 m1 k2 m2 k3 m3 k4 k5 k n o1 o2 o3 o4 hk))
     '(have hk5 (Nat.le k5 n) (le_chain5 k1 m1 k2 m2 k3 m3 k4 k5 k n o1 o2 o3 o4 hk))
     (list 'have 'vr (list 'LE.le (list 'cnodes vv) 'k1) '(And.left (ihr en k1 hk1 (And.left (And.right p1)))))
     (list 'have 'vs (list 'LE.le (list 'cnodes ww) 'k2) '(And.left (ihs en k2 hk2 (And.left (And.right p2)))))
     (list 'have 'hc1 (list 'Eq 'Bool (list 'chkf pvv dc) 'Bool.true)
        '(chk_true_ri chkf dec encTy ri n (skels D) en k4 r c (den_ri chkf dec encTy ri n e1 (skels D) Sk.unit en) (ih1 en k4 hk4 (And.left (And.right p4)))))
     (list 'have 'hc2a (list 'Eq 'Bool (list 'chkf pww '(den_ri chkf dec encTy ri n (negT c) (skels D) Sk.syn en)) 'Bool.true)
        '(chk_true_ri chkf dec encTy ri n (skels D) en k5 s (negT c) (den_ri chkf dec encTy ri n e2 (skels D) Sk.unit en) (ih2 en k5 hk5 (And.right (And.right p4)))))
     (list 'have 'hc2 (list 'Eq 'Bool (list 'chkf pww ndc) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf pww 'q) 'Bool.true)) '(den_negT_ri chkf dec encTy ri n c (skels D) en)) 'hc2a))
     (list 'refine' (list 'bool_case (list 'fn '[b :- Bool] (concl 'Exp.tEmpty '(Exp.h1 r s c e1 e2)))
                          (list 'Bool.and (list 'riPf 'ri vv) (list 'riPf 'ri ww)) '_ '_))
     ;; both phantom-free: the budget descends (§3.5)
     '(intro hpf)
     (list 'have 'hv (list 'Eq 'Bool (list 'riPf 'ri vv) 'Bool.true) (list 'band_left (list 'riPf 'ri vv) (list 'riPf 'ri ww) 'hpf))
     (list 'have 'hw (list 'Eq 'Bool (list 'riPf 'ri ww) 'Bool.true) (list 'band_right (list 'riPf 'ri vv) (list 'riPf 'ri ww) 'hpf))
     (list 'have 'bv (list 'LE.le (list 'cnodes pvv) 'k1)
        (list 'Eq.mpr (list 'congrArg '(fn [z :- Nat] (LE.le z k1)) (list 'rl_nodes 'ri 'hrl vv 'hv)) 'vr))
     (list 'have 'bw (list 'LE.le (list 'cnodes pww) 'k2)
        (list 'Eq.mpr (list 'congrArg '(fn [z :- Nat] (LE.le z k2)) (list 'rl_nodes 'ri 'hrl ww 'hw)) 'vs))
     (list 'exact (list 'False.elim (list 'h1_core_ri 'chkf 'dec 'encTy 'ri 'n 'hcs 'hout pvv pww dc 'k1 'k2 'hc1 'hc2 'bv 'bw
                                         '(h1_split_arith k1 m1 k2 m2 k n o1 o2 hk))))
     ;; a phantom: PhCons
     '(intro hpf)
     (list 'exact (list 'False.elim (list '(And.right hph) vv ww dc (list 'band_false_or (list 'riPf 'ri vv) (list 'riPf 'ri ww) 'hpf) 'hc1 'hc2)))])))
""")
# Inspect: the check is on the print of ⟦r⟧
E("ri_outer.clj", "           (chkf v (den_ri chkf dec encTy ri n c G Sk.syn en)))",
  "           (chkf (riPr ri v) (den_ri chkf dec encTy ri n c G Sk.syn en))) ;; RI: inspect checks the print")
E("ri_outer.clj", "hb :- (Eq Bool (chkf v (den_ri chkf dec encTy ri n c G Sk.syn en)) Bool.false)]",
  "hb :- (Eq Bool (chkf (riPr ri v) (den_ri chkf dec encTy ri n c G Sk.syn en)) Bool.false)] ;; RI")
E("ri_outer.clj", "        (chkf (den_ri chkf dec encTy ri n r G Sk.cert en) (den_ri chkf dec encTy ri n c G Sk.syn en))))",
  "        (chkf (riPr ri (den_ri chkf dec encTy ri n r G Sk.cert en)) (den_ri chkf dec encTy ri n c G Sk.syn en)))) ;; RI")
E("ri_outer.clj", "(list 'Eq 'Bool (list 'lblOk rvv) 'Bool.true)", "(list 'Eq 'Bool (list 'lblOk (list 'riPr 'ri rvv)) 'Bool.true)")
E("ri_outer.clj", "(list 'chkf rvv dcv) '_ '_))", "(list 'chkf (list 'riPr 'ri rvv) dcv) '_ '_)) ;; RI: inspect checks the print")


# --- ri_lemma36: the induction threads the laws and PhCons to the cases that read ri ---
CUTS.append(("ri_lemma36.clj", ";; --- the main results",
              ";; RI: Theorems 1 and 3 and Corollary 3.7 are not needed over ri: the generic\n"
              ";; chain stops at Lemma 3.6 (lemma36_ri).  Theorem 4.6 (theorem46.clj) uses\n"
              ";; lemma36_ri at the phantom interpretation, and the standard Corollary 3.7\n"
              ";; for PhCons there (lemma46a.clj).\n"))
E("ri_lemma36.clj", '(str "(F_leaf_ri " C " D us x (ih_h hw))")',
  '(str "(F_leaf_ri " C " D us hrl x (ih_h hw))") ;; RI: the cases reading ri take the laws (hrl)')
E("ri_lemma36.clj", '(str "(F_node_ri " C " D us1 us1 us2 us3 us4 d x',
  '(str "(F_node_ri " C " D us1 hrl us1 us2 us3 us4 d x')
E("ri_lemma36.clj", '(str "(F_itR_ri " C " D us1 us1 us2 us3 X g h r hX',
  '(str "(F_itR_ri " C " D us1 hrl us1 us2 us3 X g h r hX')
E("ri_lemma36.clj", '(str "(F_h1_ri " C " D us1 us2 us3 us4 us5 r s c e1 e2 hcs hout (ih_hr hw)',
  '(str "(F_h1_ri " C " D us1 us2 us3 us4 us5 r s c e1 e2 hcs hout hrl hph (ih_hr hw)')
E("ri_lemma36.clj", '(str "(F_refl_ri " C " D us1 us2 X cd r e hb hcs hout (ih_hr hw)',
  '(str "(F_refl_ri " C " D us1 us2 X cd r e hb hcs hout hrl hph (ih_hr hw)')
E("ri_lemma36.clj", "        hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH_ri chkf dec encTy ri cap),\n",
  "        hcs :- (CheckSpec chkf dec encTy),\n"
  "        ;; RI: the laws of ri, and Corollary 3.7 for trees with a phantom\n"
  "        hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri),\n"
  "        hout :- (OuterIH_ri chkf dec encTy ri cap),\n")
E("ri_lemma36.clj", "(def ^:private HYP '[hcs :- (CheckSpec chkf dec encTy), hconv :-",
  ";; RI: and the laws of ri, and PhCons.\n"
  "(def ^:private HYP '[hcs :- (CheckSpec chkf dec encTy), hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri), hconv :-")
E("ri_lemma36.clj", "(lemma36_step_ri chkf dec encTy ri n hcs ", "(lemma36_step_ri chkf dec encTy ri n hcs hrl hph ", 2)
E("ri_lemma36.clj", "(outer_succ_ri chkf dec encTy ri hcs hconv n ih_n)", "(outer_succ_ri chkf dec encTy ri hcs hrl hph hconv n ih_n)")
E("ri_lemma36.clj", "(outer_all_ri chkf dec encTy ri hcs hconv n)", "(outer_all_ri chkf dec encTy ri hcs hrl hph hconv n)")


# --- ri_conversion: the ι-steps of print and itR on a leaf read the laws of ri ----
# (ri carries them: ri_leaf, ri_node, ri_itleaf, rint.clj).
X_ = "(den_ri chkf dec encTy ri n x G Sk.lbl en)"
E("ri_conversion.clj",
  """              (coe Sk.syn s (Code.sl (den_ri chkf dec encTy ri n x G Sk.lbl en)))
              (coe Sk.syn s (den_ri chkf dec encTy ri n (Exp.leaf x) G Sk.cert en))))
  (rw [(den_leaf_at_ri chkf dec encTy ri n x G Sk.cert en)]))""",
  """              (coe Sk.syn s (Code.sl (den_ri chkf dec encTy ri n x G Sk.lbl en)))
              (coe Sk.syn s (riPr ri (den_ri chkf dec encTy ri n (Exp.leaf x) G Sk.cert en)))))
  (rw [(den_leaf_at_ri chkf dec encTy ri n x G Sk.cert en)])
  ;; RI: print (leaf x) is sl x by L1
  (exact (congrArg (fn [c :- Code] (coe Sk.syn s c)) (Eq.symm (ri_leaf ri """ + X_ + """)))))""")
E("ri_conversion.clj",
  """              (coe Sk.syn s (den_ri chkf dec encTy ri n (Exp.node d x r1 r2) G Sk.cert en))))
  (rw [(den_node_at_ri chkf dec encTy ri n d x r1 r2 G Sk.cert en)])
  (rw [(den_prn_at_ri chkf dec encTy ri n r1 G Sk.syn en)])
  (rw [(den_prn_at_ri chkf dec encTy ri n r2 G Sk.syn en)]))""",
  """              (coe Sk.syn s (riPr ri (den_ri chkf dec encTy ri n (Exp.node d x r1 r2) G Sk.cert en)))))
  (rw [(den_node_at_ri chkf dec encTy ri n d x r1 r2 G Sk.cert en)])
  (rw [(den_prn_at_ri chkf dec encTy ri n r1 G Sk.syn en)])
  (rw [(den_prn_at_ri chkf dec encTy ri n r2 G Sk.syn en)])
  ;; RI: print of a node is the node of the prints, by L2
  (exact (congrArg (fn [c :- Code] (coe Sk.syn s c))
           (Eq.symm (ri_node ri """ + X_ + """ (den_ri chkf dec encTy ri n r1 G Sk.cert en) (den_ri chkf dec encTy ri n r2 G Sk.cert en))))))""")
E("ri_conversion.clj",
  """                (den_ri chkf dec encTy ri n (Exp.leaf x) G Sk.cert en))))
  (rw [(den_leaf_at_ri chkf dec encTy ri n x G Sk.cert en)]))""",
  """                (den_ri chkf dec encTy ri n (Exp.leaf x) G Sk.cert en))))
  (rw [(den_leaf_at_ri chkf dec encTy ri n x G Sk.cert en)])
  ;; RI: itR passes riIt ri (riLf ri x) = x for the tree leaf x builds (L1)
  (exact (congrArg (den_ri chkf dec encTy ri n g G (Sk.arr Sk.lbl s) en) (Eq.symm (ri_itleaf ri """ + X_ + """)))))""")

# itR's leaf method passes riIt ri l (the itR clause over ri)
E("ri_conversion.clj", "(fn [l :- Nat] ((den_ri chkf dec encTy ri n g G (Sk.arr Sk.lbl s) en) l))",
  "(fn [l :- Nat] ((den_ri chkf dec encTy ri n g G (Sk.arr Sk.lbl s) en) (riIt ri l))) ;; RI: itR passes riIt ri l", 2)

# --- ri_convcase: the clause templates of the path congruences --------------------
E("ri_convcase.clj",
  """      (coe Sk.cert sk (Code.sl (den_ri chkf dec encTy ri cap aq G Sk.lbl en)))
      (coe Sk.cert sk (Code.sl (den_ri chkf dec encTy ri cap a G Sk.lbl en)))))
   '(exact (congrArg (fn [v :- (Car Sk.lbl)] (coe Sk.cert sk (Code.sl v))) h))])""",
  """      (coe Sk.cert sk (Code.sl (riLf ri (den_ri chkf dec encTy ri cap aq G Sk.lbl en))))
      (coe Sk.cert sk (Code.sl (riLf ri (den_ri chkf dec encTy ri cap a G Sk.lbl en))))))
   ;; RI: leaf builds sl (riLf ri ⟦a⟧)
   '(exact (congrArg (fn [v :- (Car Sk.lbl)] (coe Sk.cert sk (Code.sl (riLf ri v)))) h))])""")
E("ri_convcase.clj",
  """      (coe Sk.syn sk (den_ri chkf dec encTy ri cap rq G Sk.cert en))
      (coe Sk.syn sk (den_ri chkf dec encTy ri cap r G Sk.cert en))))
   '(exact (congrArg (fn [v :- (Car Sk.cert)] (coe Sk.syn sk v)) h))])""",
  """      (coe Sk.syn sk (riPr ri (den_ri chkf dec encTy ri cap rq G Sk.cert en)))
      (coe Sk.syn sk (riPr ri (den_ri chkf dec encTy ri cap r G Sk.cert en)))))
   ;; RI: print is riPr ri
   '(exact (congrArg (fn [v :- (Car Sk.cert)] (coe Sk.syn sk (riPr ri v))) h))])""")
E("ri_convcase.clj", "       (fn [l :- Nat] (GF l))\n",
  "       ;; RI: itR passes riIt ri l for a leaf\n       (fn [l :- Nat] (GF (riIt ri l)))\n")
E("ri_convcase.clj",
  """         (dec QR))
       (Bool.and (Nat.ble (cnodes QR) cap) (chkf QR (encTy D)))))""",
  """         (dec (riPr ri QR)))
       ;; RI: reflect decodes the print, only when the tree is phantom-free
       (Bool.and (Nat.ble (cnodes QR) cap) (Bool.and (riPf ri QR) (chkf (riPr ri QR) (encTy D))))))""")
E("ri_convcase.clj", "       (chkf RV CV)))\n", "       ;; RI: inspect checks the print\n       (chkf (riPr ri RV) CV)))\n")
E("ri_convcase.clj", "(chkf (den_ri chkf dec encTy ri cap r G Sk.cert en)\n",
  "(chkf (riPr ri (den_ri chkf dec encTy ri cap r G Sk.cert en))\n", 6)
CUTS.append(("ri_convcase.clj", "  (prove! 'conv_all_ri",
r"""  ;; RI: the Conv case for every checker and interpretation (no CheckSpec is
  ;; needed: F_conv_ri takes none).  ConvAll and the paper statements are the
  ;; standard chain's (lemma36.clj, convcase.clj); the generic chain stops at
  ;; Lemma 3.6 (lemma46a.clj assembles it with this).
  (prove! 'conv_all_ri
    '[]
    '(forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
       (forall [encTy (=> Exp Code)] (forall [ri RInt] (forall [n Nat] (ConvCase_ri chkf dec encTy ri n))))))
    ['(intro chkf dec encTy ri n)
     '(exact (F_conv_ri chkf dec encTy ri n))])
"""))


# --- generated names: model-independent families are the standard ones;
# model-dependent ones get the suffix _ri (gen_ri.py renames only literal
# tokens) ------------------------------------------------------------------------
DROP("ri_conversion.clj", "(eval\n (list 'kdef 'InvSkJ",
     ";; RI: (InvSkJ: the standard constant is used)")
DROP("ri_conversion.clj", "(doseq [[ctor fields] exp-fields :when (not= ctor 'tBrs)]\n  (let [fs (map first fields)\n        e (ctor-term ctor fs)]\n    (a/prove-theorem (symbol (str \"inv_\" ctor))",
     ";; RI: (inv_<ctor>: the standard constants are used)")
DROP("ri_conversion.clj", "(doseq [[ctor fields] exp-fields\n        :let [fs (filter #(= 'Exp (second %)) fields)",
     ";; RI: (nbr_<ctor>_<field>: the standard constants are used)")
E("ri_convcase.clj", 'nm (symbol (str "step_" ctor))]', 'nm (symbol (str "step_" ctor "_ri"))] ;; RI: generated name')
E("ri_convcase.clj", 'at (symbol (str "den_" ctor "_at"))', 'at (symbol (str "den_" ctor "_at_ri")) ;; RI: generated name', 2)
E("ri_convcase.clj", '(prove! (symbol (str "den_ig_" ctor "_" f))', '(prove! (symbol (str "den_ig_" ctor "_" f "_ri")) ;; RI: generated name\n     ')
E("ri_convcase.clj", '[(symbol (str "den_ig_h1_" f)) ', '[(symbol (str "den_ig_h1_" f "_ri")) ')
E("ri_convcase.clj", '(symbol (str "step_" (name ctor)))', '(symbol (str "step_" (name ctor) "_ri"))')
E("ri_convcase.clj", 'nm (symbol (str "step_V_" ctor))]', 'nm (symbol (str "step_V_" ctor "_ri"))] ;; RI: generated name')
E("ri_convcase.clj", "'step_V_tT_pack_ri (symbol (str \"step_V_\" ctor)))]", "'step_V_tT_pack_ri (symbol (str \"step_V_\" ctor \"_ri\")))]")

if __name__ == "__main__":
    apply(sys.argv[1], set(sys.argv[2:]) or None)
    print("applied")
