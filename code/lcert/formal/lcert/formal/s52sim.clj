(ns lcert.formal.s52sim
  "Theorem 5.2, universal form (ADR-0006): equality of runtime values up to the
  free tag of bnil.

  Ev is deterministic except for the skeleton tag the rule eBnil gives the
  empty branch list (eval.clj, adaptation 3).  Two runs of one program can
  therefore return values that differ only in bnil tags, possibly deep inside
  pairs, closure environments and branch lists.  The universal Theorem 5.2
  compares such runs, so it needs 'equal up to bnil tags'.

  flV v is v with every bnil tag replaced by Sk.unit (flL for a list of
  values).  RV is a nested inductive (closures hold a List RV), so flV is
  one use of the nested recursor RV.rec with RV.rec_1 for lists; its clauses
  are the constructors rebuilt on the recursive results.  v and w are the
  same up to tags exactly when flV v = flV w.  The constructors compute under
  flV by the recursor's iota rules, so flV (RV.nat k) is RV.nat k by rfl, and
  the lemmas below only state what the recursor does not give for free:
  injectivity, constructor discrimination and inversion, all through
  projections defined by the same recursor (the rules of RV carry no
  noConfusion/injection tactic in this kernel).

  This file is only about values; Ev and Ok are not mentioned."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.eval]))

;; --- recursor-defined maps out of RV and List RV ------------------------------

;; The minor premises of RV.rec / RV.rec_1, in the order of the recursor:
;; token star bool nat lbl code cert pair clos bnil bcons, then nil cons for
;; the nested list.  Each takes the constructor's fields, then one induction
;; hypothesis per recursive field: an RV field has type T1 (the RV motive), a
;; List RV field has type T2.
(defn- minor-sig [T1 T2]
  (walk/postwalk-replace {'T1 T1 'T2 T2}
    {:token [] :star [] :bool '[[b Bool]] :nat '[[k Nat]] :lbl '[[l Nat]]
     :code '[[c Code]] :cert '[[c Code]]
     :pair '[[x RV] [y RV] [xi T1] [yi T1]]
     :clos '[[rho (List RV)] [body Exp] [ri T2]]
     :bnil '[[s Sk]] :bcons '[[h RV] [t RV] [hi T1] [ti T1]]
     :nil [] :cons '[[h RV] [t (List RV)] [hi T1] [ti T2]]}))

(defn rv-rec
  "A kernel term applying the nested recursor to `arg` (an RV, or a List RV
  when list? is true), with motives T1 for RV and T2 for List RV.  `overrides`
  maps a constructor key to its minor premise; every other constructor gets
  the constant `dflt` (which must inhabit T1 for RV constructors and T2 for
  the list constructors)."
  [T1 T2 dflt dflt2 overrides arg list?]
  (let [sig (minor-sig T1 T2)
        minor (fn [k]
                (or (overrides k)
                    (let [d (if (#{:nil :cons} k) dflt2 dflt)]
                      (if (empty? (sig k)) d
                          (list 'fn (vec (mapcat (fn [[x t]] [x :- t]) (sig k))) d)))))]
    (concat (list (if list? 'RV.rec_1$1 'RV.rec$1)
                  (list 'fn '[_ :- RV] T1) (list 'fn '[_ :- (List RV)] T2))
            (map minor [:token :star :bool :nat :lbl :code :cert :pair :clos :bnil :bcons :nil :cons])
            [arg])))

;; A projection RV -> T (T also the list motive, defaulting to dflt).
(defn- rv-proj! [nm T dflt overrides]
  (kdef! nm (list '=> 'RV T) (list 'fn '[v :- RV] (rv-rec T T dflt dflt overrides 'v false))))
;; A projection List RV -> T.
(defn- rvl-proj! [nm T dflt overrides]
  (kdef! nm (list '=> '(List RV) T) (list 'fn '[l :- (List RV)] (rv-rec T T dflt dflt overrides 'l true))))

;; flatten: every bnil tag becomes Sk.unit.  The nil/cons clauses rebuild lists.
(def ^:private fl-minors
  {:token 'RV.token :star 'RV.star
   :bool '(fn [b :- Bool] (RV.bool b)) :nat '(fn [k :- Nat] (RV.nat k))
   :lbl '(fn [l :- Nat] (RV.lbl l)) :code '(fn [c :- Code] (RV.code c))
   :cert '(fn [c :- Code] (RV.cert c))
   :pair '(fn [x :- RV, y :- RV, xi :- RV, yi :- RV] (RV.pair xi yi))
   :clos '(fn [rho :- (List RV), body :- Exp, ri :- (List RV)] (RV.clos ri body))
   :bnil '(fn [s :- Sk] (RV.bnil Sk.unit))
   :bcons '(fn [h :- RV, t :- RV, hi :- RV, ti :- RV] (RV.bcons hi ti))
   :nil '(List.nil RV)
   :cons '(fn [h :- RV, t :- (List RV), hi :- RV, ti :- (List RV)] (List.cons RV hi ti))})

;; flV v: v with all bnil tags flattened; flL likewise for a list of values.
(kdef! 'flV '(=> RV RV)
  (list 'fn '[v :- RV] (rv-rec 'RV '(List RV) 'RV.star '(List.nil RV) fl-minors 'v false)))
(kdef! 'flL '(=> (List RV) (List RV))
  (list 'fn '[l :- (List RV)] (rv-rec 'RV '(List RV) 'RV.star '(List.nil RV) fl-minors 'l true)))

;; Constructor projections.  Each returns a field of the constructor named, and
;; a harmless default at any other constructor.
(rv-proj! 'smBool 'Bool 'Bool.false {:bool '(fn [b :- Bool] b)})
(rv-proj! 'smNat 'Nat '0 {:nat '(fn [k :- Nat] k)})
(rv-proj! 'smLbl 'Nat '0 {:lbl '(fn [l :- Nat] l)})
(rv-proj! 'smCode 'Code '(Code.sl 0) {:code '(fn [c :- Code] c)})
(rv-proj! 'smCert 'Code '(Code.sl 0) {:cert '(fn [c :- Code] c)})
(rv-proj! 'smPL 'RV 'RV.star {:pair '(fn [x :- RV, y :- RV, xi :- RV, yi :- RV] x)})
(rv-proj! 'smPR 'RV 'RV.star {:pair '(fn [x :- RV, y :- RV, xi :- RV, yi :- RV] y)})
(rv-proj! 'smCE '(List RV) '(List.nil RV)
  {:clos '(fn [rho :- (List RV), body :- Exp, ri :- (List RV)] rho)})
(rv-proj! 'smCB 'Exp 'Exp.star {:clos '(fn [rho :- (List RV), body :- Exp, ri :- Exp] body)})
(rv-proj! 'smBH 'RV 'RV.star {:bcons '(fn [h :- RV, t :- RV, hi :- RV, ti :- RV] h)})
(rv-proj! 'smBT 'RV 'RV.star {:bcons '(fn [h :- RV, t :- RV, hi :- RV, ti :- RV] t)})
;; The constructor index.
(rv-proj! 'smTag 'Nat '0
  {:token '0 :star '1 :bool '(fn [b :- Bool] 2) :nat '(fn [k :- Nat] 3)
   :lbl '(fn [l :- Nat] 4) :code '(fn [c :- Code] 5) :cert '(fn [c :- Code] 6)
   :pair '(fn [x :- RV, y :- RV, xi :- Nat, yi :- Nat] 7)
   :clos '(fn [rho :- (List RV), body :- Exp, ri :- Nat] 8)
   :bnil '(fn [s :- Sk] 9)
   :bcons '(fn [h :- RV, t :- RV, hi :- Nat, ti :- Nat] 10)})
;; Head, tail and nil/cons index of a list of values.
(rvl-proj! 'smHd 'RV 'RV.star {:cons '(fn [h :- RV, t :- (List RV), hi :- RV, ti :- RV] h)})
(rvl-proj! 'smTl '(List RV) '(List.nil RV)
  {:cons '(fn [h :- RV, t :- (List RV), hi :- (List RV), ti :- (List RV)] t)})
(rvl-proj! 'smLTag 'Nat '0 {:nil '0 :cons '(fn [h :- RV, t :- (List RV), hi :- Nat, ti :- Nat] 1)})

;; --- injectivity and congruence of flV on constructors ------------------------
;; flV (K x) is K x (data) or K (flV x) (pairs, ...) by the recursor's iota rule,
;; so each statement below is a statement about the constructor itself.

(doseq [[nm ctor proj ty] '[[bool RV.bool smBool Bool] [nat RV.nat smNat Nat] [lbl RV.lbl smLbl Nat]
                            [code RV.code smCode Code] [cert RV.cert smCert Code]]]
  (a/prove-theorem (symbol (str "sm_" nm "_inj"))
    (lv [(symbol "x") :- ty (symbol "y") :- ty
         (symbol "h") :- (list 'Eq 'RV (list 'flV (list ctor 'x)) (list 'flV (list ctor 'y)))])
    (lv (list 'Eq ty 'x 'y))
    (lv [(list 'exact (list 'congrArg proj 'h))]))
  (a/prove-theorem (symbol (str "sm_" nm "_cong"))
    (lv [(symbol "x") :- ty (symbol "y") :- ty (symbol "h") :- (list 'Eq ty 'x 'y)])
    (lv (list 'Eq 'RV (list 'flV (list ctor 'x)) (list 'flV (list ctor 'y))))
    (lv [(list 'exact (list 'congrArg (list 'fn ['z :- ty] (list 'flV (list ctor 'z))) 'h))])))

(thm sm_pair_cong [a :- RV, a2 :- RV, b :- RV, b2 :- RV,
                   h1 :- (Eq RV (flV a) (flV a2)), h2 :- (Eq RV (flV b) (flV b2))]
  (Eq RV (flV (RV.pair a b)) (flV (RV.pair a2 b2)))
  (have e (Eq RV (RV.pair (flV a) (flV b)) (RV.pair (flV a2) (flV b2)))
    (congr (congrArg RV.pair h1) h2))
  (exact e))

(thm sm_pair_inj [a :- RV, a2 :- RV, b :- RV, b2 :- RV,
                  h :- (Eq RV (flV (RV.pair a b)) (flV (RV.pair a2 b2)))]
  (And (Eq RV (flV a) (flV a2)) (Eq RV (flV b) (flV b2)))
  (constructor)
  (exact (congrArg smPL h))
  (exact (congrArg smPR h)))

(thm sm_bcons_cong [a :- RV, a2 :- RV, b :- RV, b2 :- RV,
                    h1 :- (Eq RV (flV a) (flV a2)), h2 :- (Eq RV (flV b) (flV b2))]
  (Eq RV (flV (RV.bcons a b)) (flV (RV.bcons a2 b2)))
  (have e (Eq RV (RV.bcons (flV a) (flV b)) (RV.bcons (flV a2) (flV b2)))
    (congr (congrArg RV.bcons h1) h2))
  (exact e))

(thm sm_bcons_inj [a :- RV, a2 :- RV, b :- RV, b2 :- RV,
                   h :- (Eq RV (flV (RV.bcons a b)) (flV (RV.bcons a2 b2)))]
  (And (Eq RV (flV a) (flV a2)) (Eq RV (flV b) (flV b2)))
  (constructor)
  (exact (congrArg smBH h))
  (exact (congrArg smBT h)))

(thm sm_clos_cong [r :- (List RV), r2 :- (List RV), b :- Exp, b2 :- Exp,
                   h1 :- (Eq (List RV) (flL r) (flL r2)), h2 :- (Eq Exp b b2)]
  (Eq RV (flV (RV.clos r b)) (flV (RV.clos r2 b2)))
  (have e (Eq RV (RV.clos (flL r) b) (RV.clos (flL r2) b2))
    (congr (congrArg RV.clos h1) h2))
  (exact e))

(thm sm_clos_inj [r :- (List RV), r2 :- (List RV), b :- Exp, b2 :- Exp,
                  h :- (Eq RV (flV (RV.clos r b)) (flV (RV.clos r2 b2)))]
  (And (Eq (List RV) (flL r) (flL r2)) (Eq Exp b b2))
  (constructor)
  (exact (congrArg smCE h))
  (exact (congrArg smCB h)))

(thm sm_cons_cong [x :- RV, x2 :- RV, l :- (List RV), l2 :- (List RV),
                   h1 :- (Eq RV (flV x) (flV x2)), h2 :- (Eq (List RV) (flL l) (flL l2))]
  (Eq (List RV) (flL (List.cons RV x l)) (flL (List.cons RV x2 l2)))
  (have e (Eq (List RV) (List.cons RV (flV x) (flL l)) (List.cons RV (flV x2) (flL l2)))
    (Eq.trans (congrArg (fn [z :- RV] (List.cons RV z (flL l))) h1)
              (congrArg (fn [z :- (List RV)] (List.cons RV (flV x2) z)) h2)))
  (exact e))

(thm sm_cons_inj [x :- RV, x2 :- RV, l :- (List RV), l2 :- (List RV),
                  h :- (Eq (List RV) (flL (List.cons RV x l)) (flL (List.cons RV x2 l2)))]
  (And (Eq RV (flV x) (flV x2)) (Eq (List RV) (flL l) (flL l2)))
  (constructor)
  (exact (congrArg smHd h))
  (exact (congrArg smTl h)))

;; A flattened list is nil or cons exactly as the list is.
(thm sm_nil_cons [x :- RV, l :- (List RV),
                  h :- (Eq (List RV) (flL (List.nil RV)) (flL (List.cons RV x l)))]
  False
  (have h2 (Eq Nat 0 1) (congrArg smLTag h))
  (omega))

;; --- inversion: what a value equal (up to tags) to a constructor must be -------

;; `cases` and `induction` on RV are unusable (the kernel rejects the term they
;; build from the nested recursor), so eliminations apply RV.rec by hand with
;; an inline Prop motive per value `w`; the motive for lists is trivially True.

;; Two different numerals are not equal (the tag clash of an impossible case).
(thm sm_ne [a :- Nat, b :- Nat, hne :- (Eq Bool (Nat.beq a b) Bool.false), h :- (Eq Nat a b)]
  False
  (have e (Eq Bool (Nat.beq a b) Bool.true)
    (Eq.mp (congrArg (fn [z :- Nat] (Eq Bool (Nat.beq a z) Bool.true)) h) (Nat.beq_refl a)))
  (exact (Bool.noConfusion (Eq.trans (Eq.symm e) hne))))

(def ^:private rv-order [:token :star :bool :nat :lbl :code :cert :pair :clos :bnil :bcons])
;; constructor -> [kernel name, [field name type]...]
(def ^:private rv-fields
  {:token ['RV.token] :star ['RV.star]
   :bool ['RV.bool '[b Bool]] :nat ['RV.nat '[k Nat]] :lbl ['RV.lbl '[l Nat]]
   :code ['RV.code '[c Code]] :cert ['RV.cert '[c Code]]
   :pair ['RV.pair '[x RV] '[y RV]] :clos ['RV.clos '[rho (List RV)] '[body Exp]]
   :bnil ['RV.bnil '[s Sk]] :bcons ['RV.bcons '[h RV] '[t RV]]})

(defn rv-elim-term
  "RV.rec into the Prop `mbody` (a form with the free variable w), applied to
  `arg`.  (minor c vals) gives the proof for constructor c, whose field
  values are the symbols in vals; it must prove mbody with w := (ctor vals).
  The induction hypotheses are bound as ih_<field> (True for a list field)."
  [mbody minor arg]
  (let [at (fn [x] (walk/postwalk-replace {'w x} mbody))
        mins (for [c rv-order
                   :let [[ctor & fs] (rv-fields c)
                         names (map first fs)
                         ihs (for [[f t] fs :when (#{'RV 'List} (if (seq? t) (first t) t))]
                               [(symbol (str "ih_" f)) :- (if (= t 'RV) (at f) 'True)])
                         bs (vec (concat (mapcat (fn [[f t]] [f :- t]) fs) (mapcat identity ihs)))
                         body (minor c names)]]
               (if (seq bs) (list 'fn bs body) body))]
    (concat (list 'RV.rec$0 (list 'fn '[w :- RV] mbody) '(fn [_ :- (List RV)] True))
            mins
            ['True.intro
             (list 'fn ['h :- 'RV, 't :- '(List RV), 'hi :- (at 'h), 'ti :- 'True] 'True.intro)]
            [arg])))

;; A tag clash: the hypothesis hh : flV pat = flV (ctor vals) is absurd, since
;; smTag differs.  `mine` and `c` are constructor keys; concl is the goal.
(defn- clash [mine c vals concl-of hyp]
  (let [[ctor & _] (rv-fields c)
        val (if (seq vals) (apply list ctor vals) ctor)
        idx (fn [k] (.indexOf ^java.util.List rv-order k))]
    (list 'fn ['hh :- (hyp val)]
      (list 'AT_False.elim (concl-of val)
        (list 'sm_ne (idx mine) (idx c) '(Eq.refl Bool.false) '(congrArg smTag hh))))))

(defn- val-of [c vals] (let [[ctor & _] (rv-fields c)] (if (seq vals) (apply list ctor vals) ctor)))

;; v = lbl ll, up to tags (data are untouched, so exactly).
(let [hyp (fn [val] (list 'Eq 'RV '(flV (RV.lbl ll)) (list 'flV val)))]
  (a/prove-theorem 'sm_lbl_inv
    (lv '[ll :- Nat, v :- RV, hh :- (Eq RV (flV (RV.lbl ll)) (flV v))])
    (lv '(Eq RV v (RV.lbl ll)))
    (lv [(list 'exact
           (list (rv-elim-term
                   '(=> (Eq RV (flV (RV.lbl ll)) (flV w)) (Eq RV w (RV.lbl ll)))
                   (fn [c vals]
                     (if (= c :lbl)
                       '(fn [hh :- (Eq RV (flV (RV.lbl ll)) (flV (RV.lbl l)))]
                          (congrArg RV.lbl (Eq.symm (sm_lbl_inj ll l hh))))
                       (clash :lbl c vals (fn [val] (list 'Eq 'RV val '(RV.lbl ll))) hyp)))
                   'v)
                 'hh))])))

;; An RV-eliminated inversion lemma: params |- hyp -> concl, by RV.rec.
(defn- rv-inversion! [nm params mine pat concl real]
  (let [hyp (fn [val] (list 'Eq 'RV pat (list 'flV val)))
        concl-of (fn [val] (walk/postwalk-replace {'v val} concl))
        mbody (list '=> (list 'Eq 'RV pat '(flV w)) (walk/postwalk-replace {'v 'w} concl))]
    (a/prove-theorem nm
      (lv (vec (concat params ['v :- 'RV 'hh :- (list 'Eq 'RV pat '(flV v))])))
      (lv concl)
      (lv [(list 'exact
             (list (rv-elim-term mbody
                     (fn [c vals] (if (= c mine) real (clash mine c vals concl-of hyp)))
                     'v)
                   'hh))]))))

;; v = clos r2 bb with the environment equal up to tags.
(rv-inversion! 'sm_clos_inv '[rr :- (List RV), bb :- Exp] :clos '(flV (RV.clos rr bb))
  '(Exists (fn [r2 :- (List RV)]
     (And (Eq RV v (RV.clos r2 bb)) (Eq (List RV) (flL rr) (flL r2)))))
  '(fn [hh :- (Eq RV (flV (RV.clos rr bb)) (flV (RV.clos rho body)))]
     (AT_Exists.intro (List RV)
       (fn [r2 :- (List RV)]
         (And (Eq RV (RV.clos rho body) (RV.clos r2 bb)) (Eq (List RV) (flL rr) (flL r2))))
       rho
       (And.intro
         (congrArg (fn [z :- Exp] (RV.clos rho z)) (Eq.symm (And.right (sm_clos_inj rr rho bb body hh))))
         (And.left (sm_clos_inj rr rho bb body hh))))))

;; v = bcons a2 b2 with the components equal up to tags.
(rv-inversion! 'sm_bcons_inv '[pa :- RV, pb :- RV] :bcons '(flV (RV.bcons pa pb))
  '(Exists (fn [a2 :- RV] (Exists (fn [b2 :- RV]
     (And (Eq RV v (RV.bcons a2 b2))
          (And (Eq RV (flV pa) (flV a2)) (Eq RV (flV pb) (flV b2))))))))
  '(fn [hh :- (Eq RV (flV (RV.bcons pa pb)) (flV (RV.bcons h t)))]
     (AT_Exists.intro RV
       (fn [a2 :- RV] (Exists (fn [b2 :- RV]
         (And (Eq RV (RV.bcons h t) (RV.bcons a2 b2))
              (And (Eq RV (flV pa) (flV a2)) (Eq RV (flV pb) (flV b2)))))))
       h
       (AT_Exists.intro RV
         (fn [b2 :- RV]
           (And (Eq RV (RV.bcons h t) (RV.bcons h b2))
                (And (Eq RV (flV pa) (flV h)) (Eq RV (flV pb) (flV b2)))))
         t
         (And.intro (Eq.refl (RV.bcons h t)) (sm_bcons_inj pa h pb t hh))))))

;; --- lists: lookup, and cons inversion -----------------------------------------

(thm some_injRV [x :- RV, y :- RV, h :- (Eq (Option RV) (Option.some RV x) (Option.some RV y))]
  (Eq RV x y)
  (cases h) (rfl))

(thm none_ne_someRV [r :- RV, h :- (Eq (Option RV) (Option.none RV) (Option.some RV r))]
  False
  (cases h))

(a/prove-theorem 'sm_list_cons_inv
  (lv '[pa :- RV, pt :- (List RV), rho2 :- (List RV),
        hh :- (Eq (List RV) (flL (List.cons RV pa pt)) (flL rho2))])
  (lv '(Exists (fn [a2 :- RV] (Exists (fn [t2 :- (List RV)]
         (And (Eq (List RV) rho2 (List.cons RV a2 t2))
              (And (Eq RV (flV pa) (flV a2)) (Eq (List RV) (flL pt) (flL t2)))))))))
  (lv '[(cases rho2)
        (exact (False.elim (sm_nil_cons pa pt (Eq.symm hh))))
        (constructor) (exact head) (constructor) (exact tail) (constructor) (rfl)
        (exact (sm_cons_inj pa head pt tail hh))]))

;; Looking up one index in two lists equal up to tags gives values equal up to tags.
(thm sm_lookup_zero [h1 :- RV, t1 :- (List RV), h2 :- RV, t2 :- (List RV), v1 :- RV, v2 :- RV,
                     hh :- (Eq RV (flV h1) (flV h2)),
                     l1 :- (Eq (Option RV) (rlookup (List.cons RV h1 t1) 0) (Option.some RV v1)),
                     l2 :- (Eq (Option RV) (rlookup (List.cons RV h2 t2) 0) (Option.some RV v2))]
  (Eq RV (flV v1) (flV v2))
  (have e1 (Eq RV h1 v1) (some_injRV h1 v1 l1))
  (have e2 (Eq RV h2 v2) (some_injRV h2 v2 l2))
  (exact (Eq.trans (congrArg flV (Eq.symm e1)) (Eq.trans hh (congrArg flV e2)))))

(thm sm_lookup_aux []
  (forall [rho (List RV)] (forall [rho2 (List RV)] (forall [i Nat] (forall [v1 RV] (forall [v2 RV]
    (=> (Eq (List RV) (flL rho) (flL rho2))
        (Eq (Option RV) (rlookup rho i) (Option.some RV v1))
        (Eq (Option RV) (rlookup rho2 i) (Option.some RV v2))
        (Eq RV (flV v1) (flV v2))))))))
  (intro rho) (induction rho)
  (intro rho2 i v1 v2 hf l1 l2)
  (have e (Eq (Option RV) (rlookup (List.nil RV) i) (Option.none RV)) (rfl))
  (exact (False.elim (none_ne_someRV v1 (Eq.trans (Eq.symm e) l1))))
  (intro rho2 i v1 v2 hf l1 l2)
  (refine' (exT RV _ _ (sm_list_cons_inv head tail rho2 hf) _)) (intro a2 hex)
  (refine' (exT (List RV) _ _ hex _)) (intro t2 hp)
  (have hr2 (Eq (List RV) rho2 (List.cons RV a2 t2)) (And.left hp))
  (subst hr2)
  (cases i)
  (exact (sm_lookup_zero head tail a2 t2 v1 v2 (And.left (And.right hp)) l1 l2))
  (exact (ih_tail t2 n v1 v2 (And.right (And.right hp))
    (Eq.trans (Eq.symm (rlookup_succ head tail n)) l1)
    (Eq.trans (Eq.symm (rlookup_succ a2 t2 n)) l2))))
