(ns lcert.formal.carrier
  "F3a — carriers, environments and defaults (R4-metatheory.md §3.1).

  Car s is the carrier of skeleton s: C(Unit) = {⋆}, C(Bool), C(Nat), C(Lbl)
  (labels as numbers), C(Syn) = codes, C(◇) = {◇} (Unit), C(R) = trees whose
  internal nodes carry ◇ — isomorphic to codes, since the token carries no
  information, so represented by Code — and the full function space and
  product at arrows and products.  Defined by large elimination (kdef).

  HEnv G is an environment for the skeleton context G (innermost first): a
  nested product of carrier values.

  dflt s is the explicit, computable default of §3.1: ⋆, ff, 0, the first
  label, a leaf, ◇, the constant function, the pair of defaults.

  coe s t v converts a value of skeleton s to skeleton t, structurally; it is
  the identity when s = t (coe_self), and the default where the shapes
  disagree.  Denotation only uses it at variables, where a well-typed term
  never needs a real conversion."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]))

(kdef Car (=> Sk (Sort 1))
  (fn [s :- Sk]
    (Sk.rec$2 (fn [_ :- Sk] (Sort 1))
      Unit Bool Nat Nat Code Unit Code
      (fn [x :- Sk, y :- Sk, cx :- (Sort 1), cy :- (Sort 1)] (=> cx cy))
      (fn [x :- Sk, y :- Sk, cx :- (Sort 1), cy :- (Sort 1)] (Prod cx cy))
      s)))

(kdef HEnv (=> (List Sk) (Sort 1))
  (fn [G :- (List Sk)]
    (List.rec$2$0 Sk (fn [_ :- (List Sk)] (Sort 1)) Unit
      (fn [s :- Sk, rest :- (List Sk), h :- (Sort 1)] (Prod (Car s) h))
      G)))

(kdef dflt (forall [s Sk] (Car s))
  (fn [s :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (Car t))
      Unit.unit Bool.false 0 0 (Code.sl 0) Unit.unit (Code.sl 0)
      (fn [x :- Sk, y :- Sk, dx :- (Car x), dy :- (Car y)] (fn [_ :- (Car x)] dy))
      (fn [x :- Sk, y :- Sk, dx :- (Car x), dy :- (Car y)] (Prod.mk dx dy))
      s)))

(thm car_arr [x :- Sk, y :- Sk] (= (Car (Sk.arr x y)) (=> (Car x) (Car y))) (rfl))
(thm henv_cons [s :- Sk, G :- (List Sk)] (= (HEnv (List.cons Sk s G)) (Prod (Car s) (HEnv G))) (rfl))
(thm dflt_arr [x :- Sk, y :- Sk, v :- (Car x)] (= ((dflt (Sk.arr x y)) v) (dflt y)) (rfl))

;; --- coercion between skeletons -------------------------------------------
;; coePair s t = (s → t, t → s), by recursion on s; at arrows the domain needs
;; the reverse direction, so both are built together.  Same-shaped base
;; skeletons coerce by identity; disagreeing shapes give defaults.

(kdef CoeT (=> Sk Sk (Sort 1)) (fn [s :- Sk, t :- Sk] (Prod (=> (Car s) (Car t)) (=> (Car t) (Car s)))))

;; coePair s t, by recursion on s (one definition per shape of s) and then
;; on t: identity on equal base skeletons; componentwise at arrows (the
;; domain reversed) and products; defaults where the shapes disagree.

(kdef coeFrom_unit (forall [t Sk] (CoeT Sk.unit t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.unit t))
      (Prod.mk (fn [v :- (Car Sk.unit)] v) (fn [v :- (Car Sk.unit)] v))
      (Prod.mk (fn [v :- (Car Sk.unit)] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt Sk.unit)))
      (Prod.mk (fn [v :- (Car Sk.unit)] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt Sk.unit)))
      (Prod.mk (fn [v :- (Car Sk.unit)] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt Sk.unit)))
      (Prod.mk (fn [v :- (Car Sk.unit)] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt Sk.unit)))
      (Prod.mk (fn [v :- (Car Sk.unit)] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt Sk.unit)))
      (Prod.mk (fn [v :- (Car Sk.unit)] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt Sk.unit)))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.unit c), ihd :- (CoeT Sk.unit d)] (Prod.mk (fn [v :- (Car Sk.unit)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.unit))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.unit c), ihd :- (CoeT Sk.unit d)] (Prod.mk (fn [v :- (Car Sk.unit)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.unit))))
      t)))

(kdef coeFrom_bool (forall [t Sk] (CoeT Sk.bool t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.bool t))
      (Prod.mk (fn [v :- (Car Sk.bool)] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt Sk.bool)))
      (Prod.mk (fn [v :- (Car Sk.bool)] v) (fn [v :- (Car Sk.bool)] v))
      (Prod.mk (fn [v :- (Car Sk.bool)] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt Sk.bool)))
      (Prod.mk (fn [v :- (Car Sk.bool)] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt Sk.bool)))
      (Prod.mk (fn [v :- (Car Sk.bool)] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt Sk.bool)))
      (Prod.mk (fn [v :- (Car Sk.bool)] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt Sk.bool)))
      (Prod.mk (fn [v :- (Car Sk.bool)] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt Sk.bool)))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.bool c), ihd :- (CoeT Sk.bool d)] (Prod.mk (fn [v :- (Car Sk.bool)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.bool))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.bool c), ihd :- (CoeT Sk.bool d)] (Prod.mk (fn [v :- (Car Sk.bool)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.bool))))
      t)))

(kdef coeFrom_nat (forall [t Sk] (CoeT Sk.nat t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.nat t))
      (Prod.mk (fn [v :- (Car Sk.nat)] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt Sk.nat)))
      (Prod.mk (fn [v :- (Car Sk.nat)] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt Sk.nat)))
      (Prod.mk (fn [v :- (Car Sk.nat)] v) (fn [v :- (Car Sk.nat)] v))
      (Prod.mk (fn [v :- (Car Sk.nat)] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt Sk.nat)))
      (Prod.mk (fn [v :- (Car Sk.nat)] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt Sk.nat)))
      (Prod.mk (fn [v :- (Car Sk.nat)] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt Sk.nat)))
      (Prod.mk (fn [v :- (Car Sk.nat)] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt Sk.nat)))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.nat c), ihd :- (CoeT Sk.nat d)] (Prod.mk (fn [v :- (Car Sk.nat)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.nat))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.nat c), ihd :- (CoeT Sk.nat d)] (Prod.mk (fn [v :- (Car Sk.nat)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.nat))))
      t)))

(kdef coeFrom_lbl (forall [t Sk] (CoeT Sk.lbl t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.lbl t))
      (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt Sk.lbl)))
      (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt Sk.lbl)))
      (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt Sk.lbl)))
      (Prod.mk (fn [v :- (Car Sk.lbl)] v) (fn [v :- (Car Sk.lbl)] v))
      (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt Sk.lbl)))
      (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt Sk.lbl)))
      (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt Sk.lbl)))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.lbl c), ihd :- (CoeT Sk.lbl d)] (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.lbl))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.lbl c), ihd :- (CoeT Sk.lbl d)] (Prod.mk (fn [v :- (Car Sk.lbl)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.lbl))))
      t)))

(kdef coeFrom_syn (forall [t Sk] (CoeT Sk.syn t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.syn t))
      (Prod.mk (fn [v :- (Car Sk.syn)] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt Sk.syn)))
      (Prod.mk (fn [v :- (Car Sk.syn)] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt Sk.syn)))
      (Prod.mk (fn [v :- (Car Sk.syn)] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt Sk.syn)))
      (Prod.mk (fn [v :- (Car Sk.syn)] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt Sk.syn)))
      (Prod.mk (fn [v :- (Car Sk.syn)] v) (fn [v :- (Car Sk.syn)] v))
      (Prod.mk (fn [v :- (Car Sk.syn)] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt Sk.syn)))
      (Prod.mk (fn [v :- (Car Sk.syn)] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt Sk.syn)))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.syn c), ihd :- (CoeT Sk.syn d)] (Prod.mk (fn [v :- (Car Sk.syn)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.syn))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.syn c), ihd :- (CoeT Sk.syn d)] (Prod.mk (fn [v :- (Car Sk.syn)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.syn))))
      t)))

(kdef coeFrom_dia (forall [t Sk] (CoeT Sk.dia t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.dia t))
      (Prod.mk (fn [v :- (Car Sk.dia)] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt Sk.dia)))
      (Prod.mk (fn [v :- (Car Sk.dia)] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt Sk.dia)))
      (Prod.mk (fn [v :- (Car Sk.dia)] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt Sk.dia)))
      (Prod.mk (fn [v :- (Car Sk.dia)] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt Sk.dia)))
      (Prod.mk (fn [v :- (Car Sk.dia)] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt Sk.dia)))
      (Prod.mk (fn [v :- (Car Sk.dia)] v) (fn [v :- (Car Sk.dia)] v))
      (Prod.mk (fn [v :- (Car Sk.dia)] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt Sk.dia)))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.dia c), ihd :- (CoeT Sk.dia d)] (Prod.mk (fn [v :- (Car Sk.dia)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.dia))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.dia c), ihd :- (CoeT Sk.dia d)] (Prod.mk (fn [v :- (Car Sk.dia)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.dia))))
      t)))

(kdef coeFrom_cert (forall [t Sk] (CoeT Sk.cert t))
  (fn [t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT Sk.cert t))
      (Prod.mk (fn [v :- (Car Sk.cert)] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt Sk.cert)))
      (Prod.mk (fn [v :- (Car Sk.cert)] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt Sk.cert)))
      (Prod.mk (fn [v :- (Car Sk.cert)] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt Sk.cert)))
      (Prod.mk (fn [v :- (Car Sk.cert)] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt Sk.cert)))
      (Prod.mk (fn [v :- (Car Sk.cert)] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt Sk.cert)))
      (Prod.mk (fn [v :- (Car Sk.cert)] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt Sk.cert)))
      (Prod.mk (fn [v :- (Car Sk.cert)] v) (fn [v :- (Car Sk.cert)] v))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.cert c), ihd :- (CoeT Sk.cert d)] (Prod.mk (fn [v :- (Car Sk.cert)] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt Sk.cert))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT Sk.cert c), ihd :- (CoeT Sk.cert d)] (Prod.mk (fn [v :- (Car Sk.cert)] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt Sk.cert))))
      t)))

(kdef coeFrom_arr (forall [a Sk] (forall [b Sk] (=> (forall [t Sk] (CoeT a t)) (forall [t Sk] (CoeT b t)) (forall [t Sk] (CoeT (Sk.arr a b) t)))))
  (fn [a :- Sk, b :- Sk, iha :- (forall [t Sk] (CoeT a t)), ihb :- (forall [t Sk] (CoeT b t)), t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT (Sk.arr a b) t))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt (Sk.arr a b))))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt (Sk.arr a b))))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt (Sk.arr a b))))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt (Sk.arr a b))))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt (Sk.arr a b))))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt (Sk.arr a b))))
      (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt (Sk.arr a b))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT (Sk.arr a b) c), ihd :- (CoeT (Sk.arr a b) d)] (Prod.mk (fn [f :- (Car (Sk.arr a b))] (fn [v :- (Car c)] (Prod.fst (ihb d) (f (Prod.snd (iha c) v))))) (fn [g :- (Car (Sk.arr c d))] (fn [v :- (Car a)] (Prod.snd (ihb d) (g (Prod.fst (iha c) v)))))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT (Sk.arr a b) c), ihd :- (CoeT (Sk.arr a b) d)] (Prod.mk (fn [v :- (Car (Sk.arr a b))] (dflt (Sk.prod c d))) (fn [v :- (Car (Sk.prod c d))] (dflt (Sk.arr a b)))))
      t)))

(kdef coeFrom_prod (forall [a Sk] (forall [b Sk] (=> (forall [t Sk] (CoeT a t)) (forall [t Sk] (CoeT b t)) (forall [t Sk] (CoeT (Sk.prod a b) t)))))
  (fn [a :- Sk, b :- Sk, iha :- (forall [t Sk] (CoeT a t)), ihb :- (forall [t Sk] (CoeT b t)), t :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (CoeT (Sk.prod a b) t))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.unit)) (fn [v :- (Car Sk.unit)] (dflt (Sk.prod a b))))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.bool)) (fn [v :- (Car Sk.bool)] (dflt (Sk.prod a b))))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.nat)) (fn [v :- (Car Sk.nat)] (dflt (Sk.prod a b))))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.lbl)) (fn [v :- (Car Sk.lbl)] (dflt (Sk.prod a b))))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.syn)) (fn [v :- (Car Sk.syn)] (dflt (Sk.prod a b))))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.dia)) (fn [v :- (Car Sk.dia)] (dflt (Sk.prod a b))))
      (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt Sk.cert)) (fn [v :- (Car Sk.cert)] (dflt (Sk.prod a b))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT (Sk.prod a b) c), ihd :- (CoeT (Sk.prod a b) d)] (Prod.mk (fn [v :- (Car (Sk.prod a b))] (dflt (Sk.arr c d))) (fn [v :- (Car (Sk.arr c d))] (dflt (Sk.prod a b)))))
      (fn [c :- Sk, d :- Sk, ihc :- (CoeT (Sk.prod a b) c), ihd :- (CoeT (Sk.prod a b) d)] (Prod.mk (fn [p :- (Car (Sk.prod a b))] (Prod.mk (Prod.fst (iha c) (Prod.fst p)) (Prod.fst (ihb d) (Prod.snd p)))) (fn [q :- (Car (Sk.prod c d))] (Prod.mk (Prod.snd (iha c) (Prod.fst q)) (Prod.snd (ihb d) (Prod.snd q))))))
      t)))

(kdef coePair (forall [s Sk] (forall [t Sk] (CoeT s t)))
  (fn [s :- Sk]
    (Sk.rec$1 (fn [s :- Sk] (forall [t Sk] (CoeT s t)))
      coeFrom_unit
      coeFrom_bool
      coeFrom_nat
      coeFrom_lbl
      coeFrom_syn
      coeFrom_dia
      coeFrom_cert
      (fn [a :- Sk, b :- Sk, iha :- (forall [t Sk] (CoeT a t)), ihb :- (forall [t Sk] (CoeT b t))] (coeFrom_arr a b iha ihb))
      (fn [a :- Sk, b :- Sk, iha :- (forall [t Sk] (CoeT a t)), ihb :- (forall [t Sk] (CoeT b t))] (coeFrom_prod a b iha ihb))
      s)))

(kdef coe (forall [s Sk] (forall [t Sk] (=> (Car s) (Car t))))
  (fn [s :- Sk, t :- Sk] (Prod.fst (coePair s t))))

(thm coe_nat [v :- Nat] (= (coe Sk.nat Sk.nat v) v) (rfl))
(thm coe_nat_bool [v :- Nat] (= (coe Sk.nat Sk.bool v) Bool.false) (rfl))

;; --- variables, skeleton cases, token environments ---------------------------

;; lookup G i s η: the value of variable i, coerced to skeleton s.
(kdef lookup (forall [G (List Sk)] (=> Nat (forall [s Sk] (=> (HEnv G) (Car s)))))
  (fn [G :- (List Sk)]
    (List.rec$1$0 Sk (fn [G :- (List Sk)] (=> Nat (forall [s Sk] (=> (HEnv G) (Car s)))))
      (fn [i :- Nat, s :- Sk, e :- Unit] (dflt s))
      (fn [s0 :- Sk, rest :- (List Sk), ih :- (=> Nat (forall [s Sk] (=> (HEnv rest) (Car s))))]
        (fn [i :- Nat, s :- Sk, e :- (HEnv (List.cons Sk s0 rest))]
          (Nat.rec$1 (fn [_ :- Nat] (Car s))
            (coe s0 s (Prod.fst e))
            (fn [k :- Nat, _ :- (Car s)] (ih k s (Prod.snd e)))
            i)))
      G)))

;; arrCase s f: at an arrow skeleton x → y, f x y; elsewhere the default.
(kdef arrCase (forall [s Sk] (=> (forall [x Sk] (forall [y Sk] (Car (Sk.arr x y)))) (Car s)))
  (fn [s :- Sk, f :- (forall [x Sk] (forall [y Sk] (Car (Sk.arr x y))))]
    (Sk.rec$1 (fn [t :- Sk] (Car t))
      (dflt Sk.unit) (dflt Sk.bool) (dflt Sk.nat) (dflt Sk.lbl) (dflt Sk.syn) (dflt Sk.dia) (dflt Sk.cert)
      (fn [x :- Sk, y :- Sk, dx :- (Car x), dy :- (Car y)] (f x y))
      (fn [x :- Sk, y :- Sk, dx :- (Car x), dy :- (Car y)] (dflt (Sk.prod x y)))
      s)))

;; prodCase s f: at a product skeleton x × y, f x y; elsewhere the default.
(kdef prodCase (forall [s Sk] (=> (forall [x Sk] (forall [y Sk] (Car (Sk.prod x y)))) (Car s)))
  (fn [s :- Sk, f :- (forall [x Sk] (forall [y Sk] (Car (Sk.prod x y))))]
    (Sk.rec$1 (fn [t :- Sk] (Car t))
      (dflt Sk.unit) (dflt Sk.bool) (dflt Sk.nat) (dflt Sk.lbl) (dflt Sk.syn) (dflt Sk.dia) (dflt Sk.cert)
      (fn [x :- Sk, y :- Sk, dx :- (Car x), dy :- (Car y)] (dflt (Sk.arr x y)))
      (fn [x :- Sk, y :- Sk, dx :- (Car x), dy :- (Car y)] (f x y))
      s)))

;; splitProd sp v k: if sp is a product x × y, k x y applied to v's
;; components; otherwise the default of the result skeleton s.
(kdef splitProd (forall [s Sk] (forall [sp Sk] (=> (Car sp) (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s)))) (Car s))))
  (fn [s :- Sk, sp :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (=> (Car t) (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s)))) (Car s)))
      (fn [v :- Unit, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [v :- Bool, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [v :- Nat, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [v :- Nat, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [v :- Code, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [v :- Unit, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [v :- Code, k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s))
      (fn [x :- Sk, y :- Sk, ix :- (=> (Car x) (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s)))) (Car s)),
           iy :- (=> (Car y) (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s)))) (Car s))]
        (fn [v :- (Car (Sk.arr x y)), k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))] (dflt s)))
      (fn [x :- Sk, y :- Sk, ix :- (=> (Car x) (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s)))) (Car s)),
           iy :- (=> (Car y) (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s)))) (Car s))]
        (fn [v :- (Car (Sk.prod x y)), k :- (forall [x Sk] (forall [y Sk] (=> (Car x) (Car y) (Car s))))]
          (k x y (Prod.fst v) (Prod.snd v))))
      sp)))

;; Θₘ's skeletons and its all-token environment.
(a/defn thetaSk [m :- Nat] (List Sk) (match m [zero (List.nil Sk)] [(succ k) (List.cons Sk Sk.dia (thetaSk k))]))
(kdef tokenEnv (forall [m Nat] (HEnv (thetaSk m)))
  (fn [m :- Nat]
    (Nat.rec$1 (fn [k :- Nat] (HEnv (thetaSk k))) Unit.unit
      (fn [k :- Nat, e :- (HEnv (thetaSk k))] (Prod.mk Unit.unit e))
      m)))

(thm lookup_zero [s :- Sk, G :- (List Sk), v :- (Car s), e :- (HEnv G)]
  (= (lookup (List.cons Sk s G) 0 s (Prod.mk v e)) (coe s s v)) (rfl))

;; --- skeleton inference ---------------------------------------------------
;; skOf G t: the skeleton of t in the skeleton context G, read off the term
;; (terms are Church-style: every binder and eliminator carries its type or
;; motive).  Used by denotation where a term does not record the skeleton of
;; a subterm: an application's argument, a let's pair.

(a/defn arrCod [s :- Sk] (Option Sk) (match s [(arr a b) (Option.some Sk b)] [_ (Option.none Sk)]))

(a/defn skOfF [e :- Exp] (=> (List Sk) (Option Sk))
  (match e
    [(var i) (fn [G :- (List Sk)] (nthS G i))]
    [star (fn [G :- (List Sk)] (Option.some Sk Sk.unit))]
    [(abort A t) (fn [G :- (List Sk)] (Option.some Sk (skel A)))]
    [tt (fn [G :- (List Sk)] (Option.some Sk Sk.bool))]
    [ff (fn [G :- (List Sk)] (Option.some Sk Sk.bool))]
    [(ite b t x) (fn [G :- (List Sk)] ((skOfF t) G))]
    [(elimB P b t x) (fn [G :- (List Sk)] (Option.some Sk (skel P)))]
    [zero (fn [G :- (List Sk)] (Option.some Sk Sk.nat))]
    [(succ n) (fn [G :- (List Sk)] (Option.some Sk Sk.nat))]
    [(recN P z s n) (fn [G :- (List Sk)] (Option.some Sk (skel P)))]
    [(lbl l) (fn [G :- (List Sk)] (Option.some Sk Sk.lbl))]
    [(caseL P x bs) (fn [G :- (List Sk)] (Option.some Sk (skel P)))]
    [(sleaf x) (fn [G :- (List Sk)] (Option.some Sk Sk.syn))]
    [(snode x c1 c2) (fn [G :- (List Sk)] (Option.some Sk Sk.syn))]
    [(recS P tl tn c) (fn [G :- (List Sk)] (Option.some Sk (skel P)))]
    [(leaf x) (fn [G :- (List Sk)] (Option.some Sk Sk.cert))]
    [(node d x r1 r2) (fn [G :- (List Sk)] (Option.some Sk Sk.cert))]
    [(itR X g h r) (fn [G :- (List Sk)] (Option.some Sk (skel X)))]
    [(prn r) (fn [G :- (List Sk)] (Option.some Sk Sk.syn))]
    [(lam r A t) (fn [G :- (List Sk)]
                   (match ((skOfF t) (List.cons Sk (skel A) G))
                     [none (Option.none Sk)]
                     [(some b) (Option.some Sk (Sk.arr (skel A) b))]))]
    [(app f u) (fn [G :- (List Sk)] (match ((skOfF f) G) [none (Option.none Sk)] [(some sf) (arrCod sf)]))]
    [(pair S x y) (fn [G :- (List Sk)] (Option.some Sk (skel S)))]
    [(letp C p t) (fn [G :- (List Sk)] (Option.some Sk (skel C)))]
    [(chk c d) (fn [G :- (List Sk)] (Option.some Sk Sk.bool))]
    [(h1 r s c e1 e2) (fn [G :- (List Sk)] (Option.some Sk Sk.unit))]
    [(refl D r x) (fn [G :- (List Sk)] (Option.some Sk (skel D)))]
    [(insp X r c t1 t2) (fn [G :- (List Sk)] (Option.some Sk (skel X)))]
    [_ (fn [G :- (List Sk)] (Option.none Sk))]))

(a/defn skOf [G :- (List Sk), e :- Exp] (Option Sk) ((skOfF e) G))
