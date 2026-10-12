(ns lcert.formal.p49split
  "Proposition 4.9 auxiliary: affine splitting of Boolean conjunction evidence."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.judgment]))

;; The only conversions used below are recorded head steps, either at the
;; root or underneath T. Neither lemma assumes a conversion oracle.
(thm p49split_head [chkf :- (=> Code Code Bool), x :- Exp, y :- Exp,
                   h :- (Hd chkf x y)]
  (Step chkf x y)
  (unfold Step)
  (apply (Exists.intro (List.nil Nat)))
  (apply (Exists.intro x)) (apply (Exists.intro y))
  (exact (And.intro rfl (And.intro h rfl))))

(thm p49split_underT [chkf :- (=> Code Code Bool), x :- Exp, y :- Exp,
                     h :- (Hd chkf x y)]
  (Step chkf (Exp.tT x) (Exp.tT y))
  (unfold Step)
  (apply (Exists.intro (List.cons Nat 0 (List.nil Nat))))
  (apply (Exists.intro x)) (apply (Exists.intro y))
  (exact (And.intro rfl (And.intro h rfl))))

;; Small *untrusted* builders for this one fixed derivation. The emitted
;; terms are checked against Rt/Tl/SkJ by Ansatz. The lists are telescopes,
;; innermost first. No new typing rule is introduced.
(defn- ls [ty xs]
  (reduce (fn [tail x] (list 'List.cons ty x tail)) (list 'List.nil ty) (reverse xs)))
(defn- ds [xs] (ls 'Exp xs))
(defn- us [xs] (ls 'U xs))
(defn- vr [i] (list 'Exp.var i))
(defn- ev [x] (list 'Exp.tT x))
(defn- lookup [kind ty xs i]
  (let [[x & tail] xs]
    (if (zero? i) (list (symbol (str kind ".eq_2")) x (ls ty tail))
      (list 'Eq.trans (list (symbol (str kind ".eq_3")) x (ls ty tail) (dec i))
        (lookup kind ty tail (dec i))))))
(defn- sk [A] (if (= A 'Exp.tBool) 'Sk.bool 'Sk.unit))
(defn- gs [D] (ls 'Sk (map sk D)))
(declare form typed skform sktyped)
(defn- form [D A]
  (if (symbol? A)
    (list 'Tl.fBase 'chkf (ds D) A 'rfl)
    (case (first A)
      Exp.tT (list 'Tl.fT 'chkf (ds D) (second A) (typed D (second A)))
      (Exp.tPi Exp.tSig)
      (let [[tag r X Y] A]
        (list (if (= tag 'Exp.tPi) 'Tl.fPi 'Tl.fSig) 'chkf (ds D) r X Y
          (form D X) (form (into [X] D) Y))))))
(defn- typed [D e]
  (cond
    (#{'Exp.tt 'Exp.ff} e) (list 'Tl.zConst 'chkf (ds D) e 'Exp.tBool 'rfl)
    (= 'Exp.var (first e))
    (let [i (second e)] (list 'Tl.zVar 'chkf (ds D) i (nth D i) (lookup 'nthE 'Exp D i)))
    (= 'Exp.ite (first e))
    (let [[_ x y z] e]
      (list 'Tl.zIte 'chkf (ds D) x y z 'Exp.tBool (typed D x) (typed D y) (typed D z)))))
(defn- skform [D A]
  (if (symbol? A)
    (list ({'Exp.tBool 'SkJ.wBool 'Exp.tUnit 'SkJ.wUnit 'Exp.tEmpty 'SkJ.wEmpty} A) (gs D))
    (case (first A)
      Exp.tT (list 'SkJ.wT (gs D) (second A) (sktyped D (second A)))
      (Exp.tPi Exp.tSig)
      (let [[tag r X Y] A]
        (list (if (= tag 'Exp.tPi) 'SkJ.wPi 'SkJ.wSig) (gs D) r X Y
          (skform D X) (skform (into [X] D) Y))))))
(defn- sktyped [D e]
  (cond
    (= e 'Exp.tt) (list 'SkJ.sTT (gs D))
    (= e 'Exp.ff) (list 'SkJ.sFF (gs D))
    (= 'Exp.var (first e))
    (let [i (second e) G (mapv sk D)]
      (list 'SkJ.sVar (gs D) i (nth G i) (lookup 'nthS 'Sk G i)))
    (= 'Exp.ite (first e))
    (let [[_ x y z] e]
      (list 'SkJ.sIte (gs D) x y z 'Sk.bool (sktyped D x) (sktyped D y) (sktyped D z)))))
(defn- refl [D A] (list 'Cv.cvRefl 'chkf (gs D) A (skform D A) 'rfl))
(defn- forward [D A B C h step]
  (list 'Cv.cvFwd 'chkf (gs D) A B C h step (skform D C) 'rfl))
(defn- convert [D u t A B ht hc]
  (list 'Rt.rConv 'chkf (ds D) (us u) t A B ht (form D B) hc))
(defn- variable [D u i]
  (list 'Rt.rVar 'chkf (ds D) (us u) i (nth D i) (nth u i) 'rfl
    (lookup 'nthE 'Exp D i) (lookup 'nthU 'U u i) 'rfl))

;; In the elimBool motive the bound Boolean is index 0, c is 1 and b is
;; 2. Under the evidence and Sigma binders c becomes index 3.
(def ^:private motive
  '(Exp.tPi U.u1 (Exp.tT (Exp.ite (Exp.var 0) (Exp.var 1) Exp.ff))
     (Exp.tSig U.u1 (Exp.tT (Exp.var 1)) (Exp.tT (Exp.var 3)))))
(defn- arm [truth]
  (let [A (ev (list 'Exp.ite truth (vr 0) 'Exp.ff))
        S (list 'Exp.tSig 'U.u1 (ev truth) (ev (vr 2)))
        body (if (= truth 'Exp.tt) (list 'Exp.pair S 'Exp.star (vr 0))
               (list 'Exp.abort S (vr 0)))]
    (list 'Exp.lam 'U.u1 A body)))
(def ^:private split-term
  (list 'Exp.lam 'U.uw 'Exp.tBool
    (list 'Exp.lam 'U.uw 'Exp.tBool
      (list 'Exp.elimB motive (vr 1) (arm 'Exp.tt) (arm 'Exp.ff)))))
(def ^:private split-type
  '(Exp.tPi U.uw Exp.tBool (Exp.tPi U.uw Exp.tBool
     (Exp.tPi U.u1 (Exp.tT (Exp.ite (Exp.var 1) (Exp.var 0) Exp.ff))
       (Exp.tSig U.u1 (Exp.tT (Exp.var 2)) (Exp.tT (Exp.var 2)))))))
(b/kdef! 'p49split_term 'Exp split-term)
(b/kdef! 'p49split_type 'Exp split-type)

(defn- arm-proof [truth]
  (let [A (ev (list 'Exp.ite truth (vr 0) 'Exp.ff))
        D [A 'Exp.tBool 'Exp.tBool] u ['U.u1 'U.uw 'U.u0]
        E (ev (list 'Exp.ite truth (vr 1) 'Exp.ff))
        S (list 'Exp.tSig 'U.u1 (ev truth) (ev (vr 2)))
        ite-step (list 'p49split_underT 'chkf (second E)
                   (if (= truth 'Exp.tt) (vr 1) 'Exp.ff)
                   (list (if (= truth 'Exp.tt) 'Hd.iteT 'Hd.iteF) 'chkf (vr 1) 'Exp.ff))
        h (forward D E E (ev (if (= truth 'Exp.tt) (vr 1) 'Exp.ff))
            (refl D E) ite-step)
        [_ _ _ body] (arm truth)
        hp
        (if (= truth 'Exp.tt)
          (list 'Rt.rPair 'chkf (ds D) (us ['U.u0 'U.u0 'U.u0]) (us u)
            'U.u1 (ev truth) (ev (vr 2)) 'Exp.star (vr 0) 'rfl
            (form D (ev truth)) (form (into [(ev truth)] D) (ev (vr 2)))
            (convert D ['U.u0 'U.u0 'U.u0] 'Exp.star 'Exp.tUnit (ev truth)
              (list 'Rt.rConst 'chkf (ds D) (us ['U.u0 'U.u0 'U.u0]) 'Exp.star 'Exp.tUnit 'rfl 'rfl)
              (list 'Cv.cvBwd 'chkf (gs D) 'Exp.tUnit 'Exp.tUnit (ev truth)
                (refl D 'Exp.tUnit)
                (list 'p49split_head 'chkf (ev truth) 'Exp.tUnit (list 'Hd.tTT 'chkf))
                (skform D (ev truth)) 'rfl))
            (convert D u (vr 0) E (ev (vr 1)) (variable D u 0) h))
          (list 'Rt.rAbort 'chkf (ds D) (us u) S (vr 0)
            (convert D u (vr 0) E 'Exp.tEmpty (variable D u 0)
              (forward D E (ev 'Exp.ff) 'Exp.tEmpty h
                (list 'p49split_head 'chkf (ev 'Exp.ff) 'Exp.tEmpty (list 'Hd.tTF 'chkf))))
            (form D S)))]
    (list 'Rt.rLam 'chkf (ds ['Exp.tBool 'Exp.tBool]) (us ['U.uw 'U.u0])
      'U.u1 A body S (form ['Exp.tBool 'Exp.tBool] A) hp)))

(a/prove-theorem 'p49split_typed '[chkf :- (=> Code Code Bool)]
  '(Rt chkf (List.nil Exp) (List.nil U) p49split_term p49split_type)
  [(list 'exact
     (list 'Rt.rLam 'chkf (ds []) (us []) 'U.uw 'Exp.tBool
       (nth split-term 3) (nth split-type 3) (form [] 'Exp.tBool)
       (list 'Rt.rLam 'chkf (ds ['Exp.tBool]) (us ['U.uw]) 'U.uw 'Exp.tBool
         (nth (nth split-term 3) 3) (nth (nth split-type 3) 3)
         (form ['Exp.tBool] 'Exp.tBool)
         (list 'Rt.rElimB 'chkf (ds ['Exp.tBool 'Exp.tBool])
           (us ['U.u0 'U.uw]) (us ['U.uw 'U.u0]) motive (vr 1) (arm 'Exp.tt) (arm 'Exp.ff)
           (variable ['Exp.tBool 'Exp.tBool] ['U.u0 'U.uw] 1)
           (form ['Exp.tBool 'Exp.tBool 'Exp.tBool] motive)
           (arm-proof 'Exp.tt) (arm-proof 'Exp.ff)))))])
