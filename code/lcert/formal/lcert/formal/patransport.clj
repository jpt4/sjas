(ns lcert.formal.patransport
  "F6.3 — the packaged TRANSPORT combinator of R4 §6.2."
  (:require [ansatz.core :as a]
            [clojure.walk :as w]
            [lcert.formal.base :as b]
            [lcert.formal.patyp :as pt]
            [lcert.formal.check-dt :as dt]
            [lcert.formal.check-hd :as hd]
            [lcert.formal.dtgen :as g]))

;; R4 section 6.2, closed lemma 2. The inner recursors may use q at
;; omega only after opening the package; their result variable stays affine.
(def FT '(Π :uw [n Nat] Bool))
(defn Qty [x] (w/postwalk-replace {'X x 'FT FT}
  '(Π :uw [y Nat] (Π :uw [f FT] (Π :uw [e (T (ap EQ X y))] (Π :uw [a (T (ap f X))] (T (ap f y))))))))
(defn motive [X0] (w/postwalk-replace {'X0 X0 'FT FT}
  '(Π :uw [f FT] (Π :uw [e (T (ap EQ X0 yy))] (Π :uw [a (T (ap f X0))] (T (ap f yy)))))))
(def BASE (list 'λ :uw '[y Nat]
  (list 'recN ['yy (motive 'zero)]
    (w/postwalk-replace {'FT FT} '(λ :uw [f FT] (λ :uw [e (T (ap EQ zero zero))] (λ :uw [a (T (ap f zero))] a))))
    ['y1 'r (w/postwalk-replace {'FT FT} '(λ :uw [f FT] (λ :uw [e (T (ap EQ zero (succ y1)))] (λ :uw [a (T (ap f zero))] (abort (T (ap f (succ y1))) e)))))]
    'y)))
(def STEP (list 'λ :uw '[y Nat]
  (list 'recN ['yy (motive '(succ x1))]
    (w/postwalk-replace {'FT FT} '(λ :uw [f FT] (λ :uw [e (T (ap EQ (succ x1) zero))] (λ :uw [a (T (ap f (succ x1)))] (abort (T (ap f zero)) e)))))
    ['y1 'r (w/postwalk-replace {'FT FT} '(λ :uw [f FT] (λ :uw [e (T (ap EQ (succ x1) (succ y1)))] (λ :uw [a (T (ap f (succ x1)))]
                                     (ap q y1 (λ :uw [z Nat] (ap f (succ z))) e a)))))]
    'y)))
(def TRANSPORT (list 'λ :uw '[x Nat]
  (list 'letp (Qty 'x)
    (list 'recN ['xx (list 'Σ :uw ['q (Qty 'xx)] 'Unit)]
      (list 'pair (list 'Σ :uw ['q (Qty 'zero)] 'Unit) BASE '⋆)
      ['x1 'h (list 'letp (list 'Σ :uw ['q (Qty '(succ x1))] 'Unit) 'h '[q u]
                (list 'pair (list 'Σ :uw ['q (Qty '(succ x1))] 'Unit) STEP '⋆))]
      'x)
    '[q u] 'q)))

;; A bounded reflection proof emitter, local to TRANSPORT. Each large
;; DT node becomes an application of its original inductive rule to named
;; subtheorems. Small subtrees are still checked by dtCheck_sound. No
;; theorem is installed without the ordinary elaborator and kernel check.
(defn split-proof!
  "Check a concrete DT in bounded reflection chunks joined by original rules."
  [prefix root]
  (let [rules (into {} (map (fn [[family rule]]
                             [(symbol (str "DT." (first rule))) [family rule]]) dt/dt-rules))
        memo (atom {}) serial (atom 0)
        nodes (memoize (fn size [x] (if (seq? x) (reduce + 1 (map size x)) 1)))
        fresh (fn [] (symbol (str prefix "_" (swap! serial inc))))]
    (letfn [(prove [tree]
              (or (@memo tree)
                (let [[family rule] (rules (first tree))
                      _ (assert rule (str "Not a DT node: " (first tree)))
                      fields (hd/f7-fields rule)
                      vals (zipmap (map first (remove hd/f7-side? fields)) (rest tree))
                      sub #(w/postwalk-replace vals %)
                      args (sub (if (= family 'Cv) (cons 'G (last rule)) (last rule)))
                      goal (apply list family 'chkf args)
                      proof
                      (if (<= (nodes tree) 4000)
                        (list 'dtCheck_sound 'chkf tree
                          (apply list ({'Rt 'DTJ.rt 'Tl 'DTJ.tl 'Cv 'DTJ.cv} family) args)
                          '(Eq.refl Bool.true))
                        (apply list (symbol (str family "." (first rule))) 'chkf
                          (for [[x ty :as field] fields]
                            (cond
                              (hd/f7-side? field) (list 'Eq.refl (sub (last ty)))
                              (and (seq? ty) (#{'Tl 'Rt 'Cv} (first ty))) (prove (vals x))
                              (and (seq? ty) (= 'SkJ (first ty)))
                              (apply list 'skjCheck_sound (vals x)
                                (concat (rest (sub ty)) ['(Eq.refl Bool.true)]))
                              (and (seq? ty) (= 'Step (first ty)))
                              (list 'stepDTCheck_sound 'chkf (vals x) '(Eq.refl Bool.true))
                              :else (vals x)))))
                      nm (fresh)
                      ;; Share expressions as well as trees, but never proof
                      ;; constants: each proof is an ordinary opaque theorem.
                      [defs bundle] (g/share (str nm "_data") 30 (list goal proof))
                      [goal proof] bundle]
                  (doseq [[name sort body] defs] (b/kdef! name sort body))
                  (a/prove-theorem nm '[chkf :- (=> Code Code Bool)] goal [(list 'exact proof)])
                  (let [result (list nm 'chkf)]
                    (swap! memo assoc tree result)
                    result))))]
      (prove root))))

(def transport-term (g/compile-term pt/constants TRANSPORT))
(def transport-type (g/compile-term pt/constants (list 'Π :uw '[x Nat] (Qty 'x))))

(binding [g/*names* pt/names]
  (b/kdef! 'paC_TRANSPORT 'Exp (g/sx transport-term))
  (b/kdef! 'paTransportTy 'Exp (g/sx transport-type))
  (let [proof (split-proof! "paTransport_rt" (g/rt [] [] transport-term transport-type))]
    (a/prove-theorem 'paC_TRANSPORT_rt '[chkf :- (=> Code Code Bool)]
      '(Rt chkf (List.nil Exp) (List.nil U) paC_TRANSPORT paTransportTy)
      [(list 'exact proof)]))
  (let [proof (split-proof! "paTransport_tl" (g/tl-form [] transport-type))]
    (a/prove-theorem 'paTransport_form '[chkf :- (=> Code Code Bool)]
      '(Tl chkf Bool.true (List.nil Exp) paTransportTy Exp.tUnit)
      [(list 'exact proof)])))
