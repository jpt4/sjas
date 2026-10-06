(ns lcert.formal.patyp
  "F6.3b — the closed arithmetic typings for R4-metatheory.md §6.2.

  trISZERO_rt, trPRED_rt, trEQ_rt, trPLUS_rt and trTIMES_rt prove Rt in
  the empty context with the empty usage vector, at every checker. The
  corresponding trEQ_tl, trPLUS_tl and trTIMES_tl prove Tl false: these
  constants occur inside translated types, whose formation needs Tl.

  Each proof is reflection through F7's dtCheck_sound. The Clojure surface
  terms below are only inputs to the untrusted generator; the conclusion
  names the existing patr constant, so the kernel also checks agreement
  with its actual definition. No checker specification is assumed, and no
  rule, PA definition, or translation is changed. These are the closed
  arithmetic subderivations of F6.3, not the proof-translation theorem."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b]
            [lcert.formal.patr]
            [lcert.formal.check-der]
            [lcert.formal.dtgen :as g]))

;; R4 §6.2's five concrete programs, in the generator's named syntax.
;; Keep PLUS's first binder at 1: TIMES passes its affine recursive result
;; there, and using an omega binder would scale that result to omega.
(def constants
  (reduce (fn [cs [nm form]] (assoc cs nm (g/compile-term cs form))) {}
    '[[ISZERO (λ :uw [b Nat] (recN [x Bool] tt [k y ff] b))]
      [PRED (λ :uw [b Nat] (recN [x Nat] zero [k y k] b))]
      [EQ (λ :uw [a Nat]
            (recN [x (Π :uw [_ Nat] Bool)] ISZERO
              [k h (λ :uw [b Nat]
                     (ite (ap ISZERO b) ff (ap h (ap PRED b))))] a))]
      [PLUS (λ :u1 [a Nat]
              (λ :uw [b Nat] (recN [x Nat] a [k y (succ y)] b)))]
      [TIMES (λ :uw [a Nat]
               (λ :uw [b Nat] (recN [x Nat] zero [k y (ap PLUS y a)] b)))]]))

;; Folding only closed subterms keeps repeated conversion and formation
;; trees small. Every such name is an existing kernel-checked definition.
(def names
  (into {} (for [[nm e] constants] [e (symbol (str "tr" nm))])))

(def signatures
  '[[ISZERO (Π :uw [n Nat] Bool)]
    [PRED (Π :uw [n Nat] Nat)]
    [EQ (Π :uw [a Nat] (Π :uw [b Nat] Bool))]
    [PLUS (Π :u1 [a Nat] (Π :uw [b Nat] Nat))]
    [TIMES (Π :uw [a Nat] (Π :uw [b Nat] Nat))]])

(defn- prove-closed!
  "Install a closed Rt or Tl theorem by checked reflection, sharing large
  repeated data before elaboration. The requested conclusion uses the
  kernel constant, not the generator's copy of its expression."
  [nm mode e ty]
  (binding [g/*names* names]
    (let [kn (symbol (str "tr" nm))
          pn (symbol (str kn (if (= mode :rt) "_rt" "_tl")))
          tree (if (= mode :rt) (g/rt [] [] e ty) (g/tl0-check [] e ty))
          [defs tree] (g/share (str "patyp_" pn) 30 tree)
          args (if (= mode :rt)
                 (list '(List.nil Exp) '(List.nil U) kn (g/sx ty))
                 (list 'Bool.false '(List.nil Exp) kn (g/sx ty)))
          J (apply list (if (= mode :rt) 'DTJ.rt 'DTJ.tl) args)
          goal (apply list (if (= mode :rt) 'Rt 'Tl) 'chkf args)]
      (doseq [[name sort body] defs] (b/kdef! name sort body))
      (a/prove-theorem pn '[chkf :- (=> Code Code Bool)] goal
        [(list 'exact (list 'dtCheck_sound 'chkf tree J '(Eq.refl Bool.true)))]))))

(doseq [[nm form] signatures]
  (let [ty (g/compile-term constants form)]
    (prove-closed! nm :rt (constants nm) ty)
    (when (#{'EQ 'PLUS 'TIMES} nm)
      (prove-closed! nm :tl (constants nm) ty))))
