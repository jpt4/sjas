(ns lcert.formal.s52trace
  "F5 — Theorem 5.2: the safe-trace rules, for either evaluator
  (R4-metatheory.md §5, \"How traces compose\").

  Trace52 … erasing src v (safety52.clj) is Ok (a safe trace of evalₙ) when
  erasing is false and OkE (a safe trace of evalᴱₙ) when it is true.  The
  case lemmas of the fundamental property are stated once, for both
  evaluators, so they build traces through Trace52.  Every rule of Ok whose
  fields are the same as OkE's, once Ok and OkE are both read as Trace52,
  is lifted here to trace52_<rule>: cases on the flag, then the matching
  constructor.  The rule tables are read from erase.clj's declarations (the
  same reader safety52.clj's projections use), so no third copy of the
  evaluator is kept.  One rule differs between the two evaluators: a
  successful reflect, which runs the decoded program at the smaller budget
  (Ok) or an erasure of its derivation (OkE).  Its two cases are proved
  where the outer induction supplies that run.

  trace52_no_abort, trace52_no_h1, trace52_no_H: a safe trace never roots
  at an abort, H₁ or H node.  Ok and OkE apply this at every node they
  contain (every subterm evaluated, closure applied, recursor step and
  decoded program), which is the paper's \"the whole trace enters no abort,
  H₁ or H node\".  (These are corollaries for the reader; the proofs build
  traces and never need them.)"
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.erase :refer [badNode isH]]
            [lcert.formal.safety52]))

;; The two readings of Trace52, definitionally.
(thm trace52_false [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                    encTy :- (=> Exp Code), n :- Nat, src :- EvSrc, v :- RV]
  (Eq Prop (Trace52 chkf dec encTy n Bool.false src v) (Ok chkf dec encTy n src v))
  (rfl))

(thm trace52_true [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                   encTy :- (=> Exp Code), n :- Nat, src :- EvSrc, v :- RV]
  (Eq Prop (Trace52 chkf dec encTy n Bool.true src v) (OkE chkf dec encTy n src v))
  (rfl))

;; The constructor declarations of Ok and OkE, as data: each is
;; (name [field type]… :where [indices]).
(def ^:private fields @#'lcert.formal.safety52/trace-fields)
(def ^:private p3
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code)])

;; Read every Ok / OkE premise as a Trace52 premise at the flag `erasing`.
(defn- normalize [pred f]
  (walk/postwalk
    (fn [x]
      (if (and (seq? x) (= pred (first x)))
        (let [[_ chk dec enc n src v] x]
          (list 'Trace52 chk dec enc n 'erasing src v)) x)) f))

;; The rules whose two declarations agree after that reading: all but
;; eReflOk (checked by the suite, which names every lifted rule).
(def common-rules
  (let [es (into {} (for [[c & fs] (fields 'OkE)] [c fs]))]
    (vec (for [[c & fs] (fields 'Ok)
               :when (= (normalize 'Ok fs) (normalize 'OkE (es c)))] c))))

;; trace52_<rule>: the rule, for either evaluator.  Statement: for all the
;; rule's fields (premises read as Trace52), its conclusion as Trace52.
(doseq [[c & fs] (fields 'Ok) :when (some #{c} common-rules)]
  (let [bs (take-while vector? fs)
        [n src v] (last fs)
        goal (list 'Trace52 'chkf 'dec 'encTy n 'erasing src v)
        statement (reduce (fn [q [x ty]] (list 'forall [x (normalize 'Ok ty)] q)) goal (reverse bs))
        term (fn [pred] (apply list (symbol (str pred "." c)) 'chkf 'dec 'encTy (map first bs)))]
    (a/prove-theorem (symbol (str "trace52_" c))
      (lv (vec (concat p3 '[erasing :- Bool]))) (lv statement)
      (lv ['(cases erasing)
           (apply list 'intro (map first bs)) (list 'exact (term 'Ok))
           (apply list 'intro (map first bs)) (list 'exact (term 'OkE))]))))

;; No safe trace roots at abort, H₁ or H (reflect at 0).  srcBad src: src is
;; a term whose node is abort, H₁ or H (badNode, erase.clj).  The root of a
;; safe trace is never such a node: by induction on the trace, every rule's
;; root is a good node by computation, except the reflect rules, whose root
;; is good by their premise isH D = false.  (Inversion by `cases` on the
;; indexed families is not available in Ansatz here.) An explicit recursor
;; keeps the induction motive small: the induction tactic otherwise spends
;; minutes expanding the mutually indexed trace premises while elaborating.
(a/defn srcBad [src :- EvSrc] Bool
  (match src [(tm rho t) (badNode t)] [_ Bool.false]))

(doseq [[pred nm] '[[Ok ok52_root] [OkE okE52_root]]]
  (a/prove-theorem nm
    (lv (conj '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                encTy :- (=> Exp Code), n0 :- Nat, src0 :- EvSrc, v0 :- RV, der :-]
              (list pred 'chkf 'dec 'encTy 'n0 'src0 'v0)))
    (lv (list 'Eq 'Bool '(srcBad src0) 'Bool.false))
    (lv [(list 'exact
      (apply list (symbol (str pred ".rec")) 'chkf 'dec 'encTy
        (list 'fn ['n :- 'Nat, 'src :- 'EvSrc, 'v :- 'RV,
                   'h :- (list pred 'chkf 'dec 'encTy 'n 'src 'v)]
          '(Eq Bool (srcBad src) Bool.false))
        (concat
          (for [[_ & fs] (fields pred)
                :let [bs (take-while vector? fs)]]
            (list 'fn
              (vec (concat (mapcat (fn [[x ty]] [x :- ty]) bs)
                (mapcat (fn [[x ty]]
                  (when (and (seq? ty) (= pred (first ty)))
                    [(symbol (str "ih_" x)) :-
                     (list 'Eq 'Bool (list 'srcBad (nth ty 5)) 'Bool.false)])) bs)))
              (if (some #(= 'hH (first %)) bs) 'hH '(Eq.refl Bool.false))))
          '[n0 src0 v0 der])))])))

(doseq [[nm term fs] '[[abort (Exp.abort A t) [A Exp t Exp]]
                       [h1 (Exp.h1 r s c e1 e2) [r Exp s Exp c Exp e1 Exp e2 Exp]]
                       [H (Exp.refl Exp.tEmpty r e) [r Exp e Exp]]]]
  (let [extra (vec (mapcat (fn [[x t]] [x :- t]) (partition 2 fs)))]
    (a/prove-theorem (symbol (str "trace52_no_" nm))
      (lv (vec (concat p3 '[erasing :- Bool, cap :- Nat, rho :- (List RV)] extra '[v :- RV])))
      (lv (list 'Not (list 'Trace52 'chkf 'dec 'encTy 'cap 'erasing (list 'EvSrc.tm 'rho term) 'v)))
      (lv (vec (concat
        ['(cases erasing) '(intro h)
         (list 'have 'h3 '(Eq Bool Bool.true Bool.false)
               (list 'ok52_root 'chkf 'dec 'encTy 'cap (list 'EvSrc.tm 'rho term) 'v 'h))
         '(exact (Bool.noConfusion h3))
         '(intro h)
         (list 'have 'h3 '(Eq Bool Bool.true Bool.false)
               (list 'okE52_root 'chkf 'dec 'encTy 'cap (list 'EvSrc.tm 'rho term) 'v 'h))
         '(exact (Bool.noConfusion h3))]))))))
