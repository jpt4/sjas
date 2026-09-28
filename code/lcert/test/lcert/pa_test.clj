(ns lcert.pa-test
  "Tests for the embedding of Peano arithmetic into λᶜᵉʳᵗ₀ at budget 0, step 1
  of the proof of Proposition 5 (R4-metatheory.md §6).

  The embedding maps each PA-proof line φ to a closed term of the translated
  universal closure of φ.  What is tested here is exactly what the proof
  needs of it: every template is usage-correct, has the translated type, and
  yields a derivation that Check accepts; and whole PA proofs translate."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.pa :as pa]
            [lcert.syntax :as s]
            [lcert.typing :as t]
            [lcert.encode :as e]
            [lcert.check :as c]))

(defn- certified?
  "Does the closed surface term type-check at budget 0 at the closure type of
  formula phi, with a derivation that Check accepts?"
  [term phi]
  (let [d (t/check-top 0 (s/parse-term [] term))
        want (s/parse-type [] (pa/closure-type phi))]
    (and (= want (:type d))
         (c/check (e/enc-deriv d) (e/enc-exp want)))))

;; ---------------------------------------------------------------------------
;; Random formulas over the variables x, y, z.

(def ^:private rng (java.util.Random. 20260926))

(defn- pick [xs] (nth xs (.nextInt ^java.util.Random rng (count xs))))

(defn- rand-term [d]
  (if (zero? d)
    (pick ['x 'y 'z [:z]])
    (pick [(rand-term 0) [:s (rand-term (dec d))]
           [:+ (rand-term (dec d)) (rand-term (dec d))]
           [:* (rand-term (dec d)) (rand-term (dec d))]])))

(defn- rand-formula [d]
  (if (zero? d)
    (pick [[:= (rand-term 1) (rand-term 1)] [:bot]])
    (case (.nextInt ^java.util.Random rng 4)
      0 [:= (rand-term 2) (rand-term 1)]
      1 [:-> (rand-formula (dec d)) (rand-formula (dec d))]
      2 [:all (pick ['x 'y 'z]) (rand-formula (dec d))]
      3 [:bot])))

(defn- samples [n d] (repeatedly n #(rand-formula d)))

;; ---------------------------------------------------------------------------

(deftest the-translation-of-formulas
  (testing "atoms become T(EQ s t); ⊥ becomes Void; → and ∀ become Π at ω"
    (is (= '(Pi [v_x w Nat] (-> (T (EQ v_x v_x)) Void))
           (pa/closure-type-abbrev [:-> [:= 'x 'x] [:bot]]))))
  (testing "EQ, PLUS and TIMES compute as the arithmetic axioms need"
    (is (true? (pa/computes? '(EQ (succ zero) (succ zero)) 'tt)))
    (is (true? (pa/computes? '(EQ (succ zero) zero) 'ff)))
    (is (true? (pa/computes? '(PLUS (succ zero) (succ zero)) '(succ (succ zero)))))
    (is (true? (pa/computes? '(TIMES (succ (succ zero)) (succ (succ zero)))
                             '(succ (succ (succ (succ zero)))))))))

(deftest every-scheme-is-usage-correct-and-checks
  (testing "the propositional schemes"
    (doseq [[f g h] (partition 3 (samples 18 2))]
      (is (certified? (pa/axiom-term [:A1 f g]) (pa/instance [:A1 f g])) (pr-str [:A1 f g]))
      (is (certified? (pa/axiom-term [:A2 f g h]) (pa/instance [:A2 f g h])) (pr-str [:A2 f g h]))))
  (testing "double negation, through stability of every formula"
    (doseq [f (samples 10 3)]
      (is (certified? (pa/axiom-term [:DN f]) (pa/instance [:DN f])) (pr-str [:DN f]))))
  (testing "the quantifier schemes"
    (doseq [f (samples 8 2)
            :let [t (rand-term 1)]
            :when (pa/substitutable? t 'x f)]
      (is (certified? (pa/axiom-term [:A4 'x f t]) (pa/instance [:A4 'x f t])) (pr-str [:A4 'x f t])))
    (doseq [[f g] (partition 2 (samples 16 2))
            :when (not (contains? (pa/free-vars f) 'x))]
      (is (certified? (pa/axiom-term [:A5 'x f g]) (pa/instance [:A5 'x f g])) (pr-str [:A5 'x f g]))))
  (testing "equality: reflexivity, and transport for atomic formulas"
    (is (certified? (pa/axiom-term [:E1 'x]) (pa/instance [:E1 'x])))
    (doseq [_ (range 6)
            :let [atom [:= (rand-term 2) (rand-term 1)]]]
      (is (certified? (pa/axiom-term [:E2 'x 'y 'z atom]) (pa/instance [:E2 'x 'y 'z atom]))
          (pr-str atom))))
  (testing "the arithmetic axioms"
    (doseq [ax [[:Q1 'x] [:Q2 'x 'y] [:Q3 'x] [:Q4 'x 'y] [:Q5 'x] [:Q6 'x 'y]]]
      (is (certified? (pa/axiom-term ax) (pa/instance ax)) (pr-str ax))))
  (testing "induction, through the !P packaging"
    (doseq [f (samples 6 2)]
      (is (certified? (pa/axiom-term [:IND 'x f]) (pa/instance [:IND 'x f])) (pr-str [:IND 'x f])))))

(def phi-implies-phi
  "The standard five-line proof of φ → φ, for φ := (x = y)."
  (let [f [:= 'x 'y]
        ff [:-> f f]]
    [{:formula (pa/instance [:A2 f ff f]) :by [:A2 f ff f]}
     {:formula (pa/instance [:A1 f ff]) :by [:A1 f ff]}
     {:formula [:-> [:-> f ff] ff] :by [:MP 1 0]}
     {:formula (pa/instance [:A1 f f]) :by [:A1 f f]}
     {:formula ff :by [:MP 3 2]}]))

(def all-x-refl-by-induction
  "⊢ ∀x (x = x), by the induction axiom (not by E1 and Gen alone)."
  (let [xx [:= 'x 'x]
        sxsx [:= [:s 'x] [:s 'x]]
        zz [:= [:z] [:z]]]
    [{:formula xx :by [:E1 'x]}                                          ; 0
     {:formula [:all 'x xx] :by [:Gen 0 'x]}                             ; 1
     {:formula (pa/instance [:A4 'x xx [:z]]) :by [:A4 'x xx [:z]]}      ; 2  ∀x(x=x) → 0=0
     {:formula zz :by [:MP 1 2]}                                         ; 3
     {:formula (pa/instance [:A4 'x xx [:s 'x]]) :by [:A4 'x xx [:s 'x]]} ; 4 ∀x(x=x) → Sx=Sx
     {:formula sxsx :by [:MP 1 4]}                                       ; 5
     {:formula (pa/instance [:A1 sxsx xx]) :by [:A1 sxsx xx]}            ; 6  Sx=Sx → (x=x → Sx=Sx)
     {:formula [:-> xx sxsx] :by [:MP 5 6]}                              ; 7
     {:formula [:all 'x [:-> xx sxsx]] :by [:Gen 7 'x]}                  ; 8
     {:formula (pa/instance [:IND 'x xx]) :by [:IND 'x xx]}              ; 9
     {:formula [:-> [:all 'x [:-> xx sxsx]] [:all 'x xx]] :by [:MP 3 9]} ; 10
     {:formula [:all 'x xx] :by [:MP 8 10]}]))                           ; 11

(deftest pa-proofs
  (testing "the PA proof checker accepts valid proofs and rejects invalid ones"
    (is (true? (pa/check-proof phi-implies-phi)))
    (is (true? (pa/check-proof all-x-refl-by-induction)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pa/check-proof (assoc-in phi-implies-phi [4 :by] [:MP 2 3]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pa/check-proof [{:formula (pa/instance [:A4 'x [:all 'y [:= 'x 'y]] 'y])
                                   :by [:A4 'x [:all 'y [:= 'x 'y]] 'y]}])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pa/check-proof [{:formula [:-> [:all 'x [:-> [:= 'x 'x] [:bot]]] [:-> [:= 'x 'x] [:all 'x [:bot]]]]
                                   :by [:A5 'x [:= 'x 'x] [:bot]]}]))))
  (testing "whole proofs translate to closed, checked derivations of their conclusions"
    (is (certified? (pa/proof-term phi-implies-phi) (:formula (peek phi-implies-phi))))
    (is (certified? (pa/proof-term all-x-refl-by-induction) (:formula (peek all-x-refl-by-induction))))))

(deftest a-proof-of-0=1-would-be-a-refutation
  (testing "T(EQ 0 1) converts to Void, so the last line of such a proof is a closed refutation"
    (is (true? (pa/computes? '(EQ zero (succ zero)) 'ff)))
    ;; typed in a context holding a hypothetical e : T(EQ 0 1)
    (let [ctx [[:w (s/parse-type [] (pa/expand '(T (EQ zero (succ zero)))))]]
          d (t/synth 1 ctx (s/parse-term '[e] (pa/refutation-term 'e)))]
      (is (= [:Empty] (:type d))))))
