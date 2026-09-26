(ns lcert.core-test
  "End-to-end tests of the public API, and executable versions of results in
  R4-metatheory.md: Proposition 4.10 (H° from H₁ at a constant budget, from
  review R4-02), the definable destructor of §4.7 (review R4-03),
  Proposition 4.9 at depth 0 (bounded code consistency with no tokens), and
  Theorem 5.2 (typed programs never evaluate abort, H₁ or H)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.core :as lc]
            [lcert.examples :as ex]
            [lcert.eval :as ev]
            [lcert.kernel :as k]
            [lcert.pa :as pa]
            [lcert.syntax :as s]
            [lcert.typing :as t]))

(deftest the-api
  (testing "certify a closed program, then check its certificate"
    (let [{:keys [code type nodes budget]} (lc/certify 0 ex/not-form)]
      (is (= [:Pi :w [:Bool] [:Bool]] type))
      (is (= 35 nodes))
      (is (= 0 budget))
      (is (true? (lc/check code '(-> Bool Bool))))
      (is (false? (lc/check code '(-> Nat Nat))))))
  (testing "run a program"
    (is (= true (lc/run 0 (list ex/not-form 'ff))))))

(deftest h-from-h1-at-constant-budget
  (testing "Prop 4.10: Θ_K ⊢ λr e. H₁ r lit_v c⊥ e ⋆ : H°, with K the size of a fixed certificate"
    (let [{:keys [budget term]} (ex/h-from-h1)
          d (lc/check-term budget term)
          code (lc/encode d)]
      (is (= ex/h-type (:type d)))
      (is (true? (lc/check-code code (lc/type-code ex/h-type))))
      ;; the budget is the fixed certificate's size, whatever refutation is fed in
      (is (= budget (k/nodes (:code (lc/certify 0 ex/zero-lolli-zero))))))))

(deftest the-definable-destructor
  (testing "§4.7: roll (out r) gives back r, token for token"
    (let [r '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c))
          v (lc/run 2 r)
          w (lc/run 2 (ex/roll-out r))]
      (is (= v w))))
  (testing "out exposes a node's token and children: take the left child"
    (let [left (lc/run 2 (ex/left-child '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c))))]
      (is (= :b (nth left 2)))
      (is (= 1 (count (filter #(= :rn %) (flatten left)))))))
  (testing "out and roll are closed: they use no tokens of their own"
    (is (= 0 (:budget (lc/certify 0 ex/roll-form))))
    (is (= 0 (:budget (lc/certify 0 ex/out-form))))))

(deftest bounded-code-consistency-at-depth-0
  (testing "Prop 4.9 at k = 0: a closed, token-free proof over every code of depth 0"
    (let [{:keys [type budget]} (lc/certify 0 (ex/bounded-con 0))]
      (is (= 0 budget))
      (is (= (ex/bounded-con-type 0) type)))))

(deftest parsing-a-runtime-code-into-a-certificate
  (testing "review RR2-10: a typed parser threads a supply of tokens through a code"
    (let [supply '(node $1 :a (leaf :a) (node $2 :a (leaf :a) (node $3 :a (leaf :a) (leaf :a))))
          code [:sn :b [:sn :c [:sl :a] [:sl :a]] [:sl :a]]
          prog (fn [C body] (ex/parse-then (list 'code-literal code) supply C body))]
      (testing "the parser is closed: it costs no tokens of its own"
        (is (= 0 (:budget (lc/certify 0 ex/parse-form)))))
      (testing "with enough supply, the certificate prints back to the code"
        (is (= code (lc/run 3 (prog 'Syn '(print t))))))
      (testing "one supply node per certificate node; the rest is returned"
        (is (= 1 (count (filter #(= :rn %) (flatten (lc/run 3 (prog 'R 'rest))))))))
      (testing "with too little supply, the result is truncated, which print reveals"
        (let [short '(node $1 :a (leaf :a) (leaf :a))]
          (is (not= code (lc/run 1 (ex/parse-then (list 'code-literal code) short 'Syn '(print t))))))))))

(deftest certificate-forms
  (testing "certificate-form: the surface program building a code's certificate from $1..$k"
    (let [{:keys [code nodes]} (lc/certify 0 ex/not-form)
          v (lc/run nodes (lc/certificate-form code))]
      (is (= 35 (count (filter #(= :rn %) (flatten v)))))
      (is (= code (ev/print-value v))))))

;; ---------------------------------------------------------------------------
;; Theorem 5.2: a typed program never evaluates an abort, H₁ or H node, under
;; either evaluator.  In each program below such a node lies on a branch the
;; run does not take, so an evaluator taking a wrong branch would enter it;
;; the probe ev/*unreachable* records any node entered.

(def ^:private guard
  "The tutorial's guard: the branch taken if a certificate checks as a
  refutation holds abort and H."
  '(fn [r 1 R] (inspect Nat r c-bot [x e] (abort Nat (H x e)) [x e] 7)))

(def ^:private h1-guard
  "Checks r at Bool → Bool, then s at its negation.  The branch where both
  checks pass holds H₁."
  '(fn [r 1 R s 1 R]
     (inspect Nat r (code (-> Bool Bool))
              [x e] (inspect Nat s (neg (code (-> Bool Bool)))
                             [y e2] (abort Nat (H1 x y (code (-> Bool Bool)) e e2))
                             [y e2] 1)
              [x e] 2)))

(def ^:private by-cases
  "Dependent elimination whose false branch needs evidence of T(ff), so
  holds abort."
  '(fn [b w Bool] (elim-bool [x (-o (T x) Nat)] b
                             (fn [e 1 (T tt)] 5)
                             (fn [e 1 (T ff)] (abort Nat e)))))

(defn- random-codes
  "n random codes over the labels :a :b :c with at most 6 internal nodes,
  from a fixed seed."
  [n]
  (let [rng (java.util.Random. 20260926)
        lbl (fn [] (nth [:a :b :c] (.nextInt rng 3)))]
    (letfn [(tree [k]
              (if (zero? k)
                [:sl (lbl)]
                (let [i (.nextInt rng k)]
                  [:sn (lbl) (tree i) (tree (- k 1 i))])))]
      (vec (repeatedly n #(tree (.nextInt rng 7)))))))

(defn- run-probed
  "Evaluate the closed surface program at budget n under both evaluators:
  the non-erasing one of Theorem 4, then the erasing one the language runs
  on.  Every abort, H₁ or H node entered is recorded in the atom seen.
  Returns the erasing evaluator's result."
  [seen n form]
  (let [d (t/check-top n (s/parse-term (s/token-scope n) form))]
    (binding [ev/*unreachable* (fn [kind] (swap! seen conj kind))]
      (ev/eval-deriv d n {:erase? false})
      (ev/eval-deriv d n {:erase? true}))))

(deftest typed-programs-never-evaluate-abort-h1-or-h
  (let [seen (atom [])
        not-code (:code (lc/certify 0 ex/not-form))]
    (testing "the guard, on random certificates and on not's certificate"
      (doseq [code (conj (random-codes 12) not-code)]
        (is (= 7 (run-probed seen (k/nodes code) (list guard (lc/certificate-form code))))
            (pr-str code))))
    (testing "the destructor: a leaf's view carries a T(ff) ⊸ K function made with abort"
      (let [r '(node $1 :a (leaf :b) (node $2 :c (leaf :b) (leaf :b)))]
        (is (= [:sn :a [:sl :b] [:sn :c [:sl :b] [:sl :b]]]
               (ev/print-value (run-probed seen 2 (ex/roll-out r)))))
        (is (= [:sl :b] (ev/print-value (run-probed seen 2 (ex/left-child r)))))))
    (testing "the parser, which takes its supply apart with the destructor"
      (is (= [:sn :b [:sn :c [:sl :a] [:sl :a]] [:sl :a]]
             (run-probed seen 3 (ex/parse-then '(code-literal [:sn :b [:sn :c [:sl :a] [:sl :a]] [:sl :a]])
                                               '(node $1 :a (leaf :a) (node $2 :a (leaf :a) (node $3 :a (leaf :a) (leaf :a))))
                                               'Syn '(print t))))))
    (testing "H₁ behind a check of a type and then of its negation"
      (is (= 1 (run-probed seen 36 (list h1-guard (lc/certificate-form not-code)
                                         '(node $36 :a (leaf :a) (leaf :a))))))
      (is (= 2 (run-probed seen 2 (list h1-guard '(node $1 :a (leaf :a) (leaf :a))
                                        '(node $2 :b (leaf :b) (leaf :b)))))))
    (testing "PA's transport (axiom E2): its recursion carries abort in the mismatched cases"
      (let [e2 (pa/axiom-term [:E2 'x 'y 'z [:= 'z [:s [:s [:s [:z]]]]]])]
        (is (= :star (run-probed seen 0 (list e2 3 3 'star 'star))))))
    (testing "dependent elimination: the false branch needs evidence of T(ff), so holds abort"
      (is (= 5 (run-probed seen 0 (list by-cases 'tt 'star)))))
    (testing "a certified program with a dead abort, run by reflect"
      (let [{:keys [code nodes]} (lc/certify 0 (list by-cases 'tt 'star))]
        (is (= 5 (run-probed seen nodes (list 'reflect 'Nat (lc/certificate-form code) 'star))))))
    (testing "no abort, H₁ or H node was entered, by either evaluator"
      (is (= [] @seen)))))
