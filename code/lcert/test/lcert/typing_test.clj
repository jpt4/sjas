(ns lcert.typing-test
  "Tests for the type checker, which builds explicit derivations following the
  rule table of R4-metatheory.md §1.4.

  Oracles: the draft's derivation of `not` (built exactly); the usage
  discipline (every resource violation is rejected); conversion (evidence
  types compute, and cannot be forged)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.syntax :as s]
            [lcert.typing :as t]
            [lcert.reduce :as r]
            [lcert.encode-test :refer [D0]]))

(defn- ty [form] (s/parse-type [] form))
(defn- tm [form] (s/parse-term [] form))

(defn- type-of
  "The type of a closed term, at budget 0."
  [form]
  (:type (t/check-top 0 (tm form))))

(defn- rejects?
  "True iff checking `form` at budget n raises a type error."
  [n form]
  (try (t/check-top n (s/parse-term (s/token-scope n) form)) false
       (catch clojure.lang.ExceptionInfo e (= :lcert/type-error (:type (ex-data e))))))

;; A stand-in for Check, so these tests do not depend on lcert.check: it
;; accepts exactly the one code it is told to.
(defn- check-accepting [good]
  (fn [c d] (= [c d] good)))

(deftest the-drafts-derivation-of-not
  (testing "the builder produces exactly the draft's §2.7 derivation"
    (is (= D0 (t/check-top 0 (tm '(fn [x w Bool] (if x ff tt)))))))
  (testing "its type"
    (is (= [:Pi :w [:Bool] [:Bool]] (type-of '(fn [x w Bool] (if x ff tt)))))))

(deftest basic-types
  (is (= [:Unit] (type-of 'star)))
  (is (= [:Nat] (type-of '(succ (succ zero)))))
  (is (= [:Syn] (type-of '(snode :a (sleaf :b) (sleaf :c)))))
  (is (= [:Lbl] (type-of :a)))
  (is (= [:Sigma :w [:Bool] [:Bool]] (type-of '(pair (prod Bool Bool) tt ff))))
  (is (= [:Nat] (type-of '(rec-nat [k Nat] zero [k y] (succ y) 3))))
  (is (= [:Lbl] (type-of '(rec-syn [x Lbl] [a] a [a c1 c2 y1 y2] a (sleaf :b)))))
  (is (= [:Bool] (type-of '(case-lbl [z Bool] :a {:a tt :else ff}))))
  (is (= [:Bool] (type-of '(let-pair Bool [a b] (pair (prod Bool Bool) tt ff) a)))))

(deftest dependent-elimination
  (testing "stability of T: ((T b → 0) → 0) → T b, by elimBool on b"
    (is (= (ty '(Pi [b w Bool] (-> (-> (-> (T b) Void) Void) (T b))))
           (type-of '(fn [b w Bool]
                       (elim-bool [x (-> (-> (-> (T x) Void) Void) (T x))] b
                                  (fn [k w (-> (-> (T tt) Void) Void)] star)
                                  (fn [k w (-> (-> (T ff) Void) Void)]
                                    (k (fn [z w (T ff)] z)))))))))
  (testing "abort from evidence of T(ff)"
    (is (= (ty '(-o (T ff) Bool))
           (type-of '(fn [e 1 (T ff)] (abort Bool e)))))))

(deftest usage-discipline
  (testing "a usage-1 variable used twice is rejected"
    (is (rejects? 0 '(fn [x 1 Bool] (if x x ff))))
    (is (rejects? 0 '(fn [r 1 R] (pair (tensor R R) r r)))))
  (testing "an erased variable used at runtime is rejected"
    (is (rejects? 0 '(fn [x 0 Bool] x))))
  (testing "an erased variable may be used at type level"
    (is (= (ty '(Pi [b 0 Bool] (-o (T b) (T b))))
           (type-of '(fn [b 0 Bool] (fn [e 1 (T b)] e))))))
  (testing "a token used twice is rejected"
    (is (rejects? 1 '(node $1 :a (node $1 :a (leaf :a) (leaf :a)) (leaf :a)))))
  (testing "a token captured by an iteration method is rejected"
    (is (rejects? 1 '(itr R (fn [l w Lbl] (node $1 l (leaf l) (leaf l)))
                         (fn [d 1 Dia] (fn [l w Lbl] (fn [u 1 R] (fn [v 1 R] (node d l u v)))))
                         (leaf :a)))))
  (testing "a token passed at usage ω is rejected"
    (is (rejects? 1 '((fn [d w Dia] (leaf :a)) $1))))
  (testing "no closed term builds a node: there is no token to give it"
    (is (rejects? 0 '(node star :a (leaf :a) (leaf :a)))))
  (testing "with tokens, a node is built, each token once"
    (is (= [:R] (:type (t/check-top 2 (s/parse-term (s/token-scope 2)
                                                   '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c))))))))
  (testing "branches of if share their context: each may use the same token"
    (is (= [:R] (:type (t/check-top 1 (s/parse-term (s/token-scope 1)
                                                   '(if tt (node $1 :a (leaf :a) (leaf :a)) (leaf :b)))))))))

(deftest the-root-context-is-exactly-the-budget
  (testing "unused tokens are still declared, at usage 1"
    (let [d (t/check-top 3 (s/parse-term (s/token-scope 3) 'star))]
      (is (= (vec (repeat 3 [1 [:Dia]])) (:ctx d))))))

(deftest type-errors
  (is (rejects? 0 '(if tt tt star)))
  (is (rejects? 0 '(succ tt)))
  (is (rejects? 0 '(tt ff)))
  (is (rejects? 0 '(reflect (-> Bool Bool) (leaf :a) star)))
  (testing "evidence cannot be forged: ⋆ does not inhabit T(ff)"
    (binding [r/*check* (fn [c d] false)]
      (is (rejects? 0 '(pair (Sigma [r 1 R] (T (chk (print r) c-bot))) (leaf :a) star))))))

(deftest conversion-is-recorded
  (testing "⋆ at T(tt) needs a conversion node whose chain runs 1 → T(tt)"
    (let [d (t/check-top 0 (tm '(pair (prod Unit (T tt)) star star)))
          conv (nth (:prems d) 2)]
      (is (= :Conv (:rule conv)))
      (is (= [[:Unit] [:T [:tt]]] (:chain conv)))))
  (testing "evidence computed by δ: ⋆ checks at T(chk c ⌜Bool → Bool⌝) iff Check says so"
    (let [code (s/code->term [:sn :a [:sl :b] [:sl :b]])
          S (ty '(Sigma [c w Syn] (T (chk c (code (-> Bool Bool))))))
          good [[:sn :a [:sl :b] [:sl :b]] [:sn :arrow [:sl :bool] [:sl :bool]]]]
      (binding [r/*check* (check-accepting good)]
        (is (= :Pair (:rule (t/check-top 0 [:pair S code [:star]])))))
      (binding [r/*check* (fn [c d] false)]
        (is (thrown? clojure.lang.ExceptionInfo (t/check-top 0 [:pair S code [:star]])))))))

(deftest self-reference-constants
  (testing "H is a closed inhabitant of H°"
    (is (= (ty '(Pi [r 1 R] (-o (T (chk (print r) c-bot)) Void)))
           (type-of '(fn [r 1 R] (fn [e 1 (T (chk (print r) c-bot))] (H r e)))))))
  (testing "H₁ is a closed inhabitant of H₁°"
    (is (= (ty '(Pi [r 1 R] (Pi [s 1 R] (Pi [c w Syn]
               (-o (T (chk (print r) c)) (T (chk (print s) (neg c))) Void)))))
           (type-of '(fn [r 1 R s 1 R c w Syn e1 1 (T (chk (print r) c)) e2 1 (T (chk (print s) (neg c)))]
                       (H1 r s c e1 e2))))))
  (testing "reflect targets base data types only"
    (is (rejects? 0 '(fn [r 1 R] (fn [e 1 (T (chk (print r) (code (T tt))))]
                                   (reflect (T tt) r e))))))
  (testing "inspect hands the certificate back to the branch that runs"
    (is (= (ty '(-o R R))
           (type-of '(fn [r 1 R] (inspect R r c-bot [x e] x [x e] x)))))))
