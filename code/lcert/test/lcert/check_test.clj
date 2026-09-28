(ns lcert.check-test
  "Tests for Check (R4-metatheory.md §1.6): the structural validator of
  encoded derivations, independent of the type checker that builds them."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.typing :as t]
            [lcert.check :as c]
            [lcert.kernel :as k]
            [lcert.encode-test :refer [D0]]))

(def c-not (e/enc-deriv D0))
(def bool->bool (e/enc-exp [:Pi :w [:Bool] [:Bool]]))

(defn- certify
  "The code of the derivation the type checker builds for `form` at budget n."
  [n form]
  (e/enc-deriv (t/check-top n (s/parse-term (s/token-scope n) form))))

(defn- type-code [n form]
  (e/enc-exp (:type (t/check-top n (s/parse-term (s/token-scope n) form)))))

(def corpus
  "Programs exercising every rule, as [budget form]."
  [[0 '(fn [x w Bool] (if x ff tt))]
   [0 'star] [0 :a] [0 '(succ (succ zero))]
   [0 '(pair (prod Bool Bool) tt ff)]
   [0 '(pair (prod Unit (T tt)) star star)]
   [0 '(let-pair Bool [a b] (pair (prod Bool Bool) tt ff) b)]
   [0 '(fn [e 1 (T ff)] (abort Bool e))]
   [0 '(fn [b 0 Bool] (fn [e 1 (T b)] e))]
   [0 '(fn [b w Bool] (elim-bool [x (-> (-> (-> (T x) Void) Void) (T x))] b
                                 (fn [k w (-> (-> (T tt) Void) Void)] star)
                                 (fn [k w (-> (-> (T ff) Void) Void)] (k (fn [z w (T ff)] z)))))]
   [0 '(rec-nat [k Nat] zero [k y] (succ y) 2)]
   [0 '(case-lbl [z Bool] :b {:a tt :else ff})]
   [0 '(rec-syn [x Lbl] [a] a [a c1 c2 y1 y2] a (snode :a (sleaf :b) (sleaf :c)))]
   [0 '(fn [r 1 R] (print r))]
   [0 '(chk (sleaf :a) c-bot)]
   [0 '(fn [r 1 R] (fn [e 1 (T (chk (print r) c-bot))] (H r e)))]
   [0 '(fn [r 1 R s 1 R c w Syn e1 1 (T (chk (print r) c)) e2 1 (T (chk (print s) (neg c)))]
         (H1 r s c e1 e2))]
   [0 '(fn [r 1 R] (inspect R r c-bot [x e] x [x e] x))]
   [0 '(fn [r 1 R] (fn [e 1 (T (chk (print r) (code Nat)))] (reflect Nat r e)))]
   [0 '(itr Nat (fn [l w Lbl] zero)
            (fn [d 1 Dia] (fn [l w Lbl] (fn [m 1 Nat] (fn [n 1 Nat] (succ m)))))
            (leaf :a))]
   [2 '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c))]
   [1 '(if tt (node $1 :a (leaf :a) (leaf :a)) (leaf :b))]
   [3 'star]])

(deftest the-drafts-not-certificate
  (testing "Check accepts the draft's code of `not` at Bool → Bool"
    (is (true? (c/check c-not bool->bool))))
  (testing "and at no other type"
    (is (false? (c/check c-not (e/enc-exp [:Bool]))))
    (is (false? (c/check c-not (e/enc-exp [:Pi :w [:Nat] [:Nat]]))))
    (is (false? (c/check c-not (e/enc-exp [:Pi 1 [:Bool] [:Bool]]))))))

(deftest round-trip
  (testing "every derivation the type checker builds is accepted at its type"
    (doseq [[n form] corpus]
      (is (true? (c/check (certify n form) (type-code n form))) (pr-str form)))))

(deftest strict-overhead-holds
  (testing "Lemma 2.7: every accepted code declares fewer tokens than it has nodes"
    (doseq [[n form] corpus]
      (let [code (certify n form)]
        (is (= n (k/budget code)))
        (is (< (k/budget code) (k/nodes code)) (pr-str form))))))

(defn- label-mutations
  "Every code obtained from `c` by changing the label of exactly one leaf."
  [c]
  (letfn [(muts [x]
            (case (first x)
              :sl (for [l [:a :tt :ff :bool :nat :zero :nil :empty] :when (not= l (second x))]
                    [:sl l])
              :sn (let [[_ l a b] x]
                    (concat (map (fn [a'] [:sn l a' b]) (muts a))
                            (map (fn [b'] [:sn l a b']) (muts b))))))]
    (muts c)))

(deftest mutations-are-rejected
  (testing "no single leaf-label mutation of `not`'s certificate is accepted"
    (let [ms (label-mutations c-not)]
      (is (< 100 (count ms)))
      (is (every? #(false? (c/check % bool->bool)) ms))))
  (testing "garbage never throws"
    (doseq [g [[:sl :nil] [:sn :Var [:sl :nil] [:sl :nil]] [:sn :a [:sl :a] [:sl :a]]
               [:sl :no-label] :not-a-code nil [:sn :Lam (e/enc-exp [:Bool]) [:sl :nil]]]]
      (is (false? (c/check g bool->bool))))))

(deftest root-conditions
  (let [unit-d (fn [ctx type] {:rule :Unit :j :has :ctx ctx :term [:star] :type type :prems []})]
    (testing "the root context must be tokens at usage 1"
      (is (true? (c/check (e/enc-deriv (unit-d [[1 [:Dia]]] [:Unit])) (e/enc-exp [:Unit]))))
      (is (false? (c/check (e/enc-deriv (unit-d [[:w [:Dia]]] [:Unit])) (e/enc-exp [:Unit]))))
      (is (false? (c/check (e/enc-deriv (unit-d [[1 [:Bool]]] [:Unit])) (e/enc-exp [:Unit])))))
    (testing "a rule cannot conclude a false type: ⋆ is not a refutation"
      (is (false? (c/check (e/enc-deriv (unit-d [] [:Empty])) e/c-bot))))
    (testing "the certified type must be closed"
      (let [token {:rule :Var :j :has :ctx [[1 [:Dia]]] :term [:var 0] :type [:Dia] :prems []}]
        (is (true? (c/check (e/enc-deriv token) (e/enc-exp [:Dia])))))
      ;; a valid derivation whose type mentions the token $1, at type level
      (let [form '(fn [y 1 (T (chk (print (node $1 :a (leaf :a) (leaf :a))) c-bot))] y)
            d (t/check-top 1 (s/parse-term (s/token-scope 1) form))]
        (is (not (s/closed? (:type d))))
        (is (false? (c/check (e/enc-deriv d) (e/enc-exp (:type d)))))))
    (testing "the root must be a runtime judgment"
      (is (false? (c/check (e/enc-deriv {:rule :TT :j :has0 :ctx [] :term [:tt] :type [:Bool] :prems []})
                           (e/enc-exp [:Bool])))))))

(deftest conversion-chains-must-be-typed
  (testing "review R4-01: a chain through an ill-typed expression is rejected"
    (let [unit {:rule :Unit :j :has :ctx [] :term [:star] :type [:Unit] :prems []}
          tt-type {:rule :TF :j :type :ctx [] :type [:T [:tt]]
                   :prems [{:rule :TT :j :has0 :ctx [] :term [:tt] :type [:Bool] :prems []}]}
          good {:rule :Conv :j :has :ctx [] :term [:star] :type [:T [:tt]]
                :prems [unit tt-type] :chain [[:Unit] [:T [:tt]]]}
          ;; T(tt) ≡ T((λz:Nat. tt) ff) ≡ T(tt): each step is a β-step, but
          ;; the middle expression applies a Nat function to a Boolean
          bad-mid [:T [:app [:lam 1 [:Nat] [:tt]] [:ff]]]
          bad (assoc good :chain [[:Unit] [:T [:tt]] bad-mid [:T [:tt]]])]
      (is (true? (c/check (e/enc-deriv good) (e/enc-exp [:T [:tt]]))))
      (is (false? (c/check (e/enc-deriv bad) (e/enc-exp [:T [:tt]])))))))

(deftest d1-with-a-budget
  (testing "Θ₃₅ ⊢ (lit_not, ⋆) : □(Bool → Bool), and Check accepts its certificate"
    (let [lit (c/certificate-literal c-not)
          box [:Sigma 1 [:R] [:T [:chk [:print [:var 0]] (s/code->term bool->bool)]]]
          d (t/check-top 35 [:pair box lit [:star]])
          code (e/enc-deriv d)]
      (is (= 35 (count (filter #{:node} (flatten lit)))))
      (is (= box (:type d)))
      (is (true? (c/check code (e/enc-exp box))))
      (is (= 35 (k/budget code))))))

(deftest deep-codes-are-rejected-not-crashed
  (testing "E3: a pathologically deep code is rejected, not a stack overflow"
    (let [deep (loop [c [:sl :nil] i 0] (if (< i 200000) (recur [:sn :Lam c [:sl :nil]] (inc i)) c))]
      (is (false? (c/check deep bool->bool))))))
