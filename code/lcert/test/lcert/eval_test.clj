(ns lcert.eval-test
  "Tests for the evaluators (R4-metatheory.md §5): the erasing, budgeted
  evaluator the language runs on, and the non-erasing one of Theorem 4, kept
  for comparison."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.typing :as t]
            [lcert.check :as c]
            [lcert.kernel :as k]
            [lcert.eval :as ev]))

(defn- run [n form] (ev/run n (s/parse-term (s/token-scope n) form)))

(defn- certify
  "A closed program's certificate code: its derivation at budget n, encoded."
  [n form]
  (e/enc-deriv (t/check-top n (s/parse-term (s/token-scope n) form))))

(def plus '(fn [a w Nat] (fn [b w Nat] (rec-nat [k Nat] a [k y] (succ y) b))))

(deftest ordinary-evaluation
  (is (= false (run 0 '((fn [x w Bool] (if x ff tt)) tt))))
  (is (= 5 (run 0 (list plus 2 3))))
  (is (= :b (run 0 '(case-lbl [z Lbl] :a {:a :b :else :c}))))
  (is (= :a (run 0 '(rec-syn [x Lbl] [a] a [a c1 c2 y1 y2] a (snode :a (sleaf :b) (sleaf :c))))))
  (is (= [:pv true false] (run 0 '(pair (prod Bool Bool) tt ff))))
  (is (= false (run 0 '(let-pair Bool [a b] (pair (prod Bool Bool) tt ff) b)))))

(deftest certificates-at-runtime
  (testing "a certificate is built from the tokens supplied, one per node"
    (let [v (run 2 '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c)))]
      (is (= 2 (ev/nodes v)))
      (is (= 2 (count (ev/tokens v))))
      (is (apply distinct? (ev/tokens v)))))
  (testing "print forgets the tokens"
    (is (= [:sn :a [:sl :b] [:sl :c]]
           (run 1 '(print (node $1 :a (leaf :b) (leaf :c)))))))
  (testing "itR hands each node's token to the node method"
    (is (= 2 (run 2 '(itr Nat (fn [l w Lbl] zero)
                          (fn [d 1 Dia] (fn [l w Lbl] (fn [m 1 Nat] (fn [n 1 Nat] (succ (succ zero))))))
                          (node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c))))))
    ;; rebuilding the tree with its own tokens: same shape, same tokens
    (let [v (run 2 '(itr R (fn [l w Lbl] (leaf l))
                         (fn [d 1 Dia] (fn [l w Lbl] (fn [u 1 R] (fn [w2 1 R] (node d l u w2)))))
                         (node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c))))]
      (is (= 2 (ev/nodes v)))
      (is (apply distinct? (ev/tokens v))))))

(deftest inspect-checks-without-consuming
  (testing "an arbitrary tree is not a refutation certificate; it comes back"
    (is (= [:pv false 1]
           (run 1 '(inspect (prod Bool Nat) (node $1 :a (leaf :a) (leaf :a)) c-bot
                            [x e] (pair (prod Bool Nat) tt 0)
                            [x e] (pair (prod Bool Nat) ff
                                        (itr Nat (fn [l w Lbl] zero)
                                             (fn [d 1 Dia] (fn [l w Lbl] (fn [m 1 Nat] (fn [n 1 Nat] (succ m)))))
                                             x))))))))

(deftest reflect-runs-certified-programs
  (testing "a certified Nat program runs on its certificate's tokens"
    (let [code (certify 0 '(succ (succ zero)))
          kk (k/nodes code)
          lit (c/certificate-literal code)
          prog [:reflect [:Nat] lit [:star]]]
      (is (true? (c/check code (e/enc-exp [:Nat]))))
      (is (= 2 (ev/run kk prog)))))
  (testing "a certified R program: its result uses m < ‖v‖ of the certificate's tokens"
    (let [code (certify 2 '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c)))
          kk (k/nodes code)
          v (ev/run kk [:reflect [:R] (c/certificate-literal code) [:star]])]
      (is (= 2 (k/budget code)))
      (is (= 2 (ev/nodes v)))
      (is (apply distinct? (ev/tokens v)))))
  (testing "over budget, reflect returns the default instead of running"
    ;; the same program, evaluated with a budget below the certificate's size
    (let [code (certify 0 '(succ (succ zero)))
          lit (c/certificate-literal code)
          d (t/check-top (k/nodes code) [:reflect [:Nat] lit [:star]])]
      (is (= 0 (ev/eval-deriv d (dec (k/nodes code)) {:erase? true}))))))

(deftest erasure
  (let [form '(pair (Sigma [r 0 R] Unit)
                    (node $1 :a (node $1 :a (node $1 :a (leaf :a) (leaf :a)) (leaf :a)) (leaf :a))
                    star)
        d (t/check-top 1 (s/parse-term (s/token-scope 1) form))]
    (testing "an erased component may use a token any number of times (review R4-04)"
      (is (= [:Sigma 0 [:R] [:Unit]] (:type d))))
    (testing "the non-erasing evaluator builds the tree from one token"
      (let [[_ a _] (ev/eval-deriv d 1 {:erase? false})]
        (is (= 3 (ev/nodes a)))
        (is (= 1 (count (distinct (ev/tokens a)))))))
    (testing "the erasing evaluator never builds it"
      (is (= [:pv :star :star] (ev/eval-deriv d 1 {:erase? true})))))
  (testing "Theorem 4′: on data results the two evaluators agree"
    (doseq [[n form] [[0 (list plus 2 3)] [0 '((fn [x w Bool] (if x ff tt)) ff)]
                      [0 '((fn [b 0 Bool] (fn [x w Nat] (succ x))) tt 4)]
                      [2 '(print (node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c)))]]]
      (let [d (t/check-top n (s/parse-term (s/token-scope n) form))]
        (is (= (ev/eval-deriv d n {:erase? true}) (ev/eval-deriv d n {:erase? false}))
            (pr-str form))))))

(deftest runtime-tokens-are-never-duplicated
  (testing "the runtime check fires on a duplicated token"
    (let [tok (ev/token 1)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (ev/assert-linear! [:rn tok :a [:rn tok :a [:rl :a] [:rl :a]] [:rl :a]]))))))
