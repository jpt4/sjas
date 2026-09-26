(ns lcert.reduce-test
  "Tests for the conversion steps of R4-metatheory.md §1.5 (β, ι, δ, T),
  normalization with a recorded step sequence, the one-step relation that
  Check uses to validate conversion chains, and chain construction."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.syntax :as s]
            [lcert.reduce :as r]))

(defn- numeral [n] (nth (iterate (fn [t] [:succ t]) [:zero]) n))

(def ^:private tree
  "A certificate literal with two internal nodes, tokens [:var 0] and [:var 1]."
  [:node [:var 1] [:lbl :a]
   [:node [:var 0] [:lbl :b] [:leaf [:lbl :c]] [:leaf [:lbl :c]]]
   [:leaf [:lbl :c]]])

(deftest root-contractions
  (testing "β for functions and pairs"
    (is (= [:tt] (r/contract [:app [:lam :w [:Bool] [:var 0]] [:tt]])))
    (is (= [:app [:ff] [:tt]]
           (r/contract [:let [:Bool] [:pair [:Sigma 1 [:Bool] [:Bool]] [:ff] [:tt]]
                        [:app [:var 1] [:var 0]]]))))
  (testing "ι for the Boolean eliminators and the label case"
    (is (= [:ff] (r/contract [:if [:tt] [:ff] [:tt]])))
    (is (= [:tt] (r/contract [:elimBool [:Bool] [:ff] [:ff] [:tt]])))
    (is (= [:ff] (r/contract (s/parse-term [] '(case-lbl [z Bool] :b {:b ff :else tt}))))))
  (testing "ι for recN"
    (is (= [:zero] (r/contract [:recN [:Nat] [:zero] [:succ [:var 0]] [:zero]])))
    (is (= [:succ [:recN [:Nat] [:zero] [:succ [:var 0]] [:zero]]]
           (r/contract [:recN [:Nat] [:zero] [:succ [:var 0]] [:succ [:zero]]]))))
  (testing "ι for print and itR"
    (is (= [:sleaf [:lbl :c]] (r/contract [:print [:leaf [:lbl :c]]])))
    (is (= [:snode [:lbl :a] [:print (nth tree 3)] [:print (nth tree 4)]]
           (r/contract [:print tree]))))
  (testing "T of a Boolean constant"
    (is (= [:Unit] (r/contract [:T [:tt]])))
    (is (= [:Empty] (r/contract [:T [:ff]]))))
  (testing "non-redexes contract to nil"
    (is (nil? (r/contract [:app [:var 0] [:tt]])))
    (is (nil? (r/contract [:print [:var 0]])))
    (is (nil? (r/contract [:chk [:var 0] [:sleaf [:lbl :a]]])))))

(deftest normalization
  (testing "under binders"
    (is (= [:lam :w [:Bool] [:var 0]]
           (r/nf [:lam :w [:Bool] [:if [:tt] [:var 0] [:ff]]]))))
  (testing "recN computes"
    (is (= (numeral 3) (r/nf [:recN [:Nat] [:zero] [:succ [:var 0]] (numeral 3)]))))
  (testing "recSyn computes: the label at the root of a code"
    (is (= [:lbl :a]
           (r/nf [:recSyn [:Lbl] [:var 0] [:var 4]
                  (s/code->term [:sn :a [:sl :b] [:sl :c]])]))))
  (testing "itR computes: node count of a certificate, by iteration"
    ;; g = λl. zero ; h = λd l m n. succ (m + n), with + by recN on n
    (let [plus [:lam :w [:Nat] [:lam :w [:Nat]
                               [:recN [:Nat] [:var 1] [:succ [:var 0]] [:var 0]]]]
          g [:lam :w [:Lbl] [:zero]]
          h [:lam 1 [:Dia] [:lam :w [:Lbl] [:lam 1 [:Nat] [:lam 1 [:Nat]
                  [:succ [:app [:app (s/shift plus 4) [:var 1]] [:var 0]]]]]]]]
      (is (= (numeral 2) (r/nf [:itR [:Nat] g h tree])))))
  (testing "print, then δ, then T: the recorded sequence has every step"
    (binding [r/*check* (fn [c d] (= c d))]
      (let [steps (r/normalize [:T [:chk [:print [:leaf [:lbl :a]]] [:sleaf [:lbl :a]]]])]
        (is (vector? steps))
        (is (= [[:T [:chk [:print [:leaf [:lbl :a]]] [:sleaf [:lbl :a]]]]
                [:T [:chk [:sleaf [:lbl :a]] [:sleaf [:lbl :a]]]]
                [:T [:tt]]
                [:Unit]]
               steps)))))
  (testing "a non-normalizing untyped term exhausts the fuel"
    (let [w [:lam :w [:Bool] [:app [:var 0] [:var 0]]]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (r/normalize [:app w w] 1000))))))

(deftest delta-calls-check-on-codes
  (let [calls (atom [])]
    (binding [r/*check* (fn [c d] (swap! calls conj [c d]) true)]
      (is (= [:tt] (r/contract [:chk [:sleaf [:lbl :a]] (s/code->term [:sn :b [:sl :a] [:sl :a]])])))
      (is (= [[[:sl :a] [:sn :b [:sl :a] [:sl :a]]]] @calls)))))

(deftest one-step-relation
  (binding [r/*check* (fn [c d] false)]
    (testing "a root step, a step inside, and non-steps"
      (is (r/one-step? [:if [:tt] [:ff] [:tt]] [:ff]))
      (is (r/one-step? [:succ [:if [:tt] [:ff] [:tt]]] [:succ [:ff]]))
      (is (r/one-step? [:caseLbl [:Bool] [:var 0]
                        (assoc (vec (repeat (count s/labels) [:tt])) 5 [:if [:tt] [:ff] [:tt]])]
                       [:caseLbl [:Bool] [:var 0]
                        (assoc (vec (repeat (count s/labels) [:tt])) 5 [:ff])]))
      (is (not (r/one-step? [:if [:tt] [:ff] [:tt]] [:tt])))
      (is (not (r/one-step? [:succ [:tt]] [:succ [:ff]])))
      (is (not (r/one-step? [:tt] [:tt]))))
    (testing "two steps at once are not one step"
      (is (not (r/one-step? [:app [:if [:tt] [:ff] [:tt]] [:if [:ff] [:ff] [:tt]]]
                            [:app [:ff] [:tt]]))))
    (testing "δ is a step, to the Boolean Check computes"
      (is (r/one-step? [:chk [:sleaf [:lbl :a]] [:sleaf [:lbl :a]]] [:ff]))
      (is (not (r/one-step? [:chk [:sleaf [:lbl :a]] [:sleaf [:lbl :a]]] [:tt]))))))

(deftest chains
  (binding [r/*check* (fn [c d] (= c d))]
    (let [A [:T [:chk [:print [:leaf [:lbl :a]]] [:sleaf [:lbl :a]]]]
          chain (r/conv-chain A [:Unit])]
      (testing "a chain between convertible types, valid step by step"
        (is (= A (first chain)))
        (is (= [:Unit] (last chain)))
        (is (r/chain-valid? chain)))
      (testing "reversed chains are valid too: steps count in either direction"
        (is (r/chain-valid? (vec (reverse chain)))))
      (testing "no chain between inconvertible types"
        (is (nil? (r/conv-chain A [:Empty]))))
      (testing "a chain with a skipped step is invalid"
        (is (not (r/chain-valid? [A [:Unit]])))))))
