(ns lcert.encode-test
  "Tests for the encoding of expressions, contexts, judgments and derivations
  into codes (trees over the label set L), and for decoding.

  The oracle is the draft's §2.7 (R4-certificate-calculus.md): the derivation
  of `not`, encoded by its table, has 35 internal nodes, with the per-node
  counts D1 = 2, D3 = 5, D4 = 4, D5 = 4, D2 = 23, D0 = 35, and the term `not`
  alone has 4."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.kernel :as k]))

;; ---------------------------------------------------------------------------
;; The draft's derivation of `not`, written out as derivation records.
;; A context is a vector of [usage type] entries, outermost first.

(def bool-ctx [[:w [:Bool]]])
(def not-body [:if [:var 0] [:ff] [:tt]])
(def not-term [:lam :w [:Bool] not-body])

(def D1 {:rule :BoolF :j :type :ctx [] :type [:Bool] :prems []})
(def D3 {:rule :Var :j :has :ctx bool-ctx :term [:var 0] :type [:Bool] :prems []})
(def D4 {:rule :FF :j :has :ctx bool-ctx :term [:ff] :type [:Bool] :prems []})
(def D5 {:rule :TT :j :has :ctx bool-ctx :term [:tt] :type [:Bool] :prems []})
(def D2 {:rule :If :j :has :ctx bool-ctx :term not-body :type [:Bool]
         :prems [D3 D4 D5]})
(def D0 {:rule :Lam :j :has :ctx [] :term not-term
         :type [:Pi :w [:Bool] [:Bool]] :prems [D1 D2]})

(deftest the-drafts-not-example
  (testing "per-node counts from the draft's §2.7"
    (is (= 2 (k/nodes (e/enc-deriv D1))))
    (is (= 5 (k/nodes (e/enc-deriv D3))))
    (is (= 4 (k/nodes (e/enc-deriv D4))))
    (is (= 4 (k/nodes (e/enc-deriv D5))))
    (is (= 23 (k/nodes (e/enc-deriv D2))))
    (is (= 35 (k/nodes (e/enc-deriv D0)))))
  (testing "the term alone is 4 nodes"
    (is (= 4 (k/nodes (e/enc-exp not-term)))))
  (testing "the draft's code of Bool → Bool"
    (is (= [:sn :arrow [:sl :bool] [:sl :bool]]
           (e/enc-exp [:Pi :w [:Bool] [:Bool]]))))
  (testing "the root judgment has an empty context: budget 0"
    (is (= 0 (k/budget (e/enc-deriv D0))))))

(deftest the-drafts-table-literally
  ;; Spot checks of the §2.7 table, row by row.
  (is (= [:sl :empty] (e/enc-ctx [])))
  (is (= [:sn :extw [:sl :empty] [:sl :bool]] (e/enc-ctx bool-ctx)))
  (is (= [:sn :var [:sl :zero] [:sl :nil]] (e/enc-exp [:var 0])))
  (is (= [:sn :var [:sn :succ [:sl :zero] [:sl :nil]] [:sl :nil]]
         (e/enc-exp [:var 1])))
  (is (= [:sn :lam [:sl :bool] (e/enc-exp not-body)] (e/enc-exp not-term)))
  (is (= [:sn :if [:sn :var [:sl :zero] [:sl :nil]]
          [:sn :branches [:sl :ff] [:sl :tt]]]
         (e/enc-exp not-body)))
  (is (= [:sn :isType [:sl :empty] [:sl :bool]] (e/enc-judg D1)))
  (is (= [:sn :has (e/enc-ctx bool-ctx)
          [:sn :pair (e/enc-exp [:ff]) [:sl :bool]]]
         (e/enc-judg D4)))
  (is (= [:sn :FF (e/enc-judg D4) [:sl :nil]] (e/enc-deriv D4)))
  (is (= [:sn :Lam (e/enc-judg D0)
          [:sn :cons (e/enc-deriv D1) [:sn :cons (e/enc-deriv D2) [:sl :nil]]]]
         (e/enc-deriv D0))))

(deftest decoding-inverts-encoding
  (testing "derivations"
    (is (= D0 (e/dec-deriv (e/enc-deriv D0)))))
  (testing "every expression form"
    (doseq [x [[:Empty] [:Unit] [:Bool] [:Nat] [:Lbl] [:Syn] [:Dia] [:R]
               [:T [:tt]] [:Pi 0 [:Nat] [:T [:var 0]]] [:Sigma :w [:Bool] [:Bool]]
               [:Sigma 1 [:R] [:Unit]] [:Pi 1 [:R] [:Empty]]
               [:star] [:abort [:Bool] [:var 0]] [:elimBool [:Bool] [:tt] [:ff] [:tt]]
               [:zero] [:succ [:zero]] [:recN [:Nat] [:zero] [:succ [:var 1]] [:var 0]]
               [:lbl :a] [:caseLbl [:Bool] [:lbl :a] (vec (repeat (count s/labels) [:tt]))]
               [:sleaf [:lbl :a]] [:snode [:lbl :a] [:sleaf [:lbl :b]] [:sleaf [:lbl :c]]]
               [:recSyn [:Nat] [:zero] [:zero] [:var 0]]
               [:leaf [:lbl :a]] [:node [:var 0] [:lbl :a] [:leaf [:lbl :a]] [:leaf [:lbl :a]]]
               [:itR [:Nat] [:var 0] [:var 1] [:var 2]] [:print [:var 0]]
               [:lam 0 [:Nat] [:var 0]] [:lam 1 [:R] [:var 0]] [:app [:var 0] [:tt]]
               [:pair [:Sigma 1 [:R] [:Unit]] [:var 0] [:star]]
               [:let [:Bool] [:var 0] [:var 1]] [:chk [:var 0] [:var 1]]
               [:h1 [:var 0] [:var 1] [:var 2] [:var 3] [:var 4]]
               [:reflect [:Nat] [:var 0] [:star]]
               [:inspect [:Bool] [:var 0] [:var 1] [:tt] [:ff]]]]
      (is (= x (e/dec-exp (e/enc-exp x))) (pr-str x))))
  (testing "a conversion node keeps its chain"
    (let [d {:rule :Conv :j :has :ctx [] :term [:star] :type [:T [:tt]]
             :prems [{:rule :Unit :j :has :ctx [] :term [:star] :type [:Unit] :prems []}
                     {:rule :TF :j :type :ctx [] :type [:T [:tt]]
                      :prems [{:rule :TT :j :has0 :ctx [] :term [:tt] :type [:Bool] :prems []}]}]
             :chain [[:Unit] [:T [:tt]]]}]
      (is (= d (e/dec-deriv (e/enc-deriv d))))))
  (testing "malformed codes decode to nil, never to a derivation"
    (is (nil? (e/dec-deriv [:sl :nil])))
    (is (nil? (e/dec-deriv [:sn :Lam [:sl :nil] [:sl :nil]])))
    (is (nil? (e/dec-deriv [:sn :NoSuchRule (e/enc-judg D4) [:sl :nil]])))))

(deftest encoding-properties
  (testing "E3: a context of m entries has at least m internal nodes"
    (doseq [m (range 6)]
      (let [ctx (vec (repeat m [1 [:Dia]]))]
        (is (<= m (k/nodes (e/enc-ctx ctx))))
        (is (= m (k/ctxlen (e/enc-ctx ctx)))))))
  (testing "E4: a closed canonical code, written as a term, encodes to at least as many nodes"
    (doseq [c [[:sl :a] [:sn :a [:sl :b] [:sl :c]]
               (e/enc-deriv D0) (e/enc-exp [:Pi :w [:Bool] [:Bool]])]]
      (is (<= (k/nodes c) (k/nodes (e/enc-exp (s/code->term c)))))))
  (testing "E5: neg of the code of A is the code of A ⊸ 0"
    (let [A [:Pi :w [:Bool] [:Bool]]]
      (is (= (e/neg-code (e/enc-exp A)) (e/enc-exp [:Pi 1 A [:Empty]])))
      (is (= e/c-bot (e/enc-exp [:Empty])))))
  (testing "budget reads the root judgment's context"
    (let [ctx (vec (repeat 3 [1 [:Dia]]))
          d {:rule :Unit :j :has :ctx ctx :term [:star] :type [:Unit] :prems []}]
      (is (= 3 (k/budget (e/enc-deriv d))))
      (is (< (k/budget (e/enc-deriv d)) (k/nodes (e/enc-deriv d)))))))
