(ns lcert.syntax-test
  "Tests for the abstract syntax: the label set, de Bruijn operations, the
  surface parser, and the correspondence between code values and code terms."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.syntax :as s]))

(deftest label-set
  (testing "L is finite, duplicate-free, and holds every label the encoding uses"
    (is (vector? s/labels))
    (is (= (count s/labels) (count (set s/labels))))
    (doseq [l [:nil :cons :zero :succ :empty :extw :ext1 :ext0 :has :has0 :pair
               :isType :var :bool :arrow :arrow1 :lam :if :branches :tt :ff
               :Lam :If :Var :TT :FF :BoolF :Conv :t0 :dia :cert]]
      (is (s/label? l) (str l " must be a label"))))
  (testing "label indices are a bijection onto 0..|L|-1"
    (is (= (range (count s/labels)) (map s/label-index s/labels)))
    (is (nil? (s/label-index :no-such-label)))))

(deftest de-bruijn-operations
  (testing "shift moves free indices at or above the cutoff only"
    (is (= [:app [:var 0] [:var 3]]
           (s/shift [:app [:var 0] [:var 1]] 2 1))))
  (testing "shift respects binders"
    (is (= [:lam 1 [:Bool] [:app [:var 0] [:var 2]]]
           (s/shift [:lam 1 [:Bool] [:app [:var 0] [:var 1]]] 1 0))))
  (testing "subst replaces index 0 and lowers the rest"
    (is (= [:app [:tt] [:var 0]]
           (s/subst [:app [:var 0] [:var 1]] [:tt]))))
  (testing "subst under a binder shifts the substituted term"
    (is (= [:lam :w [:Bool] [:app [:var 0] [:var 1]]]
           (s/subst [:lam :w [:Bool] [:app [:var 0] [:var 1]]] [:var 0]))))
  (testing "subst-many substitutes several innermost indices at once"
    ;; indices 0 and 1 are bound to b and a respectively (a is outer)
    (is (= [:pair [:Sigma 1 [:Bool] [:Bool]] [:ff] [:tt]]
           (s/subst-many [:pair [:Sigma 1 [:Bool] [:Bool]] [:var 1] [:var 0]]
                         [[:ff] [:tt]]))))
  (testing "binders inside types are respected"
    (is (= [:Pi 1 [:Bool] [:T [:var 0]]]
           (s/subst [:Pi 1 [:Bool] [:T [:var 0]]] [:ff])))
    ;; [:var 0] under the Π is bound by it and must not move; [:var 1] there
    ;; is free and must.
    (is (= [:Pi 1 [:Bool] [:T [:var 0]]]
           (s/shift [:Pi 1 [:Bool] [:T [:var 0]]] 1 0)))
    (is (= [:Pi 1 [:Bool] [:T [:var 2]]]
           (s/shift [:Pi 1 [:Bool] [:T [:var 1]]] 1 0))))
  (testing "free variables and closedness"
    (is (s/closed? [:Pi :w [:Bool] [:T [:var 0]]]))
    (is (not (s/closed? [:T [:var 0]])))
    (is (= #{0 2} (s/free-vars [:app [:var 0] [:lam 1 [:Bool] [:var 3]]])))))

(deftest parsing-types
  (is (= [:Bool] (s/parse-type [] 'Bool)))
  (is (= [:Pi :w [:Bool] [:Bool]] (s/parse-type [] '(-> Bool Bool))))
  (is (= [:Pi 1 [:R] [:Empty]] (s/parse-type [] '(-o R Void))))
  (is (= [:Pi 0 [:Nat] [:T [:var 0]]]
         (s/parse-type [] '(Pi [n 0 Nat] (T n)))))
  (is (= [:Sigma 1 [:R] [:Unit]] (s/parse-type [] '(tensor R Unit))))
  (is (= [:Sigma :w [:Bool] [:T [:var 0]]]
         (s/parse-type [] '(Sigma [b w Bool] (T b))))))

(deftest parsing-terms
  (testing "the draft's not"
    (is (= [:lam :w [:Bool] [:if [:var 0] [:ff] [:tt]]]
           (s/parse-term [] '(fn [x w Bool] (if x ff tt))))))
  (testing "application is curried, left-associated"
    (is (= [:app [:app [:var 1] [:var 0]] [:tt]]
           (s/parse-term '[f y] '(f y tt)))))
  (testing "shadowing: the innermost binder wins"
    (is (= [:lam 1 [:Bool] [:lam 1 [:Bool] [:var 0]]]
           (s/parse-term [] '(fn [x 1 Bool] (fn [x 1 Bool] x))))))
  (testing "labels are keywords"
    (is (= [:sleaf [:lbl :a]] (s/parse-term [] '(sleaf :a)))))
  (testing "tokens of a program's budget are named $1 .. $n, $n innermost"
    (is (= [:node [:var 1] [:lbl :a] [:leaf [:lbl :b]] [:leaf [:lbl :b]]]
           (s/parse-term (s/token-scope 2)
                         '(node $1 :a (leaf :b) (leaf :b))))))
  (testing "code sugar: the literal code term of a closed type"
    (is (= [:snode [:lbl :arrow] [:sleaf [:lbl :bool]] [:sleaf [:lbl :bool]]]
           (s/parse-term [] '(code (-> Bool Bool)))))
    (is (= [:sleaf [:lbl :t0]] (s/parse-term [] 'c-bot))))
  (testing "binders of the eliminators"
    (is (= [:recN [:Nat] [:zero] [:succ [:var 1]] [:var 0]]
           (s/parse-term '[n] '(rec-nat [k Nat] zero [k y] (succ k) n))))
    (is (= [:let [:Bool] [:var 0] [:var 1]]
           (s/parse-term '[p] '(let-pair Bool [a b] p a))))
    (is (= [:inspect [:Bool] [:var 1] [:var 0] [:tt] [:ff]]
           (s/parse-term '[r c] '(inspect Bool r c [x e] tt [x e] ff))))))

(deftest case-lbl-default-expands-to-every-label
  (let [t (s/parse-term '[a] '(case-lbl [z Bool] a {:a tt :else ff}))]
    (is (= :caseLbl (first t)))
    (is (= (count s/labels) (count (nth t 3))))
    (is (= [:tt] (nth (nth t 3) (s/label-index :a))))
    (is (= [:ff] (nth (nth t 3) (s/label-index :b))))))

(deftest unknown-names-are-rejected
  (is (thrown? clojure.lang.ExceptionInfo (s/parse-term [] 'nope)))
  (is (thrown? clojure.lang.ExceptionInfo (s/parse-term [] '(sleaf :not-a-label))))
  (is (thrown? clojure.lang.ExceptionInfo (s/parse-type [] 'Nope))))

(deftest code-values-and-code-terms
  (let [c [:sn :arrow [:sl :bool] [:sl :bool]]]
    (testing "a code value and its literal term correspond both ways"
      (is (= [:snode [:lbl :arrow] [:sleaf [:lbl :bool]] [:sleaf [:lbl :bool]]]
             (s/code->term c)))
      (is (= c (s/term->code (s/code->term c)))))
    (testing "only closed canonical code terms denote codes"
      (is (s/canonical-code-term? (s/code->term c)))
      (is (not (s/canonical-code-term? [:snode [:var 0] [:sleaf [:lbl :a]]
                                        [:sleaf [:lbl :a]]])))
      (is (not (s/canonical-code-term? [:print [:leaf [:lbl :a]]]))))))

(deftest let-is-a-beta-redex
  (testing "(let [x u A e] body) is sugar for ((fn [x u A] body) e)"
    (is (= [:app [:lam :w [:Nat] [:succ [:var 0]]] [:succ [:succ [:succ [:zero]]]]]
           (s/parse-term [] '(let [x w Nat 3] (succ x))))))
  (testing "several bindings nest, each in scope of the next"
    (is (= [:app [:lam :w [:Nat] [:app [:lam :w [:Nat] [:var 1]] [:succ [:var 0]]]] [:zero]]
           (s/parse-term [] '(let [x w Nat zero y w Nat (succ x)] x))))))

(deftest the-is-an-ascription
  (testing "(the A t) is sugar for ((fn [z 1 A] z) t): its type is A, up to conversion"
    (is (= [:app [:lam 1 [:Unit] [:var 0]] [:star]]
           (s/parse-term [] '(the Unit star))))))
