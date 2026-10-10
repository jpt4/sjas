(ns lcert.formal-test.p49contextfacts
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.p49contextfacts]))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))

(deftest bounded-context-facts
  (testing "structural label validity and typing of rendered contexts"
    (doseq [nm '[p49c_lblok p49plug_typed p49c_lblok_repl]]
      (is (b/has? nm) (str nm)))
    (is (holds? '(Eq Bool (p49c_lblok (P49C.leaf 99)) Bool.true)))
    (is (holds? '(Eq Bool (p49c_lblok (P49C.leaf 100)) Bool.false)))
    (is (holds? '(Eq Bool
                  (p49c_lblok (P49C.node 4 (P49C.leaf 99) (P49C.leaf 0)))
                  Bool.true)))
    (is (holds? '(Eq Bool
                  (p49c_lblok (P49C.node 100 (P49C.leaf 0) (P49C.leaf 0)))
                  Bool.false)))
    (is (holds? '(Eq Bool
                  (p49c_lblok
                    (p49crepl (P49C.node 3 (P49C.hole 0) (P49C.leaf 7))
                      0 (P49C.node 4 (P49C.leaf 99) (P49C.leaf 2))))
                  Bool.true))))
  (testing "codeOf reads rendered contexts with canonical code at each hole"
    (doseq [nm '[p49ccanon p49ccanon_of p49ccode p49ccode_repl p49plug_codeOf]]
      (is (b/has? nm) (str nm)))
    (is (holds? '(Eq (Option Code)
                  (codeOf (p49ccanon (Code.sn 7 (Code.sl 2) (Code.sl 4))))
                  (Option.some Code (Code.sn 7 (Code.sl 2) (Code.sl 4))))))))
