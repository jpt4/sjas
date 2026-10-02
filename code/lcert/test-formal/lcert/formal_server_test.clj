(ns lcert.formal-server-test
  "The warm server's choice of what to reload (lcert.formal-server): only the
  changed namespaces and those that depend on them, in dependency order.
  Fast: reads source files, loads no formalization."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal-server :as s]))

(deftest load-order-respects-requires
  (let [deps (s/dep-graph) order (s/load-order deps) pos (zipmap order (range))]
    (testing "every formal namespace appears once, after everything it requires"
      (is (= (count order) (count (set order)) (count deps)))
      (doseq [[n ds] deps d ds :when (deps d)]
        (is (< (pos d) (pos n)) (str d " before " n))))
    (testing "fragments (den_gen, sem_gen) are not namespaces"
      (is (not-any? #(re-find #"gen$" (str %)) order)))))

(deftest reload-set-is-the-dependents-closure
  (let [deps (s/dep-graph)]
    (testing "a namespace nothing requires reloads alone"
      (let [leaves (remove (set (mapcat val deps)) (keys deps))]
        (is (seq leaves))
        (doseq [l leaves] (is (= #{l} (s/dependents-closure deps #{l})) (str l)))))
    (testing "check.clj (requires encode) does not drag in unrelated namespaces"
      (is (not (contains? (s/dependents-closure deps #{'lcert.formal.check}) 'lcert.formal.conversion))))
    (testing "a change to den reaches lemma36, which depends on it transitively"
      (is (contains? (s/dependents-closure deps #{'lcert.formal.den}) 'lcert.formal.lemma36)))
    (testing "the closure is closed: whoever requires a member is a member"
      (let [c (s/dependents-closure deps #{'lcert.formal.conv})]
        (doseq [[n ds] deps :when (some c ds)] (is (contains? c n) (str n)))))))
