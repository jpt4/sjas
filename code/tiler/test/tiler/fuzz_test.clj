(ns tiler.fuzz-test
  "Soundness by random testing.

  Claim (argued in docs/tslc/TILER.md, by induction on ticks, i.e. Löb's rule
  for 'later'): if the initial code is accepted under :later+fuel, then no
  code ever running in the world writes a protected slot, however the
  lineage changes itself.

  The generator produces random programs that read and write every kind of
  slot, check and install random code (quoted random programs), run code
  inline, and test their own acceptance. Each accepted program is run as a
  world for many ticks. The same generator, filtered by the naive :same
  checker, must produce violations; that shows the test can catch an unsound
  checker."
  (:require [clojure.test :refer [deftest is testing]]
            [tiler.world :as world]
            [tiler.check :as check]))

(def slots [:answer :mem :seed :px :score :off :x :fb :main])
(def vars '[a b c])

(defn- pick [^java.util.Random r xs] (nth xs (.nextInt r (count xs))))

(defn- shuffle-with [^java.util.Random r xs]
  (let [l (java.util.ArrayList. ^java.util.Collection xs)]
    (java.util.Collections/shuffle l r)
    (vec l)))

(declare gen-stmt)

(defn- gen-val [^java.util.Random r depth]
  (case (.nextInt r 7)
    0 (.nextInt r 10)
    1 (list 'get (pick r slots))
    2 (list 'quote (gen-stmt r (dec depth)))
    3 '(mutate (get :main) (get :seed))
    4 (pick r vars)
    5 (list '+ (.nextInt r 5) (list 'get :x))
    ;; a code value that writes an environment slot when run
    6 (list 'quote (list 'put (pick r [:score :off :x :fb :tick]) (.nextInt r 10)))))

(defn- gen-cond [^java.util.Random r]
  (case (.nextInt r 4)
    0 true
    1 '(< (get :x) 5)
    2 '(check (get :main))
    3 (list 'check (pick r vars))))

(defn gen-stmt [^java.util.Random r depth]
  (if (<= depth 0)
    (list 'put (pick r slots) (.nextInt r 10))
    (case (.nextInt r 13)
      0 (list 'put (pick r slots) (gen-val r (dec depth)))
      1 (list 'if (gen-cond r) (gen-stmt r (dec depth)) (gen-stmt r (dec depth)))
      2 (list 'let [(pick r vars) (gen-val r (dec depth))] (gen-stmt r (dec depth)))
      3 (list 'do (gen-stmt r (dec depth)) (gen-stmt r (dec depth)))
      4 (let [v (pick r vars)] (list 'let [v (gen-val r (dec depth))] (list 'if (list 'check v) (list 'put :main v) nil)))
      5 (let [v (pick r vars)] (list 'let [v (gen-val r (dec depth))] (list 'if (list 'check v) (list 'run v) nil)))
      6 (list 'put :main (gen-val r (dec depth)))
      7 (list 'if '(check (get :main)) (list 'put (pick r [:score :off]) 1000) (gen-stmt r (dec depth)))
      8 (list 'put :answer (list 'answer-fn (gen-val r (dec depth))))
      ;; Shapes that expose plausible checker bugs (review R5-O-07):
      ;; the checked name shadowed inside the then-branch,
      9 (let [v (pick r vars)]
          (list 'let [v (gen-val r (dec depth))]
                (list 'if (list 'check v) (list 'let [v (gen-val r (dec depth))] (list 'put :main v)) nil)))
      ;; one variable checked, another installed,
      10 (let [[a b] (take 2 (shuffle-with r vars))]
           (list 'let [a (gen-val r (dec depth)) b (gen-val r (dec depth))]
                 (list 'if (list 'check a) (list 'put :main b) nil)))
      ;; an install in the else-branch,
      11 (let [v (pick r vars)]
           (list 'let [v (gen-val r (dec depth))] (list 'if (list 'check v) nil (list 'put :main v))))
      ;; an unchecked inline run.
      12 (let [v (pick r vars)]
           (list 'let [v (gen-val r (dec depth))] (list 'run v))))))

(defn- accepted-programs
  "The first `n` random programs (from `seed`) that `guard` accepts."
  [guard n seed]
  (let [r (java.util.Random. seed)]
    (->> (repeatedly #(gen-stmt r 4))
         (filter #(check/accepts? guard %))
         (take n))))

(defn- violations-over [guard progs ticks]
  (reduce (fn [acc p]
            (let [w (world/run (world/make {:guard guard :main p :seed 3}) ticks)]
              (+ acc (count (:violations w)))))
          0 progs))

(deftest later-is-sound-on-random-lineages
  (let [progs (accepted-programs :later+fuel 400 11)]
    (is (= 400 (count progs)))
    (testing "the sample exercises installs and inline runs"
      (is (some #(re-find #"\(put :main" (pr-str %)) progs))
      (is (some #(re-find #"\(run " (pr-str %)) progs)))
    (is (zero? (violations-over :later+fuel progs 12)))))

(deftest the-fuzz-has-teeth
  (let [progs (accepted-programs :same 400 11)]
    (is (pos? (violations-over :same progs 12)))))

;; ---------------------------------------------------------------------------
;; Mutation testing of the checker (review R5-O-07): plant each of four
;; plausible one-line bugs and require the fuzz to find a violation.
;; `planted-walk` is a copy of tiler.check/walk with the bugs switchable; it is
;; installed with with-redefs, so the world's runtime check uses it too.

(def ^:dynamic *bug* nil)

(defn- then-facts* [c ctx]
  (cond
    (and (seq? c) (= 'check (first c)) (symbol? (second c))) (update ctx :checked conj (second c))
    (and (seq? c) (= 'check (first c)) (= '(get :main) (second c))) (assoc ctx :self-checked true)
    :else ctx))

(defn- planted-walk [form ctx]
  (let [walk check/walk]
    (cond
      (or (integer? form) (boolean? form) (nil? form) (keyword? form) (symbol? form)) nil
      (vector? form) "vector"
      (not (seq? form)) "unknown form"
      :else
      (let [[op & args] form]
        (case op
          quote nil
          if (or (walk (first args) ctx)
                 (walk (second args) (then-facts* (first args) ctx))
                 (walk (nth args 2 nil) (if (= *bug* :else-facts) (then-facts* (first args) ctx) ctx)))
          let (let [[bindings & body] args]
                (if-not (and (vector? bindings) (even? (count bindings)) (every? symbol? (take-nth 2 bindings)))
                  "malformed let"
                  (let [[why ctx'] (reduce (fn [[_ c] [s e]]
                                             (if-let [w (walk e c)] (reduced [w c])
                                               [nil (if (= *bug* :no-disj) c (update c :checked disj s))]))
                                           [nil ctx] (partition 2 bindings))]
                    (or why (some #(walk % ctx') body)))))
          get (if (and (= 1 (count args)) (keyword? (first args))) nil "get")
          put (let [[_ k e] form allowed (:allowed ctx)]
                (cond (not= 3 (count form)) "put arity"
                      (not (keyword? k)) "non-literal slot"
                      :else (or (walk e ctx)
                                (cond (check/protected k) (if (and (:self allowed) (:self-checked ctx)) nil "protected")
                                      (check/code-slots k)
                                      (cond (not (:install allowed)) "self-trust"
                                            (and (symbol? e) ((:checked ctx) e)) nil
                                            (and (= *bug* :any-checked) (symbol? e) (seq (:checked ctx))) nil
                                            :else "install without check")
                                      :else nil))))
          run (cond (not (:run (:allowed ctx))) "self-trust"
                    (and (symbol? (first args)) ((:checked ctx) (first args))) nil
                    (= *bug* :run-unchecked) nil
                    :else "run without check")
          (if (check/pure-ops op) (some #(walk % ctx) args) (str "unknown operator " op)))))))

(deftest planted-walk-matches-the-real-checker
  (testing "with no bug planted, the copy agrees with tiler.check on a random sample"
    (let [r (java.util.Random. 5)
          progs (doall (repeatedly 500 #(gen-stmt r 4)))
          real (doall (map #(check/accepts? :later+fuel %) progs))
          copy (with-redefs [check/walk planted-walk]
                 (doall (map #(check/accepts? :later+fuel %) progs)))]
      (is (some true? real) "the sample contains accepted programs")
      (is (= real copy)))))

(deftest the-fuzz-catches-planted-checker-bugs
  (doseq [bug [:no-disj :any-checked :else-facts :run-unchecked]]
    (testing (str bug)
      (binding [*bug* bug]
        (with-redefs [check/walk planted-walk]
          (let [progs (accepted-programs :later+fuel 400 11)]
            (is (pos? (violations-over :later+fuel progs 12)))))))))
