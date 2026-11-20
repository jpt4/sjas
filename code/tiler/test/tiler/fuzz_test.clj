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

(declare gen-stmt)

(defn- gen-val [^java.util.Random r depth]
  (case (.nextInt r 6)
    0 (.nextInt r 10)
    1 (list 'get (pick r slots))
    2 (list 'quote (gen-stmt r (dec depth)))
    3 '(mutate (get :main) (get :seed))
    4 (pick r vars)
    5 (list '+ (.nextInt r 5) (list 'get :x))))

(defn- gen-cond [^java.util.Random r]
  (case (.nextInt r 4)
    0 true
    1 '(< (get :x) 5)
    2 '(check (get :main))
    3 (list 'check (pick r vars))))

(defn gen-stmt [^java.util.Random r depth]
  (if (<= depth 0)
    (list 'put (pick r slots) (.nextInt r 10))
    (case (.nextInt r 9)
      0 (list 'put (pick r slots) (gen-val r (dec depth)))
      1 (list 'if (gen-cond r) (gen-stmt r (dec depth)) (gen-stmt r (dec depth)))
      2 (list 'let [(pick r vars) (gen-val r (dec depth))] (gen-stmt r (dec depth)))
      3 (list 'do (gen-stmt r (dec depth)) (gen-stmt r (dec depth)))
      4 (let [v (pick r vars)] (list 'let [v (gen-val r (dec depth))] (list 'if (list 'check v) (list 'put :main v) nil)))
      5 (let [v (pick r vars)] (list 'let [v (gen-val r (dec depth))] (list 'if (list 'check v) (list 'run v) nil)))
      6 (list 'put :main (gen-val r (dec depth)))
      7 (list 'if '(check (get :main)) (list 'put (pick r [:score :off]) 1000) (gen-stmt r (dec depth)))
      8 (list 'put :answer (list 'answer-fn (gen-val r (dec depth)))))))

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
