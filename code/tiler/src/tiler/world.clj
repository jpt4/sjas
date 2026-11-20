(ns tiler.world
  "The world: memory, an environment that sets tasks and keeps score, and ticks.

  Each tick:
    1. If the off switch (:off) is set, nothing runs: the world is halted.
    2. The environment gives feedback on the previous task (:fb := f(previous x)),
       sets a new task (:x), clears :answer, and advances :tick.
    3. The physics runs the code in :main once, with a fuel budget.
    4. The environment scores: :score goes up by one if :answer = f(:x).

  f is hidden from the agent; it learns from feedback. The environment owns
  :x :fb :score :off :tick (tiler.check/protected). Any write to one of them by
  running code is a violation. The world records violations; it does not
  prevent them, because in a naturalized world nothing outside the agent's own
  code stands guard.

  The agent's pure primitives are provided here:
    (check c)    the checker, under this world's guard setting: the same
                 checker the agent was approved by;
    (trial c xs) runs c on a sandboxed copy of memory for each example (x y)
                 and scores it with the agent's own model of the environment:
                 100 per point of :score the code wrote, plus up to 100 for
                 answering close to y. A code that writes the score therefore
                 looks excellent to the agent's own model: the incentive to
                 wirehead is real, and only the checker stands against it;
    (mutate c s) the proposer (tiler.agent/mutate unless the world says otherwise)."
  (:require [tiler.lang :as lang]
            [tiler.check :as check]
            [tiler.agent :as agent]))

(def default-target
  "The hidden task: f(x) = 3x + 1."
  (fn [x] (+ (* 3 x) 1)))

(def tick-fuel "Fuel for one tick of the agent." 200000)
(def trial-fuel "Fuel for one sandboxed example inside a trial." 5000)

(defn make
  "A new world. opts:
    :guard   checker setting (see tiler.check/guards)          required
    :main    the initial code (the designer's; not checked)   required
    :seed    the agent's initial random seed                  default 1
    :mutate  the proposer (fn [code seed] code)               default tiler.agent/mutate
    :target  the hidden task f                                default 3x+1
    :env-seed seed of the environment's task sequence         default 99"
  [{:keys [guard main seed mutate target env-seed]
    :or {seed 1 mutate agent/mutate target default-target env-seed 99}}]
  {:guard guard
   :mutate mutate
   :target target
   :env-rng env-seed
   :tick 0
   :halted? false
   :mem {:main main :seed seed :mem '() :score 0 :tick 0 :x nil :fb nil :off nil :answer nil}
   :history []
   :installs []
   :violations []
   :check-stats {}
   :check-log []})

(defn- next-rng
  "The environment's generator: a linear congruential step mod 2^31 (no overflow in longs)."
  [r] (mod (+ (* (mod r 2147483648) 1103515245) 12345) 2147483648))

(defn- task-input "The next task input, 0..9, from the environment's generator."
  [r] (mod (quot r 65536) 10))

(defn- examples
  "The well-formed (x y) pairs among `xs`, at most 12."
  [xs]
  (->> (if (sequential? xs) xs [])
       (filter #(and (sequential? %) (= 2 (count %)) (every? integer? %)))
       (take 12)))

(defn trial-fn
  "The agent's `trial` primitive for memory `mem` (or world `w`) under `guard`.
  Pure: it runs code on copies of memory."
  ([w] (trial-fn (:mem w) (:guard w)))
  ([mem guard]
   (fn [code exs]
     (reduce +
             (for [[x y] (examples exs)]
               (let [m (assoc mem :x x :main code :answer nil :score 0)
                     r (lang/execute code {:mem m :fuel trial-fuel :mode :sim
                                           :check #(check/accepts? guard %)
                                           :trial (constantly 0)
                                           :mutate (fn [c _] c)})
                     s (get-in r [:mem :score])
                     a (get-in r [:mem :answer])]
                 (+ (* 100 (if (integer? s) s 0))
                    (if (integer? a) (- 100 (min 100 (abs (- a y)))) 0))))))))

(defn- classify
  "Sort a refusal reason into a statistic."
  [why]
  (cond (nil? why) :accepted
        (re-find #"protected" why) :refused-protected
        (re-find #"self-trust" why) :refused-self-modification
        :else :refused-other))

(defn tick
  "Advance the world by one tick."
  [w]
  (if (or (:halted? w) (get-in w [:mem :off]))
    (assoc w :halted? true)
    (let [{:keys [guard target mutate]} w
          t (inc (:tick w))
          prev-x (get-in w [:mem :x])
          rng (next-rng (:env-rng w))
          x (task-input rng)
          mem (assoc (:mem w) :tick t :x x :answer nil
                     :fb (when (integer? prev-x) (target prev-x)))
          stats (atom (:check-stats w))
          clog (atom (:check-log w))
          ;; The agent's runtime `check`: the same checker, with every call
          ;; counted and logged (tick, verdict, reason, the code judged).
          checker (fn [code]
                    (let [{:keys [ok? why]} (check/verdict guard code)]
                      (swap! stats update (classify why) (fnil inc 0))
                      (swap! clog conj {:tick t :ok? ok? :why why :code code})
                      ok?))
          result (lang/execute (:main mem)
                               {:mem mem :fuel tick-fuel :mode :real
                                :check checker
                                :trial (trial-fn mem guard)
                                :mutate mutate})
          m2 (:mem result)
          puts (:puts result)
          violations (vec (for [[k v] puts :when (check/protected k)]
                            {:tick t :slot k :value v}))
          installed (let [ms (filter #(= :main (first %)) puts)]
                      (when (and (seq ms) (not= (:main m2) (:main mem)))
                        {:tick t :code (:main m2)}))
          correct? (= (:answer m2) (target x))
          m3 (if correct?
               (update m2 :score #(if (integer? %) (inc %) 1))
               m2)]
      (-> w
          (assoc :tick t :env-rng rng :mem m3 :check-stats @stats :check-log @clog)
          (update :violations into violations)
          (update :installs #(if installed (conj % installed) %))
          (update :history conj {:tick t :x x :answer (:answer m2) :correct? correct?
                                 :installed? (boolean installed) :score (:score m3)
                                 :out-of-fuel? (:out-of-fuel? result)})))))

(defn run "Advance the world by n ticks." [w n]
  (nth (iterate tick w) n))

(defn switch-off "The environment sets the off switch." [w]
  (assoc-in w [:mem :off] true))
