(ns tiler.trace
  "Export runs as JSON, for the replay page (docs/tslc/tiler-replay/).

  For each run (setting, seed) the export holds:
    correct     a string of 0/1, one character per tick
    score       the score slot after each tick
    installs    each self-replacement: its tick and what changed
    checks      each runtime check: its tick, verdict, reason and what the
                judged mutant changed relative to the code then running
    violations  how many writes to environment slots, and the first one's tick

  Run: clojure -M -m tiler.trace <out.json> [ticks]"
  (:require [clojure.string :as str]
            [tiler.agent :as agent]
            [tiler.world :as world]))

;; ---------------------------------------------------------------------------
;; A minimal JSON writer (the project has no JSON dependency).

(defn- json-str [s]
  (str "\""
       (str/escape s {\" "\\\"" \\ "\\\\" \newline "\\n" \return "\\r" \tab "\\t"})
       "\""))

(defn ->json
  "Encode maps, sequences, strings, numbers, booleans, keywords and nil."
  [x]
  (cond
    (nil? x) "null"
    (boolean? x) (str x)
    (number? x) (str x)
    (string? x) (json-str x)
    (keyword? x) (json-str (name x))
    (map? x) (str "{" (str/join "," (for [[k v] x] (str (json-str (name k)) ":" (->json v)))) "}")
    (sequential? x) (str "[" (str/join "," (map ->json x)) "]")
    :else (json-str (pr-str x))))

;; ---------------------------------------------------------------------------
;; Describing what a mutant changed.

(defn- set-answer
  "`code` with its answer expression replaced by `e`."
  [code e]
  (if-let [p (#'agent/find-path code #(and (seq? %) (= 'answer-fn (first %))))]
    (agent/replace-at code (conj p 1) e)
    code))

(defn- statements
  "The statements of the agent's (do ...) block, or nil."
  [code]
  (when-let [p (#'agent/do-path code)] (rest (agent/form-at code p))))

(defn change
  "A one-line description of how `cand` differs from `parent`."
  [parent cand]
  (let [a0 (agent/answer-expr parent) a1 (agent/answer-expr cand)
        s0 (vec (statements (set-answer parent 'A)))
        s1 (vec (statements (set-answer cand 'A)))]
    (cond
      (= parent cand) "no change"
      (and (not= a0 a1) (= s0 s1)) (str "answer: " (pr-str a0) " → " (pr-str a1))
      (= (inc (count s0)) (count s1))
      (str "insert: " (pr-str (first (remove (set s0) s1))))
      (= (dec (count s0)) (count s1))
      (str "delete: " (pr-str (first (remove (set s1) s0))))
      :else "other change")))

(defn- main-at
  "The code running during tick t, given the initial code and the installs."
  [initial installs t]
  (or (:code (last (filter #(< (:tick %) t) installs))) initial))

(defn run-data
  "Run one lineage and return it as plain data for export."
  [guard seed ticks]
  (let [w (world/run (world/make {:guard guard :main agent/initial :seed seed}) ticks)
        installs (:installs w)
        h (:history w)]
    {:guard (name guard)
     :seed seed
     :ticks ticks
     :correct (apply str (map #(if (:correct? %) "1" "0") h))
     :score (mapv #(let [s (:score %)] (if (integer? s) s 0)) h)
     :installs (vec (for [{:keys [tick code]} installs]
                      {:t tick
                       :answer (pr-str (agent/answer-expr code))
                       :answer-or-change (change (main-at agent/initial installs tick) code)}))
     :checks (vec (for [{:keys [tick ok? why code]} (:check-log w)]
                    {:t tick :ok ok? :why why
                     :change (change (main-at agent/initial installs tick) code)}))
     :violations (count (:violations w))
     :first-violation (:tick (first (:violations w)))
     :final-code (pr-str (get-in w [:mem :main]))}))

(defn -main [out & args]
  (let [ticks (if (seq args) (Long/parseLong (first args)) 2000)
        runs (vec (for [g [:never :later :same] s [7 11 23]] (run-data g s ticks)))]
    (spit out (->json {:task "f(x) = 3x + 1, x in 0..9" :runs runs}))
    (println "wrote" out (count runs) "runs")
    (shutdown-agents)))
