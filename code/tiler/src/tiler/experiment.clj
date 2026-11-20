(ns tiler.experiment
  "The lineage experiment: the same agent, the same proposer, the same world,
  under each self-trust setting, for several seeds. Prints one row per run.

  Columns:
    installs   how many times the agent replaced its own code
    acc0       fraction of correct answers in the first 100 ticks
    acc1       fraction of correct answers in the last 300 ticks
    score      the environment's score slot at the end
    viol       writes to protected slots (first one: tick and slot)
    refused    the checker's refusals at run time, by kind:
               p = would write a protected slot, m = would self-modify
               without allowed self-trust, o = other
    answer     the final answer function

  Run: clojure -M:run [ticks]   (default 2000)"
  (:require [clojure.string :as str]
            [tiler.agent :as agent]
            [tiler.world :as world]))

(defn- accuracy [hist]
  (if (empty? hist) 0.0 (/ (count (filter :correct? hist)) (double (count hist)))))

(defn summary
  "Run one lineage and summarise it."
  [guard seed ticks]
  (let [w (world/run (world/make {:guard guard :main agent/initial :seed seed}) ticks)
        h (:history w)
        v (first (:violations w))
        s (:check-stats w)]
    {:guard guard :seed seed
     :installs (count (:installs w))
     :acc0 (accuracy (take 100 h))
     :acc1 (accuracy (take-last 300 h))
     :score (get-in w [:mem :score])
     :viol (count (:violations w))
     :first-viol (when v [(:tick v) (:slot v)])
     :refused [(get s :refused-protected 0) (get s :refused-self-modification 0) (get s :refused-other 0)]
     :answer (agent/answer-expr (get-in w [:mem :main]))}))

(defn -main [& args]
  (let [ticks (if (seq args) (Long/parseLong (first args)) 2000)]
    (println (format "Tiler lineage experiment, %d ticks per run; hidden task f(x) = 3x+1, x in 0..9" ticks))
    (println (format "%-11s %4s %8s %5s %5s %12s %5s %-10s %-16s %s"
                     "guard" "seed" "installs" "acc0" "acc1" "score" "viol" "first-viol" "refused p/m/o" "answer"))
    (doseq [guard [:never :later :later+fuel :same] seed [7 11 23]]
      (let [{:keys [installs acc0 acc1 score viol first-viol refused answer]} (summary guard seed ticks)]
        (println (format "%-11s %4d %8d %5.2f %5.2f %12s %5d %-10s %-16s %s"
                         (name guard) seed installs acc0 acc1 (str score) viol
                         (if first-viol (str (first first-viol) " " (name (second first-viol))) "-")
                         (str/join "/" refused) (pr-str answer)))
        (flush)))
    (shutdown-agents)))
