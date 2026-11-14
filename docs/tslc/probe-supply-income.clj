;; Probe: per-round token income with no new rule. The agent type takes a
;; supply certificate r :₁ R as its last argument. The runner builds it each
;; round from that round's fresh tokens, so the agent's own text names no token
;; and its derivation stays token-free. The agent pays for a certificate of a
;; score program from the supply, checks it at Nat, runs it with reflect, and
;; adopts the candidate iff the score is positive.
;; Run from code/lcert: clojure -J-Xmx1500m -J-Xss512m -M:test -i ../../docs/tslc/probe-supply-income.clj
(require '[lcert.core :as lc] '[lcert.eval :as ev] '[lcert.encode :as e] '[lcert.examples :as ex])
(def SUCC '(Sigma [c w Syn] (Sigma [b w Bool] (T (if b (chk c d) tt)))))
(def Agent (list 'Pi '[d w Syn] (list 'Pi '[self w Syn] (list 'Pi '[cand w Syn] (list 'Pi '[sc w Syn]
             (list 'Pi '[r 1 R] (list 'tensor 'Nat SUCC)))))))
(defn hand-over [c]
  (list 'pair SUCC c
        (list 'pair (list 'Sigma '[b w Bool] (list 'T (list 'if 'b (list 'chk c 'd) 'tt)))
              (list 'chk c 'd)
              (list 'elim-bool '[x (T (if x x tt))] (list 'chk c 'd) 'star 'star))))
(def score-form (ex/parse-then 'sc 'r 'Nat '(inspect Nat t (code Nat) [x e] (reflect Nat x e) [x e] zero)))
(def evaluator
  (list 'the Agent
        (list 'fn '[d w Syn self w Syn cand w Syn sc w Syn r 1 R]
              (list 'let-pair (list 'tensor 'Nat SUCC) '[s1 s2]
                    (list 'rec-nat '[x (tensor Nat Nat)] '(pair (tensor Nat Nat) zero zero)
                          '[k y] '(let-pair (tensor Nat Nat) [a b] y (pair (tensor Nat Nat) (succ a) (succ b)))
                          score-form)
                    (list 'pair (list 'tensor 'Nat SUCC) 's1
                          (list 'elim-bool ['x SUCC] '(rec-nat [q Bool] ff [k y] tt s2)
                                (hand-over 'cand) (hand-over 'self)))))))
;; A successor that ignores its supply, acts 2 and keeps itself.
(def stay2 (list 'the Agent (list 'fn '[d w Syn self w Syn cand w Syn sc w Syn r 1 R]
                                   (list 'pair (list 'tensor 'Nat SUCC) '(succ (succ zero)) (hand-over 'self)))))
(def type-code (lc/run 0 (list 'code Agent)))
(defn lit [c] (lc/parse-program 0 (list 'code-literal c)))
(defn supply-term
  "The round's supply: a right-leaning chain of the round's k fresh tokens."
  [k] (lc/parse-program k (reduce (fn [acc j] (list 'node (symbol (str "$" j)) :a '(leaf :a) acc)) '(leaf :a) (range k 0 -1))))
(defn round [code cand sc k]
  (let [dd (e/dec-deriv code)
        _ (assert (zero? (count (:ctx dd))) "the agent's derivation names no token")
        [_ out [_ succ [_ verdict _]]]
        (ev/run k [:app [:app [:app [:app [:app (:term dd) (lit type-code)] (lit code)] (lit cand)] (lit sc)] (supply-term k)])]
    {:out out :succ succ :verdict verdict :recheck (lc/check-code succ type-code)}))
(def ev-cert (lc/certify 0 evaluator))
(def ev-code (:code ev-cert))
(def s2-code (:code (lc/certify 0 stay2)))
(println "evaluator: budget" (:budget ev-cert) ", derivation nodes" (:nodes ev-cert) "; stay2 nodes" (:nodes (lc/certify 0 stay2)))
(def score-two (:code (lc/certify 0 '(succ (succ zero)))))
(def score-zero (:code (lc/certify 0 'zero)))
(doseq [[label sc k] [["score 2, supply 40" score-two 40] ["score 2, supply 5 (too small to pay)" score-two 5]
                      ["score 0, supply 40" score-zero 40]]]
  (let [r (round ev-code s2-code sc k)]
    (println label "-> output" (:out r) "| adopts stay2:" (= (:succ r) s2-code) "| keeps itself:" (= (:succ r) ev-code)
             "| verdict" (:verdict r) "| re-check" (:recheck r))))
(println "PROBE-SUPPLY-INCOME-OK")
