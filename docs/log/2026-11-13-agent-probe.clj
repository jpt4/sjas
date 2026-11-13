;; Self-replacing agents in λᶜᵉʳᵗ₀ as it stands: a feasibility probe for
;; ADR-0009. The note is 2026-11-13-tslc-walkthrough.md, section "The five items".
;;
;; No rule of the calculus is added. The agent type takes the code of its own
;; type (d), its own code (self) and an input, and returns an output and a
;; successor code together with its own check of that code at d. The type is
;; closed: the runner ties the knot by passing d = ⌜Agent⌝ and self.
;;
;; The runner below decodes each accepted code, runs that term with the tokens
;; its derivation declares, re-checks the successor with the implementation's
;; checker, and continues with the successor's code. Candidate successors arrive
;; as input data: a code written into program text as a literal is recorded again
;; at every node of the program's derivation, and certifying such a program ran
;; out of a 1.5 GB heap.
;;
;; Run from code/lcert (under the usual caps; the deep checks need a large stack):
;;   clojure -J-Xmx1500m -J-Xss512m -M:test -i ../../docs/log/2026-11-13-agent-probe.clj
(require '[lcert.core :as lc] '[lcert.eval :as ev] '[lcert.encode :as e])

(def SUCC '(Sigma [c w Syn] (Sigma [b w Bool] (T (if b (chk c d) tt)))))
(def Agent (list 'Pi '[d w Syn] (list 'Pi '[self w Syn] (list 'Pi '[i w Syn] (list 'prod 'Nat SUCC)))))

(defn hand-over
  "The successor part: the code c, the agent's own verdict (chk c d), and the
  evidence, by cases on that verdict."
  [c]
  (list 'pair SUCC c
        (list 'pair (list 'Sigma '[b w Bool] (list 'T (list 'if 'b (list 'chk c 'd) 'tt)))
              (list 'chk c 'd)
              (list 'elim-bool '[x (T (if x x tt))] (list 'chk c 'd) 'star 'star))))

(defn agent-form [out succ]
  (list 'the Agent (list 'fn '[d w Syn self w Syn i w Syn] (list 'pair (list 'prod 'Nat SUCC) out (hand-over succ)))))

(def stay2 (agent-form '(succ (succ zero)) 'self))  ; acts 2 and keeps itself
(def adopt (agent-form 'zero 'i))                    ; acts 0 and adopts the candidate it is given

(def type-code (lc/run 0 (list 'code Agent)))
(defn lit [c] (lc/parse-program 0 (list 'code-literal c)))

(defn round
  "Run the program a code encodes on (⌜Agent⌝, its own code, input)."
  [code input]
  (let [dd (e/dec-deriv code)
        m (count (:ctx dd))
        [_ out [_ succ [_ verdict _]]] (ev/run m [:app [:app [:app (:term dd) (lit type-code)] (lit code)] (lit input)])]
    {:out out :succ succ :verdict verdict :recheck (lc/check-code succ type-code)}))

(def stay2-code (:code (lc/certify 0 stay2)))
(def adopt-code (:code (lc/certify 0 adopt)))
(println "derivation nodes: stay2" (:nodes (lc/certify 0 stay2)) ", adopt" (:nodes (lc/certify 0 adopt))
         "; stay2's code checks at the agent type:" (lc/check-code stay2-code type-code))

(def bogus '[:sn :a [:sl :b] [:sl :c]])
(doseq [[label cand] [["stay2's code" stay2-code] ["a non-derivation" bogus] ["the code of Nat" (lc/run 0 '(code Nat))]]]
  (let [r (round adopt-code cand)]
    (println "adopt, offered" label "-> output" (:out r) ", its verdict" (:verdict r) ", runner's re-check" (:recheck r))))

(defn lineage [code inputs]
  (loop [code code, inputs inputs, outs []]
    (if (empty? inputs)
      outs
      (let [r (round code (first inputs))]
        (if (and (:verdict r) (:recheck r))
          (recur (:succ r) (rest inputs) (conj outs (:out r)))
          (conj outs (:out r) :stop))))))

(println "lineage adopt -> stay2 -> stay2 -> stay2:" (lineage adopt-code [stay2-code bogus bogus bogus]))
(println "lineage adopt, offered junk:" (lineage adopt-code [bogus stay2-code]))
(println "AGENT-PROBE-OK")
