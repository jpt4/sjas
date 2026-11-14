;; Probe: an agent that uses reflect, λᶜᵉʳᵗ₀'s certified self-evaluation, to
;; score a candidate before adopting it. No rule of the calculus is added.
;; Input: a candidate successor code `cand` and the code `sc` of a score program's derivation.
;; The agent pays for a certificate of the score program from a supply of
;; tokens written into its own text, checks it at Nat with `inspect`, runs it to
;; completion with `reflect` (no fuel bound), and adopts the candidate iff the
;; score is positive; otherwise it hands over to itself.
;; Run from code/lcert: clojure -J-Xmx1500m -J-Xss512m -M:test -i ../../docs/tslc/probe-reflect-adoption.clj
(require '[lcert.core :as lc] '[lcert.eval :as ev] '[lcert.encode :as e] '[lcert.examples :as ex])
(def SUCC '(Sigma [c w Syn] (Sigma [b w Bool] (T (if b (chk c d) tt)))))
(def Agent (list 'Pi '[d w Syn] (list 'Pi '[self w Syn] (list 'Pi '[cand w Syn] (list 'Pi '[sc w Syn] (list 'tensor 'Nat SUCC))))))
(defn hand-over [c]
  (list 'pair SUCC c
        (list 'pair (list 'Sigma '[b w Bool] (list 'T (list 'if 'b (list 'chk c 'd) 'tt)))
              (list 'chk c 'd)
              (list 'elim-bool '[x (T (if x x tt))] (list 'chk c 'd) 'star 'star))))
(defn agent-form [out succ] (list 'the Agent (list 'fn '[d w Syn self w Syn cand w Syn sc w Syn]
                                                  (list 'pair (list 'tensor 'Nat SUCC) out (hand-over succ)))))
(def K 40)
(def supply (reduce (fn [acc j] (list 'node (symbol (str "$" j)) :a '(leaf :a) acc)) '(leaf :a) (range K 0 -1)))
(def score-form (ex/parse-then 'sc supply 'Nat '(inspect Nat t (code Nat) [x e] (reflect Nat x e) [x e] zero)))
(def evaluator
  (list 'the Agent
        (list 'fn '[d w Syn self w Syn cand w Syn sc w Syn]
;; The score is computed from tokens, so it may be used once: duplicate it by
              ;; iteration, use one copy as the output and the other for the decision.
                    (list 'let-pair (list 'tensor 'Nat SUCC) '[s1 s2]
                          (list 'rec-nat '[x (tensor Nat Nat)] '(pair (tensor Nat Nat) zero zero)
                                '[k y] '(let-pair (tensor Nat Nat) [a b] y (pair (tensor Nat Nat) (succ a) (succ b)))
                                score-form)
                          (list 'pair (list 'tensor 'Nat SUCC) 's1
                                (list 'elim-bool ['x SUCC] '(rec-nat [q Bool] ff [k y] tt s2)
                                      (hand-over 'cand) (hand-over 'self)))))))
(def stay2 (agent-form '(succ (succ zero)) 'self))
(def type-code (lc/run 0 (list 'code Agent)))
(defn lit [c] (lc/parse-program 0 (list 'code-literal c)))
(defn round [code input]
  (let [dd (e/dec-deriv code), m (count (:ctx dd))
        [_ out [_ succ [_ verdict _]]]
        (ev/run m [:app [:app [:app [:app (:term dd) (lit type-code)] (lit code)] (lit (first input))] (lit (second input))])]
    {:out out :succ succ :verdict verdict :recheck (lc/check-code succ type-code)}))
(println "evaluator type-checks at budget" K ":" (some? (lc/check-program K evaluator)))
(def ev-cert (lc/certify K evaluator))
(def ev-code (:code ev-cert))
(def s2-code (:code (lc/certify 0 stay2)))
(println "derivation nodes: evaluator" (:nodes ev-cert) "(budget" (:budget ev-cert) "), stay2" (:nodes (lc/certify 0 stay2)))
(def score-two (:code (lc/certify 0 '(succ (succ zero)))))
(def score-zero (:code (lc/certify 0 'zero)))
(def score-bool (:code (lc/certify 0 'tt)))
(def junk '[:sn :a [:sl :b] [:sl :c]])
(doseq [[label sc] [["a score program computing 2" score-two] ["a score program computing 0" score-zero]
                    ["a Bool program" score-bool] ["junk" junk]]]
  (let [r (round ev-code [s2-code sc])]
    (println "offered stay2 with" label "-> score" (:out r) "| adopts stay2:" (= (:succ r) s2-code)
             "| keeps itself:" (= (:succ r) ev-code) "| verdict" (:verdict r) "| re-check" (:recheck r))))
(println "PROBE-REFLECT-ADOPTION-OK")
