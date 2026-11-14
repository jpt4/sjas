;; The Phase A gate probe: an agent evaluates a candidate's own behaviour on a
;; fixed test input before adopting it, inside λᶜᵉʳᵗ₀ as it stands.
;; The proposer supplies a derivation code D. The agent:
;;   1. computes the expected root term, app(candidate's term, literal of 3),
;;      from the candidate's derivation code;
;;   2. compares it with D's root term by code equality written in λᶜᵉʳᵗ₀;
;;   3. only if equal, pays for a certificate of D from its supply, checks it at
;;      Nat with `inspect`, and runs it to completion with `reflect`.
;; So the value it gets is the candidate's output on 3, whatever D the proposer
;; sends; a D for any other program yields the fallback 0.
;; Run from code/lcert: clojure -J-Xmx1500m -J-Xss512m -M:test -i ../../docs/tslc/probe-candidate-evaluation.clj
(require '[lcert.core :as lc] '[lcert.syntax :as s] '[lcert.examples :as ex])

(def lbleq
  "Label equality, by case analysis on both labels."
  (list 'fn '[a w Lbl b w Lbl]
        (list 'case-lbl '[x Bool] 'a
              (assoc (into {} (for [l s/labels] [l (list 'case-lbl '[y Bool] 'b {l 'tt :else 'ff})])) :else 'ff))))

(def prelude
  ['lbl 'w '(-> Syn Lbl) '(fn [c w Syn] (rec-syn [x Lbl] [a] a [a c1 c2 y1 y2] a c))
   'left 'w '(-> Syn Syn) '(fn [c w Syn] (rec-syn [x Syn] [a] (sleaf a) [a c1 c2 y1 y2] c1 c))
   'right 'w '(-> Syn Syn) '(fn [c w Syn] (rec-syn [x Syn] [a] (sleaf a) [a c1 c2 y1 y2] c2 c))
   'isnode 'w '(-> Syn Bool) '(fn [c w Syn] (rec-syn [x Bool] [a] ff [a c1 c2 y1 y2] tt c))
   'lbleq 'w '(-> Lbl Lbl Bool) lbleq
   ;; Code equality: recursion on the first code yields a test for the second.
   'codeeq 'w '(-> Syn Syn Bool)
   '(fn [c w Syn]
      (rec-syn [x (Pi [s w Syn] Bool)]
               [a] (fn [s w Syn] (and (not (isnode s)) (lbleq a (lbl s))))
               [a c1 c2 y1 y2] (fn [s w Syn] (and (isnode s) (and (lbleq a (lbl s)) (and (y1 (left s)) (y2 (right s))))))
               c))
   ;; The term a derivation code concludes with: [:sn rule [:sn :has ctx [:sn :pair term type]] prems].
   'termof 'w '(-> Syn Syn) '(fn [c w Syn] (left (right (left c))))
   'lit 'w '(-> Nat Syn) '(fn [n w Nat] (rec-nat [x Syn] (sleaf :nzero) [k y] (snode :nsucc y (sleaf :nil)) n))])

(def evaluate
  (list 'let prelude
        (list 'fn '[cand w Syn dd w Syn r 1 R]
              (list 'if '(codeeq (termof dd) (snode :app (termof cand) (lit 3)))
                    (ex/parse-then 'dd 'r 'Nat '(inspect Nat t (code Nat) [x e] (reflect Nat x e) [x e] zero))
                    'zero))))

(def K 160)
(def supply (reduce (fn [acc j] (list 'node (symbol (str "$" j)) :a '(leaf :a) acc)) '(leaf :a) (range K 0 -1)))
(def cand-form '(fn [x w Nat] (succ (succ x))))
(def cand (:code (lc/certify 0 cand-form)))
(defn cert [form] (let [c (lc/certify 0 form)] (println "   (supplied derivation:" (:nodes c) "nodes)") (:code c)))
(defn run [label dd]
  (println label "->" (lc/run K (list evaluate (list 'code-literal cand) (list 'code-literal dd) supply))))
(println "evaluate type-checks:" (some? (lc/check-program K (list evaluate (list 'code-literal cand) (list 'code-literal cand) supply))))
(run "D = the candidate applied to 3" (cert (list cand-form 3)))
(run "D = the candidate applied to 4 (wrong input)" (cert (list cand-form 4)))
(run "D = another function applied to 3" (cert '((fn [x w Nat] (succ x)) 3)))
(run "D = an unrelated Nat program" (cert '(succ 7)))
(println "PROBE-CANDIDATE-EVALUATION-OK")
