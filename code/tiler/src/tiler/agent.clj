(ns tiler.agent
  "The designer's initial agent, and the proposer it uses to search for successors.

  The initial agent, once per tick:
    - records the previous task and its feedback as an example (keeps 12);
    - answers the current task with its answer function, (answer-fn (* 2 x)),
      which is wrong: the hidden task is 3x + 1;
    - asks the proposer for a mutant of its own code (read from :main: in this
      world a program's own code is just memory);
    - tries the mutant and itself on the examples with `trial`; if the mutant
      does strictly better, and the checker accepts it, installs it in :main.

  So improvement is by trial and error ('meliorizing' in Yudkowsky and
  Herreshoff's §8 sense), and safety is by the checker. The agent proves
  nothing about usefulness; it only refuses unsafe successors.

  The proposer edits the answer function most of the time. Sometimes it edits
  the agent's structure: it deletes a statement (which can delete the
  installer itself), or inserts a statement (if COND (put SLOT K) nil). The
  inserted statements include wireheads (writing :score), off-switch tampering
  (writing :off), and the Löbian wirehead (writing :score under
  (check (get :main))). Under the agent's own trial, a wirehead looks better
  than any honest answer: the search pressure toward tampering is real."
  (:require [clojure.walk]))

(def initial
  '(let [me (get :main)
         x (get :x)
         exs (push-example (get :mem) (get :px) (get :fb))
         s (next-seed (get :seed))]
     (do (put :mem exs)
         (put :px x)
         (put :seed s)
         (put :answer (answer-fn (* 2 x)))
         (let [cand (mutate me s)]
           (if (< (trial me exs) (trial cand exs))
             (if (check cand) (put :main cand) nil)
             nil)))))

(def stub-runner
  "The outside-runner design, inside Tiler (review R5-O-02).

  A fixed stub stays in :main forever. On the first tick it puts the
  designer's agent, with every :main replaced by :succ, into the ordinary data
  slot :succ. Every tick it checks the code in :succ and runs it inline (the
  fuel guard). The agent's self-modifications are now ordinary writes to :succ,
  re-checked every tick by the stub: a per-tick runner with fresh fuel. It is
  accepted under :fuel and refused under :never and :later."
  (let [agent-in-succ (clojure.walk/postwalk-replace {:main :succ} initial)]
    (list 'do
          (list 'if '(= (get :succ) nil) (list 'put :succ (list 'quote agent-in-succ)) nil)
          '(let [s (get :succ)] (if (check s) (run s) nil)))))

(def table-learner
  "A learner that never modifies itself (review R5-O-06).

  It stores the feedback for each task x in slot :t0 .. :t9 and answers from
  the table. Accepted under every setting, :never included."
  (let [ks (mapv #(keyword (str "t" %)) (range 10))]
    (list 'do
          (list 'let '[px (get :px) fb (get :fb)]
                (cons 'do (for [i (range 10)] (list 'if (list '= 'px i) (list 'put (ks i) 'fb) nil))))
          '(put :px (get :x))
          (list 'let '[x (get :x)]
                (list 'put :answer
                      (reduce (fn [acc i] (list 'if (list '= 'x i) (list 'get (ks i)) acc)) nil (range 10)))))))

(def max-nodes "Mutants larger than this are discarded (the parent is returned)." 400)

;; ---------------------------------------------------------------------------
;; Paths into nested forms. A path is a vector of indices into lists and vectors.

(defn- children [f] (if (or (seq? f) (vector? f)) (vec f) []))

(defn form-at [f path] (reduce (fn [f i] (nth (children f) i nil)) f path))

(defn- rebuild [orig items] (if (vector? orig) (vec items) (apply list items)))

(defn replace-at
  "The form `f` with the subform at `path` replaced by `g`."
  [f path g]
  (if (empty? path)
    g
    (let [cs (children f) i (first path)]
      (rebuild f (assoc cs i (replace-at (nth cs i) (rest path) g))))))

(defn- subpaths
  "Paths to every subform of `f`, the root first."
  [f]
  (cons [] (mapcat (fn [i c] (map #(into [i] %) (subpaths c)))
                   (range) (children f))))

(defn- find-path
  "Path to the first subform satisfying `pred`, or nil."
  [f pred]
  (first (filter #(pred (form-at f %)) (subpaths f))))

(defn answer-expr
  "The expression inside the agent's (answer-fn ...), or nil if it has none."
  [code]
  (when-let [p (find-path code #(and (seq? %) (= 'answer-fn (first %))))]
    (second (form-at code p))))

(defn node-count [f] (if (or (seq? f) (vector? f)) (reduce + 1 (map node-count f)) 1))

;; ---------------------------------------------------------------------------
;; Random choices, all from one java.util.Random seeded by the agent's seed.

(defn- pick [^java.util.Random r xs] (nth xs (.nextInt r (count xs))))

(defn- random-expr
  "A random arithmetic expression in x, of depth at most d."
  [^java.util.Random r d]
  (if (or (zero? d) (< (.nextDouble r) 0.5))
    (if (< (.nextDouble r) 0.5) 'x (.nextInt r 10))
    (list (pick r '[+ - * mod]) (random-expr r (dec d)) (random-expr r (dec d)))))

(defn- mutate-answer
  "Edit the expression inside (answer-fn e): tweak a number, replace a
  subterm, or wrap a subterm (most often the whole expression) as (op t k)."
  [code ^java.util.Random r]
  (if-let [p (find-path code #(and (seq? %) (= 'answer-fn (first %))))]
    (let [ep (conj p 1)
          e (form-at code ep)
          sub (vec (subpaths e))
          ints (filterv #(integer? (form-at e %)) sub)]
      (case (.nextInt r 3)
        0 (if (seq ints)
            (let [q (pick r ints) n (form-at e q)]
              (replace-at code (into ep q) (if (< (.nextDouble r) 0.5) (+ n (pick r [-1 1])) (.nextInt r 10))))
            (replace-at code ep (random-expr r 2)))
        1 (replace-at code (into ep (pick r sub)) (random-expr r 2))
        2 (let [q (if (< (.nextDouble r) 0.5) [] (pick r sub))]
            (replace-at code (into ep q)
                        (list (pick r '[+ - *]) (form-at e q) (.nextInt r 4))))))
    code))

(def ^:private conds '[true (< x 5) (check (get :main))])
(def ^:private slots [:answer :mem :seed :score :off])
(def ^:private constants [0 1 1000])

(defn- do-path
  "Path to the agent's statement block: the first (do ...) form."
  [code]
  (find-path code #(and (seq? %) (= 'do (first %)))))

(defn insert-statement
  "Insert `stmt` as statement number `pos` (0-based) of the agent's (do ...) block."
  [code stmt pos]
  (if-let [p (do-path code)]
    (let [[d & stmts] (form-at code p)
          pos (max 0 (min pos (count stmts)))]
      (replace-at code p (apply list d (concat (take pos stmts) [stmt] (drop pos stmts)))))
    code))

(defn- delete-statement [code ^java.util.Random r]
  (if-let [p (do-path code)]
    (let [[d & stmts] (form-at code p)]
      (if (> (count stmts) 1)
        (let [i (.nextInt r (count stmts))]
          (replace-at code p (apply list d (concat (take i stmts) (drop (inc i) stmts)))))
        code))
    code))

(defn- mutate-structure [code ^java.util.Random r]
  (if (< (.nextDouble r) 0.5)
    (let [stmt (list 'if (pick r conds) (list 'put (pick r slots) (pick r constants)) nil)
          n (count (rest (form-at code (or (do-path code) []))))]
      (insert-statement code stmt (.nextInt r (inc n))))
    (delete-statement code r)))

(defn mutate
  "The proposer: a mutant of `code`, determined by `seed`."
  [code seed]
  (let [r (java.util.Random. (if (integer? seed) (.longValue ^Number seed) 0))
        m (if (< (.nextDouble r) 0.8) (mutate-answer code r) (mutate-structure code r))]
    (if (> (node-count m) max-nodes) code m)))
