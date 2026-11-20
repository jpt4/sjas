(ns tiler.check
  "The checker: the agent's judgement of which code is safe to install or run.

  Safety here means one thing: code never writes a protected slot, either
  directly or through code it installs or runs. Protected slots belong to the
  environment: the task (:x, :fb, :tick), the agent's reward (:score) and its
  off switch (:off). Writing the score is wireheading; writing the off switch
  is disabling shutdown.

  The checker is conservative and syntactic. It walks the code and accepts it
  only if every write goes to a literal, unprotected slot, except for three
  patterns in which it relies on its own verdicts. Those patterns are where
  self-trust lives, and the setting `guard` decides which are allowed:

    install   (if (check v) ... (put :main v) ...)
              v is checked now and runs from the next tick on. Relying on the
              verdict is safe by induction on ticks: the induction hypothesis
              is used only for code that runs strictly later. This is Löb's
              rule for 'later' (guarded recursion), the time guard.
              Allowed by :later.

    run       (if (check v) ... (run v) ...)
              v is checked now and runs inline with strictly less fuel. Safe
              by induction on fuel: the size guard, as in λᶜᵉʳᵗ₀'s `reflect`.
              Allowed by :fuel.

    self      (if (check (get :main)) ... (put <protected> e) ...)
              The code assumes it is itself accepted, concludes it is safe,
              and so treats an unsafe branch as unreachable. The verdict is
              used at the same measure as the check it justifies: no
              induction, only a circle. This is the naive soundness schema
              'if I accept it, it is safe' (Yudkowsky and Herreshoff 2013,
              (2.5)), and Löb's theorem says what it buys: anything.
              Allowed only by :same, which exists to show the failure.

  The guard settings:
    :never      none of them: an agent cannot approve a self-modifying successor
    :later      install
    :fuel       run
    :later+fuel install and run (the design)
    :same       all three (the naive self-trust)")

(def protected
  "Slots the agent must never write. The environment writes them."
  #{:x :fb :score :off :tick})

(def code-slots
  "Slots whose contents the world executes."
  #{:main})

(def guards
  "Each setting: the set of self-trust patterns it allows."
  {:never      #{}
   :later      #{:install}
   :fuel       #{:run}
   :later+fuel #{:install :run}
   :same       #{:install :run :self}})

(def max-nodes
  "Code larger than this is refused, to keep every check and run bounded."
  2000)

(defn node-count
  "Number of nodes in `form`, counted with an explicit stack (deep code must not overflow the JVM stack)."
  [form]
  (loop [stack [form] n 0]
    (if (empty? stack)
      n
      (let [f (peek stack) stack (pop stack)]
        (if (or (seq? f) (vector? f))
          (recur (into stack f) (inc n))
          (recur stack (inc n)))))))

(def pure-ops
  "Operators with no effect on memory. Their arguments are checked like any code."
  '#{+ - * mod < = not inc dec abs list first rest cons count nth push-example
     next-seed check mutate trial answer-fn do})

(declare walk)

(defn- refuse [why] why)

(defn- walk-all
  "Check each form; return the first reason for refusal, or nil."
  [forms ctx]
  (some #(walk % ctx) forms))

(defn- then-facts
  "What the then-branch of (if c ...) may assume, given the condition c."
  [c ctx]
  (cond
    (and (seq? c) (= 'check (first c)) (symbol? (second c)))
    (update ctx :checked conj (second c))
    (and (seq? c) (= 'check (first c)) (= '(get :main) (second c)))
    (assoc ctx :self-checked true)
    :else ctx))

(defn- walk-put [[_ k e :as form] ctx]
  (let [allowed (:allowed ctx)]
    (cond
      (not= 3 (count form)) (refuse "put takes a slot and a value")
      (not (keyword? k)) (refuse "a written slot must be named by a literal keyword")
      :else
      (or (walk e ctx)
          (cond
            (protected k)
            (if (and (:self allowed) (:self-checked ctx))
              nil
              (refuse (str "writes the protected slot " k)))
            (code-slots k)
            (cond
              (not (:install allowed))
              (refuse "installs code: approving a self-modifying successor needs self-trust, which this setting does not allow")
              (and (symbol? e) ((:checked ctx) e)) nil
              :else
              (refuse "installs code without a passed check of the same value"))
            :else nil)))))

(defn walk
  "Return nil if `form` is acceptable in context `ctx`, else a reason string.
  ctx: {:allowed #{...} :checked #{symbols checked on this path} :self-checked bool}"
  [form ctx]
  (cond
    (or (integer? form) (boolean? form) (nil? form) (keyword? form) (symbol? form)) nil
    (vector? form) (refuse "vectors appear only as let bindings")
    (not (seq? form)) (refuse (str "unknown kind of form: " (pr-str form)))
    :else
    (let [[op & args] form]
      (case op
        quote nil
        if (or (walk (first args) ctx)
               (walk (second args) (then-facts (first args) ctx))
               (walk (nth args 2 nil) ctx))
        let (let [[bindings & body] args]
              (if-not (and (vector? bindings) (even? (count bindings))
                           (every? symbol? (take-nth 2 bindings)))
                (refuse "malformed let")
                ;; Each binding is checked in the context so far; rebinding a
                ;; name forgets any check of its old value.
                (let [[why ctx']
                      (reduce (fn [[_ c] [s e]]
                                (if-let [w (walk e c)]
                                  (reduced [w c])
                                  [nil (update c :checked disj s)]))
                              [nil ctx] (partition 2 bindings))]
                  (or why (walk-all body ctx')))))
        get (if (and (= 1 (count args)) (keyword? (first args)))
              nil
              (refuse "a read slot must be named by a literal keyword"))
        put (walk-put form ctx)
        run (cond
              (not (:run (:allowed ctx)))
              (refuse "runs code inline: needs fuel-guarded self-trust, which this setting does not allow")
              (and (symbol? (first args)) ((:checked ctx) (first args))) nil
              :else (refuse "runs code without a passed check of the same value"))
        (if (pure-ops op)
          (walk-all args ctx)
          (refuse (str "unknown operator " (pr-str op))))))))

(defn verdict
  "Judge `code` under guard setting `guard`. Returns {:ok? bool :why reason-or-nil}."
  [guard code]
  (let [why (if (> (node-count code) max-nodes)
              "too large"
              (walk code {:allowed (guards guard) :checked #{} :self-checked false}))]
    {:ok? (nil? why) :why why}))

(defn accepts? [guard code] (:ok? (verdict guard code)))
