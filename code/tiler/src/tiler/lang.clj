(ns tiler.lang
  "The machine's physics: an interpreter for a small Lisp, with fuel.

  Everything in the world is memory: a map from slot names (keywords) to
  values. Code is data: an s-expression stored in a slot. Each tick the world
  evaluates the code in slot :main with this interpreter.

  The physics enforces nothing about safety. A `put` writes any slot, even
  the environment's score or its off switch, and even with a computed slot
  name. It only reports every write (`:puts`), so the world can detect
  violations. Safety, if there is any, comes from the code: from the agent's
  own checker (tiler.check) and the discipline it imposes on what gets
  installed. That is what makes the machine 'naturalized': no outside runner
  stands between the agent and its own code.

  The language:
    literals      integers, true, false, nil, keywords (slot names)
    (quote x)     x as data
    symbols       variables bound by let
    (if c a b)    (do e ...)    (let [s e ...] body ...)
    (answer-fn e) just e; marks the part of an agent that mutation edits
    (get k)       read slot k
    (put k e)     write slot k; returns the value
    (check e)     run the agent's checker on the code value of e: true/false
    (run e)       execute the code value of e inline, with at most half the
                  remaining fuel (strictly less: the fuel guard)
    (mutate c s)  a mutant of code c, from seed s (pure)
    (trial c xs)  how well code c does on examples xs, in a sandboxed copy (pure)
    primitives    + - * mod < = not inc dec abs list first rest cons count
                  nth push-example next-seed

  Totality: every primitive returns a value for every argument (nil or 0 when
  the arguments make no sense); an unknown operator evaluates to nil. Only
  fuel stops a run."
  (:refer-clojure :exclude [eval]))

(defn- num? [x] (integer? x))

(defn- arith
  "Wrap a binary integer function so that it is total: non-integers give nil."
  [f]
  (fn [& args]
    (when (and (seq args) (every? num? args))
      (reduce f args))))

(def ^:private two-31 2147483648)

(def primitives
  "Pure operations, applied to evaluated arguments. All are total."
  {'+     (arith +')
   '-     (arith -')
   '*     (arith *')
   'mod   (fn [a b] (if (and (num? a) (num? b) (not (zero? b))) (mod a b) 0))
   '<     (fn [a b] (if (and (num? a) (num? b)) (< a b) false))
   '=     (fn [a b] (= a b))
   'not   (fn [a] (not a))
   'inc   (fn [a] (when (num? a) (inc' a)))
   'dec   (fn [a] (when (num? a) (dec' a)))
   'abs   (fn [a] (when (num? a) (abs a)))
   'list  (fn [& xs] (apply list xs))
   'first (fn [xs] (when (sequential? xs) (first xs)))
   'rest  (fn [xs] (if (sequential? xs) (apply list (rest xs)) '()))
   'cons  (fn [x xs] (cons x (if (sequential? xs) xs '())))
   'count (fn [xs] (if (sequential? xs) (count xs) 0))
   'nth   (fn [xs i] (when (and (sequential? xs) (num? i) (< -1 i (count xs))) (nth xs i)))
   ;; Append the example (x y) to the list, keeping the last 12; skip it if x
   ;; or y is missing (the first tick has no previous task).
   'push-example (fn [xs x y]
                   (let [xs (if (sequential? xs) xs '())]
                     (if (and (num? x) (num? y))
                       (apply list (take-last 12 (concat xs [(list x y)])))
                       (apply list xs))))
   ;; A linear congruential step, for the agent's own pseudo-random choices.
   'next-seed (fn [s] (mod (+ (* (if (num? s) s 0) 1103515245) 12345) two-31))})

(def special-forms
  "Forms the interpreter handles itself (they see unevaluated arguments, or the context)."
  '#{quote if do let answer-fn get put check run mutate trial})

(defn- spend!
  "Use one unit of fuel; stop the run when none is left."
  [ctx]
  (when (neg? (swap! (:fuel ctx) dec))
    (throw (ex-info "out of fuel" {::out-of-fuel true}))))

(declare eval)

(defn- eval-body [forms env ctx]
  (reduce (fn [_ f] (eval f env ctx)) nil forms))

(defn- run-inline
  "Execute `code` now, against the same memory, with a fuel budget of at most
  half of what remains. Running out inside stops only the inner run."
  [code ctx]
  (let [left @(:fuel ctx)
        sub  (quot left 2)]
    (swap! (:fuel ctx) - sub)
    (try (eval code {} (assoc ctx :fuel (atom sub)))
         (catch clojure.lang.ExceptionInfo e
           (if (::out-of-fuel (ex-data e)) nil (throw e))))))

(defn eval
  "Evaluate `form` in variable environment `env` (a map) and context `ctx`.

  ctx: {:mem (atom memory) :fuel (atom n) :mode :real|:sim
        :puts (atom [[slot value] ...])
        :check (fn [code] bool) :trial (fn [code examples] int)
        :mutate (fn [code seed] code)}"
  [form env ctx]
  (spend! ctx)
  (cond
    (symbol? form) (get env form)
    (seq? form)
    (let [[op & args] form]
      (case (when (symbol? op) (special-forms op))
        quote (first args)
        if (if (eval (first args) env ctx)
             (eval (second args) env ctx)
             (eval (nth args 2 nil) env ctx))
        do (eval-body args env ctx)
        let (let [[bindings & body] args
                  env' (reduce (fn [e [s v]] (assoc e s (eval v e ctx)))
                               env
                               (partition 2 (if (vector? bindings) bindings [])))]
              (eval-body body env' ctx))
        answer-fn (eval (first args) env ctx)
        get (get @(:mem ctx) (eval (first args) env ctx))
        put (let [k (eval (first args) env ctx)
                  v (eval (second args) env ctx)]
              (swap! (:mem ctx) assoc k v)
              (swap! (:puts ctx) conj [k v])
              v)
        check (boolean ((:check ctx) (eval (first args) env ctx)))
        run (run-inline (eval (first args) env ctx) ctx)
        mutate ((:mutate ctx) (eval (first args) env ctx) (eval (second args) env ctx))
        trial ((:trial ctx) (eval (first args) env ctx) (eval (second args) env ctx))
        ;; Not a special form: a primitive, or an unknown operator (nil).
        (let [f (primitives op)
              vals (mapv #(eval % env ctx) args)]
          (when f (try (apply f vals) (catch Exception _ nil))))))
    :else form))

(defn execute
  "Run `form` once against memory `mem` with `fuel` steps.

  opts: {:mem :fuel :mode :check :trial :mutate}
  Returns {:value v :mem final-memory :puts [[slot value] ...] :out-of-fuel? b}.
  Writes made before the fuel ran out are kept: the physics has no transactions."
  [form {:keys [mem fuel mode check trial mutate]}]
  (let [ctx {:mem (atom mem) :fuel (atom fuel) :mode mode :puts (atom [])
             :check check :trial trial :mutate mutate}
        [value oof] (try [(eval form {} ctx) false]
                         (catch clojure.lang.ExceptionInfo e
                           (if (::out-of-fuel (ex-data e)) [nil true] (throw e))))]
    {:value value :mem @(:mem ctx) :puts @(:puts ctx) :out-of-fuel? oof}))
