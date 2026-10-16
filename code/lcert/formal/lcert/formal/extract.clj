(ns lcert.formal.extract
  "ADR-0008 build-time bridge from kernel definitions to Clojure functions.

  Ansatz 0.2.115 does not compile opaque `kdef!` definitions, and its Code
  lowering omits the tag when the first of two constructors has fields.
  Compile the checked stored value through Ansatz's emitter, in dependency
  order, with one consistent tagged representation. Every helper named in
  the order is recompiled: mixing it with an earlier a/defn function could
  silently mix the two Code representations."
  (:require [ansatz.core :as a]
            [ansatz.codegen :as cg]
            [ansatz.codegen.storedef :as sd]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as n]
            [ansatz.surface.ingest :as ingest]
            [ansatz.surface.elaborate :as el]
            [lcert.formal.base :as b]))

(defonce ^:private runtime-ns (create-ns 'lcert.extracted.runtime))
(defonce ^:private original-tagged @#'cg/tagged-inductive?)

(def ^:private code-independent-surface
  ;; These a/defn functions were already generated from checked definitions
  ;; before the Code tag workaround. They operate only on List/Exp/U/Nat, so
  ;; their representation is unchanged. Recompiling their well-founded fix
  ;; bodies currently emits an unresolved WellFounded.fix class reference.
  {'nthE 'lcert.formal.judgment/nthE
   'nthU 'lcert.formal.judgment/nthU
   'nthS 'lcert.formal.conv/nthS})

(defn- tagged-with-fielded-first-constructor?
  "Ansatz's unhandled two-constructor shape, e.g. Code.sl/Code.sn."
  [E ind]
  (boolean
    (when-let [ci (env/lookup E (n/from-string ind))]
      (let [ctors (.ctors ci)]
        (and ctors (= 2 (alength ctors))
             (when-let [c0 (env/lookup E (aget ctors 0))]
               (pos? (.numFields c0))))))))

(defn- use-consistent-tags! []
  ;; All three constructor/recursor sites in Ansatz call this Var. Patching
  ;; it before compilation keeps generated values and matches consistent.
  (alter-var-root #'cg/tagged-inductive?
    (constantly (fn [E ind]
                  (or (original-tagged E ind)
                      (tagged-with-fielded-first-constructor? E ind))))))

(def ^:private ob-eta-form
  ;; The code generator drops a function-valued Option.some minor. Making
  ;; the minor a lambda is definitionally equal by eta and lowers correctly.
  '(fn [α :- Type, β :- Type, o :- (Option α), f :- (=> α (Option β))]
     (Option.rec$1$0 α (fn [_ :- (Option α)] (Option β))
       (Option.none β) (fn [x :- α] (f x)) o)))

(defn- checked-ob-eta [E ci]
  (let [term (el/elaborate E (b/lv ob-eta-form) (.type ci))]
    ;; Check the candidate as a definition, then check it is definitionally
    ;; equal to the existing ob. The generated code has exactly ob's meaning.
    (env/check-constant E (env/mk-def (n/from-string "obEta_x8") [] (.type ci) term))
    (when-not (.isDefEq (ansatz.kernel.TypeChecker. E) (.getValue ci) term)
      (throw (ex-info "eta expansion differs from checked ob" {})))
    term))

(defn- compile-one!
  [E name]
  (let [s (str name)
        ci (env/lookup E (n/from-string s))]
    (when-not (and ci (.isDef ci) (.getValue ci))
      (throw (ex-info "no checked definition with a value" {:name s})))
    (let [[binders _] (#'sd/strip-telescope (.type ci))
          shape (cond
                  (#{"ob" "om"} s) {:arity 2 :erased 2}
                  (empty? binders)
                  {:arity 0 :erased 0}
                  :else (#'sd/audit-binders binders))
          value (if (= s "ob") (checked-ob-eta E ci) (.getValue ci))]
      (when-not shape
        (throw (ex-info "unsupported runtime binder shape" {:name s})))
      ;; Ignore a previous surface or earlier extraction entry. The requested
      ;; order supplies dependencies already compiled in this same encoding.
      (swap! ingest/arity-registry dissoc s)
      (if-let [surface-sym (get code-independent-surface name)]
        (do
          (when (or (contains? (#'sd/const-refs (.getValue ci)) "Code")
                    (re-find #"const Code" (str (.type ci))))
            (throw (ex-info "surface function depends on Code" {:name s})))
          (let [f (var-get (requiring-resolve surface-sym))]
            (swap! ingest/arity-registry assoc s
              {:sym surface-sym :arity (:arity shape) :erased (:erased shape)})
            f))
        (let [generated
            (try
              (let [emitted (sd/emit-direct-recursion E s value shape)]
                (if-let [f (:fn emitted)]
                  f
                  ;; A recursor inside a lambda may leave a curried fn whose
                  ;; binders the direct-recursion emitter cannot peel.
                  (let [raw (cg/ansatz->clj E value [])]
                    (when (or (pos? (:erased shape))
                              (re-find #"unsupported rec pattern" (pr-str raw)))
                      (throw (ex-info "unsupported generated value" {:name s})))
                    (let [curried (eval raw)]
                      (if (zero? (:arity shape))
                        curried
                        (fn [& args]
                          (reduce (fn [f arg] (f arg)) curried args)))))))
              (catch Throwable t
                (throw (ex-info "Ansatz emission failed" {:name s} t))))]
        (intern runtime-ns name generated)
        (swap! ingest/arity-registry assoc s
          {:sym (symbol (str (ns-name runtime-ns)) s)
           :arity (:arity shape) :erased (:erased shape)})
        generated)))))

(defn compile-functions!
  "Generate the named checked definitions in dependency order. Returns
  symbol -> executable fn. Call only in the formal build/test process."
  [names]
  (use-consistent-tags!)
  (let [E @a/ansatz-env]
    (into {} (for [name names] [name (compile-one! E name)]))))

(defn- unresolved-dependency [error]
  ;; The emitter evaluates its generated form. If a kernel function was
  ;; registered only by arity, Clojure names the missing callable in a cause.
  (loop [e error]
    (when e
      (if-let [[_ name] (re-find #"Unable to resolve symbol: ([A-Za-z][A-Za-z0-9_]*)"
                                 (or (.getMessage e) ""))]
        (symbol name)
        (recur (.getCause e))))))

(defn compile-closure!
  "Generate one checked function and only the dependencies its emitted body
  actually calls. A missing callable is compiled first, then the caller is
  regenerated. This avoids manually maintaining a large dependency list."
  [root]
  (use-consistent-tags!)
  (let [E @a/ansatz-env
        active (atom #{})
        done (atom {})]
    (letfn [(ensure! [name]
              (if-let [f (get @done name)]
                f
                (do
                  (when (contains? @active name)
                    (throw (ex-info "cyclic generated dependency" {:name name})))
                  (swap! active conj name)
                  (try
                    (loop [attempt 0]
                      (when (>= attempt 256)
                        (throw (ex-info "too many generated dependencies" {:name name})))
                      (let [result (try (compile-one! E name)
                                        (catch Throwable t t))]
                        (if (instance? Throwable result)
                          (if-let [dependency (unresolved-dependency result)]
                            (do (ensure! dependency) (recur (inc attempt)))
                            (throw result))
                          (do (swap! done assoc name result) result))))
                    (finally (swap! active disj name))))))]
      (ensure! root))))
