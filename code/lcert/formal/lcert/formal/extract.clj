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
            [ansatz.surface.match :as sm]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lcert.formal.base :as b]))

(defonce ^:private runtime-ns (create-ns 'lcert.extracted.runtime))
(defonce ^:private original-tagged @#'cg/tagged-inductive?)

(def ^:private direct-surface-source
  ;; a/defn checks these recursive bodies via well-founded recursion but
  ;; compiles their original surface bodies as direct recursion. Reuse that
  ;; same Ansatz path to obtain standalone generated forms.
  {'nthE "formal/lcert/formal/judgment.clj"
   'nthU "formal/lcert/formal/judgment.clj"
   'nthS "formal/lcert/formal/conv.clj"})

;; defonce takes no docstring argument, so the docstring is metadata.
(defonce ^{:doc "Every generated form, by definition name, as emitted in this process: the
  source of the standalone artifact (runtime-source). Never used as a runtime
  acceptance shortcut."}
  emitted-forms
  (atom {}))

(defn- find-surface-definition [path name]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (loop []
      (let [form (read {:eof nil} r)]
        (cond
          (nil? form) (throw (ex-info "surface definition missing" {:name name :path path}))
          (and (seq? form) (= 'a/defn (first form)) (= name (second form))) form
          :else (recur))))))

(defn- compile-surface-recursion [E name shape]
  (let [[_ _ params result body] (find-surface-definition
                                   (get direct-surface-source name) name)
        pairs (#'a/parse-params params)
        term (binding [sm/*use-cases-on?* true]
               (#'a/build-telescope-fvar E pairs result body))
        emitted (sd/emit-direct-recursion E (str name) term shape)]
    (when-not (:fn emitted)
      (throw (ex-info "Ansatz could not emit surface recursion" {:name name})))
    emitted))

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
  ;; The default enum constructor lowering maps U.u0 to nil, while its
  ;; recursor dispatches on numeric 0. Give all U constructors a tag too;
  ;; this makes bare constructors and recursors use the same representation.
  (alter-var-root #'cg/tagged-inductive?
    (constantly (fn [E ind]
                  (or (= ind "U")
                      (original-tagged E ind)
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

(defn- call-nullary-data
  "Ansatz emits a zero-argument checked definition as a function value when
  it occurs as a data field (for example, sh4_M inside sh4_doubler). Its
  generated definition is a thunk, so call it at a bare reference. Preserve
  list heads, which are already calls. Dependencies have been registered by
  compile-closure! before this form is retained."
  [form]
  (let [nullary? (fn [s]
                   (and (symbol? s)
                        (= (str (ns-name runtime-ns)) (namespace s))
                        (let [entry (get @ingest/arity-registry (name s))]
                          (and (= (:sym entry) s) (zero? (:arity entry))))))]
    (letfn [(visit [x]
              (cond
                (and (symbol? x) (nullary? x)) (list x)
                (vector? x) (mapv visit x)
                (seq? x) (apply list (first x) (map visit (rest x)))
                (map? x) (into (empty x) (map (fn [[k v]] [(visit k) (visit v)]) x))
                :else x))]
      (visit form))))

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
      (if (contains? direct-surface-source name)
        (let [emitted (compile-surface-recursion E name shape)]
          (intern runtime-ns name (:fn emitted))
          (swap! emitted-forms assoc s (:fn-form emitted))
          (swap! ingest/arity-registry assoc s
            {:sym (symbol (str (ns-name runtime-ns)) s)
             :arity (:arity shape) :erased (:erased shape)})
          (:fn emitted))
        (let [[form generated]
              (try
                (let [emitted (sd/emit-direct-recursion E s value shape)]
                  (if (:fn emitted)
                    [(:fn-form emitted) (:fn emitted)]
                    ;; A recursor inside a lambda may leave a curried fn whose
                    ;; binders the direct-recursion emitter cannot peel. The
                    ;; uncurrying wrapper is part of the recorded form, so the
                    ;; artifact and this function are the same code.
                    (let [raw (cg/ansatz->clj E value [])]
                      (when (or (pos? (:erased shape))
                                (re-find #"unsupported rec pattern" (pr-str raw)))
                        (throw (ex-info "unsupported generated value" {:name s})))
                      (let [form (if (zero? (:arity shape))
                                   raw
                                   (list 'let ['curried raw]
                                         '(fn [& args] (reduce (fn [f arg] (f arg)) curried args))))]
                        [form (eval form)]))))
                (catch Throwable t
                  (throw (ex-info "Ansatz emission failed" {:name s} t))))]
        (let [fixed (call-nullary-data form)
              generated (if (= fixed form) generated (eval fixed))]
        (swap! emitted-forms assoc s fixed)
        (intern runtime-ns name generated)
        (swap! ingest/arity-registry assoc s
          {:sym (symbol (str (ns-name runtime-ns)) s)
           :arity (:arity shape) :erased (:erased shape)})
        generated))))))

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

(defn- formal-reference [form]
  ;; A callee that resolved through an a/defn registration in a formal
  ;; namespace (e.g. lcert.formal.conv/nthS): standalone code cannot call it,
  ;; so it is a dependency to generate, like an unresolved one.
  (some (fn [x] (when (and (symbol? x) (namespace x)
                           (str/starts-with? (namespace x) "lcert.formal"))
                  (symbol (name x))))
        (tree-seq coll? seq form)))

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
                          (if-let [dependency (formal-reference (get @emitted-forms (str name)))]
                            (do (ensure! dependency) (recur (inc attempt)))
                            (do (swap! done assoc name result) result)))))
                    (finally (swap! active disj name))))))]
      (ensure! root))))

;; ---------------------------------------------------------------------------
;; The standalone artifact: src/lcert/extracted/runtime.clj.

(def runtime-roots
  "The roots of the production artifact: the canonical checker and the F7
  encoders and decoder it is used with. Their closure is the artifact."
  '[Check decCert encCert encE concl])

(defn- stable-locals
  "Rename the generator's gensym locals (self_73174, f73170_0, t_73171, …)
  by order of first appearance, so the same definitions give the same text
  on every run. A digit run of three or more marks a gensym; the extracted
  function names have none, and qualified symbols are never renamed."
  [form]
  (let [ids (atom {})
        rename (fn [x]
                 (if-let [[_ pre digits post] (and (symbol? x) (nil? (namespace x))
                                                  (re-matches #"(.*?)(\d{3,})(.*)" (name x)))]
                   (let [k (or (get @ids digits)
                               (let [k (count @ids)] (swap! ids assoc digits k) k))]
                     (symbol (str pre "G" k post)))
                   x))]
    (walk/prewalk rename form)))

(def ^:private runtime-header
  ";; GENERATED by lcert.formal.extract/write-runtime! (ADR-0008). Do not edit.
;;
;; Each definition is Ansatz's code generation of a kernel-checked definition
;; of the same name in code/lcert/formal, compiled from its stored value in
;; dependency order (see formal/lcert/formal/extract.clj for the workarounds
;; it applies). The generator is not verified; the formal suite compares
;; these functions with kernel reduction, and its standalone-runtime-source
;; test fails when this file differs from a fresh generation.
")

(defn runtime-source
  "The source text of namespace lcert.extracted.runtime: every function in
  the closure of roots, freshly generated from the checked definitions,
  in name order after one declare."
  [roots]
  ;; Forget earlier extractions, so the whole closure is generated again.
  (let [rt (str (ns-name runtime-ns))]
    (swap! ingest/arity-registry
           (fn [m] (into {} (remove (fn [[_ v]] (and (symbol? (:sym v)) (= rt (namespace (:sym v))))) m)))))
  (reset! emitted-forms {})
  (doseq [r roots] (compile-closure! r))
  (let [forms @emitted-forms
        names (sort (keys forms))]
    (binding [*print-length* nil *print-level* nil *print-meta* false
              pp/*print-right-margin* 100]
      (with-out-str
        (print runtime-header)
        (println)
        (println "(ns lcert.extracted.runtime)")
        (println)
        (pp/pprint (list* 'declare (map symbol names)))
        (doseq [nm names]
          (println)
          (pp/pprint (list 'def (symbol nm) (stable-locals (get forms nm)))))))))

(defn write-runtime!
  "Regenerate src/lcert/extracted/runtime.clj (run from code/lcert in the
  formal environment, e.g. on the warm server)."
  []
  (let [f (io/file "src/lcert/extracted/runtime.clj")]
    (io/make-parents f)
    (spit f (runtime-source runtime-roots))
    (.getPath f)))
