(ns lcert.formal.base
  "Infrastructure for the formalization of R4-metatheory.md (ADR-0006, F0).

  Loading a formal namespace runs Ansatz's elaborator and its Lean-4-compatible
  kernel on every declaration; a definition or proof the kernel rejects aborts
  the load.  The environment is Ansatz's bundled Lean Init only, offline.

  Two surface workarounds, found by the spikes of 2026-09-27:
  - `a/defn` cannot define a Type-valued or dependently typed function: it
    mis-infers the recursor's universe, and its code generator cannot compile
    a type.  `kdef` instead elaborates the body with Ansatz's surface
    elaborator and installs it with the kernel's `check-constant`, bypassing
    code generation.  The kernel checks it exactly as any other definition.
  - Recursor universe levels must then be explicit, `Sk.rec.{2}`.  The Clojure
    reader takes `.{2}` for a map, so levels are written `Sk.rec$2` and
    rewritten after reading (`lv`)."
  (:require [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as n]
            [ansatz.surface.elaborate :as el]
            [clojure.string :as str]
            [clojure.walk :as w]))

(defonce ^:private init (do (a/load-init!) true))

(defn lv
  "Rewrite symbols `Foo$2` / `Foo$1$2` into `Foo.{2}` / `Foo.{1,2}`."
  [form]
  (w/postwalk
   (fn [x]
     (if (and (symbol? x) (re-find #"\$\d" (name x)))
       (let [[base & levels] (str/split (name x) #"\$")]
         (symbol (str base ".{" (str/join "," levels) "}")))
       x))
   form))

(defn kdef!
  "Define constant `nm` of type `ty` with value `body`, kernel-checked,
  without generating Clojure code."
  [nm ty body]
  (let [env @a/ansatz-env
        t (el/elaborate env (lv ty))
        b (el/elaborate env (lv body) t)]
    (swap! a/ansatz-env env/check-constant (env/mk-def (n/from-string (str nm)) [] t b))
    nm))

(defmacro kdef
  "(kdef Name Type Body): a kernel-checked definition (see kdef!)."
  [nm ty body]
  `(kdef! '~nm '~ty '~body))

(defmacro thm
  "(thm name [params] prop tactics...): a/theorem, with `$n` levels rewritten."
  [nm params prop & tactics]
  `(a/prove-theorem '~nm '~(lv params) '~(lv prop) '~(lv (vec tactics))))

(defn has? [nm] (a/has-constant? (str nm)))

(defn rejects?
  "True iff the kernel refuses to prove `prop` with `tactics`: the negative
  checks that keep formal statements from being vacuous."
  [params prop tactics]
  (let [nm (gensym "neg_")]
    (try (a/prove-theorem nm (lv params) (lv prop) (lv (vec tactics))) false
         (catch Throwable _ true))))
