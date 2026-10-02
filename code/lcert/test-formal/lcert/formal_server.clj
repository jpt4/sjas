(ns lcert.formal-server
  "A warm checking server for the formal suite (ADR-0006).

  Loading the formalization elaborates every declaration through the Ansatz
  kernel, which takes over half an hour.  This server loads everything once
  and records, for each formal namespace, the kernel constants it added.

  To re-check after an edit it finds the changed namespaces (by file time,
  counting the fragments a namespace loads into itself) and every namespace
  that depends on them: the set to redo.  It rebuilds the kernel environment
  without that set — the environment as it stood after lcert.formal.base,
  plus every other namespace's recorded constants (Env.addConstant: added,
  not re-checked; they were checked when first loaded, against the same
  constants they are now added beside) — and reloads the set in dependency
  order, then runs its suites.  Editing a namespace nothing depends on
  reloads that namespace alone.

  Ansatz also keeps state inside the Env as extensions (simp lemmas,
  matchers, attributes, instances); the rebuild copies those from the current
  environment.  They may still hold entries for the namespaces being redone;
  these are keyed by declaration name, and the reload overwrites them.  Other
  global atoms are left as they are, for the same reason.  A changed
  lcert.formal.base needs a restart.

  The full suite (bin/test-formal, a fresh JVM) stays the authoritative
  check before a push.

  Use: bin/formal-server starts it (socket REPL on port 5699);
  bin/formal-check re-checks what changed since the last check."
  (:require [ansatz.state :as st]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t])
  (:import (ansatz.kernel ConstantInfo Env)))

(def formal-dir "formal/lcert/formal")
(def suite-dir "test-formal/lcert/formal_test")

;; ---------------------------------------------------------------------------
;; The namespace graph, read from the source files' ns forms.

(defn- first-form [f] (with-open [r (java.io.PushbackReader. (io/reader f))] (read r)))

(defn- ns-of-file
  "The namespace a source file declares, or nil for a fragment that another
  namespace loads into itself (den_gen.clj, sem_gen.clj)."
  [^java.io.File f]
  (let [form (first-form f)]
    (when (and (seq? form) (= 'ns (first form))) (second form))))

(defn- file-of-ns [dir prefix sym]
  (io/file dir (str (str/replace (subs (str sym) (count prefix)) "-" "_") ".clj")))

(defn- formal-requires
  "The lcert.formal.* namespaces a formal source file requires, read from its ns form."
  [f]
  (->> (tree-seq coll? seq (first-form f))
       (filter symbol?)
       (filter #(str/starts-with? (str %) "lcert.formal."))
       set))

(defn dep-graph
  "formal namespace -> the formal namespaces it requires."
  []
  (let [files (filter #(and (str/ends-with? (.getName ^java.io.File %) ".clj") (ns-of-file %))
                      (.listFiles (io/file formal-dir)))]
    (into {} (for [f files :let [n (ns-of-file f)]] [n (disj (formal-requires f) n)]))))

(defn load-order
  "Every formal namespace, each after the ones it requires (a topological order)."
  [deps]
  (let [visit (fn visit [[order seen] n]
                (if (seen n) [order seen]
                    (let [[order seen] (reduce visit [order (conj seen n)] (sort (filter deps (deps n))))]
                      [(conj order n) seen])))]
    (first (reduce visit [[] #{}] (sort (keys deps))))))

(defn dependents-closure
  "The namespaces in ns-set and every namespace that (transitively) requires one."
  [deps ns-set]
  (loop [acc (set ns-set)]
    (let [more (set (for [[n ds] deps :when (and (not (acc n)) (some acc ds))] n))]
      (if (empty? more) acc (recur (into acc more))))))

;; ---------------------------------------------------------------------------
;; State.

(defonce ^{:doc "file path -> modification time at its last load"} stamps (atom {}))
(defonce ^{:doc "formal namespace -> the ConstantInfos it added"} deltas (atom {}))
(defonce ^{:doc "the environment just after lcert.formal.base loaded"} base-env (atom nil))

(def ^:private extension-keys
  "Every Env extension Ansatz 0.2.115 uses (ansatz.attrs, simp_index, matchers,
  tactic.instance, codegen).  The rebuild copies these from the current env."
  [:simp-lemmas :simp-unfold :simp-priorities :simp-index :extern :csimp
   :implemented-by :instances :matcher-info])

(defn- mtime [^java.io.File f] (.lastModified f))
(defn- stamp! [^java.io.File f] (swap! stamps assoc (.getPath f) (mtime f)))
(defn- changed? [^java.io.File f] (not= (get @stamps (.getPath f)) (mtime f)))

(defn- source-files
  "A formal namespace's file and the fragments it loads into itself with
  (load \"x\") — e.g. den.clj loads den_gen.clj.  Editing a fragment reloads
  its host namespace."
  [n]
  (let [f (file-of-ns formal-dir "lcert.formal." n)
        loads (re-seq #"\(load \"([\w-]+)\"\)" (slurp f))]
    (cons f (for [[_ x] loads] (io/file formal-dir (str x ".clj"))))))

(defn- ns-changed? [n] (boolean (some changed? (source-files n))))
(defn- suite-of [n] (symbol (str "lcert.formal-test." (subs (str n) (count "lcert.formal.")))))
(defn- suite-file [n] (file-of-ns suite-dir "lcert.formal." n))

(defn- env ^Env [] @st/ansatz-env)
(defn- env-names [] (set (map #(.name ^ConstantInfo %) (.allConstants (env)))))

;; ---------------------------------------------------------------------------
;; Loading and checking.

(defn- load-ns!
  "(Re)load one formal namespace, recording the constants it adds."
  [n]
  (let [before (env-names) t0 (System/currentTimeMillis)]
    (require n :reload)
    (swap! deltas assoc n (vec (remove #(contains? before (.name ^ConstantInfo %))
                                       (.allConstants (env)))))
    (run! stamp! (source-files n))
    (println (format "  loaded %s (%.0f s, %d constants)"
                     n (/ (- (System/currentTimeMillis) t0) 1000.0) (count (@deltas n))))))

(defn- run-suites! [nss]
  (let [nss (filter #(.exists (suite-file %)) nss)]
    ;; remove-ns first: require :reload keeps vars, so a deleted deftest would still run.
    (doseq [n nss] (remove-ns (suite-of n)) (require (suite-of n) :reload) (stamp! (suite-file n)))
    ;; clojure.test writes to *test-out*, bound at load to the server's stdout;
    ;; rebind it so the report reaches the client (bin/formal-check).
    (binding [t/*test-out* *out*]
      (if (seq nss) (apply t/run-tests (map suite-of nss)) (println "  no suites to run")))))

(defn- rebuild-env!
  "Reset the kernel environment to base-env plus the recorded constants of the
  namespaces in keep (in load order), carrying over the current extensions."
  [keep]
  (let [cur (env)
        e (reduce (fn [^Env e ci] (.addConstant e ci)) @base-env (mapcat @deltas keep))
        e (reduce (fn [^Env e k] (let [v (.getExtension cur k)] (if (some? v) (.withExtension e k v) e)))
                  e extension-keys)]
    (reset! st/ansatz-env e)))

(defn warm!
  "Load everything once (the slow first pass) and run every suite."
  []
  (reset! deltas {})
  (require 'lcert.formal.base)
  (run! stamp! (source-files 'lcert.formal.base))
  (reset! base-env (env))
  (let [order (load-order (dep-graph))]
    (doseq [n order :when (not= n 'lcert.formal.base)] (load-ns! n))
    (run-suites! order)))

(defn recheck!
  "Re-check what changed since the last load: rebuild the environment without
  the changed namespaces and their dependents, reload those, and run their
  suites and any changed suite file.  New formal files are picked up."
  []
  (let [deps (dep-graph)
        order (remove #{'lcert.formal.base} (load-order deps))
        changed (set (filter #(or (ns-changed? %) (not (contains? @deltas %))) order))
        suites-changed (filter #(let [f (suite-file %)] (and (.exists f) (changed? f))) order)]
    (cond
      (ns-changed? 'lcert.formal.base)
      (println "lcert.formal.base changed: restart the server (bin/formal-server).")

      (seq changed)
      (let [redo (dependents-closure deps changed)]
        (println "Reloading" (count redo) "namespaces:" (str/join " " (filter redo order)))
        (rebuild-env! (remove redo order))
        (try (doseq [n order :when (redo n)] (load-ns! n))
             (run-suites! (distinct (concat (filter redo order) suites-changed)))
             (catch Throwable e
               (println "Load failed:" (.getMessage e))
               (println "1 failures, 1 errors."))))

      (seq suites-changed) (run-suites! suites-changed)
      :else (println "Nothing changed since the last check."))))
