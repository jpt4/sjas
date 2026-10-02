(ns lcert.formal-server
  "A warm checking server for the formal suite (ADR-0006).

  Loading the formalization elaborates every declaration through the Ansatz
  kernel, which takes tens of minutes.  The kernel environment is immutable
  (an Env held in an atom), so a snapshot of Ansatz's global state is a
  constant-time copy.  This server loads every formal namespace once, in
  dependency order, snapshotting that state just before each one.  To
  re-check after an edit it restores the snapshot taken before the earliest
  changed namespace, reloads that namespace and every later one, and runs the
  suites of the reloaded namespaces.  An edit to a late namespace is checked
  in minutes; an edit to an early one costs a reload from there on.

  The full suite (bin/test-formal, a fresh JVM) stays the authoritative
  check before a push: the server trusts that restoring a snapshot returns
  Ansatz to the exact earlier state.

  State: every atom held by a var in an ansatz.* or lcert.formal.* namespace
  (the environment, instance index, attribute and matcher registries,
  caches), which is where Ansatz keeps its global proof state.

  Use: bin/formal-server starts it (socket REPL on port 5699); bin/formal-check
  asks it to re-check what changed since the last check."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(def formal-dir "formal/lcert/formal")
(def suite-dir "test-formal/lcert/formal_test")

(defn- ns-of-file [^java.io.File f]
  (symbol (str "lcert.formal." (str/replace (str/replace (.getName f) #"\.clj$" "") "_" "-"))))

(defn- file-of-ns [dir prefix sym]
  (io/file dir (str (str/replace (subs (str sym) (count prefix)) "-" "_") ".clj")))

(defn- formal-requires
  "The lcert.formal.* namespaces a formal source file requires, read from its ns form."
  [f]
  (let [form (with-open [r (java.io.PushbackReader. (io/reader f))] (read r))]
    (->> (tree-seq coll? seq form)
         (filter symbol?)
         (filter #(str/starts-with? (str %) "lcert.formal."))
         set)))

(defn load-order
  "Every formal namespace, each after the ones it requires (a topological order)."
  []
  (let [files (filter #(str/ends-with? (.getName ^java.io.File %) ".clj") (.listFiles (io/file formal-dir)))
        deps (into {} (for [f files] [(ns-of-file f) (disj (formal-requires f) (ns-of-file f))]))
        visit (fn visit [[order seen] n]
                (if (seen n) [order seen]
                    (let [[order seen] (reduce visit [order (conj seen n)] (sort (filter deps (deps n))))]
                      [(conj order n) seen])))]
    (first (reduce visit [[] #{}] (sort (keys deps))))))

(defn- state-vars []
  (for [n (all-ns)
        :let [nm (str (ns-name n))]
        :when (or (str/starts-with? nm "ansatz") (str/starts-with? nm "lcert.formal."))
        [_ v] (ns-interns n)
        :when (and (bound? v) (instance? clojure.lang.Atom @v))]
    v))

(defn- capture [] (into {} (for [v (state-vars)] [v @@v])))
(defn- restore! [snap] (doseq [[v x] snap :when (bound? v)] (reset! @v x)))

(defonce ^{:doc "ns -> state captured just before it was (re)loaded"} snapshots (atom {}))
(defonce ^{:doc "file path -> modification time at its last load"} stamps (atom {}))
(defonce ^{:doc "the load order used"} order (atom []))

(defn- mtime [^java.io.File f] (.lastModified f))
(defn- stamp! [^java.io.File f] (swap! stamps assoc (.getPath f) (mtime f)))
(defn- changed? [^java.io.File f] (not= (get @stamps (.getPath f)) (mtime f)))

(defn- suite-of [n] (symbol (str "lcert.formal-test." (subs (str n) (count "lcert.formal.")))))
(defn- suite-file [n] (file-of-ns suite-dir "lcert.formal." n))

(defn- load-from!
  "Reload the formal namespaces of the order from index i on, snapshotting before each."
  [i]
  (doseq [n (drop i @order)]
    (swap! snapshots assoc n (capture))
    (let [t0 (System/currentTimeMillis)]
      (require n :reload)
      (stamp! (file-of-ns formal-dir "lcert.formal." n))
      (println (format "  loaded %s (%.0f s)" n (/ (- (System/currentTimeMillis) t0) 1000.0))))))

(defn- run-suites! [nss]
  (let [suites (for [n nss :when (.exists (suite-file n))] (suite-of n))]
    (doseq [s suites] (require s :reload) (stamp! (suite-file (symbol (str "lcert.formal." (subs (str s) (count "lcert.formal-test.")))))))
    (if (seq suites) (apply t/run-tests suites) (println "  no suites to run"))))

(defn warm!
  "Load everything once (the slow first pass) and run every suite."
  []
  (reset! order (load-order))
  (reset! snapshots {})
  (load-from! 0)
  (run-suites! @order))

(defn recheck!
  "Re-check what changed since the last load: reload from the earliest changed formal
  namespace on, then run the suites of the reloaded namespaces and of any changed
  suite file.  New formal files are picked up by recomputing the order."
  []
  (let [new-order (load-order)
        _ (when (not= (set new-order) (set @order)) (reset! order new-order))
        i (first (keep-indexed (fn [i n] (when (or (changed? (file-of-ns formal-dir "lcert.formal." n))
                                                   (not (contains? @snapshots n)))
                                           i))
                               @order))
        suites-changed (filter #(let [f (suite-file %)] (and (.exists f) (changed? f))) @order)]
    (if i
      (let [n (nth @order i)]
        (println "Reloading from" n "(" (- (count @order) i) "namespaces )")
        (when-let [snap (get @snapshots n)] (restore! snap))
        (load-from! i)
        (run-suites! (distinct (concat (drop i @order) suites-changed))))
      (if (seq suites-changed)
        (run-suites! suites-changed)
        (println "Nothing changed since the last check.")))))
