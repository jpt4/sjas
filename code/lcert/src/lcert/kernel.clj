(ns lcert.kernel
  "The measures on codes that the consistency proof turns on.

  R4-metatheory.md proves consistency from one inequality, Lemma 2.7 (strict
  overhead): a code that Check accepts declares fewer tokens than it has
  internal nodes, budget(c) < nodes(c).  The runtime uses these measures in
  three places:
    - Check asserts strict overhead of every accepted code (lcert.check);
    - reflect's cap compares a certificate's footprint with the budget
      (lcert.eval);
    - reports of certificate sizes.

  Two implementations exist.  The plain one below is ordinary Clojure.  The
  verified one is compiled from Ansatz definitions whose properties the Ansatz
  kernel has checked (ansatz/lcert/verified.clj, extended suite only).
  `use-verified!` switches the runtime to it; the extended test suite does so
  and re-runs every language test, and also tests the two for agreement.

  Codes are [:sl l] (leaf) and [:sn l c1 c2] (internal node).")

(defn nodes*
  "Number of internal nodes of a code.  Iterative, so deep codes are safe."
  [c]
  (loop [stack (list c) n 0]
    (if (empty? stack)
      n
      (let [x (first stack) more (rest stack)]
        (if (= :sn (first x))
          (recur (conj more (nth x 2) (nth x 3)) (inc n))
          (recur more n))))))

(defn ctxlen*
  "Length of the first-child spine of internal nodes.  For the encoding of a
  context this is its number of entries: each entry is one internal node
  whose first child is the rest of the context (lcert.encode/enc-ctx)."
  [c]
  (loop [x c n 0]
    (if (= :sn (first x)) (recur (nth x 2) (inc n)) n)))

(defn budget*
  "The budget a derivation code declares: the number of entries of the
  context in its root judgment.  A derivation code is [:sn rule J prems] and
  its judgment J is [:sn judgment-label context ...] (E2, E3), so the context
  is the first child of the first child.  Codes of any other shape declare 0."
  [c]
  (if (and (= :sn (first c)) (= :sn (first (nth c 2))))
    (ctxlen* (nth (nth c 2) 2))
    0))

(def ^:private impl
  (atom {:name :plain :nodes nodes* :ctxlen ctxlen* :budget budget*}))

(defn nodes  [c] ((:nodes @impl) c))
(defn ctxlen [c] ((:ctxlen @impl) c))
(defn budget [c] ((:budget @impl) c))

(defn implementation
  "Which measures are in use: :plain or :verified."
  []
  (:name @impl))

(defn use-plain! [] (reset! impl {:name :plain :nodes nodes* :ctxlen ctxlen* :budget budget*}))

(defn use-verified!
  "Switch the runtime measures to the Ansatz-compiled ones.  Requires the
  namespace lcert.verified, which exists only on the :ansatz classpath."
  []
  (let [v (fn [s] @(requiring-resolve (symbol "lcert.verified" (name s))))
        to-ct (v 'code->ct)
        wrap (fn [f] (fn [c] (long (f (to-ct c)))))]
    (reset! impl {:name :verified
                  :nodes (wrap (v 'nodes))
                  :ctxlen (wrap (v 'ctxlen))
                  :budget (wrap (v 'budget))})))
