;; surface.clj — mechanical "trust surface" projection of an Ansatz formal file.
;;
;; Usage: clojure -M surface.clj KEEP-NAMES-FILE IN OUT [IN OUT ...]
;;
;; Copies IN to OUT verbatim, except that in every top-level proof form
;;   (thm | pthm | a/prove-theorem | prove! | prove-n!   name params prop proof...)
;;   (prove-exp! | prove-exp-lv!                         name prop proof...)
;; the proof elements are replaced by one comment naming the elided line range
;; of IN, when the constant is named in KEEP-NAMES-FILE (one per line: the
;; headline constants of the ADR-0006 statement index).  Any other proof
;; form collapses to a one-line marker "‹lemma NAME … lines a–b›".
;; Definitions (a/inductive, kdef, a/defn, defn ...), the kept statements,
;; docstrings and comments are copied verbatim.  The scanner is character level, so it
;; understands strings, comments, character literals and reader prefixes but
;; does not evaluate or read Clojure data (no alias resolution is needed).

(require '[clojure.string :as str])

(def keep-through
  "Index of the last element kept (0 = head) for each proof-form head."
  {"thm" 3 "pthm" 3 "a/prove-theorem" 3 "prove!" 3 "prove-n!" 3
   "prove-exp!" 2 "prove-exp-lv!" 2})

(def ^:dynamic *s* nil)

(def keep-names
  "Headline constants (from the ADR-0006 statement index): their statements
  are kept.  Any other proof form collapses to a one-line marker."
  (atom #{}))

(defn ws? [c] (or (Character/isWhitespace (char c)) (= c \,)))
(defn delim? [c] (or (ws? c) (#{\( \) \[ \] \{ \} \" \;} c)))

(defn skip-comment [i]
  (let [j (str/index-of *s* "\n" i)] (if j j (count *s*))))

(defn skip-ws
  "First index at or after i that is not whitespace or a ; comment."
  [i]
  (loop [i i]
    (cond (>= i (count *s*)) i
          (ws? (.charAt *s* i)) (recur (inc i))
          (= \; (.charAt *s* i)) (recur (skip-comment i))
          :else i)))

(declare datum-end)

(defn string-end [i] ; i at the opening quote
  (loop [j (inc i)]
    (case (.charAt *s* j) \\ (recur (+ j 2)) \" (inc j) (recur (inc j)))))

(defn coll-end
  "i at an opening bracket; index just past the matching close."
  [i]
  (loop [j (inc i)]
    (let [j (skip-ws j) c (.charAt *s* j)]
      (if (#{\) \] \}} c) (inc j) (recur (datum-end j))))))

(defn atom-end [i]
  (loop [j i]
    (if (and (< j (count *s*)) (not (delim? (.charAt *s* j)))) (recur (inc j)) j)))

(defn datum-end
  "Index just past the datum starting at i (after any reader prefixes)."
  [i]
  (let [c (.charAt *s* i)]
    (cond
      (#{\( \[ \{} c) (coll-end i)
      (= \" c) (string-end i)
      (= \\ c) (let [j (atom-end (+ i 2))] j)          ; \x, \(, \newline
      (#{\' \` \@} c) (datum-end (skip-ws (inc i)))
      (= \~ c) (datum-end (skip-ws (if (= \@ (.charAt *s* (inc i))) (+ i 2) (inc i))))
      (= \^ c) (datum-end (skip-ws (datum-end (inc i)))) ; ^meta form
      (= \# c) (let [d (.charAt *s* (inc i))]
                 (cond (= \_ d) (datum-end (skip-ws (datum-end (skip-ws (+ i 2)))))
                       (#{\( \{ \" \'} d) (datum-end (inc i))
                       :else (atom-end i)))
      :else (atom-end i))))

(defn elements
  "[start end] of each element of the list opening at i, and the index of its
  closing paren."
  [i]
  (loop [j (inc i) acc []]
    (let [j (skip-ws j)]
      (if (= \) (.charAt *s* j))
        [acc j]
        (let [e (datum-end j)] (recur e (conj acc [j e])))))))

(defn line-of [i] (inc (count (filter #(= \newline %) (subs *s* 0 i)))))

(defn project [text]
  (binding [*s* text]
    (let [out (StringBuilder.)]
      (loop [i 0]
        (let [j (skip-ws i)]
          (.append out (subs *s* i j))                    ; whitespace, comments
          (if (>= j (count *s*))
            (str out)
            (let [e (datum-end j)
                  c (.charAt *s* j)]
              (if (= \( c)
                (let [[els close] (elements j)
                      head (when (seq els) (apply subs *s* (first els)))
                      k (keep-through head)]
                  (if (and k (> (count els) (inc k)))
                    (let [[ps _] (nth els (inc k))
                          [_ pe] (last els)
                          nm (str/replace (apply subs *s* (second els)) #"^'" "")]
                      (if-not (@keep-names nm)
                        (.append out (format ";; ‹lemma %s: statement and proof elided, lines %d–%d›"
                                             nm (line-of j) (line-of pe)))
                        (do
                          (.append out (subs *s* j (second (nth els k))))
                          (.append out (format "\n  ;; ‹proof elided: lines %d–%d of the full file; kernel-checked›\n  )"
                                               (line-of ps) (line-of pe)))))
                      (recur (inc close)))
                    (do (.append out (subs *s* j e)) (recur e))))
                (do (.append out (subs *s* j e)) (recur e))))))))))

(reset! keep-names (set (str/split-lines (slurp (first *command-line-args*)))))
(doseq [[in out] (partition 2 (rest *command-line-args*))]
  (spit out (project (slurp in))))
