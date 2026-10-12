;; Read every form of each file (no evaluation) to confirm a file is well
;; formed; print the number of forms per file, or FAIL with the reader error.
(defn count-forms [f]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader f))]
    (binding [*read-eval* false]
      (loop [n 0]
        (let [x (read {:eof ::eof :read-cond :allow} r)]
          (if (= x ::eof) n (recur (inc n))))))))

(doseq [f *command-line-args*]
  (try (println (count-forms f) f)
       (catch Exception e (println "FAIL" f (.getMessage e)))))
