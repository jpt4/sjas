(ns lcert.core
  "The public API of λᶜᵉʳᵗ₀ as a small programming language.

  Programs are EDN forms (see lcert.syntax for the surface syntax).  A program
  runs with a *budget* n: n tokens of the resource type ◇, named $1 .. $n.
  Certificates — trees of type R — cost one token per internal node.

    (check-program n form)   the explicit derivation of Θₙ ⊢ form : A
    (certify n form)         its derivation, code, type, node count, budget
    (check code type-form)   Check: does code certify the closed type?
    (run n form)             type check, then evaluate with n fresh tokens
    (certificate-literal code)  the term building code's tree from tokens
    (certificate-form code)     the same, as a surface form using $1 .. $k

  Terms already in abstract syntax go through check-term, encode, type-code
  and check-code."
  (:require [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.typing :as t]
            [lcert.check :as c]
            [lcert.eval :as ev]
            [lcert.kernel :as k]))

(defn parse-program "A surface form, parsed in Θₙ." [n form]
  (s/parse-term (s/token-scope n) form))

(defn check-term "The derivation of Θₙ ⊢ t :¹ A for an abstract term t." [n t]
  (t/check-top n t))

(defn check-program [n form] (check-term n (parse-program n form)))

(defn encode "The code of a derivation." [d] (e/enc-deriv d))

(defn type-code "The code of an abstract type." [A] (e/enc-exp A))

(defn certify
  "Everything about the certificate of `form` at budget n."
  [n form]
  (let [d (check-program n form)
        code (encode d)]
    {:derivation d
     :code code
     :type (:type d)
     :nodes (k/nodes code)
     :budget (k/budget code)}))

(defn check-code "Check(code, type-code)." [code tcode] (c/check code tcode))

(defn check
  "Does `code` certify the closed type written as the surface form type-form?"
  [code type-form]
  (check-code code (type-code (s/parse-type [] type-form))))

(defn run "Type check form in Θₙ and evaluate it, erasing, with n tokens." [n form]
  (ev/run n (parse-program n form)))

(defn certificate-literal "The term building code's tree from $1 .. $k." [code]
  (c/certificate-literal code))

(defn certificate-form
  "The surface program that builds code's tree as a certificate, spending
  tokens $1 .. $k in preorder, where k is the code's number of internal
  nodes.  Run it with budget k."
  [code]
  (let [counter (volatile! 0)]
    (letfn [(build [c]
              (case (first c)
                :sl (list 'leaf (second c))
                :sn (let [i (vswap! counter inc)]
                      (list 'node (symbol (str "$" i)) (second c) (build (nth c 2)) (build (nth c 3))))))]
      (build code))))
