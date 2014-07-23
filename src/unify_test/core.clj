(ns unify-test.core
  (:require [clojure.core.unify :as cu]
            [clojure.walk :as w]
            [taoensso.timbre.profiling :as p]))

(defn variable? [x] (and (symbol? x) (= \? (first (name x)))))

(defn occurs?
  "Does the varaible exist in the form/expression?"
  [variable form]
  (cond
    (nil? form)
    false

    (= variable form)
    true

    (coll? form)
    (or (occurs? variable (first form))
        (occurs? variable (next form)))))

(defn find-first-diff
  [form1 form2]
  (if (and (coll? form1) (coll? form2))

    (or (find-first-diff (first form1) (first form2))
        (find-first-diff (next form1) (next form2)))

    ;; If we've reached a symbol, or nil, make a decision.
    (if (not= form1 form2)
      [form1 form2]
      nil)))

(defn subst
  "Walk the form, replace all instances of 'to' with 'from'."
  [from to form]
  (w/prewalk-replace {from to} form))

(defn convert
  "Convert literal data forms to lists."
  [form]
  ;; TODO - add vector, others if needed
  (cond (map? form) (cons 'hash-map (seq form))
        (set? form) (cons 'set (seq form))
        :else form))

(defn robinson-unify*
  "Try and unify two forms, returning the set of substitutions if success."
  [occurs-check? form1 form2]
  ;; TODO should we be calling convert at each step?
  (loop [f1 (convert form1)
         f2 (convert form2)
         s []]
    (let [[d1 d2] (find-first-diff f1 f2)
          d1-v? (variable? d1)
          d2-v? (variable? d2)]
      (cond
        ;; No differences, unified!
        (and (nil? d1) (nil? d2)) [f1 f2 s]
        ;; Different non-variables, no way to unify.
        (and (not d1-v?) (not d2-v?)) nil
        (or d1-v? d2-v?) (let [v (if d1-v? d1 d2)
                               f (if d1-v? d2 d1)]
                           (when-not (and occurs-check? (occurs-check? v f))
                             (recur (subst v f f1)
                                    (subst v f f2)
                                    (conj s [v f]))))))))

(def unify (partial robinson-unify* nil))
(def unify-with-occurs (partial robinson-unify* occurs?))

(defn ->s [unify-result] (nth unify-result 2))

;; **** Profiling ****

(def form1 '((?a * ?x ** 2) + (?b * ?x) + ?c))
(def form2 '(?z             + (4 * 5)   + 3))

(defn doit-with-occurs []
  (p/p :core.unify (cu/unifier form1 form2))
  (p/p :mine (unify-with-occurs form1 form2)))

(defn doit-no-occurs []
  (p/p :core-unify (#'clojure.core.unify/unifier- form1 form2))
  (p/p :mine (unify form1 form2)))

#_(p/profile :info :foo (dotimes [n 1000] (doit-no-occurs)))
#_(p/profile :info :foo (dotimes [n 1000] (doit-with-occurs)))

