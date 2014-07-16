(ns unify-test.core
  (:require [clojure.core.unify :as cu]
            [clojure.walk :as w]
            [taoensso.timbre.profiling :as p]))

;; TODO make this fn swappable
(defn variable? [x] (and (symbol? x) (= \? (first (name x)))))

(defn occurs?
 "Does the varaible exist in the form/expression?"
  [variable form]
  (->> form
       (w/prewalk #(if (not (coll? %)) (= variable %) %))
       (flatten)
       (some identity)))

(defn find-first-diff
  "Scan left-to-right, find the first expressions that differ."
  [form1 form2]
  ;; If either one is not a collection, then we can find the diff right
  ;; away. They're either equal or not.
  (if (not (and (coll? form1) (coll? form2)))

    (if (= form1 form2)
      nil
      [form1 form2])

    (let [[x1 & x1s] form1
          [x2 & x2s] form2]
      (if (and (coll? x1) (coll? x2))
        (or (find-first-diff x1 x2) (find-first-diff x1s x2s))
        (cond
          (not= x1 x2) [x1 x2]
          (and (nil? x1) (nil? x2)) nil
          :else (find-first-diff x1s x2s))))))

(defn subst
  "Walk the form, replace all instances of 'to' with 'from'."
  [from to form]
  (w/prewalk-replace {from to} form))

(defn convert
  "Convert literal data forms to lists."
  [form]
  ;; TODO - add vector, others if needed
  (cond (map? form) (cons 'hash-map (flatten (seq form)))
        (set? form) (cons 'set (seq form))
        :else form))

(defn unify*
  "Try and unify two forms, returning the set of substitutions if success."
  [occurs-check? form1 form2]
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
        (or d1-v? d2-v?) (let [trans (if d1-v? [d1 d2] [d2 d1])]
                           (if (and occurs-check? (apply occurs-check? trans))
                             nil
                             (recur (apply subst (conj trans f1))
                                    (apply subst (conj trans f2))
                                    (conj s trans))))))))

(def unify (partial unify* nil))
(def unify-with-occurs (partial unify* occurs?))

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

#_(p/profile :info :foo (dotimes [n 1000] (doit-with-occurs)))
#_(p/profile :info :foo (dotimes [n 1000] (doit-no-occurs)))
