(ns unify-test.core-test
  (:require [clojure.test :refer :all]
            [unify-test.core :refer :all]))

(deftest occurs-tests
  (testing "occurs checks"
    (is (occurs? '?x '(foo A (bar B (inc ?x)))))
    (is (not (occurs? '?x '(foo A (bar B (inc ?y))))))))

(deftest find-first-diff-tests
  (testing "find first diffs"
    (is (= '[(?a ?a) a] (find-first-diff '(?a ?a) 'a)))
    (is (= '[7 3] (find-first-diff '(x * 7) '(x * 3))))
    (is (= '[5 x] (find-first-diff '(5 * (+ y 3)) '(x * (+ y 3)))))
    (is (= '[y 9] (find-first-diff '(x * (+ y 3)) '(x * (+ 9 3)))))
    (is (= '[(3) nil] (find-first-diff '(x * (+ 9 3)) '(x * (+ 9)))))
    (is (nil? (find-first-diff '(x + 1) '(x + 1))))
    (is (= '[?b 4] (find-first-diff '((?a * ?x ** 2) + (?b * ?x) + ?c)
                                    '((?a * ?x ** 2) + (4 * 5) + 3))))
    (is (= '[?x 2] (find-first-diff (convert '{?x 1}) (convert '{2 1}))))))

(deftest unify-tests
  (testing "unify"
    (is (= [] (->s (unify '(a b) '(a b)))))
    (is (= [] (->s (unify '(?a ?b) '(?a ?b)))))
    (is (nil? (->s (unify '(?a ?a) 'a))))
    (is (= [['?x 1]] (->s (unify '(f ?x ?y) '(f 1 ?y)))))
    (is (= (set [['?b 2] ['?a 1]]) (set (->s (unify '(?a ?b) '(1 2))))))
    (is (= [['?a '(q)]] (->s (unify '(?a ?b) '((q) ?b)))))
    (is (= [['?x 'y]] (->s (unify '?x 'y))))
    (is (= [['?x '?y]] (->s (unify '(?x ?x) '(?y ?y)))))
    (is (= [['?x '?y]] (->s (unify '(?x ?x ?x) '(?y ?y ?y)))))
    (is (= [['?x '?y]] (->s (unify '(?x ?y) '(?y ?x)))))
    (is (nil? (->s (unify '(f ?x ?y) '(g ?x ?y))))) ; clash
    (is (nil? (->s (unify '(?a ?a) '(1 2))))) ; clash
    (is (nil? (->s (unify '(f ?a) '(g 42))))) ; clash
    (is (nil? (->s (unify '(?a ?a) 'a)))) ; clash
    (is (= (set [['?y '(h)] ['?x '(h)]])
           (set (->s (unify '(f ?x (h)) '(f (h) ?y))))))
    (is (thrown? StackOverflowError (unify '(f (g ?x) ?y) '(f ?y ?x)))) ; cycle
    (is (thrown? StackOverflowError (unify '?x '(f ?x)))) ; cycle
    (is (= [['?y (list 'g '?x)]] (->s (unify '(f (g ?x) ?y) '(f ?y (g ?x))))))
    (is (= (set [['?z (list 'g '?x)] ['?y (list 'g '?x)]])
           (set (->s (unify '(f (g ?x) ?y) '(f ?y ?z))))))
    (is (= [['?a 'a]] (->s (unify '?a 'a))))
    (is (= [['?x 2]] (->s (unify '{?x 1} '{2 1}))))
    (is (= (set [['?y :bar] ['?x :foo]])
           (set (->s (unify '{?x 42 ?y 108} '{:foo 42 :bar 108})))))
    (is (= #{['?y 'a], ['?x '?y]} (set (->s (unify '(?x ?y a) '(?y ?x ?x))))))))

(deftest unify-occurs-tests
  (testing "unify without occurs check"
    (is (thrown? StackOverflowError (unify '(?x + 2) '((?x + 5) + 2)))))
  (testing "unify with occurs check"
    (is (nil? (unify-with-occurs '(?x + 2) '((?x + 5) + 2))))
    (is (= [['?z '(?a * ?x ** 2)] ['?b 4] ['?x 5] ['?c 3]]
           (->s (unify-with-occurs '((?a * ?x ** 2) + (?b * ?x) + ?c)
                                   '(?z + (4 * 5) + 3)))))))

