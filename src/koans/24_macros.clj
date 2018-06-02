(ns koans.24-macros
  (:require [koan-engine.core :refer :all]))

(defmacro hello [x]
  (str "Hello, " x))

(defmacro infix [form]
  (list (second form) (first form) (nth form 2)))

;(defmacro infix-concise [form]
;  `(~(second form) ; Note the syntax-quote (`) and unquote (~) characters!
;    ~(first form)
;    ~(nth form 2)))
(defmacro infix-concise [form]
  (let [[lhs# op# rhs#] form]
    `(~op# ~lhs# ~rhs#)))

;(defmacro recursive-infix [form]
;  (cond (not (seq? form))
;        form
;        (= 1 (count form))
;        `(recursive-infix ~(first form))
;        :else
;        (let [[first-arg operator & others] form]
;          `(~operator
;            (recursive-infix ~first-arg)
;            (recursive-infix ~others)))))

(defn recursive-infix-impl [form]
  (if-not (seq? form)
    form
    (let [[fst scnd & other] (seq form)]
      (if-not scnd
        (recursive-infix-impl fst)
        (list scnd (recursive-infix-impl fst) (recursive-infix-impl other)))
      )))
(defmacro recursive-infix [form] (recursive-infix-impl form))

;(macroexpand-1 '(recursive-infix (10 + 2)))
;(recursive-infix '(10 + (2 * 3) + (4 * 5)))
;(recursive-infix '(10 + (2 * 3)))

;(macroexpand-1 '(recursive-infix (10 + (2 * 3) + (4 * 5))))


(meditations
  "Macros are like functions created at compile time"
  (= "Hello, Macros!" (hello "Macros!"))

  "I can haz infix?"
  (= 10 (infix (9 + 1)))

  "Remember, these are nothing but code transformations"
  (= '(+ 9 1) (macroexpand '(infix (9 + 1))))

  "You can do better than that - hand crafting FTW!"
  (= '(* 10 2) (macroexpand '(infix-concise (10 * 2))))

  "Things don't always work as you would like them to... "
  (= '(+ 10 (2 * 3)) (macroexpand '(infix-concise (10 + (2 * 3)))))

  "Really, you don't understand recursion until you understand recursion"
  (= 36 (recursive-infix (10 + (2 * 3) + (4 * 5)))))
