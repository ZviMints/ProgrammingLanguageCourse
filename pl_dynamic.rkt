#lang pl dynamic ;; dynamic scoping and static-typing

(define x 22)
(define (getx) x)
(define (bar1 x) (getx))
(define (bar2 y) (getx))

(test (getx) => 22)
(test (let ([x 45]) (getx)) => 45)
(test (getx) => 22)
(test (bar1 999) => 999)
(test (bar2 999) => 22)

(define foo (let ([+ *]) 
                 (lambda (x y) (+ x y)))) ;; because lambda open new scope which is + (not remember the last env)
(test (foo 6 7) => 13)

(define (foo x)
	(define (helper) (+ x 1))
  helper)

(test ((foo 0)) => 23)

(define (mycons f s)
  (define (mypair selector)
    (selector f s))
  mypair)


(define (myfirst p)
  (define (first-selector f s) f)
  (p first-selector))


(define (mysecond p)
  (define (second-selector f s) s)
  (p second-selector))

(test (myfirst (mycons 1 2)) => 1) ;; reference to undefined identifier: f
