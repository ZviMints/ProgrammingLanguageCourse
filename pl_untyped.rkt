#lang pl untyped ;; static scoping and dynamic-typing

(define x 22)
(define (getx) x)
(define (bar1 x) (getx))
(define (bar2 y) (getx))

(test (getx) => 22)
(test (let ([x 45]) (getx)) => 22)
(test (getx) => 22)
(test (bar1 999) => 22)
(test (bar2 999) => 22)

(define foo (let ([+ *]) 
                 (lambda (x y) (+ x y)))) 
(test (foo 6 7) => 42)

(define (foo2 x)
	(define (helper) (+ x 1))
  helper)

(test ((foo2 0)) => 1)