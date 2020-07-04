#lang pl
;; Written by Zvi Mints - zvimints@gmail.com

;; Question1
(: stringEndsWith? : String -> Boolean)
(: plSuffixContained : (Listof String) -> (U String False))

#| Checks if string is ends with "pl", s.t string := (string-append * "pl") when * is a string in Sigma* |#
(define (stringEndsWith? s)
  (let ([s_length (string-length s)])
    (and (eq? (string-ref s (- s_length 2)) #\p) (eq? (string-ref s (- s_length 1)) #\l ))
    )
  )

#| This method is responsible to find first string in @list that contains
the string "pl" as a suffix - if one exists, and returns 'false' otherwise
@personal comment: at start i wanted to make Opt obj that consists of Some and None and then
the wrapper function (plSuffixContained) will return false on None or value on Some, and there will be
an inner function that return Some if there first suffix that ends with "pl" and None otherwise, but i chose
to go in the Question way|#
(define (plSuffixContained list)
  (match list
  [ '() #f ]  
  [ (cons head tail) (if (stringEndsWith? head) head (plSuffixContained tail)) ]
  )
)
    

; Tests for stringEndsWith
(test (stringEndsWith? "Question1") => false) ; Question1 do not ends with "pl"
(test (stringEndsWith? "Questionp1") => false) ; Questionp1 do not ends with "pl"
(test (stringEndsWith? "Questionpl") => true) ; Questionpl ends with "pl"

; Tests for plSuffixContained
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false) ; There no strings that ends with 'pl' 
(test (plSuffixContained '("1plpl" "plyy" "ppp" "lpTT" "lol")) => "1plpl") ; There one string that ends with 'pl' and its find it
(test (plSuffixContained '("plps" "2plpl" "ppl" "lpTT" "lol")) => "2plpl") ; There two strings that ends with 'pl' and it find the first one
(test (plSuffixContained '("pl_no" "pl_no" "pl_no" "pl_no" "pl_no")) => false) ; There no strings that ends with 'pl' 
(test (plSuffixContained '("pl_no" "pl_no" "pl_no" "pl_no" "pl_no" "yes_pl" )) => "yes_pl" ) ; There no strings that ends with 'pl'

;; ================ Question2 - Polyomyals ==================
;; (2.1)
(: write-poly : (Listof Number) -> String)
(: tail-rec : (Listof Number) String (String String Boolean -> String) -> String)
(: createMonom : Number Integer -> String)
#| This method is responsible to create monom from coef and degree
   for example (createMonom 3 5) yield 3x^5, for special cases
   like (createMonom 3 0) its will yield 3
   and (createMonom 3 1) will yield 3x |#
(define (createMonom a b)
    (let ([coef (number->string a)]
          [degree (number->string b)])
      (match b ;; pattern-matching for the degree, its form will be seen in other way
       [ 0 coef]
       [ 1 (string-append coef "x")]
       [ _ (string-append coef "x^" degree)]
       )
      )
  )

(define (write-poly list) ; Works like map-reduce
  (tail-rec list "" (lambda (acc monom minus_sign) ; Studied at 'Introducing Racket’s lambda' Chapter from https://pl.barzilay.org/lec98.html
                       (match acc
                         ["" monom];
                         [_ (string-append acc (if minus_sign monom (string-append "+" monom)))] ;; concat acc + monom or acc - monom, depends on minus_sign
                        )
                       )
  )
)
; This method working like map-reduce when te step phase is createMonom
(define (tail-rec list acc combiner)
  (match list
    [ '() acc ] ; For end-case
    [ (cons a b) (tail-rec b (combiner acc (createMonom a (length b)) (< a 0)) combiner)] ;; Works like map-reduce, the combiner is lambda function
    )
  )
                        
; Tests for createMonom
(test (createMonom 3 5) => "3x^5")
(test (createMonom 2 1) => "2x")
(test (createMonom 1 1) => "1x") ; This is appears in forum, 1x instead of x
(test (createMonom 3 0) => "3") 

; Tests for write-poly
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(0)) => "0")
(test (write-poly '(5)) => "5")
(test (write-poly '(5 3)) => "5x+3")
(test (write-poly '(1 3)) => "1x+3")
(test (write-poly '(5 -3)) => "5x-3")
(test (write-poly '(-7 8 -9 -10)) => "-7x^3+8x^2-9x-10")
(test (write-poly '(7 -8 -9 -10)) => "7x^3-8x^2-9x-10")
(test (write-poly '(7 -8 -1 -10)) => "7x^3-8x^2-1x-10")
;; (2.2)
(: compute-poly : Number (Listof Number) -> Number)
(: tail-rec-compute : (Listof Number) Number Number -> Number)
(: evalMonom : Number Number Integer -> Number)

#| This method is responsible to evulate to result of a monom with a specific x value,
   for example, (evalMonom 3 2 4) = 2*3^4
@input: (x: Number, a: Number, b: Integer)
@output: Monom of the type ax^b |#
(define (evalMonom x a b)
   (* a (expt x b) )
)

(define (tail-rec-compute list acc x)
  (match list
    [ '() acc ] ; For end-case
    [ (cons a b) (tail-rec-compute b (+ acc (evalMonom x a (length b))) x)] ;; calls for tail-rec-compute with acc = acc + ax^b
    )
  )
                        

#| This method is responsible to computer the
result of the polynomial, when list is a list of
coefiicients (numbers) a_1,a_2 ... a_n and x is the number
that the function consumes |#
(define (compute-poly x list)
  (tail-rec-compute list 0 x) ;; Works like map-reduce without the combiner
)


; Tests for compute-poly
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 2 '(1)) => 1)
(test (compute-poly 2 '(1 1)) => 3)
(test (compute-poly 2 '(0)) => 0)

;; ================ Question3 - KeyStack ==================
;;; Defenitions
(: search-stack  : (Symbol KeyStack -> (U String False)) )
(: pop-stack : (KeyStack -> (U KeyStack False) ))

#|
keyed-stack data structure.
Each element in the KeyStack consists of key (indexed) with a symbol. 
|#
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])
#|
The search-stack operation takes as input a symbol (key) and a keyed-stack and return the first
(LIFO, last in first out) value that is keyed accordingly.
If the key does not appear in the original stack, it should return a 'False' value
|#

(define (search-stack key stack)
  (cases stack
    [(EmptyKS) #f]
    [(Push symbol string rest) (if (eq? key symbol) string (search-stack key rest))]
    )
)
#|
The pop-stack operation takes as input a keyed-stack and
return the keyed-stack without its first (keyed) value.
If the original stack was empty, it return a 'False' value
|#
(define (pop-stack stack)
 (cases stack
    [(EmptyKS) #f]
    [(Push symbol string rest) rest]
   )
)

; Tests for KeyStack
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (pop-stack (EmptyKS)) => #f)
;; ================ Question4 - Commenets ==================
(: is-odd? : Natural -> Boolean)
#|
@description: this method is responsible to determine if an input natural number
              is odd or not
@logic: the functions works recursively as follows:
        for the basis step, if x is zero then x is even so the function returns false
        for the recursive step, we return true iff x is odd iff (x-1) is even which checked with is-even? method
@input x which is Natural Number
@output true iff [[x]] is odd Natural Number
|#

(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))
(: is-even? : Natural -> Boolean)

#|
@description: this method is responsible to determine if an input natural number
              is even or not
@logic: the functions works recursively as follows:
        for the basis step, if x is zero then x is even so the function returns true
        for the recursive step, we return true iff x is even iff (x-1) is odd which checked with is-odd? method
@input x which is Natural Number
@output true iff [[x]] is even Natural Number
|#
(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
;; -----------------------------------------------------------------
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
Generic function that checks if every element of type A is satisfying function of type (A -> Boolean)
@input: lst of type A and pred of type A -> Boolean
@output: true iff  for every x in lst, (pred x) -> true
|#
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
This method wraps the function every? with specific values, and call every? with
list of Natural numbers and pred function from (Netural -> Boolean) which checks if
on an input x, x is even. see documantion above.
@output: true iff  for every x in lst, (pred x) -> true
|#
(define (all-even? lst)
 (every? is-even? lst))

;; new-tests
(test (every? is-odd? null))
(test (every? is-odd? '(1 3 5)))
(test (not (every? is-odd? '(1 3 6))))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
;; -----------------------------------------------------------------
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
#|
Generic function thats check for each i in {1,2} if lst_i satisfying pred_i
@input: lst1 of type B, lst2 of type A, pred2 of type (B -> Boolean) and pred1 of type (A -> Boolean)
@output: true iff for each (x,y) in (lst1,lst2) then (pred1 x,pred2 y) returns true
|#
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))

;; tests
(test (every2? (lambda (x) #t) (lambda (x) #t) '("Zvi" "Mints") '(1 2 3)))
(test (not (every2? (lambda (x) #t) (lambda (x) #f) '("Zvi" "Mints") '(1 2))))
(test (not (every2?  (lambda (x) #t) (lambda (x) (not (eq? x 2))) '("Zvi" "Mints") '(1 2))))
(test (every2?  (lambda (x) #t) (lambda (x) (not (eq? x 3))) '("Zvi" "Mints") '(1 2)))


