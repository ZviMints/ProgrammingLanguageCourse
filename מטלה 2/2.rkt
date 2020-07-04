#lang pl
#|
# Exercise: Excerise 2
# By: Zvi Mints 314977489
# Date: May 01 2020
|#
;; ====== Question 1 ======
;; ===  BNF and Parsing ===
; The ROL BNF and Parsing code:

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; ======= 1.1 BNF for the Register Operation Language ROL =======
#| BNF for the RegE language:
 
 <ROL> ::= { reg-len = <num> <RegE> }

 <RegE> ::= { <Bits> }
         |  { or <RegE> <RegE> }
         |  { and <RegE> <RegE> }
         |  { shl <RegE> }

 <Bits> ::= 1 | 0 | 1 <Bits> | 0 <Bits>
|#
#|
Here 3 examples for ROL program codes with four derivation processes: 

<ROL> -> { reg-len = <num> <RegE> } ->
                                     { reg-len = 1 <RegE> } -> { reg-len = 1 { <Bits> } } -> {reg-len = 1 { 1 } } (1)
                                     { reg-len = <num> <Bits> } -> { reg-len = 1 { <Bits> } } -> {reg-len = 1 { 1 } } (2)
                                     { reg-len = <num> <Bits> } -> { reg-len = <num> { 1 } } -> {reg-len = 1 { 1 } } (3)

<ROL> -> { reg-len = <num> <RegE> } ->
                                     { reg-len = 5 <RegE> } -> { reg-len = 5 { <Bits> } } -> {reg-len = 5 { 1 <Bits> } } -> {reg-len = 5 { 1 1 <Bits> } } -> {reg-len = 5 { 1 1 1<Bits> } } -> {reg-len = 5 { 1 1 1 1 <Bits> } } -> {reg-len = 5 { 1 1 1 1 1 } }  (4)


 <ROL> -> { reg-len = <num> <RegE> } -> { reg-len = 4 <RegE> } -> { reg-len = 4  { <Bits> } }  -> { reg-len = 4  { 1 <Bits> } } -> { reg-len = 4  { 1 1 <Bits> } } -> { reg-len = 4  { 1 1 1 <Bits> } }  -> { reg-len = 4  { 1 1 1 1 } }
|#

;; ======= 1.2 Parser for the Register Operation Language ROL =======

;; RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE]
)
;; =================== list->bit-list ================================
;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
 (: list->bit-list : (Listof Any) -> Bit-List)
 ;; to cast a list of bits as a bit-list
 (define (list->bit-list lst)
 (cond
   [(null? lst) null]
   [(eq? (first lst) 1) (cons 1 (list->bit-list (rest lst)))]
   [else (cons 0 (list->bit-list (rest lst)))]
   )
  )
 ;; =================== parse-sexpr ================================
 (: parse-sexpr : Sexpr -> RegE)
 ;; to convert the main s-expression into ROL
 (define (parse-sexpr sexpr)
 (match sexpr
   [(list 'reg-len '= (number: num) rest) (if (> num 0) (parse-sexpr-RegL rest num) (error 'parse-sexpr "got ~s <= 0 number" num))]
   [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; =================== parse-sexpr-RegL ================================
 (: parse-sexpr-RegL : Sexpr Number -> RegE)
 ;; to convert s-expressions into RegEs
 (define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr    
  [(list 'or l r) (Or (parse-sexpr-RegL l reg-len) (parse-sexpr-RegL r reg-len))]
  [(list 'and l r) (And (parse-sexpr-RegL l reg-len) (parse-sexpr-RegL r reg-len))]
  [(list 'shl value) (Shl (parse-sexpr-RegL value reg-len))]
  [(cons (or 1 0) tail) (if (not (eq? (length sexpr) reg-len)) (error 'parse-sexpr "wrong number of bits in ~s with reg-len ~s" sexpr reg-len) (Reg (list->bit-list sexpr)))] 
  [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]
 )
)   

 ;; =================== parse ================================
 (: parse : String -> RegE)
 ;; parses a string containing a RegE expression to a RegE AST
 (define (parse str)
   (parse-sexpr (string->sexpr str))
 )

 ;; tests
 (test (list->bit-list '() ) => '() )
 (test (list->bit-list '(1 0 1) ) => '(1 0 1) )

 (test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
 (test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
 (test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg'(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
 (test (parse "{ reg-len = 4 {or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
 (test (parse "{ reg-len = 2 {or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl(Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
 (test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
 (test (parse "{ reg-len = -1 {or {1 1 1 1} {0 1 1}}}") =error> "got -1 <= 0 number")
 (test (parse "{ 314977489 best id in the world }" ) =error> "bad syntax in")
 (test (parse "{ reg-len = 4 {boom {1 0 0 0}}}") =error> "bad syntax in")
 (test (parse "{ reg-len = 4 boom }") =error> "bad syntax in")
 (test (parse "{ reg-len = 4 {2 0 0 0}}") =error> "bad syntax in")

;; ============== Question 2 ==============
;; ===  Accommodating Memory Operations ===
;; (1) The Problem:
#|
in considering `{* {+ {set 1} {set 2}} get}` we have some main  problems
the first one is that there ambigious anwsers, since there one on cell
{set 1} will override {set 2} or otherwise, so there side-affect for this operation
means the result will be (* 3 1) or (* 3 2) depends on priority 
which can cause to future mistakes, in order to solve the problem i would suggest to set ordering, thus change MAE to:

;; (1) The Suggested Solution:
<MAE> ::= <num>
 | { + <EXP> <MAE> } | { + <MAE> <EXP> }
 | { - <EXP> <MAE> } | { - <MAE> <EXP> }
 | { * <EXP> <MAE> } | { * <MAE> <EXP> }
 | { / <EXP> <MAE> } | { / <MAE> <EXP> }
 | get

<EXP> :: = <num>
 | { + <EXP> <EXP> }
 | { - <EXP> <EXP> }
 | { * <EXP> <EXP> }
 | { / <EXP> <EXP> }
 | { set <MAE> }
 | get

in that way only one {set E} is avaliable in each subtree and not two, so there no ambigious problem anymore
|#

;; (2) 
#|
<MAE> ::= { seq <set> <GEXP> }
        | { seq <set> <MORE> <GEXP> }
        | { seq <GEXP> }

<MORE> ::= <set> 
        |  <set> <MORE>
 
<set> ::= { set <EXP> }
<get> ::= { get }

<EXP> ::= <num>
        | { + <EXP> <EXP> }
        | { - <EXP> <EXP> }
        | { * <EXP> <EXP> }
        | { / <EXP> <EXP> }

<GEXP> ::= <num>
        | <get>
        | { + <GEXP> <GEXP> }
        | { - <GEXP> <GEXP> }
        | { * <GEXP> <GEXP> }
        | { / <GEXP> <GEXP> }


Note: <set>* is zero or more <set> expressions
|#

;; (2.2)

#| id: 314977489

<MAE> -> { seq <set> <MORE> <GEXP> } -> { seq <set>  <set> <MORE> <GEXP> } -> { seq <set>  <set> <set> <GEXP> } -> { seq { set <EXP> }  <set> <set> <GEXP> } -> { seq { set <EXP> }  { set <EXP> } <set> <GEXP> } -> { seq { set <EXP> }  { set <EXP> } { set <EXP> } <GEXP> }
-> { seq { set <num> }  { set <EXP> } { set <EXP> } <GEXP> } -> { seq { set <num> }  { set  <num> } { set <EXP> } <GEXP> } -> { seq { set <num> }  { set  <num>} { set  <num> } <GEXP> } -> { seq { set 314 }  { set  <num>} { set  <num> } <GEXP> }
-> { seq { set 314 }  { set  977 } { set  <num> } <GEXP> } -> { seq { set 314 }  { set  977 } { set  489 } <GEXP> } -> { seq { set 314 }  { set  977 } { set  489 } <get> } -> { seq { set 314 }  { set  977 } { set  489 } get }

<MAE> -> { seq <GEXP> } -> { seq  <num> } -> { seq  314977489 }


<MAE> -> { seq <set> <MORE> <GEXP> } -> { seq <set>  <set> <MORE> <GEXP> } -> { seq <set>  <set> <set> <GEXP> } -> { seq { set <EXP> }  <set> <set> <GEXP> } -> { seq { set <EXP> }  { set <EXP> } <set> <GEXP> } -> { seq { set <EXP> }  { set <EXP> } { set <EXP> } -> { seq { set 314 }  { set  977 } { set  489 } { * 01 <num> } }> <GEXP> }
-> { seq { set <num> }  { set <EXP> } { set <EXP> } <GEXP> } -> { seq { set <num> }  { set  <num> } { set <EXP> } <GEXP> } -> { seq { set <num> }  { set  <num>} { set  <num> } <GEXP> } -> { seq { set 314 }  { set  <num>} { set  <num> }> <GEXP> }
-> { seq { set 314 }  { set  977 } { set  <num> }> <GEXP> } -> { seq { set 314 }  { set  977 } { set  489 }> <GEXP> } -> { seq { set 314 }  { set  977 } { set  489 }> { * <GEXP> <GEXP> } } -> { seq { set 314 }  { set  977 } { set  489 } { * <num> <GEXP> } }
-> { seq { set 314 }  { set  977 } { set  489 } { * <num> <num> } } -> { seq { set 314 }  { set  977 } { set  489 } { * 01 <num> } } -> { seq { set 314 }  { set  977 } { set  489 }> { * 01 05 } }
|#

;; ========= Question 3 =========
;; === Higher Order Functions ===
(: sum-of-squares : (Listof Number) -> Number)
(: square : Number -> Number)
#|
@description: this function is responsible to do square operation on an input number
@input: num of type Number
@output: result of type Number
|#
(define (square num)
  (* num num)
)

;; tests
(test (square 3) => 9)
(test (square 0) => 0)
(test (square -3) => 9)

#|
@description: this function is responsible to aggregate
     the sum of squares in input list, for each value in the list
     the function will square value and combine with '+
     finally returning output of type Number
@input: list of type (Listof Number)
@output: result of type Number
|#
(define (sum-of-squares list)
  (foldl + 0 (map square list)) 
)

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1 1 1)) => 3)
(test (sum-of-squares '(1 2 3 0)) => 14)
(test (sum-of-squares '(-1 -1 -1)) => 3)
(test (sum-of-squares '(-1 -2 -3)) => 14)

;; ================= Question 4  ==================
;; ==== Typed Racket (and more H.O functions) =====
;; (a)
(define-type BINTREE
  [Node BINTREE BINTREE] ; Req case
  [Leaf Number] ; Base case
)
;; (b)
#|
@description: this function is responsible to apply f function on each element in the input BSINTREE
@input: f function of type (Number -> Number) and a BINTREE tree
@output: new BINTREE which is the result of apply f on every leaf in the original tree
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f tree)
  (cases tree
    [(Leaf value) (Leaf (f value))]
    [(Node left right) (Node (tree-map f left) (tree-map f right))]
    )
  )

;; tests
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map square (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 1) (Node (Leaf 4) (Leaf 9))))
(test (tree-map (lambda (x) 5) (Leaf 1)) => (Leaf 5))

;; (c) - Above
;; (d)
#|
@description: this function is responsible to aggregate f(leaf) for each leaf in the tree and
  combine the results via comb function
@input: comb function of type (A A -> A) which combine two results
        f of type (Number -> A) which calculate the result of f(leaf) for each leaf
        tree of type BINTREE which is the original tree
@output: aggregated value of type A after combining all the results by applying f on each leaf
|#
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold comb f tree)
  (cases tree
    [(Leaf value) (f value)]
    [(Node left right) (comb (tree-fold comb f left) (tree-fold comb f right))]
    )
)
#|
define `tree` as a BINTREE variable for future testing
|#
(define tree  (Node (Node (Leaf 3) (Leaf 4)) (Node (Leaf 1) (Node (Leaf 0) (Leaf -1)))))

;;tests
(test (tree-fold + square tree) => (+ (* 3 3) (* 4 4) (* 1 1) (* 0 0) (* -1 -1)))
(test (tree-fold + square tree) => (foldl + 0 (map square '(3 4 1 0 -1)))) ; Its the same
(test (tree-fold * square tree) => (* (* 3 3) (* 4 4) (* 1 1) (* 0 0) (* -1 -1)))

;; (e)
(: tree-flatten : BINTREE -> (Listof Number))

;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number)
tree))

;; (f)
(test (tree-flatten tree) => '(3 4 1 0 -1))
(test (tree-fold + square tree) => (foldl + 0 (map square (tree-flatten tree))))  ; Its the same

;; (g)
#|
@descrption: this function is responsible to switch between right BINTREE and left BINTREE and create NODE of it
@input: left BINTREE and right BINTREE
@output: new Node with switched BINTREE's
|#
(: switch : (BINTREE BINTREE -> BINTREE))
(define (switch left right)
  [Node right left]
)

(test (switch (Leaf 3) (Leaf 4)) => (Node (Leaf 4) (Leaf 3)))

#|
@description: this function is responsible to reverse a BINTREE which tree-fold
@input: tree of type BINTREE
@output: tree of type BINTREE which is the reverse of the input 
|#

(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse tree)
  (tree-fold switch Leaf tree)
 )
;; tests
(test (equal? (reverse (tree-flatten (Leaf 3))) (tree-flatten (tree-reverse (Leaf 3)))))
(test (equal? (reverse (tree-flatten tree)) (tree-flatten (tree-reverse tree))))