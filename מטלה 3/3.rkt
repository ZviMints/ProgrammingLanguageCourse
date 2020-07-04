  #lang pl 03

  #| BNF for the MUWAE language:
       <MUWAE> ::= list of <num>
               | { + <MUWAE> <MUWAE> }
               | { - <MUWAE> <MUWAE> }
               | { * <MUWAE> <MUWAE> }
               | { / <MUWAE> <MUWAE> }
               | { with { <id> <MUWAE> } <MUWAE> }
               | <id>
               | {sqrt <MUWAE>}
  |#

  ;; MUWAE abstract syntax trees
  (define-type MUWAE
    [Num  (Listof Number)]
    [Add  MUWAE MUWAE]
    [Sub  MUWAE MUWAE]
    [Mul  MUWAE MUWAE]
    [Div  MUWAE MUWAE]
    [Id   Symbol]
    [With Symbol MUWAE MUWAE]
    [Sqrt MUWAE])

  (: parse-sexpr : Sexpr -> MUWAE)
  ;; to convert s-expressions into MUWAEs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num (list n))]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(cons 'sqrt more)
       (match sexpr
         [(list 'sqrt E)(Sqrt (parse-sexpr E))]
         [else (error 'parse-sexpr "bad `sqrt' syntax in ~s" sexpr)])]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> MUWAE)
  ;; parses a string containing a MUWAE expression to a MUWAE AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a list of  <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
        {sqrt E1}[v/x]        = {sqrt E1[v/x]}
  |#

  (: subst : MUWAE Symbol MUWAE -> MUWAE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(Sqrt E) (Sqrt (subst E from to))]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))

(: sqrt+ : (Listof Number) -> (Listof Number)) 
#|
Function input: Listof Number
Function outpul: Listof Number
Function purpose:The function gets a list of numbers, it takes root on each number
and returns a list with the positive and negative root of each
The function operation: This is a recursive function. If the list is empty, it returns an empty list.
If one of the numbers is negative it will return an error.
Otherwise, it will return a list whose first 2 organs are the positive and negative root of the first
organ in the original list and the rest of its organs are the result of sqrt+ on all organs in the
original list without the first
process of solving: the main difficault was to solve the last condition in the cond function.
After several failed attempts by each of us, we decided to try to read the question again as the rest
focused on the clue that was given. Then zvi open the presentation of the exercises where we looked again
at how "cons" are used. We tried more until we were finally able to find a solution that seemed most
appropriate to us.It took us 20 minutes.
|#
(define (sqrt+ ns)
 (cond
   [(null? ns) '()]
   [(< (first ns) 0) (error 'sqrt+ "`sqrt+' requires a nonnegative inputs and got ~s" (first ns))]
   [else (cons (sqrt (first ns)) (cons (* -1 (sqrt (first ns))) (sqrt+ (rest ns))))])) 



(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number)
            -> (Listof Number))
#|
Function input: (Number Number -> Number) (Listof Number) (Listof Number)
Function outpul: Listof Number

Function purpose: applies a binary numeric function on all combinations of numbers from
 the two input lists, and return the list of all of the results

The function operation: Within the function, we create a helper function:
The helper function receives a number and the second list of number of the original function,
and returns a list of numbers. Within the helper function, we define a function "f" which receives a number
and returns the result of the original operator on the number it received together with the number that
the helper function received. Finally, the helper function runs the "map" function on the function "f"
along with the list of numbers that the helper function received.
Original function operation: This is a recursive function which if the first list of number received is empty
 it returns an empty list. Otherwise, it will return a 2 list connection:
the first list: The list that returns from running the helper function on the first number in the first list
together with the second list
The second list: The list that returns from running the bin-op function on the same operator it received
with the first list but without its first organ and with the second list

process of solving: the main difficault was to understand how the function works.
After reading the function several times we realized how it works and then it was
relatively easy to solve the "hole" we needed to complete
It took us 10 minutes.
|#
(define (bin-op op ls rs)
  (: helper : Number (Listof Number) -> (Listof Number))
  (define (helper l rs)
    (: f : Number -> Number)
    (define (f num)
      (op l num))
    (map f rs))

  (if (null? ls) null
    (append (helper (first ls) rs) (bin-op op (rest ls) rs)))) 


  #| Formal specs for `eval':
      (`N' is a list of  <num>)
       eval(N)         = (list N)
       eval({+ E1 E2}) = (list for e1 in eval(E1):
                                   for e2 in eval(E2)
                                      e1 + e2)
       eval({- E1 E2}) = (list for e1 in eval(E1):
                                   for e2 in eval(E2)
                                      e1 - e2)
       eval({* E1 E2}) = (list for e1 in eval(E1):
                                   for e2 in eval(E2)
                                      e1 * e2)
       eval({/ E1 E2}) = (list for e1 in eval(E1):
                                   for e2 in eval(E2)
                                      e1 / e2)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
       eval({sqrt E})  = if eval(E) < 0
                         then Error!
                         else sqrt(eval(E))
  |#

  (: eval : MUWAE -> (Listof Number))
  ;; evaluates MUWAE expressions by reducing them to numbers
  (define (eval expr)
    (cases expr
      [(Num nums) nums]
      [(Add l r) (bin-op + (eval l) (eval r))]
      [(Sub l r) (bin-op - (eval l) (eval r))]
      [(Mul l r) (bin-op * (eval l) (eval r))]
      [(Div l r) (bin-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (Num (eval named-expr))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Sqrt e) (sqrt+ (eval e))]))

  (: run : String -> (Listof Number))
  ;; evaluate a MUWAE program contained in a string
  (define (run str)
    (eval (parse str)))

  ;; tests
  (test (run "5") => '(5))
  (test (run "{+ 5 5}") => '(10))
  (test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
  (test (run "{with {x 5} {+ x x}}") => '(10))
  (test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
  (test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
  (test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
  (test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
  (test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
  (test (run "{with {x 5} {with {y x} y}}") => '(5))
  (test (run "{with {x 5} {with {x x} x}}") => '(5))
  (test (run "{with {x 1} y}") =error> "free identifier")
  (test (run "{with x 5 {+ x 9}}") =error> "bad `with' syntax in")
  (test (run "{with {x {* 2 3}} {with {y {/ x -2}} {* y y}}}") => '(9))
  (test (run "{/ 60 {with {z 3} {* 5 z}}}") => '(4))
  (test (run "{o 5 {+ 4 6}}") =error> "bad")
  (test (run "{sqrt 9}") => '(3 -3))
  (test (run "{* 3 {sqrt 0}}") => '(0 0))
  (test (run "{with {x 5} {+ x {sqrt -4}}}") =error> "`sqrt+' requires a nonnegative input")
  (test (run "{+ {* 2 {sqrt 3}} {sqrt 4}}") => '(5.464101615137754 1.4641016151377544 -1.4641016151377544 -5.464101615137754))
  (test (run "{* 4 {sqrt {sqrt 81}}}") =error> "`sqrt+' requires a nonnegative input")
  (test (run "{sqrt 1}") => '(1 -1))
  (test (run "{sqrt 0}") => '(0 0))
  (test (run "{sqrt -1}") =error> "`sqrt+' requires a nonnegative input")
  (test (run "{sqrt 6 9}") =error> "bad `sqrt' syntax")
  (test (run "{+ {sqrt 1} 3}") => '(4 2))
  (test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
  (test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))
  (test (run "{+ {sqrt {/ 16 4}} 3}") => '(5 1))
  (test (run "{with {x {sqrt 4}} {with {y {sqrt 9}} {* x {- {sqrt 16} y}}}}") => '(2 14 -14 -2 -2 -14 14 2))
  (test (run "{with {x {/ {sqrt 36} 3}} {with {x {* x {sqrt 64}}} {+ x x}}}") => '(32 0 0 32 0 -32 -32 0 0 -32 -32 0 32 0 0 32))
  (test (run "{sqrt 9}") => '(3 -3))
  (test (run "{sqrt {+ -1 1}}") => '(0 0))
  (test (run "{+ {sqrt 0} {sqrt 1}}") => '(1 -1 1 -1))
  (test (run "{+ {sqrt 4} {sqrt 1}}") => '(3 1 -1 -3))
  (test (run "{+ {sqrt -1} {sqrt 1}}") =error> "`sqrt+' requires a nonnegative input")
  (test (run "{sqrt -1}") =error> "`sqrt+' requires a nonnegative input")

;===============================================   second part   =================================================

#|

<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#



(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])



(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))

(: freeInstanceList : WAE -> (Listof Symbol))
#|
Function input: WAE
Function outpul: Listof Symbol
Function purpose: find all the free instance in the WAE expression and return them inside a list of symbols
The function operation: we operate the cases function on the input WAE expressin:
if it is NumW we return null
if it one of the Arithmetic Operators we return list of number consisting of 2 list of numbers created from
running the freeInstanceList function on the left and right expression.
if it one of the "withW" expression we return list of number consisting of 2 list of numbers created from
running the freeInstanceList function on the named-expr expression and on the bound-body
expression (after running substW on it).
if it is IdW expression we return list of the symbol name
process of solving: the main difficault was to deal with the "withW" expression. we got some errors in some tests
 but then eilon investigate the structure of Eval Function
and then We understood the idea of how this part should be done especially about
the mistake we had about running the freeInstanceList faction on named-expr as well.
It is took us about 30 minutes.
|#
(define (freeInstanceList expr)
  (cases expr
    [(NumW n) '()]
    [(AddW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(SubW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(MulW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(DivW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(WithW bound-id named-expr bound-body)
     (let ([named (freeInstanceList named-expr)]
           [body (freeInstanceList (substW bound-body bound-id (NumW 0)))])
            (append named body))]
    [(IdW name) (list name)]))




(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{* y {+ 3 a}}")) => '(y a))
(test (freeInstanceList (parseW "{/ a a}")) => '(a a))

(test (freeInstanceList (parseW "{with {x 5} {with {y {/ z 3}} {+ y y}}}")) => '(z))
(test (freeInstanceList (parseW "{with {x {+ 5 4}} {* x x}}")) => '())
(test (freeInstanceList (parseW "{with {x {with {t 3} {* t a}}} {/ t x}}")) => '(a t))
(test (freeInstanceList (NumW 0)) => '())
(test (freeInstanceList (IdW 'a)) => '(a))
(test (freeInstanceList (parseW "{with {x 1} y}")) => '(y))
(test (freeInstanceList (parseW "{with x 5 {with {y {- x 3}} {+ y y}}}")) =error> "bad `with' syntax in")
(test (freeInstanceList (parseW "{{x 5} {+ x {with {x 3} 10}}}")) =error> "bad syntax in")
(test (freeInstanceList (parseW "{with {x {- 2 2}} {+ x {with {x 3} {/ x 5}}}}")) => '())
(test (freeInstanceList (parseW "{with {x {- 3 w}} {+ x {with {x 3} {/ x r}}}}")) => '(w r))
(test (freeInstanceList (parseW "{with {x 7} {+ k {with {y 3} x}}}")) => '(k))
(test (freeInstanceList (parseW "{with {x 5} {with {y {* z z}} y}}")) => '(z z))
(test (freeInstanceList (parseW "{- x {with {x 0} {with {x x} x}}}")) => '(x))




