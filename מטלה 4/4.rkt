  ;; The Flang interpreter

  #lang pl 04

#| The grammar:
 <FLANG> ::= <num> ;; Rule 1
            | { + <FLANG> <FLANG> } ;; Rule 2
            | { - <FLANG> <FLANG> } ;; Rule 3
            | { * <FLANG> <FLANG> } ;; Rule 4
            | { / <FLANG> <FLANG> } ;; Rule 5
            | { with { <id> <FLANG> } <FLANG> } ;; Rule 6
            | <id> ;; Rule 7
            | { fun { <id> } <FLANG> } ;; Rule 8
            | { call <FLANG> <FLANG> } ;; Rule 9
            | {bool true} ;;  Rule 10
            | {bool false} ;; Rule 11
            | {= <FLANG> <FLANG>} ;; add rule for = ;; Rule 12
            | {< <FLANG> <FLANG>} ;; Rule 13
            | {> <FLANG> <FLANG>} ;; Rule 14
            | {not <FLANG>} ;; Rule 15
            | {if <FLANGE> {then-do <FLANGE>} {else-do <FLANGE>}} ;; rule 16 



Formal Substitution rules:
 subst:
     N[v/x] = N
     {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
     {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
     {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
     {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
     y[v/x] = y
     x[v/x] = v
     {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
     {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
     {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
     {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
     {fun {x} E}[v/x] = {fun {x} E}
     B[v/x] = B ;; B is Boolean
     {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
     {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
     {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
     { not E}[v/x] = {not E[v/x]}
     {if Econd {then-do Edo} {else-do Eelse}}[v/x]
       = {if Econd[v/x] {then-do Edo[v/x]} {else-do Eelse[v/x]}}


eval: Evaluation rules:
     eval(N) = N ;; N is an expression for a numeric value
     eval({+ E1 E2}) = eval(E1) + eval(E2) \ if both E1 and E2
     eval({- E1 E2}) = eval(E1) - eval(E2) \ evaluate to numbers
     eval({* E1 E2}) = eval(E1) * eval(E2) / otherwise error!
     eval({/ E1 E2}) = eval(E1) / eval(E2) /
     eval(id) = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     eval(FUN) = FUN ; assuming FUN is a function expression
     eval({call E1 E2}) = eval(Ef[eval(E2)/x])
     if eval(E1)={fun {x} Ef}
      = error! otherwise

     eval(B) = B ;; B is an expression for a Boolean value
     eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
     eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to numbers
     eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
     eval({not E}) = not(eval(E)) /E may be anything
     eval({if Econd {then-do Edo} {else-do Eelse}}) = eval(Edo) if eval(Econd) =/= false, eval(Eelse), otherwise
  |#

  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])

  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs
 #|
    Function input: Sexpr
    Function outpul: FLANG
    Function purpose: convert s-expressions into FLANGs
    The function operation: It works just like the original Fusion we just added the new options of the conditions.
    It took us 10 minutes and together we thought about the solution.
|#
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num n)]
      ['True (Bool true)]
      ['False (Bool false)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
      [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'not exp) (Not (parse-sexpr exp))]
      [(cons 'if more)
       (match sexpr
         [(list 'if condition  (list 'then-do then-exp) (list 'else-do else-exp))
          (If (parse-sexpr condition) (parse-sexpr then-exp) (parse-sexpr else-exp))]
         [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  (: subst : FLANG Symbol FLANG -> FLANG)
 #|
    Function input:  FLANG Symbol FLANG
    Function outpul: FLANG
    Function purpose: substitutes the second argument with the third argument in the
    first argument, as per the rules of substitution; the resulting
    expression contains no free instances of the second argument
    The function operation: It works just like the original Fusion we just added the new options of the conditions.
    It took us 15 minutes and together we thought about the solution.
 |#
 (define (subst expr from to)
   (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
      [(Bool b) expr]
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
      [(Not e) (Not (subst e from to))]
      [(If cond-e then-e else-e) (If (subst cond-e from to) (subst then-e from to) (subst else-e from to))]))


 ;; The following function is used in multiple places below,
 ;; hence, it is now a top-level definition
 (: Num->number : FLANG -> Number)
 ;; gets a FLANG -- presumably a Num variant -- and returns the
 ;; unwrapped number
 (define (Num->number e)
   (cases e
      [(Num n) n]
      [else (error 'Num->number "expected a number, got: ~s" e)]))

  (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
  ;; gets a Racket numeric binary operator, and uses it within a FLANG
  ;; `Num' wrapper
  (define (arith-op op expr1 expr2)
    (Num (op (Num->number expr1) (Num->number expr2))))

  (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
 #|
    Function input: (Number Number -> Boolean) FLANG FLANG 
    Function outpul: FLANG
    Function purpose:  gets a Racket Boolean binary operator (on numbers), and applies it
       to two `Num' wrapped FLANGs
    The function operation: applies the operator on the 2 `Num' wrapped FLANGs after applies on    
    them the function Num->number. the after the a boolean expression from the function warp it with `Bool'
    process of solving: we Just look at the arith-op function and did the same but just warp the
    solution with `Bool' and not with `Num'. It took us 20 minutes and together we thought about the solution.
|#
  (define (logic-op op expr1 expr2)
     (Bool (op (Num->number expr1) (Num->number expr2))))


 (: flang->bool : FLANG -> Boolean)

 #|
    Function input: FLANG
    Function outpul: Boolean
    Function purpose: gets a Flang E (of any kind) and returns a its appropiate Boolean value
    The function operation: if the function get (Bool B) it will return the matching boolean value by B
    otherwise for all other expressions it will return true.
    process of solving: the main difficault was to understand this function propose and how it
    suppose to work just un 2 lines. After trying to solve the eval function and anderstand in which cases
    we will use this function it became clear to us how to built it. It took us 20 minutes and
    together we thought about the solution.
|#
 (define (flang->bool e)
   (cases e
     [(Bool B) B]
     [else true]))

  (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
   #|
       Function input: FLANG
       Function outpul: FLANG
       Function purpose:  evaluates FLANG expressions by reducing them to *expressions*
       The function operation: The begining is the same. inside cases function it the expression is:
       (Bool B) we return it as is.
       (Equal l r)we retun (logic-op = (eval l) (eval r)) and the same for Bigger and Smaller but with their sign.
       (If l m r) we operate the flang->bool function on l. if it true we return eval(m) else eval(r)
       (Not exp) we return (Bool (not (flang->bool (eval exp)))
       process of solving: the main difficault was to understand how to solve the Not part because we didn't
       know how to deal with not inside not. but then after discussion between us and trying some options
       we came up with a solution that seemed the best to us. It took us 30 minutes.
   |#
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body)
            (eval (subst bound-body
                         bound-id
                         (eval arg-expr)))]
           [else (error 'eval "`call' expects a function, got: ~s"
                              fval)]))]
      [(Bool b) expr]
      [(Equal l r) (logic-op = (eval l) (eval r))]
      [(Bigger l r) (logic-op > (eval l) (eval r))]
      [(Smaller  l r) (logic-op < (eval l) (eval r))]
      [(If l m r)
           (let ([cond (flang->bool (eval l))])
             (if cond (eval m) (eval r)))]
      [(Not exp) (Bool (not (flang->bool (eval exp))))]))
            


  (: run : String -> (U Number Boolean FLANG))
    #|
       Function input: String
       Function outpul: (U Number Boolean FLANG)
       Function purpose: evaluate a FLANG program contained in a string
       The function operation: The function operate eval function on an AST that coming back from parse
       on the input string. then we operate cases function on the result of eval function:
       if it (Bool b) we return b, if it (Num n) we return n, otherwise we return the result itself
       process of solving: the main difficault was to understand how it
       suppose to work just un 3 lines. we consult each other and try some options. zvi think we
       need to add another row that will handle an error, but because we have just 3 rows to fill we continue to think
       then eilon suggest a new option that is just 3 rows. It took us 10 minutes.
    |#
  (define (run str)
     (let ([result (eval (parse str))])
         (cases result
            [(Bool B) B]
            [(Num n) n]
            [else result])))


;; tests
  (test (run "{call {fun {x} {+ x 1}} 4}")
        => 5)
  (test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
        => 4)
    (test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
        => 7)

   (test (run "True") => true)
   (test (run "{not True}") => false)
   (test (run "{> 3 44}") => false)
   (test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
   (test (run "{with {x 8} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
   (test (run "{with {x 0} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
   (test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
   (test (run "{with {c True} {if c {then-do {> 2 1}} {else-do 2}}}") => true)
   (test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
       => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
   (test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}")
       =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
   (test (run "true") =error> "eval: free identifier: true")
   (test (run "{< false 5}") =error> "eval: free identifier: false")
   (test (run "{< False 5}")
       =error> "Num->number: expected a number, got: #(struct:Bool #f)")

   (test (run "{not {not {not {if {> 1 3} {then-do {* 2 6}} {else-do 3}}}}}") => false)
   (test (run "{with x 0 {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `with' syntax")
   (test (run "{with {add3 {fun x {+ x 3}}}
                {with {add3 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add3 {call add3 x}}}}}") =error> "parse-sexpr: bad `fun' syntax")
  (test (run "{3 44}") =error> "parse-sexpr: bad syntax in")
  (test (run "{= 22 {/ 44 2}}") => true)
  (test (run "{with {x 0} {if {> x 8} {then-do {* 2 x}} {else-do {with {x 6} {- x 9}}}}}") => -3)
  (test (run "{with {x {* 2 0}} {if {= x 0} {then-do True} {else-do False}}}") => true)
  (test (run "{call {fun {x} {< x 2}} 6}") => false)
  (test (run "{call {fun {x} {not {* 7 x}}} 6}") => false)
  (test (run "{call False 6}") =error> "eval: `call' expects a function, got")
  (test (run "{if {not {= {* 2 5} 10}} {then-do 4} {else-do {if {= 5 5} {then-do 10} {else-do 0}}}}") => 10)
  (test (run "{= 22}") =error> "parse-sexpr: bad syntax in")
  (test (run "{not 10 2}") =error> "parse-sexpr: bad syntax in")
  (test (run "{> {< 3 2} 2}") =error> "Num->number: expected a number, got:")
  (test (run "{call {fun {y} {> {/ 33 y} 0}} {with {y 3} {if {> {/ 33 y} 0} {then-do {/ 33 y}} {else-do 0}}}}") => true)
  (test (run "{if {+ 0 0} {then-do True} {else-do {+ 2 2}}}") => true)
  (test (run "{if {+ 0 1} {then-do True} {else-do {+ 2 2}}}") => true)
  (test (run "False") => false)
  (test (run "{< 0 0}") => false)
  (test (run "{= 3 44}") => false)
  (test (run "{= 0 0}") => true)
  (test (run "false") =error> "eval: free identifier: false")
  (test (run "{with {x 5} {if {> x 0} {then-do True } {else-do x}}}") => true)
  (test (run "{< 5 true}") =error> "eval: free identifier: true")
  (test (run "{< 5 5}") => false)
  (test (run "{if {> 2 1} {then-do {if {- 3 3} {then-do 4} {else-do 5}}} {else-do {+ 2 2}}}") => 4)
  (test (run "{< 0 {if {- 3 3} {then-do 4} {else-do 5}}}") => true)
  (test (run "{= {if {- 3 3} {then-do 4} {else-do 5}} {if {- 3 3} {then-do 4} {else-do 5}}}") => true)
  (test (run "{< {with {x 8} {if {> x 0} {then-do {/ 2 x}} {else-do x}}} {if {- 3 3} {then-do 4} {else-do 5}}}") => true)
  (test (run "{< true true}") =error> "eval: free identifier: true")