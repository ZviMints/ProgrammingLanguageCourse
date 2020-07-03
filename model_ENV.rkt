#lang pl
#|
  <FLANG> ::= <num> | <id>
  | { + <FLANG> <FLANG> }
  | { - <FLANG> <FLANG> }
  | { / <FLANG> <FLANG> }
  | { * <FLANG> <FLANG> }
  | { with { <id> <FLANG> } <FLANG>}
  | { fun { <id> } <FLANG> } ;; parm-name body
  | { call <FLANG> <FLANG> } ;; fun-expr arg-expr

  <num> - identifies any expression that pl evaluates to a Number
  <id> - identifies any expression that pl evaluate to a Symbol
  |#

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV Symbol FLANG ENV]
  [PairV ((VAL VAL -> VAL) -> VAL)]
  )

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env 
    [(EmptyEnv) (error 'lookup "cannot find key ~s" name)]
    [(Extend key val rest-env) (if (eq? key name) val (lookup name rest-env))]))

(define-type FLANG
  [Num Number]
  [Id Symbol]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Fun Symbol FLANG]
  [Call FLANG FLANG]  
  [Div FLANG FLANG]
  [With Symbol FLANG FLANG]
  [Cons FLANG FLANG]
  [First FLANG]
  [Second FLANG])

(: parse-sexpr : Sexpr -> FLANG)
 (define (parse-sexpr expr)
(match expr
  [(number: num) (Num num)]
  [(symbol: name) (Id name)]
  [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
  [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
  [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
  [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
  [(list 'cons f s) (Cons (parse-sexpr f) (parse-sexpr s))]
  [(list 'first f) (First (parse-sexpr f))]
  [(list 'second s) (Second (parse-sexpr s))]
  [(cons 'with _)
     (match expr
   [(list 'with (list (symbol: name) named-expr) body)
      (With name (parse-sexpr named-expr) (parse-sexpr body))]
   [_ (error 'parse-sexpr "bad with syntax in ~s" expr)])]
  [(cons 'fun more)
   (match expr
     [(list 'fun (list (symbol: parm-name)) body) (Fun parm-name (parse-sexpr body))]
     [else (error 'parse-sexpr "bad `fun` syntax in ~s" expr)]
  )]
  [(list 'call fun-expr arg-expr) (Call (parse-sexpr fun-expr) (parse-sexpr arg-expr))] 
  [_ (error 'parse-sexpr "bad syntax in ~s" expr)]))

(: parse : String -> FLANG)
     (define (parse code)
     (parse-sexpr(string->sexpr code))
  )

(: mycons : VAL VAL -> ((VAL VAL -> VAL) -> VAL))
(define (mycons f s)
  (: mypair : (VAL VAL -> VAL) -> VAL)
  (define (mypair selector)
    (selector f s))
  mypair)

(: myfirst : ((VAL VAL -> VAL) -> VAL) -> VAL)
(define (myfirst p)
  (: first-selector : VAL VAL -> VAL)
  (define (first-selector f s) f)
  (p first-selector))

(: mysecond : ((VAL VAL -> VAL) -> VAL) -> VAL)
(define (mysecond p)
  (: second-selector : VAL VAL -> VAL)
  (define (second-selector f s) s)
  (p second-selector))


(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
(define (arith-op op expr1 expr2)
(: Num->Number : VAL -> Number)
(define (Num->Number e)
  (cases e
    [(NumV n) n ]
    [else (error 'arith-op "expects a number, got: ~s" e)] ))
(NumV (op (Num->Number expr1) (Num->Number expr2))))

(: PairV->pair : VAL -> ((VAL VAL -> VAL) -> VAL))
(define (PairV->pair v)
  (cases v
    [(PairV p) p]
    [else (error 'PairV->pair "expected a pair got: ~s" v)]))


#|
formal specifications for eval:

lookup(x, empty subst) = error
lookup(x, extend(x,E,env') = E
lookup(x, extend(y,E,env') = lookup(x, env')

 eval(N, env) = N
 eval({ op E1 E2}, env) = eval(E1, env) op eval(E2, env) for op in [+,*,/,-]
 eval(id, env) = lookup(id,env)
 eval({with {x E1} E2}, env) = eval(E2, extend(x, eval(E1, env), env))
 eval({fun {x} E}, env) = <x, E, env> ; closure
 eval({call fun arg}, env) = if <X,Ef, f-env> <--- eval(E1, env)
                              then eval(Ef, extend (x,eval(E2,env), f-env))
                              else <error>
 eval({cons E1 E2}, env) = <eval(E1, env), eval(E2, env)>
 eval({first E}, env)  = if <eval(E,env) = <a,b> then a else error
 eval({second E}, env)  = if <eval(E,env) = <a,b> then b else error
|#
( : eval : FLANG ENV -> VAL)
(define (eval expr env)
(cases expr
  [(Num n) (NumV n)] ;; change
  [(Add l r) (arith-op + (eval l env) (eval r env)) ]
  [(Sub l r) (arith-op - (eval l env) (eval r env)) ]
  [(Mul l r) (arith-op * (eval l env) (eval r env)) ]
  [(Div l r) (arith-op / (eval l env) (eval r env)) ]
  [(With bound-id named-expr bound-body)
   (eval bound-body
         (Extend bound-id (eval named-expr env) env))] ;; change extend->Extend
  [(Id name) (lookup name env) ]
  [(Fun bound-id bound-body) (FunV bound-id bound-body env)] ;; change
  [(Call fun-expr arg-expr)
   (let ([fval (eval fun-expr env)] )
     (cases fval
       [(FunV bound-id bound-body f-env)
        (eval bound-body
         (Extend bound-id (eval arg-expr env) f-env))] ;; change to f-env [!]
       [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
  [(Cons a b) (PairV (mycons (eval a env) (eval b env)))]
  [(First p) (myfirst (PairV->pair (eval p env)))]
  [(Second p) (mysecond (PairV->pair (eval p env)))])) 


(: run : String -> Number)
(define (run string)
  (let ([res (eval (parse string) (EmptyEnv))])
    (cases res
      [(NumV n) n]
      [else (error 'run "expected to recieve a number, got ~s" res)]
      )))


  ;;; Tests
(test (parse "{ with {x {* 4 2}} {* x x}}") => (With 'x (Mul (Num 4) (Num 2)) (Mul (Id 'x) (Id 'x))))
(test (parse "{fun {x} {+ x x}}") => (Fun 'x (Add (Id 'x) (Id 'x))))
(test (parse "{call sqrt 5}") => (Call (Id 'sqrt) (Num 5)))
(test (parse "{call f 1}") => (Call (Id 'f) (Num 1)))
(test (parse "{fun { f } { * f f }}") => (Fun 'f (Mul (Id 'f) (Id 'f))))
(test (parse "{with {f 1} {+ f f}}") => (With 'f (Num 1) (Add (Id 'f) (Id 'f))))
(test (parse "{call {fun {x} x} 5}") => (Call (Fun 'x (Id 'x)) (Num 5)))
 
(test (run "{with {x 3}
             {with {f {fun {y} {+ x y}}}
               {with {x 5}
                 {call f 4}}}}") =>                               
                                 ;; 9 Dynamic Approach (SubstCache)
                                 7 ;; Static Approach (subst)
                                 )

     
   