;; The Flang interpreter, using environments

#lang pl

#|
what were the main difficulties? Global enviroment.
How you solved them? I see that Env and Val contain Env.
How much time did you invest in solving it? 3 hours.
Did you need to consult others? A little bit.

 The grammar:
<FLANG> ::= <num>
            | { with { <id> <FLANG> } <FLANG> }
            | <id>
            | { fun { <id> } <FLANG> } ;;a function may have a single formal parameter
            | { fun { <id> <id> } <FLANG> } ;; or two formal parameters
            | { call <FLANG> <FLANG> } ;;a function has either a single actual parameter
            | { call <FLANG> <FLANG> <FLANG> } ;; or two actual parameters
|#

(define-type FLANG
  [Num Number]
  [Id Symbol]
  [Add FLANG FLANG] ; Never created by user
  [Sub FLANG FLANG] ; Never created by user
  [Mul FLANG FLANG] ; Never created by user
  [Div FLANG FLANG] ; Never created by user
  [With Symbol FLANG FLANG]
  [Call1 FLANG FLANG]
  [Call2 FLANG FLANG FLANG]
  [Fun1 Symbol FLANG]
  [Fun2 Symbol Symbol FLANG])

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more) 
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       ;one argument
       [(list 'fun (list (symbol: name)) body)
        (Fun1 name (parse-sexpr body))]
       ;two arguments
       [(list 'fun (list (symbol: firstSym) (symbol: secondSym)) body)
        (Fun2 firstSym secondSym (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(cons 'call more)
     (match sexpr
       ;one argument
       [(list 'call function arg)
        (Call1 (parse-sexpr function) (parse-sexpr arg))]
       ;two arguments
       [(list 'call fun firstArg secondArg)
        (Call2 (parse-sexpr fun) (parse-sexpr firstArg) (parse-sexpr secondArg))]
       [else (error 'parse-sexpr "bad `call' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function
(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV1 Symbol FLANG ENV]
  [FunV2 Symbol Symbol FLANG ENV])

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV wrapper
(define (arith-op op val1 val2)
  (: NumV->number : VAL -> Number)
  (define (NumV->number v)
    (cases v
      [(NumV n) n]
      [else (error 'arith-op "expects a number, got: ~s" v)]))
  (NumV (op (NumV->number val1) (NumV->number val2))))

#|
eval: Evaluation rules:
 eval(N,env) = N
 eval(x,env) = lookup(x,env)
 eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
 eval({fun {x1} E},env) = <{fun {x1} E}, env>
 eval({fun {x1 x2} E},env) = <{fun {x1 x2} E}, env>
 eval({call E-op E1},env1)
       = eval(Ef,extend(x1,eval(E1,env),envf))
                 if eval(E-op,env) = <{fun {x} Ef}, envf>
       = error!
                 otherwise
 eval({call E-op E1 E2},env1)
       = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf))
                 if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
       = error!
                 otherwise

|#

(: eval : FLANG ENV -> VAL)
;; evaluates FLANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(Num n) (NumV n)]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(Fun1 bound-id bound-body)
     (FunV1 bound-id bound-body env)]
    [(Fun2 firstBound-id secondBound-id bound-body)
     (FunV2 firstBound-id secondBound-id bound-body env)]
    [(Call1 fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         ;function with one argument as required
         [(FunV1 bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         ;function with two arguments but call with two arguments
         [(FunV2 firstBound-id secondBound-id bound-body f-env)
          (error 'eval "expected two arguments, got one in: ~s" fval)]
         [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
    [(Call2 fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         ;function with one argument but call with one argument
         [(FunV1 bound-id bound-body f-env)
          (error 'eval "expected a single argument, got two in: ~s" fval)]
         ;function with two arguments as required
         [(FunV2 bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id2 (eval arg-expr2 env)
                        (Extend bound-id1 (eval arg-expr1 env) f-env)))]
         [else (error 'eval "`call' expects a function, got: ~s" fval)]))]))

(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend '+
          (FunV2 'first 'second
                 (Add (Id 'first) (Id 'second))
                 (EmptyEnv)) ;;Env of the function
          (Extend '-
                  (FunV2 'first 'second
                         (Sub (Id 'first) (Id 'second))
                         (EmptyEnv)) ;;Env of the function
                  (Extend '*
                          (FunV2 'first 'second
                                 (Mul (Id 'first) (Id 'second))
                                 (EmptyEnv)) ;;Env of the function
                          (Extend '/
                                  (FunV2 'first 'second
                                         (Div (Id 'first) (Id 'second))
                                         (EmptyEnv)) ;;Env of the function
                                  (EmptyEnv)))))) ;;Global env


(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run ;;Return a function
                   "evaluation returned a non-number: ~s" result)])))


;; tests
(test (run "{call + 4 5}") => 9)
(test (run "{call * 4 5}") => 20)
(test (run "{call / 4 4}") => 1)
(test (run "{with {add3 {fun {x} {call + x 3}}}
{call add3 1}}")
      => 4)
(test (run "{with {x 3}
{with {f {fun {y} {call + x y}}}
{with {x 5}
{call f 4}}}}")
      => 7)
(test (run "{call {fun {x y} {call + x { call - y 1}}} 4 2}") => 5)
(test (run "{with {first {fun {x y} x}}
{with {second {fun {x y} y}}
{call first {call second 2 123} 124}}}")
      => 123)
(test (run "{+ 4 5}") =error> "parse-sexpr: bad syntax in")
(test (run "{* 4 5}") =error> "parse-sexpr: bad syntax in")
(test (run "{with {add3 {fun {x} {call + x 3}}}
{call add3 1 2}}")
      =error> "expected a single argument, got two in: ")
(test (run "{call {fun {n} n} 0 0}")
      =error> "expected a single argument, got two in: ")
(test (run "{with {add3 {fun {x stam} {call + x 3}}}
{call add3 1}}")
      =error> "expected two arguments, got one in: ")
(test (run "{call {fun {m n} n} 0}")
      =error> "expected two arguments, got one in: ")
(test (run "{call + 0 {fun {n} n}}")
      =error> "expects a number, got")
(test (run "{call {with {n 0} 0} 0}")
      =error> "`call' expects a function, got:")
(test (run "{call {with {n 0} 0} 0 0}")
      =error> "`call' expects a function, got:")
(test (run "{call + n 0}")
      =error> "no binding for")
(test (run "{call + 0 0 0}")
      =error> "bad `call' syntax")
(test (run "{fun {n} n n}") 
      =error> "bad `fun' syntax")
(test (run "{with {n 5} n n n}")
      =error> "bad `with' syntax")
(test (run "{fun {n} 0}") 
      =error> "evaluation returned a non-number")
(test (run "{with {square {fun {x} {call * x x}}}
{call square 2}}")
      => 4)
(test (run "{with {avg {fun {x y} {call / {call + x y} 2}}}
{call avg 2 8}}")
      => 5)