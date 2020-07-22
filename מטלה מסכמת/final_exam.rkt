#lang pl
;; Zvi Mints

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set SET]
    [Smult Number SOL]
    [Inter SOL SOL]
    [Union SOL SOL]
    [Id    Symbol]
;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
    [Fun Symbol Symbol SOL]
    [CallS SOL SOL SOL]
    [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs

;; function: ismember?
;; description: takes a number n, and a set l and check if l includes n
;; output: true iff n in l
;; process: this is regular logic
  (: ismember? : Number SET  -> Boolean)
  (define (ismember? n l)
    (cond [(null? l) #f]
          [(= n (first l)) #t]
          [else (ismember? n (rest l))]))

#| ============================= Tests ============================= |#
  (test (not (ismember? 1 '(3 4 5))))
  (test (not (ismember? 1 '( 3 2 3 5 6))))
  (test (ismember? 1 '(3 4 5 1 3 4)))
  (test (ismember? 1 '(1)))

;; function: remove-duplicates
;; description: takes an input set l which can contain duplicates elements
;; output: return new set without any duplicates, can be not sorted
;; algo: for each element, check if its already in the set, if yes, continue, else, add it to the list
;; process: this is regular logic

  (: remove-duplicates : SET  -> SET)
  (define (remove-duplicates l)
    (cond [(or (null? l) (null? (rest l))) l]
          [(ismember? (first l) (rest l)) (remove-duplicates (rest l))] ;; check if first element in l already contains in rest of l
          [else (cons (first l) (remove-duplicates (rest l)))]))

#| ============================= Tests ============================= |#
 (test (remove-duplicates '(3 4 5)) => '(3 4 5))
 (test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
 (test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
 (test (remove-duplicates '(1)) => '(1))

;; function: create-sorted-set
;; description: takes an input set l which can contain duplicates elements and/or unsorted by operator <
;; output: return new set without any duplicates and sorted by opeartor <
;; algo: create new set without duplicates, and then sort it by operator < with racket sort function
;; process: this is regular logic, but there was a problem because (... (sort l <)) need to change l, so this is why i did let, but
;; as i explained later, i can do it as follows '(remove-duplicated (sort l <))' instead of override the l value with new env with new l

  (: create-sorted-set : SET -> SET)
  (define (create-sorted-set l)
    (let ([l (remove-duplicates l)]) (sort l <))) ;; can be also (remove-duplicated (sort l <))

#| ============================= Tests ============================= |#
 (test (create-sorted-set '(3 4 5)) => '(3 4 5)) ;; added tests
 (test (create-sorted-set '(5 4 5)) => '(4 5))
 (test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
 (test (create-sorted-set '(3 4 5 1 3 4)) => '(1 3 4 5))
 (test (create-sorted-set '(1)) => '(1))

;; function: create-sorted-set
;; description: takes an input set l which can contain duplicates elements and/or unsorted by operator <
;; output: return new set without any duplicates and sorted by opeartor <
;; algo: create new set without duplicates, and then sort it by operator < with racket sort function
;; later: understood that i can make it with appending two lists together, and then create-sorted-set from it, but
;; i left the original implementation, the complexity is the same which is O(n+m) for each of the algorithms
;; process: this is regular logic, A cup B is A with every element in B which is not in A, there no duplications this way

  (: set-union : SET SET -> SET)
  (define (set-union A B)
   (: a-not-contains : Number -> Boolean)
   (define (a-not-contains ele)
     (not (ismember? ele A)))
    (let ([s1 (create-sorted-set A)]
          [s2 (create-sorted-set B)])         
          (sort (append s1 (filter a-not-contains s2)) <)
      ))

#| ============================= Tests ============================= |#
 (test (set-union '(3 4 5) '(5 4 5)) => '(3 4 5)) ;; added tests
 (test (set-union '(3 2 -2 5 6) '(-1)) => '(-2 -1 2 3 5 6))
 (test (set-union '(3 4 5 1 3 4) '(3 4 5 1 3 4)) => '(1 3 4 5))
 (test (set-union '(1) '()) => '(1))

;; function: set-intersection
;; description: takes as an input two sets, set A and set B
;; output: return new set with is the intersection between set A and set B
;; algo: filter all elements in B which contains in A
;; process: this is regular logic

  (: set-intersection : SET SET -> SET)
  (define (set-intersection A B)
    (: mem-filter : Number -> Boolean)
    (define (mem-filter n)
      (ismember? n A))
    (filter mem-filter (create-sorted-set B)))

#| ============================= Tests ============================= |#  
 (test (set-intersection '(3 4 5) '(5 4 5)) => '(4 5)) ;; added tests
 (test (set-intersection '(3 2 -2 5 6) '(-2)) => '(-2))
 (test (set-intersection '(3 4 5 1 3 4) '(3 4 2 5 1 3 4)) => '(1 3 4 5))
 (test (set-intersection '(1) '()) => '())

;; ---------------------------------------------------------
;; Parser
;; function: parse-sexpr
;; description: takes as an input a Sexpr sexpr
;; output: return an AST SOL data-type element
;; process: only the (list (number:ns) ...) was little bit tricky, cause i didnt use it before then, all other is from
;; lectures, also note that the 'with->call transformation is pretty easy, just took it from my summary from and changed it to get 2 paramaters:
;; ... [(With binding-id named-exp bound-body) (Call (Fun binding-id (parse-sexpr bound-body)) (parse-sexpr named-exp))] ...

  (: parse-sexpr : Sexpr -> SOL)
  ;; to convert s-expressions into SOLs
  (define (parse-sexpr sexpr) 
    (match sexpr
      [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] ;;; there is no With constructor replace with existing constructors
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (eq? name1 name2)
              (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
      [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
      [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; function: parse
;; description: takes as an input a racket Sring
;; output: return an AST SOL data-type element
;; logic: wrapper for parse-sexpr function
;; process: just regular logic, nothing to add here

  (: parse : String -> SOL)
  ;; parses a string containing a SOL expression to a SOL AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

#| ============================= Tests ============================= |#  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{1 2 3  4 1 4 '4' 2 3 4 1 2 3}") =error> "bad syntax")
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} D}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) D)") ;; functions require two formal parameters
  

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------

Evaluation rules:

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env) (extend (x1, eval(E1,env),f-env))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env1)
             = eval(Ef,extend(x2,eval(E2,env) (extend (x1, eval(E1,env),env))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise

|#

;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; function: SetV->set
;; description: takes as an input VAL
;; output: return an Set if input from SetV form and returns error otherwise.

  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))

;; function: smult-set
;; description: takes as an number n and VAL s
;; output: return a new VAL which is SetV( n * (each element of s) )
;; process: just regular logic here, really no need to explain...

  (: smult-set : Number VAL -> VAL)
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))
    (SetV (map mult-op (SetV->set s))))

;; function: set-op
;; description: takes an op, and two vals
;; output: return new val which is op(val1,val2)
;; process: just regular logic here, really no need to explain...

 (: set-op : (SET SET -> SET) VAL VAL -> VAL)
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))

;;---------  the eval procedure ------------------------------
;; function: eval
;; description: takes as an input a AST SOL expr and env
;; output: return a VAL value with is the symantic evaluation of the syntatice xpr
;; process: its pretty straigh forward, just the dynamic is the env and statis is f-env, all else is the same like
;; in the ENV model from the class
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV (create-sorted-set S))]
      [(Smult n set) (smult-set n (eval set env))]
      [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
      [(Union l r) (set-op set-union (eval l env) (eval r env))]
      [(Id name) (lookup name env)]
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)]
      [(CallS fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
            (eval bound-body
                  (Extend bound-id2 (eval arg-expr2 env) 
                     (Extend bound-id1 (eval arg-expr1 env)
                        f-env)))] ;; static                  
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      [(CallD fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
            (eval bound-body
                  (Extend bound-id2 (eval arg-expr2 env) 
                     (Extend bound-id1 (eval arg-expr1 env)
                        env)))] ;; dynamic   
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))

  ;; process: i took (2) before (1) in this question, the notes are written in the end of the file how i get this output, just took
  ;; the pair from ENV model and tried to compile it via run, so i will get the value for the key to put in the starting env.
  ;; this took me at least 15 minutes to get it right.

  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                                    (EmptyEnv)))))

  (: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str) (createGlobalEnv))])
       (cases result
         [(SetV S) S]
         [else result])))

#| ============================= Tests ============================= |#
;; basics 
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{intersect {1 2 3} {}}") => '())
(test (run "{union {1 2 3} {}}") => '(1 2 3))
(test (run "{scalar-mult 3 { 1 2 3 }}") => '(3 6 9))
(test (run "{scalar-mult 3 { }}") => '())
(test (run "{union {1 2 3} {1 2 3}}") => '(1 2 3))
(test (run "{union {1 2 3} {1 2 3}}") => '(1 2 3))
(test (run "x") =error> "lookup")

;; pairs
(test (run "{call-static first {call-static cons {1 2 3} {4 5 6}} {}}") => '(1 2 3))
(test (run "{call-static second {call-static cons {1 2 3} {4 5 6}} {}}") => '(4 5 6))

;;fun
(test (run "{fun {x y} x}") => (FunV
 'x
 'y
 (Id 'x)
 (Extend
  'second
  (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
  (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv)) (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) (EmptyEnv))))))
 
;; call
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))

(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))


(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
 
(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9)) 
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
 {with {foo {fun {x y} {intersect x y}}}
 {call-static p foo {}}}}")
 => '(2 3))

;; remove blacks:
(test (SetV->set (FunV 'x 'y (Id 'x) (EmptyEnv))) =error> "SetV->set: expects a set")
(test (parse "{with {please give me 110} thanks!}") =error> "parse-sexpr: bad `with' syntax in (with (please give me 110) thanks!")
(test (eval (CallD (Set '(110)) (Set '()) (Set '())) (EmptyEnv)) =error> "`call-dynamic' expects a function, got: #(struct:SetV (110))")

#| ============================= Question6 - Zvi Mints =============================
 Note: I provided short answers to get straight to the point, hope this is fine :)

(1)  Yes, (Just one note about tail-recursion in drRacket doc, not really needed...)
     Started at 07:15 and Finished Compiling in 10:08 (Writing this in this time) - Without Writing Notes.
     Used also my summary that i wrote for the course:
     https://github.com/ZviMints/Summaries/blob/master/%D7%A9%D7%A0%D7%94%20%D7%92/%20%D7%A1%D7%99%D7%9B%D7%95%D7%9D%20%D7%A9%D7%A4%D7%95%D7%AA%20%D7%AA%D7%9B%D7%A0%D7%95%D7%AA.pdf

(2) ENV with constructors: {EmptyEnv, Extend},
    VAL with constructors {SetV,FunV},
    SET,
    SOL with constuctors: {Set, Smult, Inter, Union, Id, Fun, CallS, CallD} - for the AST.
    (*) the exposed final data-types is union of VAL and SetV, or for more specific SetV or FunV

(3) First of all, note that there a built-in test for parsing 'with with CallS, so there no much choices left the the coder
    On the other hand, with is syntax sugar for callS/lambda by definition, so in this form, i used CallS, same way that (let ([x y]) z) is equiv
    to (lambda (x z) y) with static model, all variables recieve their values on definition and not on run time.

    Regarding the overcome of two arguments for the function:
    To solve that, I assigned double name, since, if i gave another name instead of double name, there was a problem,
    for example for the symbol 'space
    the following code will compile and output '(1 2 3), but should fail:
       (run "{with {S { 1 2 3 }}
          {intersect S space}}")
    
Defined a spare paramater, used call-static because todo

(4)
   tail-recursion:
     (*) ismember? - there answer in the moment that the recursion gets to the base step, its not holding the answers and calculate the result in the way "back".
   recursion:
     (*) remove-duplicates
     (*) set-union
     (*) set-intersection
     (*) create-sorted-set
   Pros for tail-recursion:
     (*) Avoiding Stack-Overflow (can run out of memory if a computation involves too much context)
     (*) Better space complexity - it is sometimes important to make sure that tail recursion is used to avoid O(n) space consumption when the computation is easily performed in constant space. (drRacket Doc)

(5) In the GlobalEnv, we could write the following and it will work fine: (I used CallS)

  [V Success]:
  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                                    (EmptyEnv)))))

  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallD (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                                    (EmptyEnv)))))

  [X Fail]:
  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second (FunV 'p 'spare-param (CallD (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                                    (EmptyEnv)))))
  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second (FunV 'p 'spare-param (CallD (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (CallD (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                                    (EmptyEnv)))))
  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second (FunV 'p 'spare-param (CallD (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (CallD (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                                    (EmptyEnv)))))

    In other words, the call for cons should be dynamic / static but the calls for first/second should be static only! and that because if its was dynamic, it was
    running with the global env and will throw exception for no binding for 'f or 's, because the clouse will be over when the first/second will be called.
    Its doesn't matter if the cons is call-dynamic or call-static because its will be called on first or second which is in the same clousure that they override.

(6)
    if we will change the call-static to call-dynamic in cons, then everything will be fine (for same reason as above),
    but if we will change the call-static p foo to call-dynamic, then there will
    be an error for same reason mentioned in (5), its out of closure and use the global env.
    This will yield an error:

    (test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
    {with {foo {fun {x y} {intersect x y}}}
    {call-dynamic p foo {}}}}")
    => '(2 3))

    because of lookup: no binding for s, because its looks first on the second argument and not the first, that why there no error in 'f but in 's.

|#
#| ================================= External Notes =================================
This is the way of thinking about the pair, this is how i got the pair value for Global Env:
cons input: 
(run "{with
         {
         cons
         {fun {f s}
          {fun {selector please-110} {call-static selector f s}}
         }
         }
         cons}")

cons result:
(FunV 'f 's (Fun 'selector 'please-110 (CallS (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv))
---------------------------------------------------------------------------------

first input:
(run "{with
         {first
         {fun {p spare-param}
           {call-static p {fun {a b} a} {}}
          }
          }
 first}")

first result:
(FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))

---------------------------------------------------------------------------------

second input:
(run "{with
         {second
         {fun {p spare-param}
           {call-static p {fun {a b} b} {}}
          }
          }
 second}")

second result:
(FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
---------------------------------------------------------------------------------
|#



