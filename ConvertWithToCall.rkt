#lang pl

  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG])


(: SynSugarWith : FLANG -> FLANG)
(define (SynSugarWith exp)
  (cases exp
    [(With binding-id named-exp bound-body) (Call (Fun binding-id (SynSugarWith bound-body)) (SynSugarWith named-exp))]
    [(Num  n) exp]
    [(Add  l r) (Add (SynSugarWith l) (SynSugarWith r))]
    [(Sub  l r) (Sub (SynSugarWith l) (SynSugarWith r))]
    [(Mul  l r) (Mul (SynSugarWith l) (SynSugarWith r))]
    [(Div l r) (Div (SynSugarWith l) (SynSugarWith r))]
    [(Id name) exp]
    [(Fun x y) (Fun x (SynSugarWith y))]
    [(Call x y) (Call (SynSugarWith x) (SynSugarWith y))]
    )
  )

(test (SynSugarWith (With 'x (Add (Num 2) (Num 3)) (With 'y (Mul (Id 'x) (Id 'x)) (With 'z (Num 4) (Add (Id 'z) (Id 'y)))))) =>
      (Call (Fun 'x (Call (Fun 'y (Call (Fun 'z (Add (Id 'z) (Id 'y))) (Num 4))) (Mul (Id 'x) (Id 'x)))) (Add (Num 2) (Num 3))))