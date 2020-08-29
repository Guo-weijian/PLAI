#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))])
  [else
   (error 'num* "one argument was not a number")])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (boby : ExprC)])

(define (interp [e : ExprC] [env : Env])]) : Value
  (type-case ExprC e
             [numC (n) (numV n)]
             [plusC (l r) (num+ (interp l env) (interp r env))]
             [multC (l r) (num* (interp l env) (interp r env))]
             [appC (fun arg) (local ([define fd (get-fundef fun fds)])
                               (interp (fdC-body fd)
                                       (extend-env (bind (fdC-arg fd)
                                                         (interp arg env fds))
                                                   mt-env)
                                       fds))]
             [idC (n) (lookup n env)]
             ))

(define (subst [e : ExprC] [var : symbol] [val : ExprC]) : ExprC
  (type-case ExprC e
             [numC (n) e]
             [idC (s) (cond
                        [(symbol=? s var) val]
                        [else e])]
             [plusC (l r) (plusC (subst l var val)
                                 (subst r var val))]
             [multC (l r) (multC (subst l var val)
                                 (subst r var val))]
             [appC (fun arg) (appC fun (subst arg var val))]))

(define (get-fundef [name : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? name (fdC-name (first fds))) (first fds)]
                   [else (get-fundef name (rest fds))])]))

(define-type Binding [bind (name : symbol) (val : number)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [name : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "reference to undefined identifier")]
    [(cons? env) (cond
                   [(symbol=? name (bind-name (first env)))
                    (bind-val (first env))]
                   [else (lookup name (rest env))])]))

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'double (idC 'x)))
              (extend-env (bind 'x 3) mt-env)
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
