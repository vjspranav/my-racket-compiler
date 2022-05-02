#lang racket
(require "../utilities.rkt")
(provide remove-complex-opera*)

(define (rco-atm e)
  (match e
    [(Int n) (values (Int n) '())]
    [(Var x) (values (Var x) '())]
    [(Let x e body)
     (define-values (atm-body atm-body-env) (rco-atm body))
     ; Explicitly convert new cons into list and appned ad follows
     ; (append `((,x . ,(rco-exp e))) atm-body-env)
     ; But it's single mapping hence cons
     (values atm-body (cons `(,x . ,(rco-exp e)) atm-body-env))]
    [(Prim op es)
     (define tmp (gensym 'tmp))
     (define-values (atm-e atm-e-env) (for/lists (l1 l2) ([e es]) (rco-atm e)))
     (values (Var tmp) (append (append* atm-e-env) `((,tmp . ,(Prim op atm-e)))))]))

(define (rco-exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    [(Prim op es)
     (define-values (atm-e atm-e-env) (for/lists (l1 l2) ([e es]) (rco-atm e)))
     (make-lets (append* atm-e-env) (Prim op atm-e))]))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))
