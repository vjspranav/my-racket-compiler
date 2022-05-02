#lang racket
(require "../utilities.rkt")

(provide uniquify)

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Let x e body)
       (define unique-x (gensym x))
       (Let unique-x ((uniquify-exp env) e) ((uniquify-exp (dict-set env x unique-x)) body))]
      [(Prim op es)
       (Prim op
             (for/list ([e es])
               ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))
