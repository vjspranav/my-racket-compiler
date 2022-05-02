#lang racket
(require "../utilities.rkt")

(provide explicate-control)

; Using or till all cases are handled
(define (explicate_tail e)
  (match e
    [(or (Var _) (Int _) (Prim _ _)) (Return e)]
    [(Let x rhs body) (explicate_assign rhs x (explicate_tail body))]
    [else (error "explicate_tail unhandled case" e)]))

(define (explicate_assign e x cont)
  (match e
    [(or (Var _) (Int _) (Prim _ _)) (Seq (Assign (Var x) e) cont)]
    [(Let y rhs body) (explicate_assign rhs y (explicate_assign body x cont))]
    [else (error "explicate_assign unhandled case" e)]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info e) (CProgram info `((start . ,(explicate_tail e))))]))
