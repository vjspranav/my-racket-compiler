#lang racket
(require "../utilities.rkt")

(provide select-instructions)

(define (select-instructions-atm e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Imm n)]))

(define (select-instructions-stmt e)
  (match e
    [(Assign x e1)
     (match e1
       [(or (Var _) (Int _)) (list (Instr 'movq (list (select-instructions-atm e1) x)))]
       [(Prim 'read '()) (list (Callq 'read_int 0) (Instr 'movq (list (Reg 'rax) x)))]
       [(Prim '- (list e1))
        (list (Instr 'movq (list (select-instructions-atm e1) x)) (Instr 'negq (list x)))]
       [(Prim op (list a b))
        (if (equal? x a)
            (list (Instr (get-instr op)
                         (list (select-instructions-atm b) (select-instructions-atm a))))
            (if (equal? x b)
                (list (Instr (get-instr op)
                             (list (select-instructions-atm a) (select-instructions-atm b))))
                (list (Instr 'movq (list (select-instructions-atm a) x))
                      (Instr (get-instr op) (list (select-instructions-atm b) x)))))])]))

(define (select-instructions-tail e)
  (match e
    [(Seq s1 s2) (append (select-instructions-stmt s1) (select-instructions-tail s2))]
    [(Return (Prim 'read '())) (list (Callq 'read_int 0) (Jmp 'conclusion))]
    [(Return e) (append (select-instructions-stmt (Assign (Reg 'rax) e)) (list (Jmp 'conclusion)))]))

(define (get-instr op)
  (match op
    ['+ 'addq]
    ['- 'subq]))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info blocks)
     (X86Program info
                 (for/list ([e blocks])
                   (match e
                     [(cons key value) (cons key (Block '() (select-instructions-tail value)))])))]))
