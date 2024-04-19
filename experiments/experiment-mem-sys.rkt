;;; written by Sun O'Brien [suncs (at) utexas (.) edu] -- Apr 19 2024 

#lang rosette/safe

(require rosette/lib/destruct rackunit)

; define register names
(define x0 0)
(define x1 1)
(define x2 2)
(define x3 3)
(define x4 4)
(define x5 5)
(define x6 6)
(define x7 7)

; define instructions
(struct ADD (src1 src2 dest) #:transparent)  ; x[src1] + x[src2] -> x[dest]
(struct MUL (src1 src2 dest) #:transparent)  ; x[src1] * x[src2] -> x[dest]
(struct LDI (src1 dest) #:transparent)       ; MEM[x[src1]] -> x[dest] 
(struct STI (src1 dest) #:transparent)       ; x[dest] -> MEM[x[src1]] 
(struct MOV (src1 dest) #:transparent)       ; x[src1] -> x[dest]

; define function to set machine state
(define (set-state state loc val)
  (lambda (r) (if (equal? loc r) val (state r))))

; define function to create initial state
(define initial-state (set-state (lambda (r) 0) 0 (bv 0 16)))

; define how instructions are executed
(define (eval-instr i state)
  (destruct i
    [ (ADD src1 src2 dest)  (set-state state dest (bvadd (state src1) (state src2))) ]
    [ (MUL src1 src2 dest)  (set-state state dest (bvmul (state src1) (state src2))) ]
    [      (LDI src1 dest)  (set-state state dest (state (state src1)))              ]
    [      (STI src1 dest)  (set-state state (state dest) (state src1))              ]
    [      (MOV src1 dest)  (set-state state dest (state src1))                      ]
  )
)

; define our starting state for tests run below
(define example-state 
        ; for 8 registers
        (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 
        ; for first 64b of memory
        (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 


        initial-state 

        x0 (bv 0 16)) 
        x1 (bv 5 16)) 
        x2 (bv 7 16))
        x3 (bv 1000 16))
        x4 (bv 0 16))
        x5 (bv 1008 16))
        x6 (bv 0 16)) 
        x7 (bv 0 16)) 

        (bv 1000 16) (bv 32 16)) 
        (bv 1008 16) (bv 0 16)) 
        (bv 1016 16) (bv 0 16)) 
        (bv 1024 16) (bv 0 16)) 
        (bv 1032 16) (bv 0 16)) 
        (bv 1040 16) (bv 0 16)) 
        (bv 1048 16) (bv 0 16)) 
        (bv 1056 16) (bv 0 16)) 
)

; starting state:
; - registers
;     x[0] = 0
;     x[1] = 5
;     x[2] = 7
;     x[3] = 1000
;     x[4] = 0
;     x[5] = 1008
;     x[6] = 0
;     x[7] = 0
; - memory
;     MEM[1000] = 32
;     MEM[1008] = 0
;     MEM[1016] = 0
;     MEM[1024] = 0
;     MEM[1032] = 0
;     MEM[1040] = 0
;     MEM[1048] = 0
;     MEM[1056] = 0

; test: add x[1] and x[2] and put the result in x[0]
(let ([final-state (eval-instr (ADD 1 2 0) example-state)])
    (check-equal? (final-state x0) (bv 12 16)) ; altered from starting state
    (check-equal? (final-state x1) (bv 5 16))
    (check-equal? (final-state x2) (bv 7 16))
    (check-equal? (final-state x3) (bv 1000 16))
    (check-equal? (final-state x4) (bv 0 16))
    (check-equal? (final-state x5) (bv 1008 16))
    (check-equal? (final-state x6) (bv 0 16))
    (check-equal? (final-state x7) (bv 0 16))
    (check-equal? (final-state (bv 1000 16)) (bv 32 16))
    (check-equal? (final-state (bv 1008 16)) (bv 0 16))
    (check-equal? (final-state (bv 1016 16)) (bv 0 16))
    (check-equal? (final-state (bv 1024 16)) (bv 0 16))
    (check-equal? (final-state (bv 1032 16)) (bv 0 16))
    (check-equal? (final-state (bv 1040 16)) (bv 0 16))
    (check-equal? (final-state (bv 1048 16)) (bv 0 16))
    (check-equal? (final-state (bv 1056 16)) (bv 0 16))
)

; test: multiply x[1] and x[2] and put the result in x[0]
(let ([final-state (eval-instr (MUL 1 2 0) example-state)])
    (check-equal? (final-state x0) (bv 35 16)) ; altered from starting state
    (check-equal? (final-state x1) (bv 5 16))
    (check-equal? (final-state x2) (bv 7 16))
    (check-equal? (final-state x3) (bv 1000 16))
    (check-equal? (final-state x4) (bv 0 16))
    (check-equal? (final-state x5) (bv 1008 16))
    (check-equal? (final-state x6) (bv 0 16))
    (check-equal? (final-state x7) (bv 0 16))
    (check-equal? (final-state (bv 1000 16)) (bv 32 16))
    (check-equal? (final-state (bv 1008 16)) (bv 0 16))
    (check-equal? (final-state (bv 1016 16)) (bv 0 16))
    (check-equal? (final-state (bv 1024 16)) (bv 0 16))
    (check-equal? (final-state (bv 1032 16)) (bv 0 16))
    (check-equal? (final-state (bv 1040 16)) (bv 0 16))
    (check-equal? (final-state (bv 1048 16)) (bv 0 16))
    (check-equal? (final-state (bv 1056 16)) (bv 0 16))
)

; test: load MEM[x[3]] (addr 1000) into register x[4]
(let ([final-state (eval-instr (LDI 3 4) example-state)])
    (check-equal? (final-state x0) (bv 0 16))
    (check-equal? (final-state x1) (bv 5 16))
    (check-equal? (final-state x2) (bv 7 16))
    (check-equal? (final-state x3) (bv 1000 16))
    (check-equal? (final-state x4) (bv 32 16)) ; altered from starting state
    (check-equal? (final-state x5) (bv 1008 16))
    (check-equal? (final-state x6) (bv 0 16))
    (check-equal? (final-state x7) (bv 0 16))
    (check-equal? (final-state (bv 1000 16)) (bv 32 16))
    (check-equal? (final-state (bv 1008 16)) (bv 0 16))
    (check-equal? (final-state (bv 1016 16)) (bv 0 16))
    (check-equal? (final-state (bv 1024 16)) (bv 0 16))
    (check-equal? (final-state (bv 1032 16)) (bv 0 16))
    (check-equal? (final-state (bv 1040 16)) (bv 0 16))
    (check-equal? (final-state (bv 1048 16)) (bv 0 16))
    (check-equal? (final-state (bv 1056 16)) (bv 0 16))
)

; test: store x[2] into MEM[x[5]] (addr 1008)
(let ([final-state (eval-instr (STI 2 5) example-state)])
    (check-equal? (final-state x0) (bv 0 16))
    (check-equal? (final-state x1) (bv 5 16))
    (check-equal? (final-state x2) (bv 7 16))
    (check-equal? (final-state x3) (bv 1000 16))
    (check-equal? (final-state x4) (bv 0 16))
    (check-equal? (final-state x5) (bv 1008 16))
    (check-equal? (final-state x6) (bv 0 16))
    (check-equal? (final-state x7) (bv 0 16))
    (check-equal? (final-state (bv 1000 16)) (bv 32 16))
    (check-equal? (final-state (bv 1008 16)) (bv 7 16)) ; altered from starting state
    (check-equal? (final-state (bv 1016 16)) (bv 0 16))
    (check-equal? (final-state (bv 1024 16)) (bv 0 16))
    (check-equal? (final-state (bv 1032 16)) (bv 0 16))
    (check-equal? (final-state (bv 1040 16)) (bv 0 16))
    (check-equal? (final-state (bv 1048 16)) (bv 0 16))
    (check-equal? (final-state (bv 1056 16)) (bv 0 16))
)

; test: move the value in x[1] to x[0]
(let ([final-state (eval-instr (MOV 1 0) example-state)])
    (check-equal? (final-state x0) (bv 5 16)) ; altered from starting state
    (check-equal? (final-state x1) (bv 5 16))
    (check-equal? (final-state x2) (bv 7 16))
    (check-equal? (final-state x3) (bv 1000 16))
    (check-equal? (final-state x4) (bv 0 16))
    (check-equal? (final-state x5) (bv 1008 16))
    (check-equal? (final-state x6) (bv 0 16))
    (check-equal? (final-state x7) (bv 0 16))
    (check-equal? (final-state (bv 1000 16)) (bv 32 16))
    (check-equal? (final-state (bv 1008 16)) (bv 0 16))
    (check-equal? (final-state (bv 1016 16)) (bv 0 16))
    (check-equal? (final-state (bv 1024 16)) (bv 0 16))
    (check-equal? (final-state (bv 1032 16)) (bv 0 16))
    (check-equal? (final-state (bv 1040 16)) (bv 0 16))
    (check-equal? (final-state (bv 1048 16)) (bv 0 16))
    (check-equal? (final-state (bv 1056 16)) (bv 0 16))
)



