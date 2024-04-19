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

; define PC register name
(define PC 8)

; define instructions
(struct ADD (src1 src2 dest) #:transparent)  ; x[src1] + x[src2] -> x[dest]
(struct MUL (src1 src2 dest) #:transparent)  ; x[src1] * x[src2] -> x[dest]
(struct LDI (src1 dest) #:transparent)       ; MEM[x[src1]] -> x[dest] 
(struct STI (src1 dest) #:transparent)       ; x[dest] -> MEM[x[src1]] 
(struct MOV (src1 dest) #:transparent)       ; x[src1] -> x[dest]
(define-symbolic HLT integer?)

; define function to set machine state
(define (set-state state loc val)
  (lambda (r) (if (equal? loc r) val (state r)))
)

; define function to create initial state
(define initial-state (set-state (lambda (r) 0) 0 (bv 0 16)))

; define how instructions are executed
(define (eval-instr i state)
  (set-state 
    (destruct i
      [ (ADD src1 src2 dest)  (set-state state dest (bvadd (state src1) (state src2))) ]
      [ (MUL src1 src2 dest)  (set-state state dest (bvmul (state src1) (state src2))) ]
      [      (LDI src1 dest)  (set-state state dest (state (state src1)))              ]
      [      (STI src1 dest)  (set-state state (state dest) (state src1))              ]
      [      (MOV src1 dest)  (set-state state dest (state src1))                      ]
    )
    PC (bvadd (state PC) (bv 4 16))
  )
)

; define how a program is evaluated
(define (eval-prog state)
  (if
    ; IF: the instr at MEM[PC] is HLT
    (equal? (state (state PC)) HLT)
    ; THEN: we halted, return current state
    state
    ; ELSE: we have an instr to run, recurse
    (eval-prog (eval-instr (state (state PC)) state))
  )
)

; define our starting state for tests run below
(define example-state 
  ; for 8 registers
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 
  ; for PC
  (set-state 
  ; for first 64b of data memory
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

  PC (bv 2000 16)) ; instruction memory begins at addr 2000

  (bv 1000 16) (bv 32 16)) 
  (bv 1008 16) (bv 0 16)) 
  (bv 1016 16) (bv 0 16)) 
  (bv 1024 16) (bv 0 16)) 
  (bv 1032 16) (bv 0 16)) 
  (bv 1040 16) (bv 0 16)) 
  (bv 1048 16) (bv 0 16)) 
  (bv 1056 16) (bv 0 16)) 
)

; base starting state:
; - PC = 2000
; - registers
;     x[0] = 0
;     x[1] = 5
;     x[2] = 7
;     x[3] = 1000
;     x[4] = 0
;     x[5] = 1008
;     x[6] = 0
;     x[7] = 0
; - data memory
;     MEM[1000] = 32
;     MEM[1008] = 0
;     MEM[1016] = 0
;     MEM[1024] = 0
;     MEM[1032] = 0
;     MEM[1040] = 0
;     MEM[1048] = 0
;     MEM[1056] = 0
; - instruction memory
;     [nothing]



; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; define state for our first test
(define test-one-state 
  (set-state (set-state

  example-state

  (bv 2000 16) (ADD x1 x2 x0))
  (bv 2004 16) HLT)
)

; base starting state:
; - PC = 2000
; - registers
;     x[0] = 0
;     x[1] = 5
;     x[2] = 7
;     x[3] = 1000
;     x[4] = 0
;     x[5] = 1008
;     x[6] = 0
;     x[7] = 0
; - data memory
;     MEM[1000] = 32
;     MEM[1008] = 0
;     MEM[1016] = 0
;     MEM[1024] = 0
;     MEM[1032] = 0
;     MEM[1040] = 0
;     MEM[1048] = 0
;     MEM[1056] = 0
; - instruction memory
;     MEM[2000] = ADD x1 x2 x0
;     MEM[2004] = HLT

; test: add x[1] and x[2] and put the result in x[0]
(let ([final-state (eval-prog test-one-state)])
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

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; define state for our first test
(define test-two-state 
  (set-state (set-state (set-state (set-state (set-state (set-state

  example-state

  (bv 2000 16) (ADD x1 x2 x0)) ; sets x0 to 12
  (bv 2004 16) (MUL x0 x2 x0)) ; sets x0 to 84
  (bv 2008 16) (LDI x3 x4))    ; sets x4 to 32
  (bv 2012 16) (STI x2 x5))    ; sets MEM[1008] to 7
  (bv 2016 16) (MOV x5 x6))    ; sets x6 to 1008
  (bv 2020 16) HLT)            ; HLT
)

; base starting state:
; - PC = 2000
; - registers
;     x[0] = 0
;     x[1] = 5
;     x[2] = 7
;     x[3] = 1000
;     x[4] = 0
;     x[5] = 1008
;     x[6] = 0
;     x[7] = 0
; - data memory
;     MEM[1000] = 32
;     MEM[1008] = 0
;     MEM[1016] = 0
;     MEM[1024] = 0
;     MEM[1032] = 0
;     MEM[1040] = 0
;     MEM[1048] = 0
;     MEM[1056] = 0
; - instruction memory
;     MEM[2000] = ADD x1 x2 x0
;     MEM[2004] = MUL x0 x2 x0
;     MEM[2008] = ADD x1 x2 x0
;     MEM[2012] = ADD x1 x2 x0
;     MEM[2016] = ADD x1 x2 x0
;     MEM[2004] = HLT

; test: add x[1] and x[2] and put the result in x[0]
(let ([final-state (eval-prog test-two-state)])
    (check-equal? (final-state x0) (bv 84 16)) ; altered from starting state
    (check-equal? (final-state x1) (bv 5 16))
    (check-equal? (final-state x2) (bv 7 16))
    (check-equal? (final-state x3) (bv 1000 16))
    (check-equal? (final-state x4) (bv 32 16))  ; altered from starting state
    (check-equal? (final-state x5) (bv 1008 16))
    (check-equal? (final-state x6) (bv 1008 16))  ; altered from starting state
    (check-equal? (final-state x7) (bv 0 16))
    (check-equal? (final-state (bv 1000 16)) (bv 32 16))
    (check-equal? (final-state (bv 1008 16)) (bv 7 16))  ; altered from starting state
    (check-equal? (final-state (bv 1016 16)) (bv 0 16))
    (check-equal? (final-state (bv 1024 16)) (bv 0 16))
    (check-equal? (final-state (bv 1032 16)) (bv 0 16))
    (check-equal? (final-state (bv 1040 16)) (bv 0 16))
    (check-equal? (final-state (bv 1048 16)) (bv 0 16))
    (check-equal? (final-state (bv 1056 16)) (bv 0 16))
)