#lang rosette/safe

(require rackunit)
(require "../src/isa/lc3b.rkt" "../src/isa/riscv.rkt" "../src/state.rkt" "../src/util.rkt")

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test

; test lc3b implementation

; define our starting state for tests run below
(define example-state 
  ; for 8 registers
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 
  ; for PC
  (set-state 
  ; for first 64b of data memory
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 

  initial-state 

  x0 (val 0)) 
  x1 (val 5)) 
  x2 (val 7))
  x3 (addr 1000))
  x4 (val 0))
  x5 (addr 1008))
  x6 (val 0)) 
  x7 (val 14)) 

  PC (addr 2000)) ; instruction memory begins at addr 2000

  (addr 1000) (val 32)) 
  (addr 1008) (val 0)) 
  (addr 1016) (val 0)) 
  (addr 1024) (val 0)) 
  (addr 1032) (val 0)) 
  (addr 1040) (val 0)) 
  (addr 1048) (val 0)) 
  (addr 1056) (val 0)) 
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
;     x[7] = 14
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


; define state for test one
(define test-one-state 
  (set-state (set-state (set-state (set-state (set-state (set-state

  example-state

  (addr 2000) (3_ADD x1 x2 x0))
  (addr 2004) (3_AND x1 x2 x2))
  (addr 2008) (3_LDW x3 (imm6 0) x6))
  (addr 2012) (3_JSR (imm11 4)))
  (addr 2016) (3_LSHF x3 (imm4 1) x4))
  (addr 2020) HLT)
)

(check-equal?
    (bvadd (addr 2012) (L_SH_1 (SXT_16 (imm4 4))))  ;;; JSR on 2012 should skip LSHF on 2016
    (addr 2020)
)

(let ([final-state (eval-lc3b-prog test-one-state)])
    (check-equal?          (final-state x0) (val 12))     ; altered from starting state
    (check-equal?          (final-state x1) (val 5))
    (check-equal?          (final-state x2) (val 5))      ; altered from starting state
    (check-equal?          (final-state x3) (addr 1000))
    (check-equal?          (final-state x4) (val 0))      ; should NOT be changed from starting state
    (check-equal?          (final-state x5) (addr 1008))
    (check-equal?          (final-state x6) (val 32))     ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
    (check-equal?          (final-state PC) (addr 2020))  ; altered from starting state
    (check-equal? (final-state (addr 1000)) (val 32))
    (check-equal? (final-state (addr 1008)) (val 0))
    (check-equal? (final-state (addr 1016)) (val 0))
    (check-equal? (final-state (addr 1024)) (val 0))
    (check-equal? (final-state (addr 1032)) (val 0))
    (check-equal? (final-state (addr 1040)) (val 0))
    (check-equal? (final-state (addr 1048)) (val 0))
    (check-equal? (final-state (addr 1056)) (val 0))
)


; define state for test two
(define test-two-state 
  (set-state (set-state (set-state (set-state (set-state (set-state
  (set-state

  example-state

  COND_CODES 3_Z_True)

  (addr 2000) (3_ADD x1 x2 x0))
  (addr 2004) (3_AND x1 x2 x2))
  (addr 2008) (3_BR_P (imm11 6)))
  (addr 2012) (3_JSR (imm11 4)))       ; skipped
  (addr 2016) (3_LSHF x3 (imm4 1) x4)) ; skipped
  (addr 2020) HLT)
)

(check-equal?
    (bvadd (addr 2012) (L_SH_1 (SXT_16 (imm4 4))))  ;;; JSR on 2012 should skip LSHF on 2016
    (addr 2020)
)

(let ([final-state (eval-lc3b-prog test-two-state)])
    (check-equal?          (final-state x0) (val 12))     ; altered from starting state
    (check-equal?          (final-state x1) (val 5))
    (check-equal?          (final-state x2) (val 5))      ; altered from starting state
    (check-equal?          (final-state x3) (addr 1000))
     ; v  x4 should NOT be changed from starting state  v
    (check-equal?          (final-state x4) (test-two-state x4))    
    (check-equal?          (final-state x5) (addr 1008))
    (check-equal?          (final-state x6) (val 0))     
     ; v  x7 should NOT be changed from starting state  v
    (check-equal?          (final-state x7) (test-two-state x7))
    (check-equal?          (final-state PC) (addr 2020))  ; altered from starting state
    (check-equal?  (final-state COND_CODES) 3_P_True)     ; altered from starting state
    (check-equal? (final-state (addr 1000)) (val 32))
    (check-equal? (final-state (addr 1008)) (val 0))
    (check-equal? (final-state (addr 1016)) (val 0))
    (check-equal? (final-state (addr 1024)) (val 0))
    (check-equal? (final-state (addr 1032)) (val 0))
    (check-equal? (final-state (addr 1040)) (val 0))
    (check-equal? (final-state (addr 1048)) (val 0))
    (check-equal? (final-state (addr 1056)) (val 0))
)


; define state for test three
(define test-three-state 
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state

  example-state

  (addr 2000) (3_XOR x0 x0 x0))
  (addr 2004) (3_ADDI x0 (imm5 1) x0))
  (addr 2008) (3_ADDI x0 (imm5 1) x0))
  (addr 2012) (3_JSR (imm11 6)))      
  (addr 2016) (3_ADDI x0 (imm5 1) x0))
  (addr 2020) HLT)

  (addr 2024) (3_ADDI x0 (imm5 1) x0))
  (addr 2028) (3_RET 0))
)

(check-equal?
    (bvadd (addr 2012) (L_SH_1 (SXT_16 (imm4 6))))  ;;; JSR on 2012 should jump to 2024
    (addr 2024)
)

(let ([final-state (eval-lc3b-prog test-three-state)])
    (check-equal?          (final-state x0) (val 4))      ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
    (check-equal?  (final-state COND_CODES) 3_P_True)   ; altered from starting state
)


; test risc-v implementation


;; basically same as lc3b test three
(define risc-test-one-state
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state

  example-state

  (addr 2000) (5_XOR x0 x0 x0))
  (addr 2004) (5_ADDI x0 (imm5 1) x0))
  (addr 2008) (5_ADDI x0 (imm5 1) x0))
  (addr 2012) (5_JAL (addr 2024) x7))    
  (addr 2016) (5_ADDI x0 (imm5 1) x0))
  (addr 2020) HLT)

  (addr 2024) (5_ADDI x0 (imm5 1) x0))
  (addr 2028) (5_JALR x7 (imm12 0) x1)) ; return
)

(let ([final-state (eval-riscv-prog risc-test-one-state)])
    (check-equal?          (final-state x0) (val 4))      ; altered from starting state
    (check-equal?          (final-state x1) (addr 2032))  ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
)



;; basically same as lc3b test three
(define risc-test-two-state
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state
  (set-state

  example-state

  (addr 2000) (5_XOR x0 x0 x0))
  (addr 2004) (5_XOR x1 x1 x1))
  (addr 2008) (5_ADDI x0 (imm5 12) x0))
  (addr 2012) (5_ADDI x1 (imm5 15) x1))
  ; L1.
  (addr 2016) (5_BGE x1 x0 (imm12 12))) ; BGEU x1 x0 .L2
  (addr 2020) (5_ADDI x0 (imm5 1) x0))
  (addr 2024) HLT)
  ; L2.
  (addr 2028) (5_AND x0 x1 x1))
  (addr 2032) (5_BEQ x0 x1 (imm12 -12)))  ; BEQ  x0 x1 .L1
)

(let ([final-state (eval-riscv-prog risc-test-two-state)])
    (check-equal?          (final-state x0) (val 13))      ; altered from starting state
    (check-equal?          (final-state x1) (val 12))      ; altered from starting state
    (check-equal?          (final-state PC) (addr 2024))   ; altered from starting state
)

) ; /module+ test