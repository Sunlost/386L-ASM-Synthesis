#lang rosette/safe

(require rackunit)
(require "../common.rkt" "../../src/isa/lc3b.rkt" "../../src/state.rkt" "../../src/util.rkt")

; ----------------------------------------------------------------------------------------------- ;
; -------------------------------------- LC-3B -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 1 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define lc3b-test-1
    (set-state (set-state (set-state (set-state (set-state (set-state

    test-state-1 ; see tests/common.rkt

    ; LC-3b program
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

(let ([final-state (eval-lc3b-prog-state lc3b-test-1)])
    (check-equal?          (final-state x0) (val 12))     ; altered from starting state
    (check-equal?          (final-state x1) (val 5))
    (check-equal?          (final-state x2) (val 5))      ; altered from starting state
    (check-equal?          (final-state x3) (addr 1000))
    (check-equal?          (final-state x4) (val 0))      ; should NOT be changed from starting state
    (check-equal?          (final-state x5) (addr 1008))
    (check-equal?          (final-state x6) (val 32))     ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
    (check-equal?          (final-state PC) (addr 2020))  ; altered from starting state
    (check-equal? (final-state (addr 1000)) (mem_val 32))
    (check-equal? (final-state (addr 1008)) (mem_val 0))
    (check-equal? (final-state (addr 1016)) (mem_val 0))
    (check-equal? (final-state (addr 1024)) (mem_val 0))
    (check-equal? (final-state (addr 1032)) (mem_val 0))
    (check-equal? (final-state (addr 1040)) (mem_val 0))
    (check-equal? (final-state (addr 1048)) (mem_val 0))
    (check-equal? (final-state (addr 1056)) (mem_val 0))
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 2 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define lc3b-test-2 
  (set-state (set-state (set-state (set-state (set-state (set-state

  test-state-1

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

(let ([final-state (eval-lc3b-prog-state lc3b-test-2)])
    (check-equal?          (final-state x0) (val 12))     ; altered from starting state
    (check-equal?          (final-state x1) (val 5))
    (check-equal?          (final-state x2) (val 5))      ; altered from starting state
    (check-equal?          (final-state x3) (addr 1000))
     ; v  x4 should NOT be changed from starting state  v
    (check-equal?          (final-state x4) (lc3b-test-2 x4))    
    (check-equal?          (final-state x5) (addr 1008))
    (check-equal?          (final-state x6) (val 0))     
     ; v  x7 should NOT be changed from starting state  v
    (check-equal?          (final-state x7) (lc3b-test-2 x7))
    (check-equal?          (final-state PC) (addr 2020))  ; altered from starting state
    (check-equal?  (final-state COND_CODES) 3_P_True)     ; altered from starting state
    (check-equal? (final-state (addr 1000)) (mem_val 32))
    (check-equal? (final-state (addr 1008)) (mem_val 0))
    (check-equal? (final-state (addr 1016)) (mem_val 0))
    (check-equal? (final-state (addr 1024)) (mem_val 0))
    (check-equal? (final-state (addr 1032)) (mem_val 0))
    (check-equal? (final-state (addr 1040)) (mem_val 0))
    (check-equal? (final-state (addr 1048)) (mem_val 0))
    (check-equal? (final-state (addr 1056)) (mem_val 0))
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 3 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define lc3b-test-3
    (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state

    test-state-1 ; see tests/common.rkt

    ; Program 3 (LC-3B)
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

(let ([final-state (eval-lc3b-prog-state  lc3b-test-3)])
    (check-equal?          (final-state x0) (val 4))      ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
    (check-equal?  (final-state COND_CODES) 3_P_True)   ; altered from starting state
)

) ; /module+ test
