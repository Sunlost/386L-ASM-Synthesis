#lang rosette/safe

(require rackunit)
(require "../common.rkt" "../../src/isa/riscv.rkt" "../../src/state.rkt")

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- RISC-V -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 1 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;



; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 2 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;



; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 3 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define riscv-test-3
    (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state
    (set-state

    test-state-1 ; see tests/common.rkt

    ; Program 3 (RISC-V)
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

(let ([final-state (eval-riscv-prog-state riscv-test-3)])
    (check-equal?          (final-state x0) (val 13))      ; altered from starting state
    (check-equal?          (final-state x1) (val 12))      ; altered from starting state
    (check-equal?          (final-state PC) (addr 2024))   ; altered from starting state
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 4 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define riscv-test-1
    (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state

    test-state-1 ; see tests/common.rkt

    ; Program 4 (RISC-V)
    (addr 2000) (5_XOR x0 x0 x0))
    (addr 2004) (5_ADDI x0 (imm5 1) x0))
    (addr 2008) (5_ADDI x0 (imm5 1) x0))
    (addr 2012) (5_JAL (addr 2024) x7))
    (addr 2016) (5_ADDI x0 (imm5 1) x0))
    (addr 2020) HLT)

    (addr 2024) (5_ADDI x0 (imm5 1) x0))
    (addr 2028) (5_JALR x7 (imm12 0) x1)) ; return
)

(let ([final-state (eval-riscv-prog-state riscv-test-1)])
    (check-equal?          (final-state x0) (val 4))      ; altered from starting state
    (check-equal?          (final-state x1) (addr 2032))  ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
)

) ; /module+ test
