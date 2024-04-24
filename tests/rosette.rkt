#lang rosette/safe

(require rackunit (only-in racket for/list))
(require "../src/isa/lc3b.rkt" "../src/isa/riscv.rkt" "../src/state.rkt" "../src/rosette.rkt")

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test

; TODO: Test LC-3b -> RISC-V translation using Rosette on a few dummy programs and inputs

(define program-one-lc3b
    (list
        (3_ADD     x1 x2 x0)
        (3_AND     x1 x2 x2)
        (3_BR_P    (imm11 6))
        (3_JSR     (imm11 4))
        (3_LSHF x3 (imm4  1) x4)
    )
)

; TODO: Fix this! It's enlarging forever.
(define program-one-riscv
    (rosette-compile "lc3b" "riscv" program-one-lc3b)
)

; Print the new program
(for/list ([instr program-one-riscv])
    (displayln instr)
)



; TODO: Test RISC-V -> LC-3b translation using Rosette on a few dummy programs and inputs


) ; /module+ test