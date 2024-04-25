#lang rosette/safe

(require rackunit (only-in racket for/list))
(require "common.rkt" "../src/isa/lc3b.rkt" "../src/isa/riscv.rkt" "../src/state.rkt" "../src/rosette.rkt")

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Rosette ------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test


(define (rosette-testbench test-num lc3b-prog riscv-prog)

    (displayln (format "------------- Test ~a -------------" test-num))
    (displayln "--------- LC-3B -> RISC-V --------")
    (let ([ final-prog (rosette-compile "lc3b" "riscv" lc3b-prog) ])
        (displayln "Old program:")
        (for/list ([ instr lc3b-prog ])
            (displayln instr)
        )
        (displayln "\nNew program:")
        (for/list ([ instr final-prog ])
            (displayln instr)
        )
    )

    (displayln "--------- RISC-V -> LC-3B --------")
    (let ([ final-prog (rosette-compile "riscv" "lc3b" riscv-prog) ])
        (displayln "Old program:")
        (for/list ([ instr riscv-prog ])
            (displayln instr)
        )
        (displayln "\nNew program:")
        (for/list ([ instr final-prog ])
            (displayln instr)
        )
    )
    (displayln "")
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 1 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Add x0 to itself (i.e. multiply by 2)

(define lc3b-prog-1
    (list
        (3_ADD     x0 x0 x0)     ;  0
    )
)

(define riscv-prog-1
    (list
        (5_ADD     x0 x0 x0)     ;  0
    )
)

(let ([ final-prog (rosette-compile "lc3b" "riscv" lc3b-prog-1) ])
    (check-equal? (list-ref final-prog 0) (5_ADD x0 x0 x0))
)

(let ([ final-prog (rosette-compile "riscv" "lc3b" riscv-prog-1) ])
    (check-equal? (list-ref final-prog 0) (3_ADD x0 x0 x0))
)

(rosette-testbench 1 lc3b-prog-1 riscv-prog-1)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 2 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;


) ; /module+ test