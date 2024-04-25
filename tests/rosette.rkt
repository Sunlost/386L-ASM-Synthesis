#lang rosette/safe

(require rackunit (only-in racket for/list))
(require "common.rkt" "../src/isa/lc3b.rkt" "../src/isa/riscv.rkt" "../src/state.rkt" "../src/rosette.rkt")

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Rosette ------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test

(define (rosette-testbench test-num lc3b-prog riscv-prog initial_state)
    ; Testbench function for trying out and running the Roestte
    ; compiler for LC-3b <-> RISC-V compilation.
    ;
    ; Parameters:
    ;     test-num      : The test number
    ;     lc3b-prog     : LC-3b program
    ;     riscv-prog    : RISC-V program
    ;                     (ideally, semantically equivalent to
    ;                      the LC-3b program)
    ;     initial_state : The initial state of the machine.
    ;
    ; Returns:
    ;     Nothing
    ;

    (displayln "")
    (displayln (format "--------------- Test ~a ---------------" test-num))
    (displayln "----------- LC-3B -> RISC-V ----------")
    (let ([ final-prog (rosette-compile "lc3b" "riscv" lc3b-prog initial_state) ])
        (displayln "")
        (displayln "Old program:")
        (for/list ([ instr lc3b-prog ])
            (displayln instr)
        )
        (displayln "\nNew program:")
        (for/list ([ instr final-prog ])
            (displayln instr)
        )
    )
    (displayln "")

    (displayln "----------- RISC-V -> LC-3B ----------")
    (let ([ final-prog (rosette-compile "riscv" "lc3b" riscv-prog initial_state) ])
        (displayln "")
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

; Add x0 to itself
; x0 <- x0 * 2

(define lc3b-prog-1
    (list
        (3_ADD     x0 x0 x0) ;  0
    )
)

(define riscv-prog-1
    (list
        (5_ADD     x0 x0 x0) ;  0
    )
)

(let ([ final-prog (rosette-compile "lc3b" "riscv" lc3b-prog-1 initial-state) ])
    (check-equal? (list-ref final-prog 0) (5_ADD x0 x0 x0))
)

(let ([ final-prog (rosette-compile "riscv" "lc3b" riscv-prog-1 initial-state) ])
    (check-equal? (list-ref final-prog 0) (3_ADD x0 x0 x0))
)

(rosette-testbench 1 lc3b-prog-1 riscv-prog-1 initial-state)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 2 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Add x0 to itself, plus 7
; x0 <- (x0 * 2) + 7

(define lc3b-prog-2
    (list
        (3_ADD  x0       x0 x1) ;  0
        (3_ADDI x1 (imm5 7) x0) ;  4
    )
)

(define riscv-prog-2
    (list
        (5_ADD  x0       x0 x1) ;  0
        (5_ADDI x1 (imm5 7) x0) ;  4
    )
)


; Not performing checks on one rigorously, since
; there are multiple optimal solutions with
; minimal instructions.

(rosette-testbench 2 lc3b-prog-2 riscv-prog-2 initial-state)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 3 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; AND x0 and (x0 + 10)
; x0 <- x0 & (x0 + 10)
;
; Also try to see if Rosette simplifies these to a single ANDI instruction.

(define lc3b-prog-3
    (list
        (3_XOR  x1 x1 x1)        ;  0 : Clear x1
                                 ;      (should be optimized out since
                                 ;       it's 0 in initial-state)
        (3_ADDI x1 (imm5 10) x1) ;  4 : Set x1 to 10
        (3_AND  x0 x1 x0)        ;  8 : x0 <- x0 & x1
    )
)

(define riscv-prog-3
    (list
        (5_XOR  x1 x1 x1)        ;  0 : Clear x1
                                 ;      (should be optimized out since
                                 ;       it's 0 in initial-state)
        (5_ADDI x1 (imm5 10) x1) ;  4 : Set x1 to 10
        (5_AND  x0 x1 x0)        ;  8 : x0 <- x0 & x1
    )
)

(let ([ final-prog (rosette-compile "lc3b" "riscv" lc3b-prog-3 initial-state) ])
    (check-equal? (list-ref final-prog 0) (5_ANDI x0 (imm5 10) x0))
)

(let ([ final-prog (rosette-compile "riscv" "lc3b" riscv-prog-3 initial-state) ])
    (check-equal? (list-ref final-prog 0) (3_ANDI x0 (imm5 10) x0))
)

(rosette-testbench 3 lc3b-prog-3 riscv-prog-3 initial-state)

) ; /module+ test