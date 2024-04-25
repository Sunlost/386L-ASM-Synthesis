#lang rosette/safe

(require rackunit (only-in racket for/list))
(require "common.rkt" "../src/isa/lc3b.rkt" "../src/isa/riscv.rkt" "../src/state.rkt" "../src/rosette.rkt")

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Rosette ------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(module+ test

(define (test-rosette-compile source_isa target_isa source_prog initial_state)
    ; Test the rosette-compile function.
    ;
    ; Parameters:
    ;     source_isa    : The source ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     target_isa    : The target ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     source_prog   : Source program
    ;     initial_state : The initial state of the machine.
    ;
    ; Returns:
    ;     target_prog   : The compiled program.
    ;

    (displayln
        (format "----------- ~a -> ~a ----------" source_isa target_isa)
    )
    (let ([ target_prog (
            rosette-compile source_isa target_isa source_prog initial_state
        ) ])
        (displayln "\nOld program:")
        (for/list ([ instr source_prog ])
            (displayln instr)
        )
        (displayln "\nNew program:")
        (for/list ([ instr target_prog ])
            (displayln instr)
        )
        (displayln "")
        target_prog
    )
)

(define (test-rosette-shrink isa prog initial_state)
    ; Test the rosette-shrink function.
    ;
    ; Parameters:
    ;     isa           : The ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     prog          : The list of instructions representing
    ;                     the program in the ISA.
    ;     initial_state : The initial state of the machine.
    ;
    ; Returns:
    ;     prog'         : The shrunk program.
    ;
    (displayln
        (format "----------- Shrink ~a ----------" isa)
    )
    (let ([ shrunk-prog (
            rosette-shrink isa prog initial_state
        ) ])
        (displayln "\nOld program:")
        (for/list ([ instr prog ])
            (displayln instr)
        )
        (displayln "\nNew program:")
        (for/list ([ instr shrunk-prog ])
            (displayln instr)
        )
        (displayln "")
        shrunk-prog
    )
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

; No rigorous checks for this test, because
; there are multiple solutions with minimal
; instructions.

(displayln "")
(displayln "--------------- Test 1 ---------------")
(let ([ final-prog (test-rosette-compile "lc3b" "riscv" lc3b-prog-1 initial-state) ])
    ; (check-equal? (list-ref final-prog 0) (5_ADD x0 x0 x0))
    (check-true #t)
)
(let ([ final-prog (test-rosette-compile "riscv" "lc3b" riscv-prog-1 initial-state) ])
    ; (check-equal? (list-ref final-prog 0) (3_ADD x0 x0 x0))
    (check-true #t)
)


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

; No rigorous checks for this test, because
; there are multiple solutions with minimal
; instructions.

(displayln "\n--------------- Test 2 ---------------")
(let ([ final-prog (test-rosette-compile "lc3b" "riscv"  lc3b-prog-2 initial-state) ])
    (check-true #t)
)
(let ([ final-prog (test-rosette-compile "riscv" "lc3b" riscv-prog-2 initial-state) ])
    (check-true #t)
)

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

(displayln "\n--------------- Test 3 ---------------")
(let ([ final-prog (test-rosette-compile  "lc3b" "riscv"  lc3b-prog-3 initial-state) ])
    (check-equal? (list-ref final-prog 0) (5_ANDI x0 (imm5 10) x0))
)
(let ([ final-prog (test-rosette-compile "riscv"  "lc3b" riscv-prog-3 initial-state) ])
    (check-equal? (list-ref final-prog 0) (3_ANDI x0 (imm5 10) x0))
)
(let ([ final-prog (test-rosette-shrink "lc3b" lc3b-prog-3 initial-state) ])
    (check-equal? (list-ref final-prog 0) (3_ANDI x0 (imm5 10) x0))
)
(let ([ final-prog (test-rosette-shrink "riscv" riscv-prog-3 initial-state) ])
    (check-equal? (list-ref final-prog 0) (5_ANDI x0 (imm5 10) x0))
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- Test 4 -------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; RISC-V does not have a NOT instruction. Can Rosette figure out
; that XORI with an immediate of -1 can do the same thing?

(define lc3b-prog-4
    (list
        (3_NOT x0 x0) ;  0
    )
)

(define riscv-prog-4
    (list
        (5_XORI x0 (imm5 -1) x0) ;  0
    )
)

(displayln "\n--------------- Test 4 ---------------")
(let ([ final-prog (test-rosette-compile  "lc3b" "riscv"  lc3b-prog-4 initial-state) ])
    (check-equal? (list-ref final-prog 0) (5_XORI x0 (imm5 -1) x0))
)
(let ([ final-prog (test-rosette-compile "riscv"  "lc3b" riscv-prog-4 initial-state) ])
    (check-equal? (list-ref final-prog 0) (3_NOT x0 x0))
)

) ; /module+ test