#lang rosette/safe

(require rackunit)
(require "../src/isa/lc3b.rkt" "../src/isa/riscv.rkt" "../src/state.rkt" "../src/util.rkt")

(provide (all-defined-out))

; ----------------------------------------------------------------------------------------------- ;
; --------------------------------------- COMMON ------------------------------------------------ ;
; ----------------------------------------------------------------------------------------------- ;

; Test samples used for testing ISAs/synthesis/etc.


; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- TEST STATES --------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Define test states, i.e. initial state of registers, memory, etc.

(define test-state-1
    ; Test State 1:
    ;
    ; - Registers:
    ;     x0 = 0
    ;     x1 = 5
    ;     x2 = 7
    ;     x3 = 1000
    ;     x4 = 0
    ;     x5 = 1008
    ;     x6 = 0
    ;     x7 = 14
    ;     PC = 0
    ; - Memory:
    ;     MEM[1000] = 32
    ;     MEM[1008] = 0
    ;     MEM[1016] = 0
    ;     MEM[1024] = 0
    ;     MEM[1032] = 0
    ;     MEM[1040] = 0
    ;     MEM[1048] = 0
    ;     MEM[1056] = 0
    ; - Instructions:
    ;     [nothing]

    ; set Memory
    (set-memory (set-memory (set-memory (set-memory (set-memory (set-memory (set-memory 
    (set-memory 
    ; set PC
    (set-pc 
    ; set registers
    (set-register (set-register (set-register (set-register (set-register (set-register 
    (set-register 

    initial-state 

    ; Registers
    x1 (val    5)) 
    x2 (val    7))
    x3 (val 1000))
    x4 (val    0))
    x5 (val 1008))
    x6 (val    0)) 
    x7 (val   14)) 

    ; PC
    (val 0)) 

    ; Memory : data memory begins at addr 1000
    (addr 1000) (mem_val 32)) 
    (addr 1008) (mem_val 0)) 
    (addr 1016) (mem_val 0)) 
    (addr 1024) (mem_val 0)) 
    (addr 1032) (mem_val 0)) 
    (addr 1040) (mem_val 0)) 
    (addr 1048) (mem_val 0)) 
    (addr 1056) (mem_val 0)) 
)