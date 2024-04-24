#lang rosette/safe

(provide (all-defined-out))

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

;
; State is modeled as an associative map.
;
; Each component is assigned a unique number, and maintains a certain value inside
; the state map. Specificially, it's modeled as a function that generates a new
; lookup function every time a component's value is set.
;
; All components are assumed to be 16-bits wide (i.e. (bv "N" 16) for some number "N").
;

(define (set-state state loc val)
    ; Set the value of a component in the state map.
    (lambda (r) (if (equal? loc r) val (state r)))
)

;
; Define constructs for the state map which will be used by instructions.
;

; Instruction operands
(define (addr   val) (bv val 16))
(define (val    val) (bv val 16))
(define (offset val) (bv val 16))
(define (imm4   val) (bv val  4))
(define (imm5   val) (bv val  5))
(define (imm6   val) (bv val  6))
(define (imm9   val) (bv val  9))
(define (imm11  val) (bv val 11))
(define (imm12  val) (bv val 12))

; return a 16-length bitvector containing val (for readability later)
; yes these could absolutely call one common function which takes in val and the length...
; but this is for readability. adding another layer doesn't make sense here.
; no imm16 since that's just an addr in all places used

; Condition codes (LC-3B only)
(define 3_N_True (bv #b100 3))
(define 3_Z_True (bv #b010 3))
(define 3_P_True (bv #b001 3))


; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Define variables within the state (i.e. keys which will contain a value)

; Register file
(define x0 0)
(define x1 1)
(define x2 2)
(define x3 3)
(define x4 4)
(define x5 5)
(define x6 6)
(define x7 7)

; Program Counter
(define PC 8)

; Condition Codes (LC-3B-only)
(define COND_CODES 9) ; Modeled as 3-bit bit-vector {N, Z, P}


; Fixed-zero register (RISC-V only)
(define xZ 10) ; Should always contain value zero.

;
; Define intial state and some other useful functions.
;

(define initial-state
    ; Set the initial state of the state map.
    (set-state (set-state (lambda (r) 0) 
        9  3_Z_True)  ; COND_CODES = State[9]  = Z (0b                100)
        10 (bv 0 16)) ; xZ         = State[10] = 0 (0b0000 0000 0000 0000)
)

(define (NO_OP state) 
    ; Represent a NO_OP as a function that returns the state unchanged.
    ;
    ; i dont know how to just return state without trying to invoke it
    state
)

; halt instruction
(define-symbolic HLT integer?)
