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

(define (valid-memory-loc loc)
    ; Determine if a location is a valid memory address.
    ;
    ; Parameters:
    ;     loc : The location to check
    ;
    ; Returns:
    ;     #t : If the location is a valid memory address
    ;     #f : Otherwise
    ;
    ; Rules:
    ;     1. Location must be of type <addr> (16-bit bit-vector)
    ;     2. Location must NOT be to one of the registers x0 - x7, PC, or COND_CODE.

    #t

    ; (and
    ;     ; type check
    ;     (bv? loc)
    ;     ; length check
    ;     (equal?
    ;         (length (bitvector->bits loc))
    ;         (length (bitvector->bits (addr 0)))
    ;     )
    ;     ; value check
    ;     (not
    ;         (or
    ;             (equal? loc x0)
    ;             (equal? loc x1)
    ;             (equal? loc x2)
    ;             (equal? loc x3)
    ;             (equal? loc x4)
    ;             (equal? loc x5)
    ;             (equal? loc x6)
    ;             (equal? loc x7)
    ;             (equal? loc PC)
    ;             (equal? loc COND_CODES)
    ;         ) ; /or
    ;     ) ; /not
    ; )
)

(define (valid-memory-val val)
    ; Determine if a value a valid memory value.
    ;
    ; Parameters:
    ;     val : The value to check
    ;
    ; Returns:
    ;     #t : If the value is a valid memory data value
    ;     #f : Otherwise
    ;
    ; Rules:
    ;    1. Value must be of type <mem_val> (8-bit bit vector)

    #t

    ; (and
    ;     ; type check
    ;     (or
    ;         (bv? val)
    ;     )
    ;     ; length check
    ;     (or
    ;         (equal?
    ;             (length (bitvector->bits val))
    ;             (length (bitvector->bits (mem_val 0)))
    ;         )
    ;     ) ; /or
    ; )     ; /and
)

(define (valid-register-loc loc)
    ; Determine if a location is a valid register address.
    ;
    ; Parameters:
    ;     loc : The location to check
    ;
    ; Returns:
    ;     #t : If the location is a valid register address
    ;     #f : Otherwise
    ;
    ; Rules:
    ;     1. Location must be of type <addr> (16-bit bit-vector)
    ;     2. Location must be to one of the general-purpose registers x0 - x7

    #t

    ; (and
    ;     ; type check
    ;     (bv? loc)
    ;     ; length check
    ;     (equal?
    ;         (length (bitvector->bits loc))
    ;         (length (bitvector->bits (addr 0)))
    ;     )
    ;     ; value check
    ;     (or
    ;         (equal? loc x0)
    ;         (equal? loc x1)
    ;         (equal? loc x2)
    ;         (equal? loc x3)
    ;         (equal? loc x4)
    ;         (equal? loc x5)
    ;         (equal? loc x6)
    ;         (equal? loc x7)
    ;     )
    ; )
)

(define (valid-register-val val)
    ; Determine if a value is a valid register value.
    ;
    ; Parameters:
    ;     val : The value to check
    ;
    ; Returns:
    ;     #t : If the value is a valid register value
    ;     #f : Otherwise
    ;
    ; Rules:
    ;     1. Value must be of type <val> (16-bit bit-vector)

    #t

    ; (and
    ;     ; type check
    ;     (bv? val)
    ;     ; length check
    ;     (equal?
    ;         (length (bitvector->bits val))
    ;         (length (bitvector->bits (reg_val 0)))
    ;     )
    ; )
)

(define (valid-pc-val val)
    ; Determine if a value is a valid program counter value.
    ;
    ; Parameters:
    ;     val : The value to check
    ;
    ; Returns:
    ;     #t : If the value is a valid program counter value
    ;     #f : Otherwise
    ;
    ; Rules:
    ;     1. Value must be of type <addr> (16-bit bit-vector)

    #t

    ; (and
    ;     ; type check
    ;     (bv? val)
    ;     ; length check
    ;     (equal?
    ;         (length (bitvector->bits val))
    ;         (length (bitvector->bits (addr 0)))
    ;     )
    ; )
)

(define (set-memory state loc val)
    ; Set the value of a memory address in the state map.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     loc    : The memory address to set
    ;     val    : The value to set the memory address to
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with:
    ;         - Mem[loc] <- val (if loc is valid location)
    ;
    ; Note: This doesn't update the PC. You'll have to do that
    ;       separately!

    (set-state state loc val)

    ; (if
    ;     ; IF  : Loc and value are valid as a memory address / value
    ;     (and (valid-memory-loc loc) (valid-memory-val val))
    ;     ; THEN : Return state function with updated map
    ;     (set-state state loc val)
    ;     ; ELSE : Return current state function
    ;     #f
    ; )
)

(define (set-register-cc state loc val)
    ; Wrapper around set-register that also sets
    ; condition codes.
    ;
    ; Parameters:
    ;     state    : current state of the machine
    ;     loc      : location of the register to be updated
    ;                and to use to update condition codes
    ;     val      : value to be stored in the register
    ;
    ; Returns:
    ;     state'   : updated state with updated condition codes
    ;
    ; Exactly one condition code {N, Z, P} is active at any time.

    (let ([ new_state (set-register state loc val) ])
        (cond [ (bveq  (state loc) (bv 0 16)) (set-state new_state COND_CODES Z_True) ]
              [ (bvsgt (state loc) (bv 0 16)) (set-state new_state COND_CODES P_True) ]
              [ else                          (set-state new_state COND_CODES N_True) ]
        ) ; /cond
    ) ; /let
)

(define (set-register state loc val)
    ; Set the value of a register in the state map.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     loc    : The register address to set
    ;     val    : The value to set the register to
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with:
    ;         - R[loc] <- val (if loc is valid location)
    ;         - PC     <- PC + 4
    ;
    ; Notes:
    ;     This function only supports setting x0 - x7.

    ; (displayln (format "[DEBUG][set-register] loc ~a old_val ~a new_val ~a" loc (state loc) val))
    (set-pc
        (set-state state loc val)
        (bvadd (state PC) (reg_val 4))
    )

    ; (if
    ;     ; IF  : Loc and value are valid as a register number / value
    ;     (and (valid-register-loc loc) (valid-register-val val))
    ;     ; THEN : Return state function with updated map
    ;     (set-state state loc val)
    ;     ; ELSE : Return current state function
    ;     #f
    ; )
)

(define (set-pc state val)
    ; Set the value of the program counter register.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     val    : The value to set the program counter to
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with
    ;              PC = val.
    ; (displayln (format "[DEBUG][set-pc      ] loc PC old_val ~a new_val ~a" (bitvector->natural (state PC)) (bitvector->natural val)))

    (set-state state PC val)

    ; (if
    ;     ; IF  : val is a valid PC
    ;     (valid-pc-val val)
    ;     ; THEN : Return state function with updated map
    ;     (set-state state PC val)
    ;     ; ELSE : Return current state function
    ;     #f
    ; )
)

(define (set-cc state val)
    ; Set the value of the condition code register.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     val    : The value to set the condition codes to
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with
    ;              COND_CODES = val.

    (cond [(equal? val N_True) (set-state state COND_CODES N_True)]
          [(equal? val Z_True) (set-state state COND_CODES Z_True)]
          [(equal? val P_True) (set-state state COND_CODES P_True)]
          [else state] )

    ; (set-state state COND_CODES val)
)

(define (set-state state loc val)
    ; Set the value of a location in the state map, with no restrictions
    ; on the location.
    ;
    ; ** You should NOT call this function, but instead use one of the
    ;    wrappers above! **
    ;
    ; Parameters:
    ;    state  : The current state mapping function
    ;    loc    : The location to set
    ;    val    : The value to set the location to
    ;
    ; Returns:
    ;    state' : The updated state mapping function, with
    ;             loc = val.

    (lambda (r) (if (equal? loc r) val (state r)))
)

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------- PROGRAMS -------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define (get-instr prog pc)
    ; Get the instruction which is referenced by the PC register.
    ;
    ; Parameters:
    ;     pc   : The machine's program counter (as a 16-bit bit-vector)
    ;     prog : The list of instructions (as a vanilla list)
    ;
    ; Returns:
    ;     instr : The instruction at the memory location pointed to by PC.
    ;             If the PC goes out of bounds, return HLT.
    ;
    ; This function works by converting the bitvector PC into
    ; an integer list index. Our state assumes that PCs are
    ; four bytes apart. So, we do the following:
    ;
    ; 1. Cast bitvector PC to an unsigned integer
    ; 2. Divide by 4 to get the index
    ; 3. Return the instruction at that index

    (let ([index (/ (bitvector->natural pc) 4)])
        (cond [ (>= index (length prog)) HLT ]
              [ else   (list-ref prog index) ])
    ) ; /let

)

; ----------------------------------------------------------------------------------------------- ;
; --------------------------------------- CONSTRUCTS -------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Instruction operands
;
;       but this is for readability. adding another layer doesn't make sense here.

(define (addr     val) (bv val 16))
(define (val      val) (bv val 16)) ; TODO: Replace with reg_val
(define (reg_val  val) (bv val 16)) ; Register value (16b)
(define (mem_val  val) (bv val  8)) ; Memory value   (8b)
(define (offset   val) (bv val 16))
(define (imm4     val) (bv val  4))
(define (imm5     val) (bv val  5))
(define (imm6     val) (bv val  6))
(define (imm9     val) (bv val  9))
(define (imm11    val) (bv val 11))
(define (imm12    val) (bv val 12))
; no imm16 since that's just an addr in all places used

(define       N_True (bv #b100 3)) ; Condition codes (LC-3B only)
(define       Z_True (bv #b010 3))
(define       P_True (bv #b001 3))

; Symbolic values for Rosette
(define reg_val? (bitvector 16))
(define mem_val? (bitvector  8))
(define offset?  (bitvector 16))
(define imm4?    (bitvector  4))
(define imm5?    (bitvector  5))
(define imm6?    (bitvector  6))
(define imm9?    (bitvector  9))
(define imm11?   (bitvector 11))
(define imm12?   (bitvector 12))
(define imm16?   (bitvector 16))

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- VARIABLES ----------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Define variables within the state (i.e. keys which will contain a value)

; Register file
(define         x0 (bv  0 16))
(define         x1 (bv  1 16))
(define         x2 (bv  2 16))
(define         x3 (bv  3 16))
(define         x4 (bv  4 16))
(define         x5 (bv  5 16))
(define         x6 (bv  6 16))
(define         x7 (bv  7 16))
(define         PC (bv  8 16)) ; Program Counter
(define COND_CODES (bv  9 16)) ; Condition Codes (LC-3B-only)      -> 3b one-hot bit-vector {N, Z, P}
(define         xZ (bv 10 16)) ; Fixed-zero register (RISC-V only) -> always contains value zero

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------- INITIALIZATION -------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Define intial state and some other useful functions.

; Rosette uses r0 as the input to the program
(define-symbolic input_x0 reg_val?)

(define initial-state
    ; Set the initial state of the state map.
    ;
    ; The following values are set by default:
    ;    - x0 - x7     : (val 0)     (16b)
    ;    - PC          : (val 0)     (16b)
    ;    - COND_CODES  : Z           ( 3b)
    ;    - Elswehere   : (mem_val 0) ( 8b)

    (set-cc
    ; for PC
    (set-pc
    ; for 8 general-purpose registers
    (set-state (set-state (set-state (set-state (set-state (set-state
    (set-state (set-state

        ; Initial state : Map all values to <mem_val> 0
        (lambda (r) (mem_val 0))

        ; Set general-purpose registers
        x0 input_x0) ; For Rosette, assume input_x0 contains the input already
        x1 (val 0))
        x2 (val 0))
        x3 (val 0))
        x4 (val 0))
        x5 (val 0))
        x6 (val 0))
        x7 (val 0))

        ; Set PC
        (val 0))

        ; Set condition code
        Z_True)
)

(define (NO_OP state)
    ; Represent a NO_OP as a function that returns the state unchanged.
    ;
    ; We don't know how to just return state, without trying to invoke it...
    state
)

; Halt instruction
(define-symbolic HLT integer?)
