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

    (and
        ; type check
        (bv? loc)
        ; length check
        (equal?
            (length (bitvector->bits loc))
            (length (bitvector->bits (addr 0)))
        )
        ; value check
        (not
            (or
                (equal? loc x0)
                (equal? loc x1)
                (equal? loc x2)
                (equal? loc x3)
                (equal? loc x4)
                (equal? loc x5)
                (equal? loc x6)
                (equal? loc x7)
                (equal? loc PC)
                (equal? loc COND_CODES)
            ) ; /or
        ) ; /not
    )
)

(define (valid-memory-val val)
    ; Determine if a value a valid memory DATA value.
    ;
    ; Parameters:
    ;     val : The value to check
    ;
    ; Returns:
    ;     #t : If the value is a valid memory data value
    ;     #f : Otherwise
    ;
    ; Rules:
    ;    1. Value must either be of type:
    ;        a. <mem_val> (8-bit bit vector)
    ;        b. An instruction struct.
    ;
    ; Notes: When initializing programs, the memory locations containing
    ;        instructions are assumed to be fixed. This function is used
    ;        only to set data, not instructions. Instructions should be
    ;        set using set-state (TODO: Create separate set-instruction)

    (and
        ; type check
        (or
            (bv? val)
        )
        ; length check
        (or
            (equal?
                (length (bitvector->bits val))
                (length (bitvector->bits (mem_val 0)))
            )
        ) ; /or
    )     ; /and
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

    ; (displayln (format "[DEBUG][valid-register-loc] loc=~a" loc))

    (and
        ; type check
        (bv? loc)
        ; length check
        (equal?
            (length (bitvector->bits loc))
            (length (bitvector->bits (addr 0)))
        )
        ; value check
        (or
            (equal? loc x0)
            (equal? loc x1)
            (equal? loc x2)
            (equal? loc x3)
            (equal? loc x4)
            (equal? loc x5)
            (equal? loc x6)
            (equal? loc x7)
        )
    )
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

    (and
        ; type check
        (bv? val)
        ; length check
        (equal?
            (length (bitvector->bits val))
            (length (bitvector->bits (reg_val 0)))
        )
    )
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

    (and
        ; type check
        (bv? val)
        ; length check
        (equal?
            (length (bitvector->bits val))
            (length (bitvector->bits (addr 0)))
        )
    )
)

(define (valid-condition-code-val val)
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
    ;     1. Value must be of type <imm3> (3-bit bit-vector)
    ;     2. Value must be EXACTLY one of the following:
    ;         N: 0b100
    ;         Z: 0b010
    ;         P: 0b001

    (and
        ; type check
        (bv? val)
        ; length check
        (equal?
            (length (bitvector->bits val))
            (length (bitvector->bits 3_N_True))
        )
        ; value check
        (or
            (bveq val 3_N_True)
            (bveq val 3_Z_True)
            (bveq val 3_P_True)
        ) ; /or
    )     ; /and
)

(define (valid-instr-val val)
    ; Determine if a value is a valid instruction value.
    ;
    ; Parameters:
    ;     val : The value to check
    ;
    ; Returns:
    ;     #t : If the value is a valid instruction value
    ;     #f : Otherwise

    #t ; TODO: Implement stricter checking
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
    ;     state' : The updated state mapping function, with
    ;              M[loc] = val, if loc is a valid location.

    (if
        ; IF  : Loc and value are valid as a memory address / value
        (and (valid-memory-loc loc) (valid-memory-val val))
        ; THEN : Return state function with updated map
        (set-state state loc val)
        ; ELSE : Return current state function
        state
    )
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
    ;     state' : The updated state mapping function, with
    ;              R[loc] = val, if loc is a valid location.
    ;
    ; Notes:
    ;     This function only supports setting x0 - x7.
    ;     Setting the PC and condition codes should
    ;     be done using their respective functions below.

    (if
        ; IF  : Loc and value are valid as a register number / value
        (and (valid-register-loc loc) (valid-register-val val))
        ; THEN : Return state function with updated map
        (set-state state loc val)
        ; ELSE : Return current state function
        state
    )
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

    (if
        ; IF  : val is a valid PC
        (valid-pc-val val)
        ; THEN : Return state function with updated map
        (set-state state PC val)
        ; ELSE : Return current state function
        state
    )
)

(define (set-condition-code state val)
    ; Set the value of the condition code register.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     val    : The value to set the condition codes to
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with
    ;              COND_CODES = val.

    (if
        ; IF  : val is a valid condition code
        (valid-condition-code-val val)
        ; THEN : Return state function with updated map
        (set-state state COND_CODES val)
        ; ELSE : Return current state function
        state
    )
)

(define (set-instr state loc val)
    ; Map an instruction to a particular memory address.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     loc    : The memory address to set the instruction to
    ;     val    : The instruction to store
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with
    ;              M[loc] = instr, if loc is a valid location.

    (if
        ; IF  : Loc and value are valid as a memory address / value
        (and (valid-memory-loc loc) (valid-instr-val val))
        ; THEN : Return state function with updated map
        (set-state state loc val)
        ; ELSE : Return current state function
        state
    )
)

(define (set-instrs state loc instrs)
    ; Map a list of instructions linearly onto the
    ; memory address space, starting at loc.
    ;
    ; Parameters:
    ;     state  : The current state mapping function
    ;     loc    : The memory address to start storing instructions
    ;     instrs : The list of instructions to store
    ;
    ; Returns:
    ;     state' : The updated state mapping function, with
    ;              M[loc + i] = instrs[i], for all i in instrs.
    ;
    ; This is done recursively.

    (if
        ; IF   : instrs is not empty
        (not (null? instrs))
        ; THEN : Recursively set the first instruction, and then the rest
        (set-instrs
            (set-instr state loc (car instrs)) ; Set this instruction
            (bvadd loc (addr 4))               ; loc += 4
            (cdr instrs)
        )
        ; ELSE : Return the state as is
        state
    )
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

        ; (displayln (format "[DEBUG][get-instr] pc=~a index=~a" pc index))
        (if
            ; IF  : Index is within bounds
            (and
                (>= index 0)
                (<  index (length prog))
            )
            ; THEN : Return the instruction at that index
            (list-ref prog index)
            ; ELSE : Return HLT
            HLT
        )
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

(define       3_N_True (bv #b100 3)) ; Condition codes (LC-3B only)
(define       3_Z_True (bv #b010 3))
(define       3_P_True (bv #b001 3))

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
(define-symbolic input_r0 reg_val?)

(define initial-state
    ; Set the initial state of the state map.
    ;
    ; The following values are set by default:
    ;    - x0 - x7     : (val 0)     (16b)
    ;    - PC          : (val 0)     (16b)
    ;    - COND_CODES  : Z           ( 3b)
    ;    - Elswehere   : (mem_val 0) ( 8b)

    (set-condition-code
    ; for PC
    (set-pc
    ; for 8 general-purpose registers
    (set-register (set-register (set-register (set-register (set-register (set-register
    (set-register (set-register

        ; Initial state : Map all values to <mem_val> 0
        (lambda (r) (mem_val 0))

        ; Set general-purpose registers
        x0 input_r0) ; For Rosette, assume input_r0 contains the input already
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
        3_Z_True)
)

(define (NO_OP state)
    ; Represent a NO_OP as a function that returns the state unchanged.
    ;
    ; We don't know how to just return state, without trying to invoke it...
    state
)

; Halt instruction
(define-symbolic HLT integer?)
