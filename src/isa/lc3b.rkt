#lang rosette/safe

(require rosette/lib/destruct)
(require "../state.rkt" "../util.rkt")

(provide (all-defined-out))

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------- SYNTAX -------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

;
; Define LC-3b ASM syntax / instructions
;
; The following limitations are made on LC-3b programs:
;
; 1. Writing to R7 is not allowed (except by JSR and JSRR).
;
;    This simplifies condition code handling, as JSR and JSRR are
;    the only instructions that write to a register, but don't set
;    condition codes.
;
; 2. Interrupts and exceptions are not supported. (including RTI instruction)
;
;    This simplifies our state and avoids the complexity of dealing with
;    stack pointers / the program status register / etc.
;
; Template  : (struct 3_... () #:transparent)
;
; SXT(x, y) : Means "sign-extend x to y bits."
;             If y undefined, default y = 16.

;; ARITHMETIC
(struct 3_ADD    (src1 src2   dst) #:transparent) ;;; src1 + src2 -> dst
(struct 3_ADDI   (src1 imm5   dst) #:transparent) ;;; src1 + imm5 -> dst
;; BIT MANIPULATION
(struct 3_AND    (src1 src2   dst) #:transparent) ;;; src1 & src2 -> dst
(struct 3_ANDI   (src1 imm5   dst) #:transparent) ;;; src1 & imm5 -> dst
(struct 3_NOT    (     src1   dst) #:transparent) ;;; ~src1 -> dst
(struct 3_XOR    (src1 src2   dst) #:transparent) ;;; src1 ^ src2 -> dst
(struct 3_XORI   (src1 imm5   dst) #:transparent) ;;; src1 ^ imm5 -> dst
;; BRANCH (w/ COND)
(struct 3_BR     (           imm9) #:transparent)
(struct 3_BR_N   (           imm9) #:transparent)
(struct 3_BR_Z   (           imm9) #:transparent)
(struct 3_BR_P   (           imm9) #:transparent)
(struct 3_BR_NP  (           imm9) #:transparent)
(struct 3_BR_ZP  (           imm9) #:transparent)
(struct 3_BR_NZ  (           imm9) #:transparent)
(struct 3_BR_NZP (           imm9)  #:transparent)
;; JUMP
(struct 3_JMP    (           base) #:transparent) ;;; base -> PC
(struct 3_RET    (               ) #:transparent) ;;; R7 -> PC
(struct 3_JSR    (          imm11) #:transparent) ;;; PC + 4 -> R7, PC + (SXT(imm11) << 1) -> PC
(struct 3_JSRR   (           base) #:transparent) ;;; PC + 4 -> R7, base -> PC
;; LOAD
(struct 3_LDB    (base imm6   dst) #:transparent) ;;; SXT(MEM[base + SXT(imm6)]) -> dst
(struct 3_LDW    (base imm6   dst) #:transparent) ;;; MEM[base + (SXT(imm6) << 1)] -> dst
(struct 3_LEA    (     imm9   dst) #:transparent) ;;; MEM[PC + (SXT(imm9) << 1)] -> dst
;; SHIFT
(struct 3_LSHF   (src1 imm4   dst) #:transparent) ;;; src1 << imm4 -> dst
(struct 3_RSHFL  (src1 imm4   dst) #:transparent) ;;; src1 >> imm4 -> dst
(struct 3_RSHFA  (src1 imm4   dst) #:transparent) ;;; src1 >>> imm4 -> dst
;; STORE
(struct 3_STB    (base src1  imm6) #:transparent) ;;; src1[7:0] -> MEM[base + SXT(imm6)]
(struct 3_STW    (base src1  imm6) #:transparent) ;;; src1 -> MEM[base + (SXT(imm6) << 1)]

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------ SEMANTICS ------------------------------------------ ;
; ----------------------------------------------------------------------------------------------- ;

;
; Define LC-3b ASM semantics
;

(define (set-lc3b-cond-codes state register)
    ; Set condition code of the current state based on the value inside a register.
    ;
    ; Parameters:
    ;     state    : current state of the LC-3b machine
    ;     register : destination register of the executed instruction
    ;
    ; Returns:
    ;     state'   : updated state with updated condition codes
    ;
    ; Only one condition code {N, Z, P} is active at a time.

    (if
        ; IF: X[register] is zero
        (bveq (state register) (bv 0 16))
        ; THEN: set Z = 1
        (set-condition-code state 3_Z_True)
        ; ELSE: check between P/N
        (if
            ; IF: X[register] is positive
            (bvsgt (state register) (bv 0 16))
            ; THEN: set P = 1
            (set-condition-code state 3_P_True)
            ; ELSE: set N = 1
            (set-condition-code state 3_N_True)
        )
    )
)

(define (eval-lc3b-instr instr state)
    ; Apply the semantics of the given LC-3b instruction to the current state.
    ;
    ; Parameters:
    ;     instr  : LC-3b instruction to be executed
    ;     state  : current state of the LC-3b machine
    ;
    ; Returns:
    ;     state' : updated state after executing the instruction
    ;
    ; This function updates all components of the state:
    ;     - The register file
    ;     - The condition codes
    ;     - The program counter
    ; (displayln (format "[DEBUG][eval-lc3b-instr] Before x0: ~a" (state x0)))

    (let ((new_pc (bvadd (state PC) (offset 4)))) ; save sequential instr PC
    (set-pc ; sets PC to new_pc after current instruction has been run
    (destruct instr

    ;; ARITHMETIC
    [   (3_ADD src1 src2 dst)
        (set-lc3b-cond-codes
            (set-register state dst (bvadd (state src1) (state src2)))
        dst)   ]

    [   (3_ADDI src1 imm5 dst)
        (set-lc3b-cond-codes
            (set-register state dst (bvadd (state src1) (SXT_16 imm5)))
        dst)   ]


    ;; BIT MANIPULATION
    [   (3_AND src1 src2 dst)
        (set-lc3b-cond-codes
            (set-register state dst (bvand (state src1) (state src2)))
        dst)   ]

    [   (3_ANDI src1 imm5 dst)
        (set-lc3b-cond-codes
            (set-register state dst (bvand (state src1) (SXT_16 imm5)))
        dst)   ]

    [   (3_NOT src1 dst)
        (set-register state dst (bvnot (state src1)))   ]

    [   (3_XOR src1 src2 dst)
        (set-lc3b-cond-codes
            (set-register state dst (bvxor (state src1) (state src2)))
        dst)   ]

    [   (3_XORI src1 imm5 dst)
        (set-lc3b-cond-codes
            (set-register state dst (bvxor (state src1) (SXT_16 imm5)))
        dst)   ]


    ;; BRANCH (w/ COND)
    [   (3_BR imm9)         ; unconditionally branch
        (begin
            (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
            (NO_OP state)
        )   ]

    [   (3_BR_N imm9)       ; branch if (N = 1)
        (begin
            (if
                ; IF: (N = 1)
                (bveq (extract 2 2 (state COND_CODES)) (bv 1 1))
                ; THEN: branch
                (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
                ; ELSE: do nothing, return current state
                (void)
            ) ; /if
            (NO_OP state)
        )     ; /begin
    ]

    [   (3_BR_Z imm9)       ; branch if (Z = 1)
        (begin
            (if
                ; IF: (Z = 1)
                (bveq (extract 1 1 (state COND_CODES)) (bv 1 1))
                ; THEN: branch
                (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
                ; ELSE: do nothing, return current state
                (void)
            ) ; /if
            (NO_OP state)
        )     ; /begin
    ]

    [   (3_BR_P imm9)       ; branch if (P = 1)
        (begin
            (if
                ; IF: (P = 1)
                (bveq (extract 0 0 (state COND_CODES)) (bv 1 1))
                ; THEN: branch
                (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
                ; ELSE: do nothing, return current state
                (void)
            ) ; /if
            (NO_OP state)
        )     ; /begin
    ]

    [   (3_BR_NP imm9)      ; branch if (N = 1) OR (P = 1), equiv to (Z = 0)
        (begin
            (if
                ; IF: (Z = 0)
                (bveq (extract 1 1 (state COND_CODES)) (bv 0 1))
                ; THEN: branch
                (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
                ; ELSE: do nothing, return current state
                (void)
            ) ; /if
            (NO_OP state)
        )     ; /begin
    ]

    [   (3_BR_ZP imm9)      ; branch if (Z = 1) OR (P = 1), equiv to (N = 0)
        (begin
            (if
                ; IF: (N = 0)
                (bveq (extract 2 2 (state COND_CODES)) (bv 0 1))
                ; THEN: branch
                (set! new_pc (+ (state PC) (TO_INT (L_SH_1 (SXT_16 imm9)))))
                ; ELSE: do nothing, return current state
                (void)
            ) ; /if
            (NO_OP state)
        )     ; /begin
    ]

    [   (3_BR_NZ imm9)      ; branch if (N = 1) OR (Z = 1), equiv to (P = 0)
        (begin
            (if
                ; IF: (P = 0)
                (bveq (extract 0 0 (state COND_CODES)) (bv 0 1))
                ; THEN: branch
                (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
                ; ELSE: do nothing, return current state
                (void)
            ) ; /if
            (NO_OP state)
        )     ; /begin
    ]

    [   (3_BR_NZP imm9)     ; branch if (N = 1) OR (Z = 1) OR (P = 1) ... so always branch.
        (begin
            (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
            (NO_OP state)
        )   ]


    ;; JUMP
    [   (3_JMP base)
        (begin
            (set! new_pc (state base))
            (NO_OP state)
        )   ]

    [   (3_RET)
        (begin
            (set! new_pc (state x7))
            (NO_OP state)
        )    ]

    [   (3_JSR imm11)
        (begin
            (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm11))))
            (set-register state x7 (bvadd (state PC) (offset 4)))
        )   ]

    [   (3_JSRR base)
        (begin
            (set! new_pc (state base))
            (set-register state x7 (bvadd (state PC) (offset 4)))
        )   ]


    ;; LOAD
    [   ; LDB : Load Byte
        (3_LDB src1 imm6 dst)
        (set-lc3b-cond-codes
            (let* ([ addr (bvadd  (state src1) (SXT_16 imm6)) ]
                   [ data (SXT_16 (state addr))               ])
            (set-register state dst data)
            ) ; /let
        dst)  ; /set-lc3b-condition-codes
    ]

    [   ; LDW : Load Word
        (3_LDW src1 imm6 dst)
        (set-lc3b-cond-codes
            (let* ([ addr_low  (bvadd  (state src1) (L_SH_1 (SXT_16 imm6))) ]
                   [ addr_high (bvadd  addr_low     (addr 1))               ]
                   [ data      (CAT_16 (state addr_high) (state addr_low))  ])
            (set-register state dst data)
            ) ; /let
        dst)  ; /set-lc3b-condition-codes
    ]

    [   ; LEA : Load Effective Address
        (3_LEA imm9 dst)
        (set-lc3b-cond-codes
            (set-register state dst (state (bvadd (state PC) (L_SH_1 (SXT_16 imm9)))))
        dst)
    ]


    ;; SHIFT
    [   (3_LSHF src1 imm4 dst)
        (set-register state dst (bvshl (state src1) (SXT_16 imm4)))   ]

    [   (3_RSHFL src1 imm4 dst)
        (set-register state dst (bvlshr (state src1 (SXT_16 imm4))))   ]

    [   (3_RSHFA src1 imm4 dst)
        (set-register state dst (bvashr (state src1 (SXT_16 imm4))))   ]


    ;; STORE
    [   ; STB : Store Byte
        (3_STB base src1 imm6)
        (let* ([ addr (bvadd    (state base) (SXT_16 imm6)) ]
               [ data (GET_LOW8 (state src1)) ])
        (set-memory state addr data)
        ) ; /let
    ]

    [   ; STW : Store word
        (3_STW base src1 imm6)
        (let* ([ addr_low  (bvadd     (state base) (L_SH_1 (SXT_16 imm6))) ]
               [ addr_high (bvadd     (addr_low) (addr 1)) ]
               [ data_low  (GET_LOW8  (state src1)) ]
               [ data_high (GET_HIGH8 (state src1)) ])
        (set-memory state addr_low  data_low)
        (set-memory state addr_high data_high)
        ) ; /let
    ]

    )       ; /destruct
    new_pc) ; /set-pc
    )       ; /let
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- PROGRAM EVALUATION -------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define (eval-lc3b-prog* prog state)
    ; Evaluate an LC-3b system starting from a state.
    ;
    ; Parameters:
    ;     prog   : List of LC-3b instructions.
    ;     state  : Current state of the LC-3b machine.
    ;
    ; Returns:
    ;     state' : The final state of the LC-3b machine
    ;              after running the program.

    (let ([instr (get-instr prog (state PC))])
        ; (displayln (format "[DEBUG][eval-lc3b-prog] Before x0: ~a instr: ~a" (state x0) instr))
        (if
            ; IF: the instr at MEM[PC] is HLT
            (equal? instr HLT)
            ; THEN: we halted, return current state
            state
            ; ELSE: we have an instr to run, recurse
            (eval-lc3b-prog* prog (eval-lc3b-instr instr state))
        ) ; /if
    )     ; /let
)

(define (eval-lc3b-prog prog)
    (define final-state (eval-lc3b-prog* prog initial-state))
    (final-state x0)
)