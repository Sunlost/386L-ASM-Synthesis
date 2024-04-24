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

; Template : (struct 3_... () #:transparent)
;
; SXT      : Means "sign-extend to 16 bits".

;; ARITHMETIC
(struct 3_ADD    (src1 src2 dst) #:transparent) ;;; src1 + src2 -> dst
(struct 3_ADDI   (src1 imm5 dst) #:transparent) ;;; src1 + imm5 -> dst

;; BIT MANIPULATION
(struct 3_AND    (src1 src2 dst) #:transparent) ;;; src1 & src2 -> dst
(struct 3_ANDI   (src1 imm5 dst) #:transparent) ;;; src1 & imm5 -> dst
(struct 3_NOT    (src1 dst     ) #:transparent) ;;; ~src1 -> dst
(struct 3_XOR    (src1 src2 dst) #:transparent) ;;; src1 ^ src2 -> dst
(struct 3_XORI   (src1 imm5 dst) #:transparent) ;;; src1 ^ imm5 -> dst

;; BRANCH (w/ COND)
(struct 3_BR     (imm9)          #:transparent)
(struct 3_BR_N   (imm9)          #:transparent)
(struct 3_BR_Z   (imm9)          #:transparent)
(struct 3_BR_P   (imm9)          #:transparent)
(struct 3_BR_NP  (imm9)          #:transparent)
(struct 3_BR_ZP  (imm9)          #:transparent)
(struct 3_BR_NZ  (imm9)          #:transparent)
(struct 3_BR_NZP (imm9)          #:transparent)

;; JUMP
(struct 3_JMP    (base )         #:transparent) ;;; base -> PC
(struct 3_RET    (arg  )         #:transparent) ;;; R7 -> PC
(struct 3_JSR    (imm11)         #:transparent) ;;; PC + 4 -> R7, PC + (SXT(imm11) << 1) -> PC
(struct 3_JSRR   (base )         #:transparent) ;;; PC + 4 -> R7, base -> PC

;; LOAD
(struct 3_LDB    (base imm6 dst) #:transparent) ;;; SXT(MEM[base + SXT(imm6)]) -> dst
(struct 3_LDW    (base imm6 dst) #:transparent) ;;; MEM[base + (SXT(imm6) << 1)] -> dst
(struct 3_LEA    (imm9 dst)      #:transparent) ;;; MEM[PC + (SXT(imm9) << 1)] -> dst

;; SHIFT
(struct 3_LSHF   (src1 imm4 dst)  #:transparent) ;;; src1 << imm4 -> dst
(struct 3_RSHFL  (src1 imm4 dst)  #:transparent) ;;; src1 >> imm4 -> dst
(struct 3_RSHFA  (src1 imm4 dst)  #:transparent) ;;; src1 >>> imm4 -> dst

;; STORE
(struct 3_STB    (base src1 imm6) #:transparent) ;;; src1[7:0] -> MEM[base + SXT(imm6)]
(struct 3_STW    (base src1 imm6) #:transparent) ;;; src1 -> MEM[base + (SXT(imm6) << 1)]

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------ SEMANTICS ------------------------------------------ ;
; ----------------------------------------------------------------------------------------------- ;

; Define LC-3b ASM semantics

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
        (set-state state COND_CODES 3_Z_True)
        ; ELSE: check between P/N
        (if
            ; IF: X[register] is positive
            (bvsgt (state register) (bv 0 16))
            ; THEN: set P = 1
            (set-state state COND_CODES 3_P_True)
            ; ELSE: set N = 1
            (set-state state COND_CODES 3_N_True)
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

    (let ((new_pc (bvadd (state PC) (offset 4)))) ; save sequential instr PC
    (set-state ; sets PC to new_pc after current instruction has been run
    (destruct instr

    ;; ARITHMETIC
    [   (3_ADD src1 src2 dst)
        (set-lc3b-cond-codes
            (set-state state dst (bvadd (state src1) (state src2)))
        dst)   ]

    [   (3_ADDI src1 imm5 dst)
        (set-lc3b-cond-codes
            (set-state state dst (bvadd (state src1) (SXT_16 imm5)))
        dst)   ]


    ;; BIT MANIPULATION
    [   (3_AND src1 src2 dst)
        (set-lc3b-cond-codes
            (set-state state dst (bvand (state src1) (state src2)))
        dst)   ]

    [   (3_ANDI src1 imm5 dst)
        (set-lc3b-cond-codes
            (set-state state dst (bvand (state src1) (SXT_16 imm5)))
        dst)   ]

    [   (3_NOT src1 dst)
        (set-state state dst (bvnot (state src1)))   ]

    [   (3_XOR src1 src2 dst)
        (set-lc3b-cond-codes
            (set-state state dst (bvxor (state src1) (state src2)))
        dst)   ]

    [   (3_XORI src1 imm5 dst)
        (set-lc3b-cond-codes
            (set-state state dst (bvxor (state src1) (SXT_16 imm5)))
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
                (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm9))))
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

    [   (3_RET _arg)
        (begin
            (set! new_pc (state x7))
            (NO_OP state)
        )    ]

    [   (3_JSR imm11)
        (begin
            (set! new_pc (bvadd (state PC) (L_SH_1 (SXT_16 imm11))))
            (set-state state x7 (bvadd (state PC) (offset 4)))
        )   ]

    [   (3_JSRR base)
        (begin
            (set! new_pc (state base))
            (set-state state x7 (bvadd (state PC) (offset 4)))
        )   ]


    ;; LOAD
    [   (3_LDB src1 imm6 dst)
        ; do not keep any existing contents of register.
        ; always load into bottom 8 bits.
        (set-lc3b-cond-codes
            (let ((load_addr (bvadd (state src1) (SXT_16 imm6))))
            (if
                ; IF: the 8-place bit in the address is 1
                (equal?   (bvand load_addr (bv #x0008 16))   (bv #x0008 16))
                ; THEN: load the low 8 bits into low 8 bits of register
                (set-state state dst (MASK_LOW8 (state load_addr)))
                ; ELSE: load the high 8 bits into low 8 bits of register
                (set-state state dst (SXT_16 (GET_HIGH8 (state load_addr))))
            ) ; /if
            ) ; /let
        dst)  ; /set-lc3b-condition-codes
    ]

    [   (3_LDW src1 imm6 dst)
        (set-lc3b-cond-codes
            (set-state state dst (state (bvadd (state src1) (L_SH_1 (SXT_16 imm6)))))
        dst)   ]

    [   (3_LEA imm9 dst)
        (set-lc3b-cond-codes
            (set-state state dst (state (bvadd (state PC) (L_SH_1 (SXT_16 imm9)))))
        dst)   ]


    ;; SHIFT
    [   (3_LSHF src1 imm4 dst)
        (set-state state dst (bvshl (state src1) (SXT_16 imm4)))   ]

    [   (3_RSHFL src1 imm4 dst)
        (set-state state dst (bvlshr (state src1 (SXT_16 imm4))))   ]

    [   (3_RSHFA src1 imm4 dst)
        (set-state state dst (bvashr (state src1 (SXT_16 imm4))))   ]


    ;; STORE
    [   (3_STB base src1 imm6)
        (let ((store_addr (bvadd (state base) (SXT_16 imm6))))
        (if
            ; IF: check if the 8-place bit in the address is 1
            (equal?   (bvand store_addr (bv #x0008 16))   (bv #x0008 16))
            ; THEN: store src1's low 8 bits into low 8 bits of address
            (set-state state store_addr (concat (GET_HIGH8 (state store_addr)) (GET_LOW8 (state src1))))
            ; ELSE: store src1's low 8 bits into high 8 bits of address
            (set-state state store_addr (concat (GET_LOW8 (state src1)) (GET_LOW8 (state store_addr))))
        ) ; /if
        ) ; /let
    ]

    [   (3_STW base src1 imm6)
        (set-state state (bvadd (state base) (SXT_16 imm6)) (state src1))   ]

    )          ; /destruct
    PC new_pc) ; /set-state
    )          ; /let
)

(define (eval-lc3b-prog state)
    ; Evaluate a full LC-3b program using our syntax and semantics.
    ;
    ; Parameters:
    ;     state  : Current state of the LC-3b machine.
    ;
    ; Returns:
    ;     state' : Final state after the machine halts (if ever).

    (if
        ; IF: the instr at MEM[PC] is HLT
        (equal? (state (state PC)) HLT)
        ; THEN: we halted, return current state
        state
        ; ELSE: we have an instr to run, recurse
        (eval-lc3b-prog (eval-lc3b-instr (state (state PC)) state))
    )
)
