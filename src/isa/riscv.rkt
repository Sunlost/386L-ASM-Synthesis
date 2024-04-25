#lang rosette/safe

(require rosette/lib/destruct)
(require "../state.rkt" "../util.rkt")

(provide (all-defined-out))

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------- SYNTAX -------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

;
; Define RISC-V ASM syntax / instructions
;
; The following limitations are made on RISC-V programs:
;
; 1. Only a subset of instructions are supported.
;
;    This makes the mapping much easier, as there are many
;    RISC-V instructions with no obvious equivalent or
;    meaningful equivalent in LC-3b.
;
; Template  : (struct 5_... () #:transparent)
;
; SXT(x, y) : Means "sign-extend x to y bits."
;             If y undefined, default y = 16.

;; ARITHMETIC
(struct 5_ADD  (src1  src2   dst) #:transparent) ;;; src1 + src2 -> dst
(struct 5_ADDI (src1  imm5   dst) #:transparent) ;;; src1 + imm5 -> dst
;; BIT MANIPULATION
(struct 5_AND  (src1  src2   dst) #:transparent) ;;; src1 & src2 -> dst
(struct 5_ANDI (src1  imm5   dst) #:transparent) ;;; src1 & imm5 -> dst
;; 5_NOT: pseudo-instr, XORI w/ imm5 = -1        ;;; src1 ^ -1 -> dst
(struct 5_XOR  (src1  src2   dst) #:transparent) ;;; src1 ^ src2 -> dst
(struct 5_XORI (src1  imm5   dst) #:transparent) ;;; src1 ^ imm5 -> dst
;; BRANCH
(struct 5_BEQ  (src1  src2 imm12) #:transparent)
(struct 5_BNE  (src1  src2 imm12) #:transparent)
(struct 5_BLT  (src1  src2 imm12) #:transparent)
(struct 5_BGE  (src1  src2 imm12) #:transparent)
(struct 5_BLTU (src1  src2 imm12) #:transparent)
(struct 5_BGEU (src1  src2 imm12) #:transparent)
;; JUMP
(struct 5_JAL  (     imm16   dst) #:transparent) ;;; PC + 4 -> dst, imm16 -> PC
(struct 5_JALR (src1 imm12   dst) #:transparent) ;;; PC + 4 -> dst, (src1 + SXT(imm12)) & ~1 -> PC
;; LOAD
(struct 5_LB   (src1 imm12   dst) #:transparent) ;;; SXT(MEM[src1 + SXT(imm12)][7:0]) -> dst
(struct 5_LH   (src1 imm12   dst) #:transparent) ;;; MEM[src1 + SXT(imm12)][15:0] -> dst
;; SHIFT
;; (S)hift {(L)eft, (R)ight} {(A)rithmetic, (L)ogical} (I)mmediate
(struct 5_SLLI (src1  imm4   dst) #:transparent) ;;; src1 << imm4 -> dst
(struct 5_SRLI (src1  imm4   dst) #:transparent) ;;; src1 >> imm4 -> dst
(struct 5_SRAI (src1  imm4   dst) #:transparent) ;;; src1 >>> imm4 -> dst
;; STORE
(struct 5_SB   (src1  src2  imm5) #:transparent) ;;; src2[7:0] -> MEM[src1 + SXT(imm5)][7:0]
(struct 5_SH   (src1  src2  imm5) #:transparent) ;;; src2[15:0] -> MEM[src1 + SXT(imm5)][15:0]

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------ SEMANTICS ------------------------------------------ ;
; ----------------------------------------------------------------------------------------------- ;

;
; Define RISC-V ASM semantics
;

(define (eval-riscv-instr instr state)
    ; Apply the semantics of the given RISC-V instruction to the current state.
    ;
    ; Parameters:
    ;     instr  : RISC-V instruction to be executed
    ;     state  : current state of the RISC-V machine
    ;
    ; Returns:
    ;     state' : updated state after executing the instruction
    ;
    ; This function updates all components of the state:
    ;     - The register file
    ;     - The condition codes
    ;     - The program counter

    (let ((new_pc (bvadd (state PC) (offset 4)))) ; save sequential instr PC
    (set-pc ; sets PC to new_pc after current instruction has been run
    (destruct instr

    ;; ARITHMETIC
    [   (5_ADD src1 src2 dst)
        (set-register state dst (bvadd (state src1) (state src2)))   ]

    [   (5_ADDI src1 imm5 dst)
        (set-register state dst (bvadd (state src1) (SXT_16 imm5)))   ]


    ;; BIT MANIPULATION
    [   (5_AND src1 src2 dst)
        (set-register state dst (bvand (state src1) (state src2)))   ]

    [   (5_ANDI src1 imm5 dst)
        (set-register state dst (bvand (state src1) (SXT_16 imm5)))    ]

    [   (5_XOR src1 src2 dst)
        (set-register state dst (bvxor (state src1) (state src2)))   ]

    [   (5_XORI src1 imm5 dst)
        (set-register state dst (bvxor (state src1) (SXT_16 imm5)))   ]


    ;; BRANCH (w/ COND)
    [   (5_BEQ src1 src2 imm12)      ; branch if MEM[src1] == MEM[src2]
        (begin
            (if
                ; IF: MEM[src1] == MEM[src2]
                (bveq (state src1) (state src2))
                ; THEN: take the branch
                (set! new_pc (bvadd (state PC) (SXT_16 imm12)))
                ; ELSE: do nothing, return state
                (void)
            )
            (NO_OP state)
        )   ]

    [   (5_BNE src1 src2 imm12)      ; branch if MEM[src1] != MEM[src2]
        (begin
            (if
                ; IF: MEM[src1] != MEM[src2]
                (equal? (bveq (state src1) (state src2)) false)
                ; THEN: take the branch
                (set! new_pc (bvadd (state PC) (SXT_16 imm12)))
                ; ELSE: do nothing, return state
                (void)
            )
            (NO_OP state)
        )   ]

    [   (5_BLT src1 src2 imm12)      ; branch if MEM[src1] < MEM[src2]
        (begin
            (if
                ; IF: MEM[src1] < MEM[src2]
                (bvslt (state src1) (state src2))
                ; THEN: take the branch
                (set! new_pc (bvadd (state PC) (SXT_16 imm12)))
                ; ELSE: do nothing, return state
                (void)
            )
            (NO_OP state)
        )   ]

    [   (5_BGE src1 src2 imm12)      ; branch if MEM[src1] >= MEM[src2]
        (begin
            (if
                ; IF: MEM[src1] >= MEM[src2]
                (bvsge (state src1) (state src2))
                ; THEN: take the branch
                (set! new_pc (bvadd (state PC) (SXT_16 imm12)))
                ; ELSE: do nothing, return state
                (void)
            )
            (NO_OP state)
        )   ]

    [   (5_BLTU src1 src2 imm12)      ; branch if MEM[src1] < MEM[src2] (both unsigned)
        (begin
            (if
                ; IF: MEM[src1] < MEM[src2]
                (bvult (state src1) (state src2))
                ; THEN: take the branch
                (set! new_pc (bvadd (state PC) (SXT_16 imm12)))
                ; ELSE: do nothing, return state
                (void)
            )
            (NO_OP state)
        )   ]

    [   (5_BGEU src1 src2 imm12)      ; branch if MEM[src1] >= MEM[src2] (both unsigned)
        (begin
            (if
                ; IF: MEM[src1] >= MEM[src2]
                (bvuge (state src1) (state src2))
                ; THEN: take the branch
                (set! new_pc (bvadd (state PC) (SXT_16 imm12)))
                ; ELSE: do nothing, return state
                (void)
            )
            (NO_OP state)
        )   ]


    ;; JUMP
    [   (5_JAL imm16 dst) ;;; PC + 4 -> dst, imm16 -> PC
        (begin
            (set! new_pc imm16)
            (set-register state dst (bvadd (state PC) (offset 4)))
        )   ]

    [   (5_JALR src1 imm12 dst) ;;; PC + 4 -> dst, (src1 + SXT(imm12)) & ~1 -> PC
        (begin
            (set! new_pc (bvand (bvadd (state src1) (SXT_16 imm12)) (bvnot (bv 1 16))))
            (set-register state dst (bvadd (state PC) (offset 4)))
        )    ]


    ;; LOAD
    [   ; LB   : Load Byte
        (5_LB src1 imm12 dst) ;;; SXT-=(MEM[src1 + SXT(imm12)][7:0]) -> dst
        (let* ([ addr (bvadd  (state src1) (SXT_16 imm12)) ]
               [ data (SXT_16 (state addr)) ])
            (set-register state dst data)
        )
    ]


    [   ; LH   : Load Halfword
        (5_LH src1 imm12 dst)
        (let* ([ addr_low  (bvadd  (state src1) (SXT_16 imm12)) ]
               [ addr_high (bvadd  addr_low (addr 1)) ]
               [ data      (CAT_16 (state addr_high) (state addr_low))  ])
            (set-register state dst data)
        )
    ]


    ;; SHIFT
    [   (5_SLLI src1 imm4 dst)
        (set-register state dst (bvshl (state src1) (SXT_16 imm4)))   ]

    [   (5_SRLI src1 imm4 dst)
        (set-register state dst (bvlshr (state src1 (SXT_16 imm4))))    ]

    [   (5_SRAI src1 imm4 dst)
        (set-register state dst (bvashr (state src1 (SXT_16 imm4))))   ]


    ;; STORE
    [
        ; SB    : Store Byte
        (5_SB src1 src2 imm5) ;;; src2[7:0] -> MEM[src1 + SXT(imm5)][7:0]
        (let* ([ addr (bvadd (state src1) (SXT_16 imm5)) ]
               [ data (GET_LOW8 (state src2)) ])
            (set-memory state addr data)
        )
    ]

    [   ; SH    : Store Halfword
        (5_SH src1 src2 imm5) ;;; src2[15:0] -> MEM[src1 + SXT(imm5)][15:0]
        (let* ([ addr_low  (bvadd     (state src1) (SXT_16 imm5))]
               [ addr_high (bvadd     (addr_low) (addr 1))]
               [ data_low  (GET_LOW8  (state src2))]
               [ data_high (GET_HIGH8 (state src2))])
            (set-memory state addr_low  data_low)
            (set-memory state addr_high data_high)
        );
    ]

    )       ; /destruct
    new_pc) ; /set-pc
    )       ; /let
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- PROGRAM EVALUATION -------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define (eval-riscv-prog* prog state)
    ; Evaluate a RISC-V system starting from a state.
    ;
    ; Parameters:
    ;     prog   : List of RISC-V instructions.
    ;     state  : Current state of the RISC-V machine.
    ;
    ; Returns:
    ;     state' : The final state of the RISC-V machine
    ;              after running the program.

    (let ([instr (get-instr prog (state PC))])
        (if
            ; IF: the instr at MEM[PC] is HLT
            (equal? instr HLT)
            ; THEN: we halted, return current state
            state
            ; ELSE: we have an instr to run, recurse
            (eval-riscv-prog* prog (eval-riscv-instr instr state))
        ) ; /if
    )     ; /let
)

(define (eval-riscv-prog prog)
    (define final-state (eval-riscv-prog* prog initial-state))
    (final-state x0)
)