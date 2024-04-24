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
(struct 5_ADD  (src1 src2 dst)   #:transparent) ;;; src1 + src2 -> dst
(struct 5_ADDI (src1 imm5 dst)   #:transparent) ;;; src1 + imm5 -> dst

;; BIT MANIPULATION
(struct 5_AND  (src1 src2 dst)   #:transparent) ;;; src1 & src2 -> dst
(struct 5_ANDI (src1 imm5 dst)   #:transparent) ;;; src1 & imm5 -> dst
  ;; 5_NOT: pseudo-instr, XORI w/ imm5 = -1     ;;; src1 ^ -1 -> dst
(struct 5_XOR  (src1 src2 dst)   #:transparent) ;;; src1 ^ src2 -> dst
(struct 5_XORI (src1 imm5 dst)   #:transparent) ;;; src1 ^ imm5 -> dst

;; BRANCH
(struct 5_BEQ  (src1 src2 imm12) #:transparent)
(struct 5_BNE  (src1 src2 imm12) #:transparent)
(struct 5_BLT  (src1 src2 imm12) #:transparent)
(struct 5_BGE  (src1 src2 imm12) #:transparent)
(struct 5_BLTU (src1 src2 imm12) #:transparent)
(struct 5_BGEU (src1 src2 imm12) #:transparent)

;; JUMP
(struct 5_JAL  (     imm16 dst)  #:transparent) ;;; PC + 4 -> dst, imm16 -> PC
(struct 5_JALR (src1 imm12 dst)  #:transparent) ;;; PC + 4 -> dst, (src1 + SXT(imm12)) & ~1 -> PC

;; LOAD
(struct 5_LB   (src1 imm12 dst)  #:transparent) ;;; SXT(MEM[src1 + SXT(imm12)][7:0]) -> dst 
(struct 5_LH   (src1 imm12 dst)  #:transparent) ;;; MEM[src1 + SXT(imm12)][15:0] -> dst

;; SHIFT
;;; (S)hift {(L)eft, (R)ight} {(A)rithmetic, (L)ogical} (I)mmediate
(struct 5_SLLI (src1 imm4 dst)   #:transparent) ;;; src1 << imm4 -> dst
(struct 5_SRLI (src1 imm4 dst)   #:transparent) ;;; src1 >> imm4 -> dst
(struct 5_SRAI (src1 imm4 dst)   #:transparent) ;;; src1 >>> imm4 -> dst

;; STORE
(struct 5_SB   (src1 src2 imm5)  #:transparent) ;;; src2[7:0] -> MEM[src1 + SXT(imm5)][7:0]
(struct 5_SH   (src1 src2 imm5)  #:transparent) ;;; src2[15:0] -> MEM[src1 + SXT(imm5)][15:0]

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------ SEMANTICS ------------------------------------------ ;
; ----------------------------------------------------------------------------------------------- ;

; Define RISC-V ASM semantics

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
    (set-state ; sets PC to new_pc after current instruction has been run
    (destruct instr
    
    ;; ARITHMETIC
    [   (5_ADD src1 src2 dst)
        (set-state state dst (bvadd (state src1) (state src2)))   ]

    [   (5_ADDI src1 imm5 dst)
        (set-state state dst (bvadd (state src1) (SXT_16 imm5)))   ]


    ;; BIT MANIPULATION
    [   (5_AND src1 src2 dst)
        (set-state state dst (bvand (state src1) (state src2)))   ]

    [   (5_ANDI src1 imm5 dst)
        (set-state state dst (bvand (state src1) (SXT_16 imm5)))    ]

    [   (5_XOR src1 src2 dst)
        (set-state state dst (bvxor (state src1) (state src2)))   ]

    [   (5_XORI src1 imm5 dst)
        (set-state state dst (bvxor (state src1) (SXT_16 imm5)))   ]


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
            (set-state state dst (bvadd (state PC) (offset 4)))
        )   ]

    [   (5_JALR src1 imm12 dst) ;;; PC + 4 -> dst, (src1 + SXT(imm12)) & ~1 -> PC
        (begin
            (set! new_pc (bvand (bvadd (state src1) (SXT_16 imm12)) (bvnot (bv 1 16))))
            (set-state state dst (bvadd (state PC) (offset 4)))
        )    ]


    ;; LOAD
    [   (5_LB src1 imm12 dst) ;;; SXT-=(MEM[src1 + SXT(imm12)][7:0]) -> dst 
        ; always place into lowest byte of register. sign-extend to fill register.
        (let ((load_addr (state (bvadd (state src1) (SXT_16 imm12)))))
        (if
            ; IF: the 8-place bit in the address is 1
            (equal?   (bvand load_addr (bv #x0008 16)   (bv #x0008 16)))
            ; THEN: load the low 8 bits into low 8 bits of register
            (set-state state dst (SXT_16 (GET_LOW8 (state load_addr))))
            ; ELSE: load the high 8 bits into low 8 bits of register
            (set-state state dst (SXT_16 (GET_HIGH8 (state load_addr))))
        ) ; /if
        ) ; /let
    ]

    [   (5_LH src1 imm12 dst) ;;; MEM[src1 + SXT(imm12)][15:0] -> dst
        (set-state state dst (state (bvadd (state src1) (SXT_16 imm12))))   ]


    ;; SHIFT
    [   (5_SLLI src1 imm4 dst)
        (set-state state dst (bvshl (state src1) (SXT_16 imm4)))   ]

    [   (5_SRLI src1 imm4 dst)
        (set-state state dst (bvlshr (state src1 (SXT_16 imm4))))    ]

    [   (5_SRAI src1 imm4 dst)
        (set-state state dst (bvashr (state src1 (SXT_16 imm4))))   ]


    ;; STORE
    [   
        (5_SB src1 src2 imm5) ;;; src2[7:0] -> MEM[src1 + SXT(imm5)][7:0]
        ; place into byte specified by store_addr. keep existing other byte, don't overwrite
        (let ((store_addr (bvadd (state src1) (SXT_16 imm5))))
        (if
            ; IF: the 8-place bit in the address is 1
            (equal?   (bvand store_addr (bv #x0008 16))   (bv #x0008 16))
            ; THEN: store src2's low 8 bits into low 8 bits of mem[store_addr]
            (set-state state store_addr (concat (GET_HIGH8 (state store_addr)) (GET_LOW8 (state src2))))
            ; ELSE: store src2's low 8 bits into high 8 bits of mem[store_addr]
            (set-state state store_addr (concat (GET_LOW8 (state src2)) (GET_LOW8 (state store_addr))))
        ) ; /if
        ) ; /let
    ]

    [   (5_SH src1 src2 imm5) ;;; src2[15:0] -> MEM[src1 + SXT(imm5)][15:0]
        (set-state state  (bvadd (state src1) (SXT_16 imm5))  (state src2))   ]


    )          ; /destruct
    PC new_pc) ; /set-state
    )          ; /let
)


(define (eval-riscv-prog state)
    ; Evaluate a full RISC-V program using our syntax and semantics.
    ;
    ; Parameters:
    ;     state  : Current state of theRISC-V machine.
    ;
    ; Returns:
    ;     state' : Final state after the machine halts (if ever).

    (if
        ; IF: the instr at MEM[PC] is HLT
        (equal? (state (state PC)) HLT)
        ; THEN: we halted, return current state
        state
        ; ELSE: we have an instr to run, recurse
        (eval-riscv-prog (eval-riscv-instr (state (state PC)) state))
    )
)
