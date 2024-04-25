#lang rosette/safe

(require rosette/lib/destruct rosette/lib/angelic)
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

    (destruct instr

    ;; ARITHMETIC
    [   ; ADD  : Add
        (5_ADD src1 src2 dst)
        (set-register state dst (bvadd (state src1) (state src2)))
    ]

    [   ; ADDI : Add immediate
        (5_ADDI src1 imm5 dst)
        (set-register state dst (bvadd (state src1) (SXT_16 imm5)))
    ]


    ;; BIT MANIPULATION
    [   ; AND  : And
        (5_AND src1 src2 dst)
        (set-register state dst (bvand (state src1) (state src2)))
    ]

    [   ; ANDI : And immediate
        (5_ANDI src1 imm5 dst)
        (set-register state dst (bvand (state src1) (SXT_16 imm5)))
    ]

    [   ; XOR  : Exclusive or
        (5_XOR src1 src2 dst)
        (set-register state dst (bvxor (state src1) (state src2)))
    ]

    [   ; XORI : Exclusive or immediate
        (5_XORI src1 imm5 dst)
        (set-register state dst (bvxor (state src1) (SXT_16 imm5)))
    ]


    ;; BRANCH (w/ COND)
    [   ; BEQ : Branch If Equal
        (5_BEQ src1 src2 imm12)
        (let ([ taken_pc (bvadd (state PC) (SXT_16 imm12)) ]
              [ next_pc  (bvadd (state PC) (reg_val 4)) ])
            (cond [ (bveq (state src1) (state src2)) (set-pc state taken_pc) ]
                  [ else                             (set-pc state  next_pc) ]
            ) ; /cond
        )     ; /let
    ]

    [   ; BNE : Branch If Not Equal
        (5_BNE src1 src2 imm12)
        (let ([ taken_pc (bvadd (state PC) (SXT_16 imm12)) ]
              [ next_pc  (bvadd (state PC) (reg_val 4)) ])
            (cond [ (not (bveq (state src1) (state src2))) (set-pc state taken_pc) ]
                  [ else                                   (set-pc state  next_pc) ]
            ) ; /cond
        )     ; /let
    ]

    [   ; BLT : Branch If Less Than (Signed)
        (5_BLT src1 src2 imm12)
        (let ([ taken_pc (bvadd (state PC) (SXT_16 imm12)) ]
              [ next_pc  (bvadd (state PC) (reg_val 4)) ])
            (cond [ (bvslt (state src1) (state src2)) (set-pc state taken_pc) ]
                  [ else                              (set-pc state  next_pc) ]
            ) ; /cond
        )     ; /let
    ]

    [   ; BGE : Branch If Greater Than or Equal (Signed)
        (5_BGE src1 src2 imm12)
        (let ([ taken_pc (bvadd (state PC) (SXT_16 imm12)) ]
              [ next_pc  (bvadd (state PC) (reg_val 4)) ])
            (cond [ (bvsge (state src1) (state src2)) (set-pc state taken_pc) ]
                  [ else                              (set-pc state  next_pc) ]
            ) ; /cond
        )     ; /let
    ]

    [   ; BLTU : Branch If Less Than (Unsigned)
        (5_BLTU src1 src2 imm12)
        (let ([ taken_pc (bvadd (state PC) (SXT_16 imm12)) ]
              [ next_pc  (bvadd (state PC) (reg_val 4)) ])
            (cond [ (bvult (state src1) (state src2)) (set-pc state taken_pc) ]
                  [ else                              (set-pc state  next_pc) ]
            ) ; /cond
        )     ; /let
    ]

    [   ; BGEU : Branch If Greater Than or Equal (Unsigned)
        (5_BGEU src1 src2 imm12)
        (let ([ taken_pc (bvadd (state PC) (SXT_16 imm12)) ]
              [ next_pc  (bvadd (state PC) (reg_val 4)) ])
            (cond [ (bvuge (state src1) (state src2)) (set-pc state taken_pc) ]
                  [ else                              (set-pc state  next_pc) ]
            ) ; /cond
        )     ; /let
    ]


    ;; JUMP
    [   ; JAL : Jump and Link
        (5_JAL imm16 dst)
        (let ([ old_pc (bvadd (state PC) (reg_val 4)) ]
              [ new_pc imm16 ])
            ; PC <- imm16
            (set-pc
                ; dst <- PC + 4
                (set-register state dst old_pc)
                new_pc
            )
        )
    ]

    [   ; JALR : Jump and Link Register
        (5_JALR src1 imm12 dst) ;;; PC + 4 -> dst, (src1 + SXT(imm12)) & ~1 -> PC
        (let ([ old_pc (bvadd (state PC) (reg_val 4)) ]
              [ new_pc (bvand (bvadd (state src1) (SXT_16 imm12)) (reg_val -1)) ])
            ; PC <- (src1 + SXT(imm12)) & ~1
            (set-pc
                ; dst <- PC + 4
                (set-register state dst old_pc)
                new_pc
            )

        )
    ]

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
    [   ; SLLI : Shift Left Logical Immediate
        (5_SLLI src1 imm4 dst)
        (set-register state dst (bvshl (state src1) (SXT_16 imm4)))   ]

    [   ; SRLI : Shift Right Logical Immediate
        (5_SRLI src1 imm4 dst)
        (set-register state dst (bvlshr (state src1 (SXT_16 imm4))))    ]

    [   ; SRAI : Shift Right Arithmetic Immediate
        (5_SRAI src1 imm4 dst)
        (set-register state dst (bvashr (state src1 (SXT_16 imm4))))   ]

    ;; STORE
    [
        ; SB    : Store Byte
        (5_SB src1 src2 imm5) ;;; src2[7:0] -> MEM[src1 + SXT(imm5)][7:0]
        (let* ([ addr    (bvadd (state src1) (SXT_16 imm5)) ]
               [ data    (GET_LOW8 (state src2)) ]
               [ next_pc (bvadd (state PC) (reg_val 4)) ])
            (set-pc
                (set-memory
                    state
                    addr
                    data
                ) ; /set-memory
                next_pc
            ) ; /set-pc
        ) ; /let
    ]

    [   ; SH    : Store Halfword
        (5_SH src1 src2 imm5) ;;; src2[15:0] -> MEM[src1 + SXT(imm5)][15:0]
        (let* ([ addr_low  (bvadd     (state src1) (SXT_16 imm5)) ]
               [ addr_high (bvadd     (addr_low) (addr 1)) ]
               [ data_low  (GET_LOW8  (state src2)) ]
               [ data_high (GET_HIGH8 (state src2)) ]
               [ next_pc   (bvadd (state PC) (reg_val 4)) ])
            (set-pc
                (set-memory
                    (set-memory
                        state
                        addr_high
                        data_high
                    ) ; /set-memory
                    addr_low
                    data_low
                ) ; /set-memory
                next_pc
            ) ; /set-pc
        ) ; /let
    ]
    ) ; /destruct
)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------------ SYMBOLICS ------------------------------------------ ;
; ----------------------------------------------------------------------------------------------- ;

(define (??riscv_instr)
    ; Generate a single symbolic RISC-V instruction, which Rosette will try to fill in.
    ;
    ; Returns:
    ;     inst? : A symbolic RISC-V instruction.

    (define-symbolic*  src1 src2 dst reg_val?)
    (define-symbolic*  imm4    imm4?)
    (define-symbolic*  imm5    imm5?)
    (define-symbolic* imm12   imm12?)
    (define-symbolic* imm16   imm16?)
    (choose*
        ;; ARITHMETIC
        (5_ADD  src1  src2   dst)
        (5_ADDI src1  imm5   dst)
        ;; BIT MANIPULATION
        (5_AND  src1  src2   dst)
        (5_ANDI src1  imm5   dst)
        (5_XOR  src1  src2   dst)
        (5_XORI src1  imm5   dst)
        ;; BRANCH
        ; (5_BEQ  src1  src2 imm12)
        ; (5_BNE  src1  src2 imm12)
        ; (5_BLT  src1  src2 imm12)
        ; (5_BGE  src1  src2 imm12)
        ; (5_BLTU src1  src2 imm12)
        ; (5_BGEU src1  src2 imm12)
        ;; JUMP
        ; (5_JAL       imm16   dst)
        ; (5_JALR src1 imm12   dst)
        ;; LOAD
        (5_LB   src1 imm12   dst)
        (5_LH   src1 imm12   dst)
        ; ;; SHIFT
        (5_SLLI src1  imm4   dst)
        (5_SRLI src1  imm4   dst)
        (5_SRAI src1  imm4   dst)
        ; ;; STORE
        (5_SB   src1  src2  imm5)
        (5_SH   src1  src2  imm5)
    ) ; /choose*
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

(define (eval-riscv-prog prog state)
    (define final-state (eval-riscv-prog* prog state))
    (final-state x0)
)