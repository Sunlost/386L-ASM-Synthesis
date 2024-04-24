#lang rosette/safe

(require rosette/lib/destruct)
(require "isa/lc3b.rkt" "isa/riscv.rkt" "state.rkt")

; (provide map-compile-riscv map-compile-lc3b)

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------- INSTRUCTION MAPPINGS -------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Set up cross-synthesis functions that use direct mappings to create an equivalent program
; in the other ISA.

(define (map-lc3b-to-riscv-instr instr xCC)
    ; Map an LC-3b instruction to one or more equivalent
    ; RISC-V instructions.
    ;
    ; Parameters:
    ;     instr  : The LC-3b instruction
    ;     xCC    : The register which the last condition-code
    ;              setting instruction wrote to.
    ;
    ; Returns:
    ;     instr' : The equivalent RISC-V instruction(s)
    ;     xCC'   : If instr sets the condition code, is
    ;              instr's destination register. Otherwise,
    ;              equals xCC.
    ;
    ; Notes:
    ;     - For branches, the mapped RISC-V instruction
    ;       reads xCC and compares it to xZ (which always
    ;       has value 0) to infer what the condition code
    ;       would have been.
    ;
    ; TODOs:
    ;     - Handle JSR/JSRR, which overwrites R7 but does
    ;       not set the condition code.
    ;           - Exclude JSR/JSRR?
    ;             (easiest, but worst)
    ;           - Re-interpret JSR as pushing R7 onto the
    ;             stack?
    ;             (easy, but slow)
    ;           - Track condition code separately from
    ;             the register contents?
    ;             (hard, but fast)
    ;    - Handle LEA

    (destruct instr
        ;; ARITHMETIC
        ;;     immediate 5-bit -> 5-bit
        [ (3_ADD  src1 src2 dst) (5_ADD  src1 src2 dst) ]
        [ (3_ADDI src1 imm5 dst) (5_ADDI src1 imm5 dst) ]

        ;; BIT MANIPULATION
        ;;     immediate 5-bit -> 5-bit
        [ (3_AND  src1 src2 dst) (5_AND  src1 src2 dst) ]
        [ (3_ANDI src1 imm5 dst) (5_ANDI src1 imm5 dst) ]
        [ (3_NOT  src1 dst)      (5_XORI src1  dst  -1) ] ; 5_NOT identical to 5_XORI w/ imm5 = -1
        [ (3_XOR  src1 src2 dst) (5_XOR  src1 src2 dst) ]
        [ (3_XORI src1 imm5 dst) (5_XORI src1 imm5 dst) ]

        ;; BRANCH (w/ COND)
        ;;     immediate  : 9-bit -> 12-bit
        ;;     resolution : condition code -> evaluation
        [ (3_BR     imm9) (5_JAL imm9  xZ)      ] ; BR    : unconditional branch
        [ (3_BR_N   imm9) (5_BLT  xCC  xZ imm9) ] ; BRn   : xCC <  0
        [ (3_BR_Z   imm9) (5_BEQ  xCC  xZ imm9) ] ; BRz   : xCC == 0
        [ (3_BR_P   imm9) (5_BLT   xZ xCC imm9) ] ; BRp   : xCC >  0
        [ (3_BR_NP  imm9) (5_BNE  xCC  xZ imm9) ] ; BRnp  : xCC != 0
        [ (3_BR_ZP  imm9) (5_BGE  xCC  xZ imm9) ] ; BRzp  : xCC >= 0
        [ (3_BR_NZ  imm9) (5_BGE   xZ xCC imm9) ] ; BRnz  : xCC <= 0
        [ (3_BR_NZP imm9) (5_JAL imm9  xZ)      ] ; BRnzp : unconditional branch

        ;; JUMP
        ;;     TODO: Implement JSR / JSRR
        [ (3_JMP    src) (5_JALR src 0 xZ) ] ; JMP  : src -> PC
        [ (3_RET      _) (5_JALR  x7 0 xZ) ] ; RET  : x7  -> PC
        [ (3_JSR  imm11) (NO_OP) ]           ; JSR  : PC + 4 -> x7, PC + (SXT(imm11) << 1) -> PC
        [ (3_JSRR   src) (NO_OP) ]           ; JSRR : PC + 4 -> x7, src -> PC

        ;; LOAD (TODO: LEA)
        ;;     immediate : 6-bit -> 12-bit
        ;;     TODO: Implement LEA
        [ (3_LDB src1 imm6 dst) (5_LB src1 imm6 dst) ]
        [ (3_LDW src1 imm6 dst) (5_LH src1 imm6 dst) ]
        [ (3_LEA imm9 dst)      (NO_OP) ]

        ;; SHIFT
        ;;    immediate : 4-bit -> 4-bit
        [ (3_LSHF  src1 imm4 dst) (5_SLLI src1 imm4 dst) ]
        [ (3_RSHFL src1 imm4 dst) (5_SRLI src1 imm4 dst) ]
        [ (3_RSHFA src1 imm4 dst) (5_SRAI src1 imm4 dst) ]

        ;; STORE
        ;;     immediate : 6-bit -> 5-bit
        ;;     TODO: Handle immediates > 5 bits (RISC-V limited)
        [ (3_STB base src1 imm6) (5_SB base src1 imm6) ]
        [ (3_STW base src1 imm6) (5_SH base src1 imm6) ]
    ) ; /destruct
)

(define (map-riscv-to-lc3b-instr instr)
    ; Map a RISC-V instruction to one or more equivalent
    ; LC-3b instructions.
    ;
    ; Parameters:
    ;     instr  : The RISC-V instruction
    ;
    ; Returns:
    ;     instr' : The equivalent LC-3b instruction(s)

    (destruct instr
        ;; ARITHMETIC
        ;;     immediate 5-bit -> 5-bit
        [ (5_ADD  src1 src2 dst) (3_ADD  src1 src2 dst) ]
        [ (5_ADDI src1 imm5 dst) (3_ADDI src1 imm5 dst) ]
    
        ;; BIT MANIPULATION
        ;;     immediate 5-bit -> 5-bit
        [ (5_AND  src1 src2 dst) (3_AND  src1 src2 dst) ]
        [ (5_ANDI src1 imm5 dst) (3_ANDI src1 imm5 dst) ]
        [ (5_XOR  src1 src2 dst) (3_XOR  src1 src2 dst) ]
        [ (5_XORI src1 imm5 dst) (3_XORI src1 imm5 dst) ]

        ;; BRANCH
        ;;     immediate  : 12-bit -> 9-bit
        ;;     resolution : evaluation -> condition code
        ;;     TODO: Implement
        ;;     TODO: Handle immediates > 9 bits (LC-3b limited)

        ;; JUMP
        ;;     TODO: Implement

        ;; LOAD (TODO: LEA)
        ;;     immediate : 12-bit -> 6-bit
        ;;     TODO: Handle immediates > 6 bits (LC-3b limited)
        [ (5_LB src1 imm12 dst) (3_LDB src1 imm12 dst) ]
        [ (5_LH src1 imm12 dst) (3_LDW src1 imm12 dst) ]

        ;; SHIFT
        ;;    immediate : 4-bit -> 4-bit
        [ (5_SLLI src1 imm4 dst) (3_LSHF  src1 imm4 dst) ]
        [ (5_SRLI src1 imm4 dst) (3_RSHFL src1 imm4 dst) ]
        [ (5_SRAI src1 imm4 dst) (3_RSHFA src1 imm4 dst) ]
    
        ;; STORE
        ;;     immediate 5-bit -> 6-bit
        [ (5_SB src1 src2 imm5) (3_STB src1 src2 imm5) ]
        [ (5_SH src1 src2 imm5) (3_STW src1 src2 imm5) ]
    ) ; /destruct
)





