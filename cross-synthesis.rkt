#lang rosette/safe

(require rosette/lib/destruct rosette/lib/angelic rosette/lib/roseunit rackunit (only-in racket for/list))
(define TODO 42)  ; Just an arbitrary value for TODOs to return

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; define LC-3b ASM instructions
    ; template: (struct 3_ () #:transparent)
    ; SXT is sign-extend to 16 bits

;; ARITHMETIC
(struct 3_ADD (src1 src2 dst) #:transparent)     ;;; src1 + src2 -> dst
(struct 3_ADDI (src1 imm5 dst) #:transparent)    ;;; src1 + imm5 -> dst

;; BIT MANIPULATION
(struct 3_AND (src1 src2 dst) #:transparent)     ;;; src1 & src2 -> dst
(struct 3_ANDI (src1 imm5 dst) #:transparent)    ;;; src1 & imm5 -> dst
(struct 3_NOT (src1 dst) #:transparent)          ;;; ~src1 -> dst
(struct 3_XOR (src1 src2 dst) #:transparent)     ;;; src1 ^ src2 -> dst
(struct 3_XORI (src1 imm5 dst) #:transparent)    ;;; src1 ^ imm5 -> dst

;; BRANCH (w/ COND)
    ;;; (struct 3_BR (...) #:transparent)
    ;;; (struct 3_BR_N (...) #:transparent)
    ;;; (struct 3_BR_Z (...) #:transparent)
    ;;; (struct 3_BR_P (...) #:transparent)
    ;;; (struct 3_BR_NP (...) #:transparent)
    ;;; (struct 3_BR_ZP (...) #:transparent)
    ;;; (struct 3_BR_NZ (...) #:transparent)
    ;;; (struct 3_BR_NZP (...) #:transparent)

;; JUMP
(struct 3_JMP (src1) #:transparent)              ;;; src1 -> PC
(struct 3_RET (_) #:transparent)                 ;;; R7 -> PC
(struct 3_JSR (imm11) #:transparent)             ;;; PC + 4 -> R7, PC + (SXT(imm11) << 1) -> PC
(struct 3_JSRR (src1) #:transparent)             ;;; PC + 4 -> R7, src1 -> PC

;; LOAD
(struct 3_LDB (src1 imm6 dst) #:transparent)     ;;; MEM[src1 + SXT(imm6)] -> dst
(struct 3_LDW (src1 imm6 dst) #:transparent)     ;;; MEM[src1 + (SXT(imm6) << 1)] -> dst
(struct 3_LEA (imm9, dst) #:transparent)         ;;; MEM[PC + (SXT(imm9) << 1)] -> dst

;; SHIFT
(struct 3_LSHF (src1 imm4 dst) #:transparent)    ;;; src1 << imm4 -> dst
(struct 3_RSHFL (src1 imm4 dst) #:transparent)   ;;; src1 >> imm4 -> dst
(struct 3_RSHFA (src1 imm4 dst) #:transparent)   ;;; src1 >>> imm4 -> dst

;; STORE
(struct 3_STB (src1 imm6) #:transparent)         ;;; src1[7:0] -> MEM[src1 + SXT(imm6)][7:0]
(struct 3_STW (src1 imm6) #:transparent)         ;;; src1 -> MEM[src1 + (SXT(imm6) << 1)]

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; define supported RISC-V instructions
    ; template: (struct 5_ () #:transparent)
    ; SXT(x, y) is sign-extend x to y bits. if undefined, default y = 16.

;; ARITHMETIC
(struct 5_ADD (src1 src2 dst) #:transparent)     ;;; src1 + src2 -> dst
(struct 5_ADDI (src1 imm5 dst) #:transparent)    ;;; src1 + imm5 -> dst

;; BIT MANIPULATION
(struct 5_AND (src1 src2 dst) #:transparent)     ;;; src1 & src2 -> dst
(struct 5_ANDI (src1 imm5 dst) #:transparent)    ;;; src1 & imm5 -> dst
  ;; 5_NOT: pseudo-instr, XORI w/ imm5 = -1      ;;; src1 ^ -1 -> dst
(struct 5_XOR (src1 src2 dst) #:transparent)     ;;; src1 ^ src2 -> dst
(struct 5_XORI (src1 imm5 dst) #:transparent)    ;;; src1 ^ imm5 -> dst

;; BRANCH
    ;;; (struct 5_BEQ () #:transparent)
    ;;; (struct 5_BNE () #:transparent)
    ;;; (struct 5_BLT () #:transparent)
    ;;; (struct 5_BGE () #:transparent)
    ;;; (struct 5_BLTU () #:transparent)
    ;;; (struct 5_BGEU () #:transparent)

;; JUMP
(struct 5_JAL (imm16 dst) #:transparent)         ;;; PC + 4 -> dst, imm16 -> PC
(struct 5_JALR (src1 imm12 dst) #:transparent)   ;;; PC + 4 -> dst, (src1 + SXT(imm12)) & ~1 -> PC

;; LOAD
(struct 5_LB (src1 imm12 dst) #:transparent)     ;;; SXT(MEM[src1 + SXT(imm12)][7:0]) -> dst 
(struct 5_LH (src1 imm12 dst) #:transparent)     ;;; MEM[src1 + SXT(imm12)][15:0] -> dst

;; SHIFT
;;; S (L)eft/(R)ight (A)rithmetic/(L)ogical I
(struct 5_SLLI (src1 imm4 dst) #:transparent)    ;;; src1 << imm4 -> dst
(struct 5_SRLI (src1 imm4 dst) #:transparent)    ;;; src1 >> imm4 -> dst
(struct 5_SRAI (src1 imm4 dst) #:transparent)    ;;; src1 >>> imm4 -> dst

;; STORE
(struct 5_SB (src1 src2 imm5) #:transparent)     ;;; src2[7:0] -> MEM[src1 + SXT(imm5)][7:0]
(struct 5_SH (src1 src2 imm5) #:transparent)     ;;; src2[15:0] -> MEM[src1 + SXT(imm5)][15:0]