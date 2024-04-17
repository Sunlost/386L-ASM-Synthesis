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
(struct 3_JMP (base) #:transparent)              ;;; base -> PC
(struct 3_RET (_) #:transparent)                 ;;; R7 -> PC
(struct 3_JSR (imm11) #:transparent)             ;;; PC + 4 -> R7, PC + (SXT(imm11) << 1) -> PC
(struct 3_JSRR (base) #:transparent)             ;;; PC + 4 -> R7, base -> PC

;; LOAD
(struct 3_LDB (base imm6 dst) #:transparent)     ;;; MEM[base + SXT(imm6)] -> dst
(struct 3_LDW (base imm6 dst) #:transparent)     ;;; MEM[base + (SXT(imm6) << 1)] -> dst
(struct 3_LEA (imm9, dst) #:transparent)         ;;; MEM[PC + (SXT(imm9) << 1)] -> dst

;; SHIFT
(struct 3_LSHF (src1 imm4 dst) #:transparent)    ;;; src1 << imm4 -> dst
(struct 3_RSHFL (src1 imm4 dst) #:transparent)   ;;; src1 >> imm4 -> dst
(struct 3_RSHFA (src1 imm4 dst) #:transparent)   ;;; src1 >>> imm4 -> dst

;; STORE
(struct 3_STB (base src1 imm6) #:transparent)    ;;; src1[7:0] -> MEM[base + SXT(imm6)][7:0]
(struct 3_STW (base src1 imm6) #:transparent)    ;;; src1 -> MEM[base + (SXT(imm6) << 1)]

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

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; define stuff common to both ISAs

; symbolic variable `var` representing the input to our program.
(define-symbolic var integer?)

; halt instruction
(struct HALT (_) #:transparent)

; define PC register as register #8 since LC-3b only has regs 0-7.
(define PC 8)

; create some kind of bit representation infrastructure

    ;; TODO !!

; write common bit-manipulation functions used by both ISAs

; bitwise AND
(define (AND val_a val_b)
    (TODO)
)

; bitwise XOR
(define (XOR val_a val_b)
    (TODO)
)

; bitwise NOT
(define (NOT val_a)
    ; just invokes XOR with val_b == -1
    (TODO)
)

; shift val_a left by one bit
(define (LS_1b val_a)
    (TODO)
)

; sign extend val to 16 bits
(define (SXT val_a)
    (TODO)
)

; mask off all but first 8 bits
(define (MASK_HIGH8 val_a)
    (TODO)
)

; mask off all but last 8 bits
(define (MASK_LOW8 val_a)
    (TODO)
)

; return bit at bit_num pos
(define (GET_BIT bit_num val_a)
    (TODO)
)

; define common machine infrastructure

; set register to val
(define (set-state state reg val)
  (lambda (r) (if (equal? dst r) val (state r))())
)

; set lower 8 bits of register to val
(define (set-state-lower8 state reg val)
    ;;; (lambda (r) (if (equal? dst r) val (state r))())
    (TODO) ;;; have to set lower 8 bits only
)

; load memory
(define (load-memory state mem_addr)
    (TODO)
)

; set memory
(define (set-memory-1b state mem_addr val)
    (TODO)
)

(define (set-memory-8b state mem_addr val)
    ;; does this even work? who knows.
    (set-memory-1b state    mem_addr    (GET_BIT 0 val))
    (set-memory-1b state (+ mem_addr 1) (GET_BIT 1 val))
    (set-memory-1b state (+ mem_addr 2) (GET_BIT 2 val))
    (set-memory-1b state (+ mem_addr 3) (GET_BIT 3 val))
    (set-memory-1b state (+ mem_addr 4) (GET_BIT 4 val))
    (set-memory-1b state (+ mem_addr 5) (GET_BIT 5 val))
    (set-memory-1b state (+ mem_addr 6) (GET_BIT 6 val))
    (set-memory-1b state (+ mem_addr 7) (GET_BIT 7 val))
)

(define (set-memory state mem_addr val)
    ;; does this even work? who knows.
    (set-memory-8b state    mem_addr    (MASK_HIGH8 val))
    (set-memory-8b state (+ mem_addr 8) (MASK_LOW8 val))
)

; setup initial machine state
(define initial-state 
    (set-state (lambda (r) 0) 0 var) 
    (set-memory (lambda (r) 0) 0 var)
)

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; set up evaluation functions for both LC-3b and RISC-V

(define (eval-lc3b-instr instr state)
    (destruct instr
      ;; ARITHMETIC
        [ (3_ADD src1 src2 dst)
            (set-state state dst (+ (state src1) (state src2))) ]
        [ (3_ADDI src1 imm5 dst)
            (set-state state dst (+ (state src1) (imm5))) ]
      ;; BIT MANIPULATION
        [ (3_AND src1 src2 dst)
            (set-state state dst (AND (state src1) (state src2))) ]
        [ (3_ANDI src1 imm5 dst)
            (set-state state dst (AND (state src1) (imm5))) ]
        [ (3_NOT src1 dst)
            (set-state state dst (NOT (state src1))) ]
        [ (3_XOR src1 src2 dst)
            (set-state state dst (XOR (state src1) (state src2))) ]
        [ (3_XORI src1 imm5 dst)
            (set-state state dst (XOR (state src1) (imm5))) ]
      ;; BRANCH (w/ COND)
        ;;; (TODO)
      ;; JUMP
        [ (3_JMP base)
            (set-state state PC (state base)) ]
        [ (3_RET _)
            (set-state state PC (state 7)) ]
        [ (3_JSR imm11)
            ; NOTE: no idea if i can do multiple effects of an instruction like this
            (set-state state 7 (+ (state PC) 4))
            (set-state state PC (+ (state PC) (LS_1b (SXT imm11)))) ]
        [ (3_JSRR base)
            ; NOTE: no idea if i can do multiple effects of an instruction like this
            (set-state state 7 (+ (state PC) 4))
            (set-state state PC (state base)) ]
      ;; LOAD
        [ (3_LDB src1 imm6 dst)
            (set-state state dst (MASK_8 (load-memory (+ (state src1) (SXT imm6))))) ]
        [ (3_LDW src1 imm6 dst)
            (set-state state dst (load-memory (+ (state src1) (LS_1b (SXT imm6))))) ]
        [ (3_LEA imm9, dst)
            (set-state state dst (load-memory (+ (state PC) (LS_1b (SXT imm9))))) ]
      ;; SHIFT
        [ (3_LSHF src1 imm4 dst)
            (TODO) ]
        [ (3_RSHFL src1 imm4 dst)
            (TODO) ]
        [ (3_RSHFA src1 imm4 dst)
            (TODO) ]
      ;; STORE
        [ (3_STB base src1 imm6)
            (set-memory-8b state (+ (state base) (SXT imm6))) (MASK_8 (state src1)) ]
        [ (3_STW base src1 imm6)
            (set-memory state (+ (state base) (SXT imm6))) (state src1) ]
      ;; HALT
        [ (HLT _)
            (TODO) ]
    )
)



(define (eval-riscv-instr instr state)
    (TODO)
    ;;; (destruct instr
    ;;;     ; TODO
    ;;; )
)