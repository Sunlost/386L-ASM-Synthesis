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
(struct 3_LDB (base imm6 dst) #:transparent)     ;;; SXT(MEM[base + SXT(imm6)]) -> dst
(struct 3_LDW (base imm6 dst) #:transparent)     ;;; MEM[base + (SXT(imm6) << 1)] -> dst
(struct 3_LEA (imm9 dst) #:transparent)          ;;; MEM[PC + (SXT(imm9) << 1)] -> dst

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

; define register names
(define x0 0)
(define x1 1)
(define x2 2)
(define x3 3)
(define x4 4)
(define x5 5)
(define x6 6)
(define x7 7)

; define PC register name
(define PC 8)

; halt instruction
(define-symbolic HLT integer?)

; define function to set machine state
(define (set-state state loc val)
  (lambda (r) (if (equal? loc r) val (state r)))
)

; setup initial machine state
(define initial-state (set-state (lambda (r) 0) 0 (bv 0 16)))



; convenience functions

; sign extend a bitvector to 16 bits
(define (SXT_16 vec)
    (sign-extend vec (bitvector 16))
)

; shift a bitvector left by 1 bit
(define (L_SH_1 vec)
    (bvshl vec (bv 1 16))
)

; mask off bits, leaving only the high 8 bits
(define (MASK_HIGH8 vec)
    (bvand vec (bv #b1111111100000000 16))
)

; mask off bits, leaving only the low 8 bits
(define (MASK_LOW8 vec)
    (bvand vec (bv #b0000000011111111 16))
)

; return a 16-length bitvector containing val (for readability later)
    ; yes these could absolutely call one common function which takes in val and the length...
    ; but this is for readability. adding another layer doesn't make sense here.
(define (addr val) (bv val 16))
(define (val val) (bv val 16))
(define (offset val) (bv val 16))
(define (imm4 val) (bv val 4))
(define (imm5 val) (bv val 5))
(define (imm6 val) (bv val 6))
(define (imm11 val) (bv val 11))
(define (imm12 val) (bv val 12))

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; set up evaluation functions for both LC-3b and RISC-V

(define (eval-lc3b-instr instr state)
    ; create a new pc val (pc + 4)
    ; edit it when processing instruction (if the instr alters pc)
    ; set new pc after
    (let ((new_pc (bvadd (state PC) (offset 4))))
        (set-state
            (destruct instr
            ;; ARITHMETIC
                [   (3_ADD src1 src2 dst)
                    (set-state state dst (bvadd (state src1) (state src2)))   ]

                [   (3_ADDI src1 imm5 dst)
                    (set-state state dst (bvadd (state src1) (SXT_16 imm5)))   ]

            ;; BIT MANIPULATION
                [   (3_AND src1 src2 dst)
                    (set-state state dst (bvand (state src1) (state src2)))   ]

                [   (3_ANDI src1 imm5 dst)
                    (set-state state dst (bvand (state src1) (SXT_16 imm5)))   ]

                [   (3_NOT src1 dst)
                    (set-state state dst (bvnot (state src1)))   ]

                [   (3_XOR src1 src2 dst)
                    (set-state state dst (bvxor (state src1) (state src2)))   ]

                [   (3_XORI src1 imm5 dst)
                    (set-state state dst (bvxor (state src1) (SXT_16 imm5)))   ]

            ;; BRANCH (w/ COND)
                ;;; (TODO)

            ;; JUMP
                [   (3_JMP base)
                    (set! new_pc (state base))   ]

                [   (3_RET _)
                    (set! new_pc (state x7))   ]

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
                ;;; in future, these should also set condition codes based on value loaded
                [   (3_LDB src1 imm6 dst) 
                    (set-state state dst (SXT_16 (MASK_LOW8 (state (bvadd (state src1) (SXT_16 imm6))))))   ]

                [   (3_LDW src1 imm6 dst)
                    (set-state state dst (state (bvadd (state src1 ) (L_SH_1 (SXT_16 imm6)))))   ]

                [   (3_LEA imm9 dst)
                    (set-state state dst (state (bvadd (state PC) (L_SH_1 (SXT_16 imm9)))))   ]   

            ;; SHIFT
                [   (3_LSHF src1 imm4 dst)
                    (set-state state dst (bvshl (state src1) (SXT_16 imm4)))   ]

                [   (3_RSHFL src1 imm4 dst)
                    (set-state state dst (bvlshr (state src1 (SXT_16 imm4))))   ]

                [   (3_RSHFA src1 imm4 dst)
                    (set-state state dst (bvashr (state src1 (SXT_16 imm4))))   ]

            ;; STORE
                [   (3_STB base src1 imm6)
                    (set-state state (bvadd (state base) (SXT_16 imm6)) (MASK_LOW8 (state src1)))   ]

                [   (3_STW base src1 imm6)
                    (set-state state (bvadd (state base) (SXT_16 imm6)) (state src1))   ]
            )
        PC new_pc)
    )
)

(define (eval-lc3b-prog state)
  (if
    ; IF: the instr at MEM[PC] is HLT
    (equal? (state (state PC)) HLT)
    ; THEN: we halted, return current state
    state
    ; ELSE: we have an instr to run, recurse
    (eval-lc3b-prog (eval-lc3b-instr (state (state PC)) state))
  )
)

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; test lc3b implementation

; define our starting state for tests run below
(define example-state 
  ; for 8 registers
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 
  ; for PC
  (set-state 
  ; for first 64b of data memory
  (set-state (set-state (set-state (set-state (set-state (set-state (set-state (set-state 

  initial-state 

  x0 (val 0)) 
  x1 (val 5)) 
  x2 (val 7))
  x3 (addr 1000))
  x4 (val 0))
  x5 (addr 1008))
  x6 (val 0)) 
  x7 (val 14)) 

  PC (addr 2000)) ; instruction memory begins at addr 2000

  (addr 1000) (val 32)) 
  (addr 1008) (val 0)) 
  (addr 1016) (val 0)) 
  (addr 1024) (val 0)) 
  (addr 1032) (val 0)) 
  (addr 1040) (val 0)) 
  (addr 1048) (val 0)) 
  (addr 1056) (val 0)) 
)

; base starting state:
; - PC = 2000
; - registers
;     x[0] = 0
;     x[1] = 5
;     x[2] = 7
;     x[3] = 1000
;     x[4] = 0
;     x[5] = 1008
;     x[6] = 0
;     x[7] = 14
; - data memory
;     MEM[1000] = 32
;     MEM[1008] = 0
;     MEM[1016] = 0
;     MEM[1024] = 0
;     MEM[1032] = 0
;     MEM[1040] = 0
;     MEM[1048] = 0
;     MEM[1056] = 0
; - instruction memory
;     [nothing]


; define state for test one
(define test-one-state 
  (set-state (set-state (set-state (set-state (set-state (set-state

  example-state

  (addr 2000) (3_ADD x1 x2 x0))
  (addr 2004) (3_AND x1 x2 x2))
  (addr 2008) (3_LDW x3 (imm6 0) x6))
  (addr 2012) (3_JSR (imm11 4)))
  (addr 2016) (3_LSHF x3 (imm4 1) x4))
  (addr 2020) HLT)
)

(check-equal?
    (bvadd (addr 2012) (L_SH_1 (SXT_16 (imm4 4))))  ;;; JSR on 2012 should skip LSHF on 2016
    (addr 2020)
)

(let ([final-state (eval-lc3b-prog test-one-state)])
    (check-equal?          (final-state x0) (val 12))     ; altered from starting state
    (check-equal?          (final-state x1) (val 5))
    (check-equal?          (final-state x2) (val 5))      ; altered from starting state
    (check-equal?          (final-state x3) (addr 1000))
    (check-equal?          (final-state x4) (val 0))      ; should NOT be changed from starting state
    (check-equal?          (final-state x5) (addr 1008))
    (check-equal?          (final-state x6) (val 32))     ; altered from starting state
    (check-equal?          (final-state x7) (addr 2016))  ; altered from starting state
    (check-equal?          (final-state PC) (addr 2020))  ; altered from starting state
    (check-equal? (final-state (addr 1000)) (val 32))
    (check-equal? (final-state (addr 1008)) (val 0))
    (check-equal? (final-state (addr 1016)) (val 0))
    (check-equal? (final-state (addr 1024)) (val 0))
    (check-equal? (final-state (addr 1032)) (val 0))
    (check-equal? (final-state (addr 1040)) (val 0))
    (check-equal? (final-state (addr 1048)) (val 0))
    (check-equal? (final-state (addr 1056)) (val 0))
)

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define (eval-riscv-instr instr state)
    (TODO)
    ;;; (destruct instr
    ;;;     ; TODO
    ;;; )
)