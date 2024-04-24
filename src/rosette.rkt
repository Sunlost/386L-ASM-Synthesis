#lang rosette/safe

(require rosette/lib/angelic)
(require "isa/lc3b.rkt" "isa/riscv.rkt" "state.rkt")

(provide rosette-compile-riscv)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- ROSETTE MAPPINGS ---------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Set up cross-synthesis functions that use Rosette to create an equivalent program
; in the other ISA.

(define-symbolic var integer?)

(define (??lc3b_instr)
    ; Generate a single symbolic LC-3B instruction, which Rosette will try to fill in.
    ;
    ; Returns:
    ;     inst : A symbolic LC-3b instruction.

    (define-symbolic* src1 src2 dst imm4 imm5 imm6 imm9 imm11 base arg integer?)
    (choose*
        ;; ARITHMETIC
        (3_ADD    src1 src2 dst)
        (3_ADDI   src1 imm5 dst)
        ;; BIT MANIPULATION
        (3_AND    src1 src2 dst)
        (3_ANDI   src1 imm5 dst)
        (3_NOT    src1 dst)
        (3_XOR    src1 src2 dst)
        (3_XORI   src1 imm5 dst)
        ;; BRANCH (w/ COND)
        (3_BR     imm9)
        (3_BR_N   imm9)
        (3_BR_Z   imm9)
        (3_BR_P   imm9)
        (3_BR_NP  imm9)
        (3_BR_ZP  imm9)
        (3_BR_NZ  imm9)
        (3_BR_NZP imm9)
        ;; JUMP
        (3_JMP    base)
        (3_RET    arg)
        (3_JSR    imm11)
        (3_JSRR   base)
        ;; LOAD
        (3_LDB    base imm6 dst)
        (3_LDW    base imm6 dst)
        (3_LEA    imm9 dst)
        ;; SHIFT
        (3_LSHF   src1 imm4 dst)
        (3_RSHFL  src1 imm4 dst)
        (3_RSHFA  src1 imm4 dst)
        ;; STORE
        (3_STB    base src1 imm6)
        (3_STW    base src1 imm6)
    ) ; /choose*
)

(define (??riscv_instr)
    ; Generate a single symbolic RISC-V instruction, which Rosette will try to fill in.
    ;
    ; Returns:
    ;     inst : A symbolic RISC-V instruction.

    (define-symbolic* src1 src2 dst imm4 imm5 imm12 imm16 integer?)
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
        (5_BEQ  src1  src2 imm12)
        (5_BNE  src1  src2 imm12)
        (5_BLT  src1  src2 imm12)
        (5_BGE  src1  src2 imm12)
        (5_BLTU src1  src2 imm12)
        (5_BGEU src1  src2 imm12)
        ;; JUMP
        (5_JAL       imm16   dst)
        (5_JALR src1 imm12   dst)
        ;; LOAD
        (5_LB   src1 imm12   dst)
        (5_LH   src1 imm12   dst)
        ;; SHIFT
        (5_SLLI src1  imm4   dst)
        (5_SRLI src1  imm4   dst)
        (5_SRAI src1  imm4   dst)
        ;; STORE
        (5_SB   src1  src2  imm5)
        (5_SH   src1  src2  imm5)
    ) ; /choose*
)

(define (gen-list-instr-n instr_type n)
    ; Generate a length-n list of symbolic instructions.
    ;
    ; Parameters:
    ;     instr_type : The type of symbolic instruction to generate. 
    ;                  (??lc3b_instr, ??riscv_instr)
    ;     n          : The desired length of the list.
    ;
    ; Returns:
    ;     insts : A list of n symbolic instructions.

    (if 
        ; IF   : n < 1
        (< n 1) 
        ; THEN : return empty list
        null 
        ; ELSE : Return ??instr appended to list obtained 
        ;        from a recursive call.
        (cons (instr_type) (gen-list-instr-n instr_type (- n 1)))
    )
)

(define (gen-list-riscv-instr-n n)
    ; Generate a length-n list of symbolic RISC-V instructions.
    ;
    ; Parameters:
    ;     n : The desired length of the list.
    ;
    ; Returns:
    ;     insts : A list of n symbolic RISC-V instructions.

    (gen-list-instr-n ??riscv_instr n)
)

(define (gen-list-lc3b-instr-n n)
    ; Generate a length-n list of symbolic LC-3B instructions.
    ;
    ; Parameters:
    ;     n : The desired length of the list.
    ;
    ; Returns:
    ;     insts : A list of n symbolic LC-3B instructions.

    (gen-list-instr-n ??lc3b_instr n)
)

(define (rosette-compile-riscv* lc3b_program n)
    ; Compile an equivalent RISC-V program from an LC-3b program using Rosette.
    ;
    ; Parameters:
    ;     lc3b_program : A list of LC-3b instructions
    ;     n            : The desired number of instructions in the compiled RISC-V program.
    ;
    ; Returns:
    ;     If satisfiable: 
    ;         riscv_program : A list of RISC-V instructions of length n
    ;     Otherwise:
    ;         #f
    (define riscv_program (gen-list-riscv-instr-n n))
    (define M 
        (synthesize
            #:forall    (list var)
            #:guarantee (assert (= (eval-riscv-prog riscv_program) (eval-lc3b-prog lc3b_program)))
        ) ; /synthesize
    )     ; /define
    (if 
        (sat? M) 
        (evaluate riscv_program M) 
        #f
    )
)

(define (rosette-compile-riscv-wrapper lc3b_program n)
    ; Wrapper function that tries to compile the expression with n instructions.
    ; If it cannot, it tries again with one more instruction.
    (displayln 
        (format "[DEBUG][rosette-compile-riscv-wrapper] Trying with ~a instructions" n)
    )
    (define riscv_program (rosette-compile-riscv* lc3b_program n))
    (if
        ; IF   : Compiling with n instructions failed
        (equal? riscv_program #f) 
        ; THEN : Try again with n+1 instructions
        (rosette-compile-riscv-wrapper lc3b_program (+ n 1)) 
        ; ELSE : Return the compiled program
        riscv_program
    )
)

(define (rosette-compile-riscv lc3b_program)
    ; Compile an equivalent RISC-V program from an LC-3b program using Rosette.
    ;
    ; Parameters:
    ;     lc3b_program : A list of LC-3b instructions
    ;
    ; Returns:
    ;     riscv_program : A list of RISC-V instructions
    ;
    ; We iteratively try to increase the value of n for rosette-compile-riscv* using
    ; the rosette-compile-riscv-wrapper. function rosette-compile-riscv-wrapper.

    (rosette-compile-riscv-wrapper lc3b_program 1)
)
