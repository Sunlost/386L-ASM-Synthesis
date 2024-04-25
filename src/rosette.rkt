#lang rosette/safe

(require racket/string)
(require rosette/lib/angelic)
(require "isa/lc3b.rkt" "isa/riscv.rkt" "state.rkt")

(provide rosette-compile)

; ----------------------------------------------------------------------------------------------- ;
; ------------------------------------- ROSETTE MAPPINGS ---------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; Set up cross-synthesis functions that use Rosette to create an equivalent program
; in the other ISA.

(define (??lc3b_instr)
    ; Generate a single symbolic LC-3B instruction, which Rosette will try to fill in.
    ;
    ; Returns:
    ;     inst : A symbolic LC-3b instruction.
    (define-symbolic*  src1 reg_val?)
    (define-symbolic*  src2 reg_val?)
    (define-symbolic*   dst reg_val?)
    (define-symbolic*  imm4    imm4?)
    (define-symbolic*  imm5    imm5?)
    (define-symbolic*  imm6    imm6?)
    (define-symbolic*  imm9    imm9?)
    (define-symbolic* imm11   imm11?)
    (define-symbolic*  base reg_val?)
    (choose*
        ;; ARITHMETIC
        (3_ADD     src1 src2  dst)
        (3_ADDI    src1 imm5  dst)
        ;; BIT MANIPULATION
        (3_AND     src1 src2  dst)
        (3_ANDI    src1 imm5  dst)
        (3_NOT          src1  dst)
        (3_XOR     src1 src2  dst)
        (3_XORI    src1 imm5  dst)
        ;; BRANCH (w/ COND)
        (3_BR                imm9)
        (3_BR_N              imm9)
        (3_BR_Z              imm9)
        (3_BR_P              imm9)
        (3_BR_NP             imm9)
        (3_BR_ZP             imm9)
        (3_BR_NZ             imm9)
        (3_BR_NZP            imm9)
        ;; JUMP
        (3_JMP               base)
        (3_RET                   )
        (3_JSR              imm11)
        (3_JSRR              base)
        ;; LOAD
        (3_LDB     base imm6  dst)
        (3_LDW     base imm6  dst)
        (3_LEA          imm9  dst)
        ;; SHIFT
        (3_LSHF    src1 imm4  dst)
        (3_RSHFL   src1 imm4  dst)
        (3_RSHFA   src1 imm4  dst)
        ;; STORE
        (3_STB     base src1 imm6)
        (3_STW     base src1 imm6)
    ) ; /choose*
)

(define (??riscv_instr)
    ; Generate a single symbolic RISC-V instruction, which Rosette will try to fill in.
    ;
    ; Returns:
    ;     inst : A symbolic RISC-V instruction.
    (define-symbolic*  src1 reg_val?)
    (define-symbolic*  src2 reg_val?)
    (define-symbolic*   dst reg_val?)
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

; ----------------------------------------------------------------------------------------------- ;
; -------------------------------------- ROSETTE HELPERS ---------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define (gen-list-instr-n isa n)
    ; Generate a length-n list of symbolic instructions.
    ;
    ; Parameters:
    ;     isa : The Rosette mapping of instructions to use.
    ;           {??lc3b_instr, ??riscv_instr}
    ;     n   : The desired length of the list.
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
        (cons (isa) (gen-list-instr-n isa (- n 1)))
    )
)

(define (get-isa isa_string)
    ; Get the Rosette mapping for an ISA.
    ;
    ; Parameters:
    ;    isa_string : The ISA name as a string.
    ;                 {"lc3b", "riscv"}
    ;
    ; Returns:
    ;    isa : The Rosette mapping of instructions to use.
    ;          {??lc3b_instr, ??riscv_instr}

    (case isa_string
        (("lc3b")  ??lc3b_instr)
        (("riscv") ??riscv_instr)
    )
)

(define (get-eval-prog isa_string)
    ; Get the program evaluation function for an ISA.
    ;
    ; Parameters:
    ;    isa_string : The ISA name as a string.
    ;                 {"lc3b", "riscv"}
    ;
    ; Returns:
    ;    eval_prog : The program evaluation function
    ;                for the ISA.
    ;                {eval-riscv-prog, eval-lc3b-prog}

    (case isa_string
        (("lc3b")  eval-lc3b-prog)
        (("riscv") eval-riscv-prog)
    )
)

; ----------------------------------------------------------------------------------------------- ;
; --------------------------- ROSETTE SYNTHESIS (LC-3b -> RISC-V) ------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

(define (rosette-compile* source_isa target_isa source_prog initial_state n)
    ; Compile an equivalent program in the target ISA, from
    ; a program written in some source ISA, using Rosette.
    ;
    ; Parameters:
    ;     source_isa    : The source ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     target_isa    : The target ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     source_prog   : The list of instructions representing
    ;                     the program in the source ISA.
    ;     initial_state : The initial state of the system
    ;     n             : The desired number of instructions in
    ;                     the compiled program.
    ;
    ; Returns (if satisfiable):
    ;     target_prog : A list of instructions representing
    ;                   a semantically-equivalent program
    ;                   in the target ISA.
    ;
    ; Returns (if not satisfiable):
    ;     #f
    ;
    ; Rosette assumes that the program output is located in
    ; x0 upon reaching a HLT instruction.

    (displayln
        (format "[rosette-compile] ~a -> ~a n = ~a" source_isa target_isa n)
    )
    (displayln
        (format "[DEBUG] prog = ~a" source_prog)
    )

    (define target_prog      (gen-list-instr-n (get-isa target_isa) n))
    (define target_eval_prog (get-eval-prog target_isa))
    (define source_eval_prog (get-eval-prog source_isa))
    (define M
        (synthesize
            #:forall    (list input_r0)
            #:guarantee (assert 
                ( equal?
                    (target_eval_prog target_prog initial_state) 
                    (source_eval_prog source_prog initial_state)
                )
            ) ; /#:guarantee
        )     ; /synthesize
    )         ; /define
    (if
        ; IF   : Satisfiable
        (sat? M)
        ; THEN : Return the compiled program
        (evaluate target_prog M)
        ; ELSE : Return #f
        #f
    )
)

(define (rosette-compile-wrapper source_isa target_isa source_prog initial_state n)
    ; Wrapper around rosette-compile*.
    ;
    ; Parameters:
    ;     source_isa    : The source ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     target_isa    : The target ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     source_prog   : The list of instructions representing
    ;                     the program in the source ISA.
    ;     initial_state : The initial state of the system
    ;     n             : The desired number of instructions in
    ;                     the compiled program.
    ;
    ; Returns (if halts):
    ;     target_prog : A list of instructions representing
    ;                   a semantically-equivalent program
    ;                   in the target ISA.
    ;
    ; The program will repeatedly try larger values of n on
    ; rosette-compile* until a satisfiable program is found.

    (define target_prog
        (rosette-compile*
            source_isa
            target_isa
            source_prog
            initial_state
            n
        )
    )
    (if
        ; IF   : Compiling with n instructions failed
        (equal? target_prog #f)
        ; THEN : Try again with n + 1 instructions
        (rosette-compile-wrapper
            source_isa
            target_isa
            source_prog
            initial_state
            (+ n 1)
        )
        ; ELSE : Return the compiled program
        target_prog
    )
)


(define (rosette-compile source_isa target_isa source_prog initial_state)
    ; Compile an equivalent program in the target ISA, from
    ; a program written in some source ISA, using Rosette.
    ;
    ; Parameters:
    ;     source_isa    : The source ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     target_isa    : The target ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     source_prog   : The list of instructions representing
    ;                     the program in the source ISA.
    ;     initial_state : The initial state of the system
    ;
    ; Returns (if halts):
    ;     target_prog : A list of instructions representing
    ;                   a semantically-equivalent program
    ;                   in the target ISA.
    ;
    ; The program will use rosette-compile-wrapper, which
    ; repeatedly tries larger values of n on rosette-compile*
    ; until a satisfiable program is found.

    (rosette-compile-wrapper
        source_isa
        target_isa
        source_prog
        initial_state
        1
    )
)
