#lang rosette/safe

(require rosette/lib/angelic)
(require "isa/lc3b.rkt" "isa/riscv.rkt" "state.rkt")

(provide rosette-compile rosette-shrink)

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
    ;     initial_state : The initial state of the machine.
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
    ; (displayln
    ;     (format "[DEBUG] prog = ~a" source_prog)
    ; )

    (define target_prog      (gen-list-instr-n (get-isa target_isa) n))
    (define target_eval_prog (get-eval-prog target_isa))
    (define source_eval_prog (get-eval-prog source_isa))
    (define M
        (synthesize
            #:forall    (list input_x0)
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
    ;     initial_state : The initial state of the machine.
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
    ;     initial_state : The initial state of the machine.
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

(define (rosette-shrink isa prog initial_state)
    ; Compile an equvialent program with the same or fewer
    ; instructions in some ISA, using Rosette.
    ;
    ; Parameters:
    ;     isa           : The ISA name as a string.
    ;                     {"lc3b", "riscv"}
    ;     prog          : The list of instructions representing
    ;                     the program in the ISA.
    ;     initial_state : The initial state of the machine.
    ;
    ; Returns:
    ;     prog'       : A list of instructions representing
    ;                   a semantically-equivalent program
    ;                   in the same ISA, with the same or
    ;                   fewer instructions.
    ;
    ; This program uses rosette-compile, but with the
    ; source and target ISA being the same.

    (rosette-compile
        isa
        isa
        prog
        initial_state
    )
)