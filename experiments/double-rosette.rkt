#lang rosette/safe

; Add a symbolic bitvector to itself in Rosette. What do we get?

(define (add_yourself x)
  (bvadd x x)
)

(define reg_val? (bitvector 16))
(define-symbolic input_x0 reg_val?)

(displayln (format "x0 Before   : ~a" input_x0))

(displayln (format "x0 After    : ~a\n" (add_yourself input_x0)))


; Trying example from lecture notes

(define (absv x)
  (if (< x 0) (- x) x))

(define-symbolic y integer?)

(displayln (format "y  Before   : ~a" y))

(displayln (format "y  After    : ~a" (absv y)))

; Solve a constraint saying |y| = 5.

(define result
    (solve
        (assert (= (absv y) 5)))
)

(displayln (format "y  Result 1 :\n~a" result))

(define result2
    (solve
        (assert (= (absv y) -5)))
)

(displayln (format "y  Result 2 :\n~a" result2))