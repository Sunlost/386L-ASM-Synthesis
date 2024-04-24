#lang rosette/safe

(provide (all-defined-out))

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;

; 
; Utility functions for use in other modules.
;

(define (SXT_16 vec)
    ; Sign extend a bit-vector to 16 bits
    (sign-extend vec (bitvector 16))
)

(define (ZXT_16 vec)
    ; Zero extend a bit-vector to 16 bits
    (zero-extend vec (bitvector 16))
)

(define (CAT_16 vec1 vec2)
    ; Concatenate two 8-bit bit-vectors into a 16-bit bit-vector
    (bvadd
        (L_SH_8 (ZXT_16 vec1))
        (ZXT_16 vec2)
    )
)

(define (L_SH_1 vec)
    ; Shift a 16-bit bit-vector left by 1 bit
    (bvshl vec (bv 1 16))
)

(define (L_SH_8 vec)
    ; Shift a 16-bit bit-vector left by 8 bits
    (bvshl vec (bv 8 16))
)

(define (MASK_HIGH8 vec)
    ; Zero out the lower 8 bits, leaving only the upper 8 bits. 
    ; Returns a 16-bit bit-vector.
    (bvand vec (bv #b1111111100000000 16))
)

(define (MASK_LOW8 vec)
    ; Zero out the higher 8 bits, leaving only the lower 8 bits. 
    ; Returns a 16-bit bit-vector.
    (bvand vec (bv #b0000000011111111 16))
)

(define (GET_HIGH8 vec)
    ; Get the higher 8 bits.
    ; Returns an 8-bit bit-vector.
    (extract 15 8 vec)
)

(define (GET_LOW8 vec)
    ; Get the lower 8 bits.
    ; Returns an 8-bit bit-vector.
    (extract 7 0 vec)
)

