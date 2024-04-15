(set-logic QF_BV)

(declare-fun p () (_ BitVec 40))
(declare-fun q () (_ BitVec 8))
(declare-fun w () (_ BitVec 64))
(declare-fun x () (_ BitVec 32))
(declare-fun y () (_ BitVec 32))
(declare-fun z () (_ BitVec 32))

; Bit-vector operations
(assert (bvult x y))      ; Unsigned less than
(assert (bvslt x y))     ; Signed less than
(assert (= z (bvnot x)))        ; Bitwise negation
(assert (= z (bvand x y)))      ; Bitwise AND
(assert (= z (bvxor x y)))      ; Bitwise XOR
(assert (= z (bvshl x y)))      ; Shift left
(assert (= z (bvlshr x y)))     ; Logical shift right
(assert (= z (bvashr x y)))     ; Arithmetic shift right
(assert (= z (bvadd x y)))      ; Addition
(assert (= z (bvmul x y)))      ; Multiplication
(assert (= z (bvlshr x y)))     ; Unsigned remainder
(assert (= z (bvudiv x y)))     ; Unsigned division

; Combinations of operations
(assert (= z (bvmul (bvadd x y) z)))      ; Addition
(assert (bvult (bvadd x y) z))      ; Addition


;; Concatenation
(assert (= w (concat x y)))
;
;; Sign extension
(assert (= p ((_ sign_extend 8) x)))
;
;; Bit-vector extraction
(assert (= q ((_ extract 7 0) x))) ; Extract bits 7 to 0 from x

(check-sat)