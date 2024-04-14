(set-logic QF_BV)

(declare-fun w () (=> Real Real))
(declare-fun w () (f Real Real))
(declare-fun x () (_ BitVec 8))
;(declare-fun y () (_ BitVec 8))

;(assert (bvult x y)) ; x < y

(check-sat)
