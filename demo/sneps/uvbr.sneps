;;; -*- Mode:Common-Lisp; Base:10 -*-
;;;
;;; Demonstration of UBVR in action in SNePS-2
;;; from Appendix of
;;; S. C. Shapiro, Symmetric Relations, Intensional Individuals, and Variable Binding
;;;                Proc. IEEE 74, 10(Oct. 1986), 1354-1363
;;;
(resetnet t)
;;;
;;; Define arc labels
(define member class object property rel argument a1 a2 a3)
;;;
;;; Example 1: If x and y are brothers, then x likes y.
;;;
(describe (assert rel brothers argument (Bob Joe Harry)))
(describe (assert forall ($brother $brother2)
		  ant (build rel brothers argument (*brother *brother2))
		  cq  (build rel likes a1 *brother a2 *brother2)))
(describe (deduce rel likes a1 *brother a2 *brother2))
;;;
;;; Example 2: If a tract of the CNS is malfunctioning, adjacent tracts should be examined.
;;;
(describe (assert forall $tract
		 &ant ((build member *tract class tract)
		       (build object *tract property malfunctioning))
		 cq   (build forall $tract2
			     &ant ((build member *tract2 class tract)
				   (build rel adjacent argument (*tract *tract2)))
			     cq   (build rel should a1 examine a2 *tract2))))
(describe (assert member 16R class tract))
(describe (assert member 13RC8-C2 class tract))
(describe (assert rel adjacent argument (16R 13RC8-C2 )))
(describe (assert object 16R property malfunctioning))
(describe (deduce rel should a1 examine a2 *tract2))
;;;
;;; Example 3: If an AND gate has high inputs and a low output, it is faulty.
;;; 
(describe (assert forall ($and-gate $input $input2 $output)
		  &ant ((build member *and-gate class and-gate)
			(build rel gate a1 *and-gate a2 (*input *input2) a3 *output))
		  cq   (build &ant ((build object *input property high)
				    (build object *input2 property high)
				    (build object *output property low))
			      cq   (build object *and-gate property faulty))))
(describe (assert member A1 class and-gate))
(describe (assert rel gate a1 A1 a2 (A1inp1 A1inp2) a3 A1outp))
(describe (assert object A1inp1 property high))
(describe (assert object A1inp2 property low))
(describe (assert object A1outp property low))
(describe (assert member A2 class and-gate))
(describe (assert rel gate a1 A2 a2 (A2inp1 A2inp2) a3 A2outp))
(describe (assert object A2inp1 property high))
(describe (assert object A2inp2 property high))
(describe (assert object A2outp property low))
(describe (deduce object *and-gate property faulty))
;;;
;;; Example 4: Elephants hate each other.
;;; 
(describe (assert member Clyde class elephant))
(describe (assert member Jumbo class elephant))
(describe (assert forall ($elephant $elephant2)
		  &ant ((build member *elephant class elephant)
			(build member *elephant2 class elephant))
		  cq   (build rel hates a1 *elephant a2 *elephant2)))
(describe (deduce rel hates a1 *elephant a2 *elephant2))
