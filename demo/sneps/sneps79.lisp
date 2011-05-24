; Examples based on the figures of S. C. Shapiro, The SNePS Semantic Network
; Processing System, in N. Findler, ed. Associative Networks: Representation
; and Use of Knowledge by Computers, Academic Press, 1979, 179-203.
;
^ (setq snip::*infertrace* nil)
;
; Fig. 3
(define member class)
(assert member moby-dick class whale)
(build member moby-dick class $x)
;
; Fig. 4
(assert member orca class whale) = new
;
; Fig. 5
(define agent verb object)
(dump (assert agent john verb knows object *new) *new)
(describe m3)
;
; Fig. 6
(describe (find member moby-dick class whale))
(find member- (find class whale))
(describe (find verb knows object (find class whale)))
;
; Fig. 7
(find verb knows object (find member ?known-whale class whale))
*known-whale
;
; Fig. 8
(assert agent john verb knows object m3)
(assert agent henry verb knows object m3)
(describe (find agent ?x verb knows
                object (find agent ?x verb knows)))
*x
;
; Fig. 9
(find member- (find class whale) - (find object- (find verb knows)))
;
; Fig. 10
(find member- (find class whale) _ (object-))
;
; Fig. 18
(assert forall $man
       ant (build member *man class man)
       cq  (build evb $woman min 2 max 2
                  arg ((build member *woman class woman)
                       (build agent *man verb loves object *woman))))
(assert forall $r
       ant (build member *r class transitive)
       cq  (build forall ($x $y $z)
                  &ant ((build agent *x verb *r object *y)
                        (build agent *y verb *r object *z))
                  cq  (build agent *x verb *r object *z)))
;
; Fig. 19
(assert member john class man)
(assert member henry class man)
(describe (deduce agent *man verb loves object *woman))
(describe (findconstant class woman))
;
; Fig. 20
(assert agent a verb supports object b)
(assert agent b verb supports object c)
(assert agent c verb supports object d)
(assert min 0 max 0
       arg (build agent b verb supports object e))
(assert member supports class transitive)
(describe (deduce (4 0) agent $x verb supports object $y))
;
; Fig. 21
; (describe (resume lastinfer)) ;;;; NOT WORKING ;;;;
;
; Fig. 22
(describe (add member sam class man))
(describe (find member b3))
;
; You may now enter any SNePSUL commands.
; Enter (exit) when done.
(End of demonstration)
