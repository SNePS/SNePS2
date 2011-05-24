;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011
;; Research Foundation of State University of New York

;; Version: $Id: mental-acts.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;; 
;; The contents of this file are subject to the University at
;; Buffalo Public License Version 1.0 (the "License"); you may
;; not use this file except in compliance with the License. You
;; may obtain a copy of the License at http://www.cse.buffalo.
;; edu/sneps/Downloads/ubpl.pdf.
;; 
;; Software distributed under the License is distributed on an
;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;; or implied. See the License for the specific language gov
;; erning rights and limitations under the License.
;; 
;; The Original Code is SNePS 2.7.
;; 
;; The Initial Developer of the Original Code is Research Foun
;; dation of State University of New York, on behalf of Univer
;; sity at Buffalo.
;; 
;; Portions created by the Initial Developer are Copyright (C)
;; 2011 Research Foundation of State University of New York, on
;; behalf of University at Buffalo. All Rights Reserved.
;; 
;;  
;; 
;; 


;; $END LICENSE$


(in-package :snip)


;;;
;;; Representation
;;;
;;;   believe-act = action believe
;;;         	    object1 prop
;;;   believe-act = action believe
;;;         	    object1 prop


;;; HC: Believing of the effects of an act should really be done simultaneously
;;; (or in parallel) instead of the current method that uses a DO-ALL in
;;; the function `schedule-believe-effects'.  The reason is that an act can
;;; have multiple effects, some of which might also be represented by
;;; rules.  For example, the rule Held(BLOCK) => ~Clear(BLOCK) in conjunction
;;; with the effects Held(BLOCK) and ~Clear(BLOCK) of the pickup action will
;;; lead to a contradiction if after picking up a block we first believe
;;; Held(BLOCK) and forward-chain on it, since that will derive ~Clear(BLOCK)
;;; which will contradict the still not disbelieved Clear(BLOCK).

;;; In preparation for changing this the `believe' function now uses
;;; `believe-propositions' which believes a set of propositions simultaneously.

;;; Changed FLJ/SCS 8/1/04 to do the following...
;;; Provides an error message, instead of causing a Lisp error, if no proposition.
(defun believe (believe-act)
  "Causes the propositions in object1(BELIEVE-ACT) to be believed."
  (if (isnew.ns (nodeset.n believe-act 'object1))
      (sneps:sneps-error "Attempt to perform believe with no proposition."
			 'snip 'believe)
    (believe-propositions (nodeset.n believe-act 'object1))))



;;; Want `believe P' always to do: retract ~P; assert P.
;;; Modified by flj on 2/12/04 for scs

;;; This version calls assert.n directly
;;;    rather than going via #! to avoid package problems.
(defun believe-propositions (propositions)
  "Causes PROPOSITIONS to be believed quasi-simultaneously.
   By `simultaneously' is meant that before believing any PROPOSITIONS
   all currently believed negations to any of them will be disbelieved,
   as well as all currently believed linked propositions."
  (declare (special *most-entrenched-props*))
  (let* ((sneps:*with-snepsul-eval-function*
	  ;; Make sure no tracing occurs in #! contexts:
          #'sneps:with-snepsul-standard-eval))

    ;; Make sure nothing is most entrenched while disbelieving
    (setf *most-entrenched-props* nil)

;    ;; Disbelieve negations:
    (do.ns (proposition propositions)
	   (disbelieve-negation proposition))

    ;; Make the new propositions we are believing maximally entrenched
    (setf *most-entrenched-props* propositions)
    ;; Assert propositions:
    (do.ns (proposition propositions)
	   (cond ((isassert.n proposition)
	  (plantrace
	   "I already believe " (makeone.ns proposition) (new.restr)))
		 (t (assert.n proposition crntct)
		    (sneps:set.sv 'sneps:assertions
			    (insert.ns proposition
				       (sneps:value.sv 'sneps:assertions)))
		    (plantrace "Believe " (makeone.ns proposition) (new.restr)))))
    ;; Forward-chain on newly believed propositions:
    (dynamic-add* propositions)))

;;;  Added by flj on 2/12/04 for scs
(defun disbelieve-linked (proposition)
  "Retracts any believed propositions linked to PROPOSITION."
  (let* ((state-constraints
	  #!((findassert max 1 arg ~proposition :context ~crntct)))
	 ;; Let p be the proposition that is to be believed.
	 ;; There are 3 kinds of propositions that look like state constraints
	 ;;    linking p with other (linked) propositions.
	 ;; Call a proposition that looks like a state constraint sc
	 ;;    (it will look like andor(i,1){p, q, ...});
	 ;;     where q is currently believed.
	 ;; If q is a real linked-proposition,
	 ;;    q will be disbelieved before believing p.
	 ;;
	 ;; The three kinds of "state constraints" sc:
	 ;; 1) sc is an hypothesis, 
	 ;;    Treat sc as a real state constraint and disbelieve q.
	 ;; 2) sc is derived from ~p,
	 ;;       which itself is derived from a case (1) state constraint
	 ;;       like andor(i,1){p, r, ...} and the asserted r.
	 ;;    sc will no longer be asserted once r is disbelieved.
	 ;;    So put off considering these "state constraints"
	 ;;       until the case (1) state constraints are used.
	 ;; 3) sc is derived, but is a real state constraint.
	 ;;    It could be derived from rules like
	 ;;       all(x)(meeting(x) => andor(1,1){time(x,morning), time(x,afternoon)}).
	 ;;       all(x,y)({meeting(x),meeting(y)} &=> all(t)(andor(1,1){time(x,t),time(y,t)})).
	 ;;    This sc will still be asserted
	 ;;       after the case (1) state constraints are used.
	 ;;    So use them then.
	 ;; The SNePS 2.7 User's Manual says that the state-constraint doesn't have to be an hypothesis,
	 ;;     and ignores the difference between cases (2) and (3).
	 (derived-state-constraints
	  ;; Retract propositions that are linked by case (1) state constraints,
	  ;;    and collect the case (2) and (3) ones for later consideration.
	  (loop for sc in state-constraints
	      when (ismemb.ns sc (sneps:context-hyps crntct))
	      do (disbelieve-linked-prop proposition sc)
	      else collect sc)))
    ;; Retract propositions that are linked by derived state constraints
    ;;    that are still asserted.
    (loop for sc in derived-state-constraints
	when (isassert.n sc crntct)
	do (disbelieve-linked-prop  proposition sc))))

(defun disbelieve-linked-prop (proposition sc)
  "Disbelieve the propositions
        that are linked to the given proposition via the state constraint sc,
        as long as the proposition to be disbelieved
        is an hypothesis in the current context."
  (declare (special crntct))
  (disbelieve-propositions
   (remove-if #'(lambda (linked-proposition)
		  (not (ismemb.ns linked-proposition
				  (sneps:context-hyps crntct))))
	      #!((- (find arg- ~sc) ~proposition)))))

;;; Changed scs/flj 6/29/04 to use sneps:negate.n
;;; Changed scs/flj 8/01/04 for following reason...
;;; Now that sneps:contradictory-nodes.n uses find-negate instead of negate.n,
;;;    it might return an empty nodeset.
;;; Changed aif 2011-05-07 to use autobr
(defun disbelieve-negation (proposition
			    &aux
			    (not-prop (sneps:contradictory-nodes.n proposition)))
  "Retracts believed propositions that are contradictions of PROPOSITION."
  (unless (isnew.ns not-prop)
    (disbelieve-propositions 
     (sneps:remove-if-not.ns #'isassert.n not-prop))))

(defun disbelieve (disbelieve-act)
  "Causes the propositions in object1(DISBELIEVE-ACT) to be disbelieved."
  (disbelieve-propositions (nodeset.n disbelieve-act 'object1)))

;;;  Want `disbelieve P' always to do just retract P.
;;;  Modified by flj on 2/12/04 for scs

 ;;; This version does a clear-infer before disbelieving anything,
;;;    because processes aren't informed
;;;    when an antecedent is no longer believed.
(defun disbelieve-propositions (propositions)
  "Causes PROPOSITIONS to be disbelieved."
  (declare (special sneps:crntct))
  (unless (isnew.ns propositions)
;    (sneps:clear-infer) ;should be done by autobr now
    (dolist (proposition propositions)
      (let ((context (ctcs-to-cts (sneps:node-asupport proposition))))
        (snebr::autobr context context (value.sv sneps:crntct) nil nil)))))
        

;(defun disbelieve-propositions (propositions)
;  "Causes PROPOSITIONS to be disbelieved."
;  (unless (isnew.ns propositions)
;    (sneps:clear-infer)
;    (let ((sneps:*with-snepsul-eval-function*
;	   #'sneps:with-snepsul-standard-eval))
;      (do.ns (proposition propositions)
;	     (cond ((isassert.n proposition)
;		    (cond ((ismemb.ns proposition (sneps:context-hyps crntct))
;			   (plantrace "Disbelieve "
;				      (makeone.ns proposition) (new.restr))
;			   #3!((remove-from-context ~proposition ~crntct)))
;			  (t (plantrace
;			      "Can't disbelieve derived proposition "
;			      (makeone.ns proposition) (new.restr)))))
;		   (t (plantrace
;		       "already didn't believe "
;		       (makeone.ns proposition)
;		       (new.restr))))))))

;;; For backward compatibility:

(defun forget (forget-act)
  "Alias for `disbelieve'."
  (disbelieve forget-act))



    
     ;;;
;;; Representation
;;;
;;;   adopt-act = action adopt
;;;         	    object1 prop
;;;   unadopt-act = action unadopt
;;;         	       object1 prop

(defun adopt (adopt-act)
  "Causes the policies in object1(ADOPT-ACT) to be adopted."
  ;;; Adoption is just like belief, but there is no negated policy.
  (if (isnew.ns (nodeset.n adopt-act 'object1))
      (sneps:sneps-error "Attempt to perform adopt with no policy."
			 'snip 'adopt)
    
    (let ((sneps:*with-snepsul-eval-function*
	    ;; Make sure no tracing occurs in #! contexts:
	   #'sneps:with-snepsul-standard-eval)
	  (policies (nodeset.n adopt-act 'object1)))
      ;; Adopt policies:
      (do.ns (policy policies)
	     (cond ((isassert.n policy)
		    (plantrace
		     "I already intend " (makeone.ns policy) (new.restr)))
		   (t #!((! ~policy :context ~crntct))
		      (plantrace "Adopt " (makeone.ns policy)
				 (new.restr)))))
      ;; Forward-chain on newly believed policies:
      (dynamic-add* policies))))


(defun unadopt (unadopt-act)
  "Causes the policies in object1(UNADOPT-ACT) to be unadopted."
  ;;; Unadoption is just like disbelief, but there is no negated policy.
  (let ((sneps:*with-snepsul-eval-function*
         #'sneps:with-snepsul-standard-eval))
    (do.ns (policy (nodeset.n unadopt-act 'object1))
	   (cond ((isassert.n policy)
		  (cond ((ismemb.ns policy (sneps:context-hyps crntct))
			 (plantrace "Unadopt "
				    (makeone.ns policy) (new.restr))
			 #3!((remove-from-context ~policy ~crntct)))
			(t (plantrace
			    "Can't unadopt derived policy "
			    (makeone.ns policy) (new.restr)))))
		 (t (plantrace
		     "already didn't intend "
		     (makeone.ns policy)
		     (new.restr)))))))


