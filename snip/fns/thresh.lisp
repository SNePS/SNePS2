;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: thresh.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;;;                      THRESH FUNCTIONS
;;;                      ================
;;;
;;; Representation
;;; --------------
;;; (build thresh i [threshmax j] arg (P1, ..., Pn))
;;;
;;; If threshmax j is omitted j defaults to n-1
;;; THRESH(i, j) = not(ANDOR(i, j))
;;;
;;; Semantics
;;; ---------
;;; Either fewer than i of the arguments are true,
;;;        or else more than j of the arguments are true
;;;
;;; Rules of Inference
;;; ------------------
;;; When >=i args are true, and n-j-1 args are false, conclude that any other arg is true.
;;; When i-1 args are true, and >=n-j args are false, conclude that any other arg is false.
;;;
;;; Detects Contradictions 
;;; ----------------------
;;; When at least i args are true and at more than n-j-1 args are false.
;;;
;;;
;;;


;
; ==============================================================================
;

(defun rule-handler.thresh (ant-report cqch)
  (let* ((i (node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::thresh))))
	 (n (cardinality.ns (nodeset.n *NODE* 'sneps::arg)))
	 (j (if (nodeset.n *NODE* 'sneps::threshmax)
		(node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::threshmax)))
		(1- n)))
	 (ants (ants.cqch cqch)))

    ;; if the antecedents have the same set of variables,
    ;; use S-indexing, otherwise use linear ruiset handling    
    (do.set (rui (if (is-all-pat-same-vars ants)
		     (get-rule-use-info-sindexing ant-report cqch)
		     (get-rule-use-info ant-report cqch)))
      (cond ((and (>= (poscount.rui rui) i) (> (negcount.rui rui) (- n j 1)))
	     (report-contradiction.thresh (context.ch (channel.cqch cqch)) i j ants
					  *NODE* rui))
	    ((and (>= (poscount.rui rui) i) (= (negcount.rui rui) (- n j 1)))
	     (let ((restr (make.restr (subst.rui rui)))
		   (ch (channel.cqch cqch)))
	       (unless-remarkedp.rui rui (inform-pos.thresh restr ch ants rui))
	       (send-reports
		 (makeone.repset
		   (make.rep (restrict-binding-to-pat (subst.rui rui)
						      (destination.ch ch))
			     (compute-new-support.thresh
				   ch
				   (combinations.fns i  (select-pos.rui rui)
						        (select-neg.rui rui))
				   (subst.rui rui))
			     'POS  *NODE*   nil  (context.ch ch)))
		 ch)))
	    ((and (= (poscount.rui rui) (1- i)) (>= (negcount.rui rui) (- n j)))
	     (let ((restr (make.restr (subst.rui rui)))
		   (ch (channel.cqch cqch)))
	       (unless-remarkedp.rui rui (inform-neg.thresh restr ch ants rui))
	       (send-reports
		 (makeone.repset
		   (make.rep (restrict-binding-to-pat (subst.rui rui)
						      (destination.ch ch))
			     (compute-new-support.thresh 
				      ch
				      (combinations.fns (- n j)
							(select-neg.rui rui)
							(select-pos.rui rui))
				      (subst.rui rui))
			     'NEG  *NODE*  nil  (context.ch ch)))
		 ch)))))))
;
; ==============================================================================
;
; inform-pos.thresh 
; -----------------
;
;       arguments     : restr   - <restriction>
;                       ch      - <channel>
;                       ants - <node set>
;                       rui     - <rule use info>
;
;       returns       : <never mind>
;
;       description   : informs the user the inference of a positive instance
;       
;
;                                        written :  ???
;                                        modified:  njm  10/27/88
;
;
(defun inform-pos.thresh (restr ch ants rui)   
  (remark '"~%Since" (makeone.ns *NODE*) nil)
  (do* ((ant-set ants (others.ns ant-set))
	(next-ant (choose.ns ant-set) (choose.ns ant-set)))
       ((isnew.ns ant-set))
    (cond ((eq (flag.fns next-ant (fns.rui rui)) 'TRUE)
	   (remark '"and" (makeone.ns next-ant) restr))
	  ((eq (flag.fns next-ant (fns.rui rui)) 'FALSE)
	   (remark '"and it is not the case that"
		   (makeone.ns next-ant)
		   restr))))
  (remark '"I infer" (makeone.ns (destination.ch ch)) restr))

;
;
; =============================================================================
;
; inform-neg.thresh 
; -----------------
;
;       arguments     : restr - <restriction>
;                       ch    - <channel>
;                       ants  - <node set>
;                       rui   - <rule use info>
;
;       returns       : <never mind>
;
;       description   : informs the user the inference of a negative instance
;       
;
;                                        written :  ???
;                                        modified:  njm  10/27/88
;
;
(defun inform-neg.thresh (restr ch ants rui)
  (remark '"~%Since" (makeone.ns *NODE*) nil)
  (do* ((ant-set ants (others.ns ant-set))
	(next-ant (choose.ns ant-set) (choose.ns ant-set)))
       ((isnew.ns ant-set))
    (cond ((eq (flag.fns next-ant (fns.rui rui)) 'TRUE)
	   (remark '"and" (makeone.ns next-ant) restr))
	  ((eq (flag.fns next-ant (fns.rui rui)) 'FALSE)
	   (remark '"and it is not the case that"
		   (makeone.ns next-ant)
		   restr))))
  (remark '"I infer it is not the case that"
	  (makeone.ns (destination.ch ch))
	  restr))
;
;
; =============================================================================
;
; report-contradiction.thresh 
; ---------------------------
;
;       arguments     : ct   - <context>
;                       i    - <integer>
;                       j    - <integer>
;                       ants - <node set>
;                       nd   - <node>
;                       rui  - <rui>
;
;       returns       : 
;
;       description   : Reports contradictions found in THRESH
;       
;
;                                        written :  jpm  12/2/82
;                                        modified:  njm  10/27/88
;                                        modified:  scs/flj  6/20/04
;
;
(defun report-contradiction.thresh (ct i j ants nd rui)
  (sneps:sneps-error (format nil "Contradiction in a thresh: in node ~A ~
                                  number of true args in excluded range"
			     nd)
		     'SNIP 'report-contradiction.thresh))

;
; ==============================================================================
;

(defun usability-test.thresh (sign)
  (and (eq sign 'POS)
       (or (isnew.ns (quantified-vars.n *NODE*))
           (not (isnew.ns (nodeset.n *NODE* 'sneps::forall))))))


;
; ==============================================================================
;
; compute-new-support.thresh
; --------------------------
;
;       arguments     : ch  - <channel>
;                       fns-combinations  - a set of <flag-node-set>s
;                       subst - <substitution>
;
;       returns       : <support>
;
;       description   : Computes a new support based on:
;                        1- the support of the rule node if it is asserted;
;                        2- all possible combinations of flag-node-sets that
;                           satisfy the thresh and are sufficient to allow to
;                           deduce new information
;                        3- the support of the instances (of the rule) which
;                           are asserted in the 'ch' context and have the 
;                           appropriate substitution. 
;
;
;                                        written :  cpf/njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support.thresh (ch fns-combinations subst)
  (let ((crntct (context.ch ch))
	(newsupport (new.sup))
	(freevars (freevars.n *NODE*)))
    (when (isassert.n *NODE*)
      (do.fns (one-fns-comb fns-combinations)
	(setq newsupport
	      (merge.sup newsupport
			 (compute-new-support1.and-or
			   one-fns-comb (sneps:node-asupport *NODE*))))))
    (when *KNOWN-INSTANCES*
      (do.set (inst *KNOWN-INSTANCES*)
	(let* ((instnode (match::applysubst *NODE* (subst.inst inst)))
	       (supinstnode (filter.sup (sneps:node-asupport instnode) crntct)))
	  (when (and (not (isnew.sup supinstnode))
		     (match:issubset.sbst (restrict.sbst subst freevars)
					  (restrict.sbst (subst.inst inst) freevars)))
	    (do.fns (one-fns-comb fns-combinations)
	      (setq newsupport
		    (merge.sup newsupport
			       (compute-new-support1.and-or
				 one-fns-comb supinstnode))))))))
    newsupport))


;
; ==============================================================================
;
; combinations.fns
; ----------------
;
;       arguments     : k - <integer>
;                       fns - <flag-node-set>
;                       otherfns - <flag-node-set>
;
;       returns       : set of <flag-node-set>s
;
;       description   : Generates all 'k'-element subsets of 'fns', unions
;                       each of the with 'otherfns' and returns these sets
;                       as a list (the version that didn't use fns-access 
;                       functions was somehow easier to understand).
;
;
;                                        written :  njm/hc  05/06/89
;                                        modified: 
;
;
(defun combinations.fns (k fns otherfns)
  (let ((restset fns)
	combinations)
    (cond ((= k 1)
	   (do.fns (fn fns)
	     (push (putin.fns fn otherfns) combinations)))
	  (t (do.fns (fn fns)
	       (setq restset (others.fns restset))
	       (setq combinations
		     (append combinations
			     (mapcar #'(lambda (combset)
					 (putin.fns fn combset))
				     (combinations.fns (1- k) restset otherfns)))))))
    combinations))



    
    




