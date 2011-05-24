;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: rule-finfers.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


; =============================================================================
;
; process-forward-inferences.rule
; -------------------------------
;
;       nonlocal-vars : *PENDING-FORWARD-INFERENCES* register
;
;       description   : handles all the reports in the PENDING-FORWARD-
;                         INFERENCES: register of the current rule node
;
;       side-effects  : updates various registers
;
;                                        written :  rgh  4/20/86
;                                        modified:
;
;
(defun process-forward-inferences.rule ()
  (let ((repset *PENDING-FORWARD-INFERENCES*))
    (setq *PENDING-FORWARD-INFERENCES* (new.repset))
    (do ()
	((isnew.repset repset))
	(process-one-forward-inference.rule (choose.repset repset))
	(setq repset (others.repset repset)))))
;
;
; =============================================================================
;
; process-one-forward-inference.rule
; ----------------------------------
;
;       arguments     : report - <report>
;
;       description   : handles a single forward inference report for a rule
;                       node
;
;       side-effects  : updates several process registers
;
;       implementation: sets up rule use channels to all possible consequents
;                       of the rule.  If the forward inference report came
;                       from an antecedent, it broadcasts it to all the
;                       rule-use channels.  If the report is of a new rule
;                       instance, requests are broadcast to all antecedents
;                       so that the rule may be used -- and the new rule
;                       instance is also handled as a forward inference would
;                       be for a non-rule.
;
;                                        written :  rgh  4/20/86
;                                        modified:  rgh  4/24/86
;                                                   choi 9/19/90
;
(defun process-one-forward-inference.rule (report)
  (when (not (is-there-rule-instance-forward report))
    (set-up-rule-use-channels report)
    (cond ((or (is-ant-to-rule.rep report)
	      ;; in a numerical quantifier rule, a report from
	      ;; its consequent should also be broadcast
	      (and (eq *TYPE* 'num-quantifier)
		   (is-cq-to-rule.rep report)))
	   (setq *INCOMING-CHANNELS*
	       (insert.feedset
	        (make.feeder (make.restr (subst.rep report))
			     (context.rep report)
			     (signature.rep report)
			     'OPEN)
		*INCOMING-CHANNELS*))
	   (broadcast-ant-report.rule report))
	  (t (process-one-forward-inference.non-rule report)
	     (try-applying-one-report report))))
  (when (not (isassert.n *NODE*))
    (let ((rule-sbst 
	   (restrict-binding-to-pat (subst.rep report) *NODE*))
	  (restr nil)
	  (pathfrom-result nil))
      (declare (special pathfrom-result))
      (setq restr (make.restr rule-sbst))
      (send-rule-use-requests restr crntct 'USER))))


(defun is-there-rule-instance-forward (report)
  ;;"Check if there is any known rule instance in the *KNOWN-INSTANCES* 
  ;; register with respect to the substitution of report."
  (let ((all-args (nodeset.n *NODE* 'arg))
	(any-instances-checked nil))
      (when (or (and all-args (is-all-pat-same-vars all-args))
		(eq *TYPE* 'num-quantifier))
	  (do.set (instance *KNOWN-INSTANCES*)
	       (if (acceptable-rule-instances-forward instance report)
		   (setq any-instances-checked t))))
    any-instances-checked))

(defun acceptable-rule-instances-forward (inst rep)
  (and (compatible (get-var-bindings (subst.inst inst))
		   (get-var-bindings (subst.rep rep)))
       (not (isnew.sup (filter.sup (support.inst inst)
				   (sneps:value.sv (context.rep rep)))))))

(defun get-var-bindings (sbst)
  ;; removes those bindings that are not associated with variables.
  ;; e.g. something like (P5 . M6!) is removed
  (let ((new-sbst nil))
    (do.set (binding sbst new-sbst)
	 (if (isvar.n (first binding))
	     (setq new-sbst (append new-sbst (list binding)))))))
;
;
; =============================================================================
;
; consequents
; -----------
;
;       arguments     : rule - <node>
;                       ant - <node>
;
;       returns       : <node set>
;
;       description   : returns the set of all nodes which are in consequent
;                       position relative to "ant" in "rule"
;
;                                        written :  rgh  4/20/86
;                                        modified:  njm 10/25/88
;                                        modified:  dk   6/3/93
; added the DO arc for the when-do transformer as a consequent -:dk
;
;
(defmacro consequents (rule ant)
  `(let ((cqs (union.ns (nodeset.n ,rule 'sneps::cq)
			(union.ns (nodeset.n ,rule 'sneps::do)
				  (nodeset.n ,rule 'sneps::dcq))))
         (args (remove.ns ,ant (nodeset.n ,rule 'sneps::arg))))
     (if (not (isnew.ns cqs))
	 cqs
	 args)))
;
;
; =============================================================================
;
; all-consequents
; ---------------
;
;       arguments     : rule - <node>
;
;       returns       : <node set>
;
;       description   : returns the set of all nodes which are in consequent
;                       position in the "rule"
;
;                                        written :  rgh  4/20/86
;                                        modified:  njm 10/25/88
;                                        modified:   dk  6/3/93
; add the DO for a when-do as a consequent -:dk
;
;
(defmacro all-consequents (rule)
  `(let ((cqs (union.ns (nodeset.n ,rule 'sneps::cq)
			(union.ns (nodeset.n ,rule 'sneps::do)
				  (nodeset.n ,rule 'sneps::dcq))))
         (args (nodeset.n ,rule 'sneps::arg)))
      (if (not (isnew.ns cqs))
	  cqs
          args)))
;
;
; =============================================================================
;
; set-up-rule-use-channels
; ------------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : *RULE-USE-CHANNELS* register
;
;       description   : installs new rule use channels on all appropriate
;                       consequent arcs (based on where the report came from)
;                       whose channels contain the reported substitution as
;                       their filters
;
;       side-effects  : updates *RULE-USE-CHANNELS*
;
;       implementation: if the report came from a node at the end of an arg
;                       arc emanating from the current node, then all other
;                       args are possible consequents.  If the report is of
;                       a new rule instance, and the rule has arg arcs, then
;                       all args are possible consequents.
;
;                                        written :  rgh  4/20/86
;                                        modified:  rgh  4/24/86
;                                        modified:  scs  6/17/88
;                                                   choi 9/19/90
;                                                    scs 9/14/94
;                                                    scs 12/22/94
;                                                     hi 03/31/99
; added whenever-do's. hi
;
(defun set-up-rule-use-channels (report)
  ;;"Install rule use channels for forward inference."
  (let ((subst (subst.rep report))
	(signature (signature.rep report))
	(ct (context.rep report)))
    (cond
      ((eq *TYPE* 'num-quantifier)
       ;; P-tree for numerical quantifier rules
       (let* ((ants (nodeset.n *NODE* 'sneps::&ant))
	      (cq  (nodeset.n *NODE* 'sneps::cq))
	      (ptree (ptree-for-num-quant ants cq))
	      (adj-node-list (ptree-to-adj-list ptree))
;	      (chsub (restrict-binding-to-pat subst (first cq)))
	      (ch (make.ch (new.filter) (new.switch) ct (first cq) 'open))
	      (ptree-ruiset (list ptree adj-node-list nil))
	      (cqch (make.cqch ch (append ants cq) ptree-ruiset)))
	 (setq *RULE-USE-CHANNELS* (insert.cqchset cqch *RULE-USE-CHANNELS*))
          ;; bi-directional inference :
	 (send-ant-requests.rule cqch)))

      ;; if a rule's antecedents (or arguments) have the same set
      ;; of variables, use the S-indexing method
      ((is-all-pat-same-vars
	(or (nodeset.n *NODE* 'sneps::ant)
	    (nodeset.n *NODE* 'sneps::when)
	    (nodeset.n *NODE* 'sneps::whenever)
	    (nodeset.n *NODE* 'sneps::&ant)
	    (nodeset.n *NODE* 'sneps::arg)))
       (do.ns (cq (if (is-ant-to-rule.rep report)
		      (consequents *NODE* signature)
		      (all-consequents *NODE*)))
	  (let* ((ants (antecedents *NODE* cq))
		 (chsub (restrict-binding-to-pat subst cq))
		 ;;(ch (make.ch chsub (new.switch) ct cq 'open))
		 (ch (make.ch subst chsub ct cq 'open)) ; scs 12/22/94
		 (cqch (make.cqch ch ants nil)))
	    (setq *RULE-USE-CHANNELS* (insert.cqchset cqch *RULE-USE-CHANNELS*))
	    ;; bi-directional inference :
	    (unless (and (eql *TYPE* 'or-entailment)    ;scs 9/14/94
			 (is-ant-to-rule.rep report))   ;scs 9/14/94
	      (send-ant-requests.rule cqch)))))
;	    (broadcast-ant-request ch 
;				   (remove signature ants)
;				   (ch-to-restr ch)
;				   subst)

      ;; a P-tree is built for an and-entailment rule whose antecedents
      ;; do not have the same set of variables
      ((eq *TYPE* 'and-entailment)
       (let* ((ants (nodeset.n *NODE* 'sneps::&ant))
	      (ptree (ptree-for-and-ent ants))
	      (adj-node-list (ptree-to-adj-list ptree))
	      (ptree-ruiset (list ptree adj-node-list nil)))
	 (do.ns (cq (nodeset.n *NODE* 'sneps::cq))
	    (let* ((chsub (restrict-binding-to-pat subst cq))
		   ;;(ch (make.ch chsub (new.switch) ct cq 'open))
		   (ch (make.ch subst chsub ct cq 'open)) ;scs 12/22/94
		   (cqch (make.cqch ch ants ptree-ruiset)))
	      (setq *RULE-USE-CHANNELS* (insert.cqchset cqch *RULE-USE-CHANNELS*))
	      ;; bi-directional inference :
	      (send-ant-requests.rule cqch)))))
;	      (broadcast-ant-request ch
;				     (remove signature ants)
;				     (ch-to-restr ch)
;				     subst)

      ;; a linear RUI set is built for non-conjunctive rules whose
      ;; antecedents do not have the same set of variables
      (t (do.ns (cq (if (is-ant-to-rule.rep report)
			(consequents *NODE* signature)
		      (all-consequents *NODE*)))
	  (let* ((ants (antecedents *NODE* cq))
		 (chsub (restrict-binding-to-pat subst cq))
		 ;;(ch (make.ch chsub (new.switch) ct cq 'open))
		 (ch (make.ch subst chsub ct cq 'open)) ;scs 12/22/94
		 (fants (nodeset-to-fnodeset ants))
		 (rui (make.rui chsub 0 0 fants nil)))
	    (setq *RULE-USE-CHANNELS*
		  (insert.cqchset 
		   (make.cqch ch ants (makeone.ruis rui))
		   *RULE-USE-CHANNELS*))
	    ;; bi-directional inference :
	    (unless (and (or (eql *TYPE* 'or-entailment)
			     (eql *TYPE* 'when-do)
			     (eql *TYPE* 'whenever-do))
			 (is-ant-to-rule.rep report))
	      (broadcast-ant-request ch
				     (remove signature ants)
				     (ch-to-restr ch)
				     subst))))))))
;
;
; =============================================================================



    
    




