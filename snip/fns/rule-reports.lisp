;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: rule-reports.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;;; The contents of this file are subject to the University at
;;; Buffalo Public License Version 1.0 (the "License"); you may
;;; not use this file except in compliance with the License. You
;;; may obtain a copy of the License at 
;;; http://www.cse.buffalo. edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an
;;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;;; or implied. See the License for the specific language gov
;;; erning rights and limitations under the License.
;;; 
;;; The Original Code is SNePS 2.8.
;;; 
;;; The Initial Developer of the Original Code is Research Foun
;;; dation of State University of New York, on behalf of Univer
;;; sity at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C)
;;; 2011 Research Foundation of State University of New York, on
;;; behalf of University at Buffalo. All Rights Reserved.


;; $END LICENSE$




(in-package :snip)


;=============================================================================
;
; process-reports.rule
; --------------------
;
;       nonlocal-vars : *REPORTS* and *PENDING-FORWARD-INFERENCES* registers
;
;       description   : processes incoming reports, one-by-one, for rule
;                       nodes.
;
;       side-effects  : updates various registers
;
;                                        written :  rgh  3/30/86
;                                        modified:
;
;
(defun process-reports.rule ()
  (let ((reports *REPORTS*))
    (declare (special *PRIORITY*))
    (clear-incoming-reports)
    (do ()
	((isnew.repset reports))
      (process-one-report.rule (choose.repset reports))
      (setq reports (others.repset reports)))
    (unless (isnew.repset *PENDING-FORWARD-INFERENCES*)
      (setq *PRIORITY* 'LOW)
      (initiate (activation.n *NODE*)))))
;
;
; =============================================================================
;
; process-one-report.rule
; -----------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : the *NODE*, *KNOWN-INSTANCES* and PENDING-FORWARD-
;                       INSTANCES: registers, plus *ADDED-NODES*
;
;       description   : ???????????????????????????????????????????????
;
;       side-effects  : *KNOWN-INSTANCES*, *PENDING-FORWARD-INFERENCES* and
;                       *ADDED-NODES* are updated
;
;                                        written :  rgh  3/30/86
;                                        modified:  rgh  4/20/86
;                                        modified:  scs  5/25/88
;                                        modified:  scs  6/22/88
;                                        modified:  njm/cpf 10/21/88
;                                        modified:  njm 10/24/88
;                                        modified:  njm 11/09/88
;                                                   choi 09/19/90
;                                                   choi 11/06/91
;
(defun process-one-report.rule (report)
  (let ((ant-to-rule (is-ant-to-rule.rep report))
	(cq-to-rule (is-cq-to-rule.rep report)))
    (cond ((and cq-to-rule (eq *TYPE* 'NUM-QUANTIFIER))
	   (process-one-ant-report.rule report))
	  ((and cq-to-rule ant-to-rule)
	   (process-one-special-report.rule report))
	  (ant-to-rule (process-one-ant-report.rule report))
	  (cq-to-rule (process-one-introduction-report.rule report))
	  (t (process-one-instance-report.rule report)))))


;
; =============================================================================
;
; process-one-instance-report.rule
; --------------------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : the *NODE*, *KNOWN-INSTANCES* and PENDING-FORWARD-
;                       INSTANCES: registers, plus *ADDED-NODES*
;
;       description   : handles reports of inferred instances of the rule.
;                       The instance is broadcast to all apropriate outgoing
;                       channels.  Also, if this rule instance was
;                       requested for use in deducing a consequent, the new
;                       rule instance is applied.  Unused and/or unsent
;                       instances are placed into *PENDING-FORWARD-INFERENCES*.
;                       If the report is sent than the instance contained in it
;                       is built and asserted.
;
;       side-effects  : *KNOWN-INSTANCES*, *PENDING-FORWARD-INFERENCES* and
;                       *ADDED-NODES* are updated
;
;                                        written :  njm 10/24/88
;                                        modified:  
;
;
(defun process-one-instance-report.rule (report)
  (let ((used-yet nil)
	(instance (rep-to-instance report)))
    (declare (special *ADDED-NODES*))
    (when (unknown.inst instance)
      (setq *ADDED-NODES* (insert.ns (node.rep report) *ADDED-NODES*))
      (when (subst.inst instance)
	(setq *KNOWN-INSTANCES* (insert.iset instance *KNOWN-INSTANCES*)))
      (setq used-yet (broadcast-one-report (make.rep (subst.rep report)
						     (support.rep report)
						     (sign.rep report)
						     *NODE*
						     nil
						     (context.rep report))))
      (setq used-yet (or (try-applying-one-report report)
			 used-yet))
      (when (and (not used-yet)
		 (apply *USABILITY-TEST* (list (sign.rep report))))
	(setq *PENDING-FORWARD-INFERENCES* 
	      (insert.repset report *PENDING-FORWARD-INFERENCES*))))))
;
;
; =============================================================================
;
; process-one-ant-report.rule
; ---------------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : *PENDING-FORWARD-INFERENCES* register
;
;       description   : tries to use one antecedent report to draw a conclusion
;                       and if there was no cq-channel waiting for it, puts it
;                       into *PENDING-FORWARD-INFERENCES*
;
;       side-effects  : updates *PENDING-FORWARD-INFERENCES*
;
;                                        written :  rgh  2/09/86
;                                        modified:  njm 10/25/88
;
;
(defun process-one-ant-report.rule (report)
  (unless (broadcast-ant-report.rule report)
    (setq *PENDING-FORWARD-INFERENCES*
	  (insert.repset report *PENDING-FORWARD-INFERENCES*))))
;
;
; =============================================================================
;
; process-one-special-report.rule
; -------------------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : *PENDING-FORWARD-INFERENCES* register
;
;       description   : tries to use the report as a antecedent report and as a
;                       consequent report (hypothectical reasoning) to draw
;                       conclusions.
;                       If there was no cq-channel or i-channel waiting for it, 
;                       puts it into *PENDING-FORWARD-INFERENCES*
;
;       side-effects  : updates *PENDING-FORWARD-INFERENCES*
;
;                                        written :  rgh  2/09/86
;                                        modified:  njm 10/25/88
;
;
(defun process-one-special-report.rule (report)
  (let ((used-report nil))
    (setq used-report (broadcast-ant-report.rule report))
    (setq used-report (or (process-one-introduction-report.rule report) used-report)) 
    (unless used-report 
      (setq *PENDING-FORWARD-INFERENCES*
	    (insert.repset report *PENDING-FORWARD-INFERENCES*)))))
;
;
; =============================================================================
;
; broadcast-ant-report.rule
; -------------------------
;
;       arguments     : report - <report>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *RULE-USE-CHANNELS* register
;
;       description   : tries to send "report" to all channels in RULE-USE-
;                       CHANNELS: -- returns "true" if "report" is sent
;                       through at least one channel, "false" otherwise.
;
;       side-effects  : sends "report" if possible
;
;                                        written :  rgh  2/09/86
;                                        modified:
;
;
(defun broadcast-ant-report.rule (report)
  (let (anysent)
    (do.set (cqch *RULE-USE-CHANNELS* anysent)
      (setq anysent (or (try-to-send-cq-report.rule report cqch) anysent)))))
;
;
; =============================================================================
;
; try-applying-one-report
; -----------------------
;
;       arguments     : report - <report>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *RULE-USE-CHANNELS*, *USABILITY-TEST*
;
;       description   : tries applying the rule instance contained in "report"
;                       and returns "true" if a rule-use-channel is found
;                       which had requested this rule-use, "false" otherwise
;
;                                        written :  rgh  3/30/86
;                                        modified:  rgh  4/03/86
;                                                   rgh  4/06/86
;                                                   rgh  4/20/86
;                                                   njm  3/15/89
;                                                   dk   6/2/93
; added the trying of DO-IF transformer
;
;  
(defun try-applying-one-report (report) 
  (when (funcall *USABILITY-TEST* (sign.rep report))
    (let ((inst (rep-to-instance report))
	  (any-applied nil))
      (do.set (cqch *RULE-USE-CHANNELS* any-applied)
	(unless  (isnew.sup (filter.sup (support.rep report)
					(context.ch (channel.cqch cqch))))
	  (when (or
		 (and (eq *TYPE* 'DO-IF)
		      (funcall *RULE-HANDLER*
			       (antecedents *NODE* nil)
			       (make.ch (subst.rep report)  ; only this is used
					nil                 ; bogus
					(context.rep report); bogus
					(new.ns)            ; bogus
					'open)))            ; bogus
		 (and (member *TYPE* '(AND NOR))
		      (try-to-send-cq-report.rule report cqch))
		 (try-to-apply-instance.rule inst cqch))
	    (setq any-applied t)))))))
;
;
; =============================================================================
;
; try-to-apply-instance.rule
; --------------------------
;
;       arguments     : instance - <instance>
;                       cqch - <cq-channel>
;
;       returns       : <boolean>
;
;       description   : tries to apply a given rule instance to a given rule-
;                       use-channel.  That is, it finds out if this rule
;                       instance matches the filter on the channel and if the
;                       its support is a subset of the channel's context. If so,
;                       it sends requests to the antecedents and returns
;                       "true", otherwise it returns "false".
;
;       side-effects  : sends requests to rule antecedents
;
;       modification  : send down the substitution in which the rule is refered
;                       so that more specific instances may be requested.
;                       (scs 10/20/88)
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/03/86
;                                                   rgh  4/06/86
;                                                   scs 10/20/88
;                                                   njm 04/28/89
;                                    
;
;
(defun try-to-apply-instance.rule (instance cqch)
  (when (acceptable-rule-instances instance (channel.cqch cqch))
    (send-ant-requests.rule cqch (subst.inst instance))))
;
;
; =============================================================================
;
; send-ant-requests.rule
; ----------------------
;
;       arguments     : cqch  - <cq-channel>
;                       subst - <substitution> [optional]
;
;       returns       : <boolean>
;
;       nonlocal-vars : *NODE* register
;
;       description   : sends requests to the nodes in antecedent position
;                       of the current rule node, with respect to a given
;                       consequent.  Always returns "true".
;
;       side-effects  : sends requests to the antecedent nodes
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/03/86
;                                                   rgh  4/24/86
;
;
(defun send-ant-requests.rule (cqch &optional subst)
  (let ((ch (channel.cqch cqch))
	(ants (ants.cqch cqch))
	(ruis (ruiset.cqch cqch)))
    (unless (or (member *TYPE* '(and-entailment num-quantifier))
		(is-all-pat-same-vars ants))
      (setq *RULE-USE-CHANNELS*
	    (update.cqchset
	     (make.cqch ch ants (request-all ruis))
	     *RULE-USE-CHANNELS*)))
    (broadcast-ant-request ch ants (ch-to-restr ch) subst)))
;
;
; =============================================================================
;
; request-all
; -----------
;
;       arguments     : ruis - <rule-use-info set>
;
;       returns       : <rule-use-info set>
;
;       description   : returns "ruis" with all previously 'UNKNOWN antecedents
;                       now flagged to indicate 'REQUESTED
;
;                                        written :  rgh  4/03/86
;                                        modified:
;
;
(defun request-all (ruis)
  (if (isnew.ruis ruis)
      ruis
      (putin.ruis
	(make.rui
	  (subst.rui (choose.ruis ruis))
	  (poscount.rui (choose.ruis ruis))
	  (negcount.rui (choose.ruis ruis))
	  (request-all-1 (fns.rui (choose.ruis ruis)))
	  (remarkedp.rui (choose.ruis ruis)))
	(request-all (others.ruis ruis)))))
;
;
; =============================================================================
;
; request-all-1
; -------------
;
;       arguments     : fns - <flagged-node set>
;
;       returns       : <flagged-node set>
;
;       description   : helper function for request-all which returns a
;                       <flagged-node set> in which all of the previously
;                       'UNKNOWN nodes in "fns" have be flagged 'REQUESTED
;
;                                        written :  rgh  4/03/86
;                                        modified:  njm 10/25/88
;                                        modified:
;
;
(defun request-all-1 (fns)
  (if (isnew.fns fns)
      fns
      (putin.fns
	(make.fn
	  (node.fn (choose.fns fns))
	  (support.fn (choose.fns fns))
	  (if (eq (flag.fn (choose.fns fns)) 'UNKNOWN)
	      'REQUESTED
	      (flag.fn (choose.fns fns))))
	(request-all-1 (others.fns fns)))))
;
;
; =============================================================================
;
; broadcast-ant-request
; ---------------------
;
;       arguments     : req - <channel>
;                       ns - <node set>
;                       restr - <restriction>
;
;       returns       : T
;
;       nonlocal-vars : *NODE* register
;
;       description   : Sends requests to all of the nodes in "ns", where
;                       "ns" consists of the antecedents of the current rule
;                       node, with respect to the destination of "req" as a
;                       consequent.
;
;       side-effects  : sends the requests
;
;       modification  : so the request is not overly general (scs 10/20/88)
;
;                                        written :  rgh  4/24/86
;                                        modified:  scs  4/22/88
;                                        modified:  scs  6/20/88
;                                        modified:  scs  6/20/88
;   
;
;
(defun broadcast-ant-request (req ns restr subst)
  (let ((reqsubst (filter.ch req)))
    (do.ns (n ns t)
      (send-request
	(make.ch (mapcan #'(lambda (v)
			     (let ((term (or (match:bindingOf v reqsubst)
					     (match:bindingOf v subst))))
			       (when term (list (cons v term)))))
			 (freevars.n n))
		 (new.sbst)
		 (context.ch req)
		 *NODE*
		 'OPEN)
	n
	restr))))
;
;
; =============================================================================
;
; acceptable-rule-instances
; -------------------------
;
;       arguments     : inst - <instance>
;                       ch - <channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *NODE* register
;
;       description   : returns T if the INST is compatible with the filter
;                       of "ch", and belongs to the belief space of the "ch"
;                       context. Returns NIL otherwise.
;
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/06/86
;                                                   scs  3/15/88
;                                                   scs  3/24/88
;                                                   njm  4/28/89
;
;
(defun acceptable-rule-instances (inst ch)
  (and (compatible (subst.inst inst) (filter.ch ch))
       (not (isnew.sup (filter.sup (support.inst inst) (context.ch ch))))))
;
;
; =============================================================================
;
; try-to-send-cq-report.rule
; --------------------------
;
;       arguments     : report - <report>
;                       cqch - <cq-channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *NODE*, *KNOWN-INSTANCES*, *USABILITY-TEST* registers
;
;       description   : tests the antecedent report "report" against the rule-
;                       use-channel "cqch", and if it passes the channel
;                       filter, and either the rule is asserted or there are
;                       known instances which can be applied in this case,
;                       an attempt is made to draw conclusions using
;                       the rule.  Returns "true" if "report" passes the
;                       channel filter of "cqch", "false" otherwise.
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/06/86
;                                                   rgh  4/24/86
;                                                   cpf/njm 10/24/88
;
;
(defun try-to-send-cq-report.rule (report cqch)
  (let ((repset (acceptable-versions report (channel.cqch cqch)))
	(crntct (context.ch (channel.cqch cqch))))
    (declare (special crntct))
    (unless (isnew.repset repset)
      (cond ((isassert.n *NODE*)
	     (when (apply *USABILITY-TEST* (list 'POS))
	       (try-to-draw-conclusions.rule repset cqch)))
	    (t (do* ((iset *KNOWN-INSTANCES* (others.iset iset))
		     (inst (choose.iset iset) (choose.iset iset))
		     (updated-cqch cqch (get-updated-cqch cqch *RULE-USE-CHANNELS*)))
		    ((isnew.iset iset) t)
		 (when (and (match::issubset.sbst
			      (restrict.sbst
				(union.sbst (subst.rep report)
					    (filter.ch (channel.cqch cqch)))
				(freevars.n *NODE*))
			      (subst.inst inst))
			    (apply *USABILITY-TEST* (list (sign.inst inst))))
		   (try-to-draw-conclusions.rule
		     (fill-out-report-substs repset (subst.inst inst))
		     updated-cqch))))))))
;
;
; =============================================================================
;
; acceptable-versions
; -------------------
;
;       arguments     : rep - <report>
;                       ch - <channel>
;
;       returns       : <report set>
;
;       description   : tests each <mbind> in the <filter> of the <channel>
;                       "ch" against "report", and returns a <report set>
;                       consisting of all updates of "report" which will
;                       pass through the <filter>.  (A report may be updated
;                       by further binding of variables, and since in the
;                       case of multi-arcs this could generate several
;                       versions, a <report set> must be returned.)
;
;                                        written :  rgh 07/29/85
;                                        modified:  rgh 08/21/85
;                                        modified:  scs 05/24/88
;
;      new description: returns a singleton set of rep if its substitution
;                       passes the filter of ch, else returns (new.repset)
;
;                                        modified:  scs 06/17/88
;
;      modification: the subst of rep need only be compatible with the filter of ch
;                    because this is being used to check buckets in a rule-use-channel
;
;                                        modified:  ???????
;
;      modification: the support of "rep" need also to be included in the
;                    context of "ch"
;
;                                        modified:  njm 10/25/88
;
;
;
(defun acceptable-versions (rep ch)
  (if (and (compatible     (subst.rep rep) (filter.ch ch))
	   (not (isnew.sup (filter.sup (support.rep rep) (context.ch ch)))))
      (makeone.repset rep)
      (new.repset)))
;
;
; =============================================================================
;
; fill-out-report-substs
; ----------------------
;
;       arguments     : repset - <report set>
;                       sbst - <substitution>
;
;       returns       : <report set>
;
;       description   : Goes through the reports in "repset" and adds the
;                       bindings of "subst" to each.
;
;                                        written :  rgh  4/24/86
;                                        modified:  njm 10/21/88
;
;
(defun fill-out-report-substs (repset sbst)
  (let ((rep (choose.repset repset)))
    (cond ((isnew.repset repset) repset)
	  (t (putin.repset
	       (make.rep
                 (union.sbst sbst (subst.rep rep))
		 (support.rep rep)
		 (sign.rep rep)
		 (signature.rep rep)
		 (node.rep rep)
		 (context.rep rep))
	       (fill-out-report-substs (others.repset repset) sbst))))))
;
;
; =============================================================================
;
; try-to-draw-conclusions.rule
; ----------------------------
;
;       arguments     : repset - <report set>
;                       cqch - <cq-channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *RULE-HANDLER* register
;
;       description   : uses each of the reports in "repset" to try to draw
;                       conclusions requested by "cqch".  Always returns
;                       "true".
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/03/86
;
;
(defun try-to-draw-conclusions.rule (repset cqch)
  (do ((reps repset (others.repset reps)))
      ((isnew.repset reps) t)
    (apply *RULE-HANDLER* (list (choose.repset reps) cqch))))
;
;
; =============================================================================
;
; get-rule-use-info
; -----------------
;
;       arguments     : report - <report>
;                       cqch - <cq-channel>
;
;       returns       : <rule use info set>
;
;       description   : returns the set of rule-use-infos which are compatible
;                       with the reported instance, with each rule-use-info
;                       updated to include any new bindings in the report
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/24/86
;                                        modified:  scs  3/15/88
;                                        modified:  scs  6/21/88
;                                        modified:  njm/cpf 10/20/88
;
(defun get-rule-use-info (report cqch)
  ;;"Returns the set of rule-use-infos which are compatible
  ;; with the reported instance, with each rule-use-info
  ;; updated to include any new bindings in the report. 
  ;; 
  ;; modified by J. Choi 5/6/92.
  ;; major modification:
  ;;     Add a routine for updating the *RULE-USE-CHANNELS* register
  ;;     with compatible rule-use-infos.
  ;;     In the old version, this update of the *RULE-USE-CHANNLES*
  ;;     register is done in each rule handler procedure.
  ;;     This modification is made to be consistent with
  ;;     'get-rule-use-info-ptree' and 'get-rule-use-info-sindexing'
  ;;     which also update the *RULE-USE-CHANNELS* register. "

  (let ((flag nil)
	(sbst (subst.rep report))
	(ant (signature.rep report))
	(supp (support.rep report))
	(ants (ants.cqch cqch))
	(updated-ruiset (ruiset.cqch cqch))
	(result (new.ruis)))
    (do.set (rui (ruiset.cqch cqch))
      (setq flag (flag.fns ant (fns.rui rui)))
      (when (match::is-compatible.sbst sbst (subst.rui rui))
	(cond
	  ((or (null (fns.rui rui)) (eq flag 'UNKNOWN) (eq flag 'REQUESTED))
	   (setq result
		 (update.ruis
		   (update.rui
		     (make.rui
		      (union.sbst (subst.rui rui) sbst) (poscount.rui rui)
		      (negcount.rui rui) (fns.rui rui) (remarkedp.rui rui))
		     ant supp (sign.rep report))
		   result)))
	  ((and (member flag '(TRUE FALSE))
                (filter.sup (support.fns ant (fns.rui rui))
                            (context.rep report)))
	   (setq result
		 (update.ruis
		   (make.rui
		     (subst.rui rui) (poscount.rui rui) (negcount.rui rui)
		     (update.fns (fns.rui rui) ant supp flag)
		     (remarkedp.rui rui))
		   result))))))

    ;; the following statements are added to update *RULE-USE-CHANNELS*
    ;; with compatible rule-use-infos
    
    (do.set (rui result)
	(setq updated-ruiset (update.ruis rui updated-ruiset))
	(setq *RULE-USE-CHANNELS*
	      (update.cqchset
	        (make.cqch (channel.cqch cqch) ants updated-ruiset)
		*RULE-USE-CHANNELS*)))
    result))


(defun get-rule-use-info-sindexing (report cqch)
  ;;"Modified 'get-rule-use-info' for the S-indexing method that is applied 
  ;; when a rule's antecedents have the same set of variables. 
  ;; It returns the set of rule-use-infos which are compatible with
  ;; the reported instance.
  ;; The data structure of cqch for S-indexing is:
  ;;    <cqch> = (<channel> <antecedents> <sindex-ruiset>)
  ;;    <sindex-ruiset> = ((<sindex> <rui>) (<sindex> <rui>) ...)
  ;;          sindex-ruiset is initially empty.                       "
  
  (let* ((ant (signature.rep report))
	 (sbst (subst.rep report))
	 (sign (sign.rep report))
	 (supp (support.rep report))
	 (ch (channel.cqch cqch))
	 (ants (ants.cqch cqch))
	 (sindex-ruiset (ruiset.cqch cqch))
	 ;; sindex is obtained from variable substiutions.
	 ;; e.g., sbst = ((v1 . a) (v2 . b))
	 ;;       sindex = (a b)
	 (sindex
	  (mapcar #'cdr (restrict.sbst sbst (sneps::all-vars.n *NODE*))))
	 (ruis (rest (assoc sindex sindex-ruiset :test 'equal)))
	 (result nil))

    ;; build an initial (skeleton) rui for a given sindex
    ;; if there is no corresponding rui associated with the sindex

    (when (null ruis)
      (setq ruis
	    (request-all
	     (makeone.ruis
	      (make.rui (filter.ch ch)
			0
			0
			(nodeset-to-fnodeset ants)
			nil))))
      (setq sindex-ruiset (acons sindex ruis sindex-ruiset)))

    ;; update existing rui with the current instance
    (let* ((rui (choose.ruis ruis))
	   (flag (flag.fns ant (fns.rui rui))))
      (cond ((or (null (fns.rui rui))
		 (eq flag 'UNKNOWN) (eq flag 'REQUESTED))
	     (setq ruis
		   (makeone.ruis
		     (update.rui
		       (make.rui (union.sbst (subst.rui rui) sbst)
				 (poscount.rui rui)
				 (negcount.rui rui)
				 (fns.rui rui)
				 (remarkedp.rui rui))
		       ant supp sign)))
	     (rplacd (assoc sindex sindex-ruiset :test 'equal) ruis)
	     (setq result ruis))

	    ((and (member flag '(TRUE FALSE))
                  (filter.sup (support.fns ant (fns.rui rui))
                              (context.rep report)))
             (setq ruis
		     (makeone.ruis
		       (make.rui (subst.rui rui)
				 (poscount.rui rui)
				 (negcount.rui rui)
				 (update.fns (fns.rui rui) ant supp flag)
				 (remarkedp.rui rui))))
	       (rplacd (assoc sindex sindex-ruiset :test 'equal) ruis)
	       (setq result nil))))
    
    ;; update the *rule-use-channels* register with new sindex-ruiset
    (setq *RULE-USE-CHANNELS*
	  (update.cqchset
	   (make.cqch ch ants sindex-ruiset)
	   *RULE-USE-CHANNELS*)) 
    result))



    
    




