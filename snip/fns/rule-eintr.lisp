;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: rule-eintr.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; process-one-introduction-report.rule 
; ------------------------------------
;
;       arguments     : report - <report>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *INTRODUCTION-CHANNELS* register
;
;       description   : tries to send "report" to all channels in
;                       *INTRODUCTION-CHANNELS*.
;
;       side-effects  : asserts the rule node (or an instance rule node)
;                       if possible and sends a report.  
;
;                                        written : njm  11/06/88
;                                        modified:
;
;
(defun process-one-introduction-report.rule (report)
      (let ((anysent nil)) 
	(do.set (ich *INTRODUCTION-CHANNELS* anysent)
		(setq anysent (or (try-to-send-introduction-conclusion.rule report ich)
				  anysent)))))

;
;
; =============================================================================
;
; try-to-send-introduction-conclusion.rule 
; ----------------------------------------
;
;       arguments     : report - <report>
;                       ich - <i-channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *NODE*, *KNOWN-INSTANCES*, *USABILITY-TEST* registers
;
;       description   : tests the consequent report "report" against the
;                       *INTRODUCTION-CHANNELS* "ich", and if it passes the
;                       channel filter and the support of the report has tag 'DER
;                       and includes the rose hypothesis, an attempt is made to
;                       draw conclusions using report.
;
;
;                                        written :  njm 11/06/88
;
; 
;
(defun try-to-send-introduction-conclusion.rule (report ich)
  (let ((ch (channel.cqch ich)))
    (when (compatible (subst.rep report) (filter.ch ch))
      (let ((filtered-support (if (member *TYPE* '(AND NOR))
				  (filter.sup (support.rep report) (context.ch ch))
				  (filter-introduction-support (support.rep report)
							       (context.ich ich)
							       (context.ch ch)))))
	(unless (isnew.sup filtered-support)
	  (apply #'INTRODUCTION-END-HANDLER
		 (list (make.rep (subst.rep report)
				 filtered-support
				 (sign.rep report)
				 (signature.rep report)
				 (node.rep report)
				 (context.rep report)) 
		       ich)))))))

;
;
; =============================================================================
;
; filter-introduction-support 
; ---------------------------
;
;       arguments     : support-rep - <support>
;                       extra-hyps  - <node set>
;                       ct-channel  - <context>
;
;       returns       : <support>
;
;       description   : filters "support-rep":
;                        a) only 'DER tags are accepted
;                        b) only contexts that:
;                             1) are a subset of the union of "extra-hyps" with
;                                "ct-channel", and
;                             2) include "extra-hyps"
;                           are returned.
;
;                                        written :  njm 11/06/88
;
;
;
(defun filter-introduction-support (support-rep extra-hyps ct-channel)
  (let ((new-support (new.sup))
	(ct-channel-hyps (sneps:context-hyps ct-channel)))
    (dolist (ct (getcontextset.sup 'sneps:der support-rep) new-support)
      (when (equal extra-hyps (sneps:compl.ns (sneps:context-hyps ct)
					      ct-channel-hyps))
	(setq new-support
	      (insert.sup 'der
			  (fullbuildcontext (sneps:compl.ns (sneps:context-hyps ct)
							    extra-hyps)
					    (new.cts))
			  new-support))))))

;
; =============================================================================
;
(defun INTRODUCTION-END-HANDLER (intr-report ich)
  (do* ((ruis (get-introduction-info intr-report ich) (others.ruis ruis))
	(rui (choose.ruis ruis) (choose.ruis ruis))
	(ch (channel.ich ich))
	(cqs (consequents.ich ich))
	(updated-ruiset (ruiset.ich ich))
	(dest (destination.ch ch))
	(crntct (context.ch ch)))
       ((isnew.ruis ruis) t)

    ;; the negation of an AND rule is derived when a negative instance 
    ;; is reported from any argument of the rule.
    (if (and (equal *TYPE* 'AND)
	     (= (poscount.rui rui) 0)
	     (= (negcount.rui rui) 1))
	(let* ((restr (make.restr (subst.rui rui)))
	       (subst (if (is.n dest)
			  (restrict.sbst (subst.rui rui)
					 (sneps::all-vars.n dest))
			(subst.rui rui)))
	       (newsup (compute-new-support.intr (fns.rui rui)))
	       (report (make.rep subst newsup 'NEG  *NODE* nil crntct)))
	  (unless-remarkedp.rui
	   rui
	   (remark '"~%Since it is not the case that"
		   (makeone.ns (signature.rep intr-report)) restr)
	   (remark '"I infer it is not the case that"
		   (makeone.ns *NODE*) restr))
	  (send-reports (makeone.repset report) ch)))

    ;; the negation of a NOR rule is derived when a positive instance 
    ;; is reported from any argument of the rule.
    (if (and (equal *TYPE* 'NOR)
	     (= (poscount.rui rui) 1)
	     (= (negcount.rui rui) 0))
	(let* ((restr (make.restr (subst.rui rui)))
	       (subst (if (is.n dest)
			  (restrict.sbst (subst.rui rui)
					 (sneps::all-vars.n dest))
			  (subst.rui rui)))
	       (newsup (compute-new-support.intr (fns.rui rui)))
	       (report (make.rep subst newsup 'NEG  *NODE* nil crntct)))
	  (unless-remarkedp.rui
	   rui
	   (remark '"~%Since it is the case that"
		   (makeone.ns (signature.rep intr-report)) restr)
	   (remark '"I infer it is not the case that"
		   (makeone.ns *NODE*) restr))
	  (send-reports (makeone.repset report) ch)))

    ;; an AND rule is derived when a positive report is received from 
    ;; all arguments of the rule, and a NOR rule is derived when
    ;; a negative report is received from all arguments of the rule.

    (when (eq (if (equal *TYPE* 'NOR) (negcount.rui rui) (poscount.rui rui))
	      (cardinality.ns cqs))
      (let* ((aants (context.ich ich))
	     (restr (make.restr (subst.rui rui)))
	     (subst (if (is.n dest)
			(restrict.sbst (subst.rui rui)
				       (sneps::all-vars.n dest))
		      (subst.rui rui)))
	     (newsup (compute-new-support.intr (fns.rui rui)))	     
	     (report (make.rep subst newsup 'POS *NODE* nil crntct)))
	(inform-introduction restr aants cqs)
	(send-reports (makeone.repset report) ch)
	(try-to-use-introduction-conclusion report)))
    (setq updated-ruiset (update.ruis rui updated-ruiset))
    (setq *INTRODUCTION-CHANNELS*
	  (update.ichset (make.ich ch (context.ich ich) cqs updated-ruiset)
			 *INTRODUCTION-CHANNELS*))))

;
; =============================================================================
;
; try-to-use-introduction-conclusion
; ----------------------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : the *KNOWN-INSTANCES* and *NODE* registers,
;                       plus *ADDED-NODES*
;
;       description   : handles reports of instances of the rule due
;                       to the aplication of a introduction deduction rule.
;                       The instance is broadcast to all apropriate outgoing
;                       channels.  Also, if this rule instance was
;                       requested for use in deducing a consequent, the new
;                       rule instance is applied.
;
;       side-effects  : *KNOWN-INSTANCES*, and *ADDED-NODES* are updated.
;
;                                        written :  njm 11/10/88
;                                        modified:  
;
;
(defun try-to-use-introduction-conclusion (report)
  (let ((instance (rep-to-instance report)))
    (declare (special *ADDED-NODES*))
    (cond ((isnew.sbst (subst.inst instance))
	   (broadcast-one-report report)
	   (try-applying-one-report report))
	  ((unknown.inst instance)
	   (let ((newnode (if (quantified-vars.n *NODE*)
			      (choose.ns
				(sneps:find-or-build
				  (sneps::clean-quantifiers.cs
				    (apply-subst.cs (subst.rep report)
						    (n-to-downcs *NODE*)))))
			      (match::applysubst *NODE* (subst.rep report)))))
	     (setq *ADDED-NODES* (insert.ns newnode *ADDED-NODES*)
		   *KNOWN-INSTANCES* (insert.iset instance *KNOWN-INSTANCES*)))
	   (broadcast-one-report report)
	   (try-applying-one-report report)))))
;
;
; =============================================================================
;
; get-introduction-info 
; ---------------------
;
;       arguments     : report - <report>
;                       ich - <i-channel>
;
;       returns       : <rule use info set>
;
;       description   : returns the set of rule-use-infos which are compatible
;                       with the reported instance, with each rule-use-info
;                       updated to include any new bindings in the report
;
;                                        written :  njm 11/07/88
;
(defun get-introduction-info (report ich)
  (let ((flag nil)
	(sbst (subst.rep report))
	(cq (signature.rep report))
	(supp (support.rep report))
	(result (new.ruis)))
    (do.set (rui (ruiset.ich ich) result)
      (setq flag (flag.fns cq (fns.rui rui)))
      (when (match::is-compatible.sbst sbst (subst.rui rui))
	(cond
	  ((or (null (fns.rui rui)) (eq flag 'unknown) (eq flag 'requested))
	   (setq result
		 (update.ruis
		   (update.rui
		     (make.rui (union.sbst (subst.rui rui) sbst)
                               (poscount.rui rui)
			       (negcount.rui rui)
                               (fns.rui rui)
                               (remarkedp.rui rui))
		     cq supp (sign.rep report))
		   result)))
	  ((and (member flag '(true false))
                (filter.sup (support.fns cq (fns.rui rui))
                            (context.rep report)))
	   (setq result
		 (update.ruis
		   (make.rui (subst.rui rui)
                             (poscount.rui rui)
                             (negcount.rui rui)
			     (update.fns (fns.rui rui) cq supp flag)
                             (remarkedp.rui rui))
		   result))))
	))))

;
;
; =============================================================================
;
; inform-introduction 
; -------------------
;
;       arguments     : restr - <restriction>
;                       cqs   - <node set>
;                       aants - <node set>
;
;       returns       : <never mind>
;
;       description   : informs the user the inference of a rule, based on
;                       hypothectical reasoning.
;       
;
;                                        written :  njm  11/06/88
;                                        modified:  hc   10/08/92
;
(defun inform-introduction (restr aants cqs)
  (case *TYPE*
    (AND (remark "~%Since" (makeone.ns (choose.ns cqs)) restr)
	 (do.ns (cq (others.ns cqs))
	   (remark "and " (makeone.ns cq) restr))
	 (remark "I infer" (makeone.ns *NODE*) restr))
    (NOR (remark "~%Since it is not the case that"
		 (makeone.ns (choose.ns cqs))
		 restr)
	 (do.ns (cq (others.ns cqs))
	   (remark "and since not" (makeone.ns cq) restr))
	 (remark "I infer" (makeone.ns *NODE*) restr))
    (t (remark "~%Since" (makeone.ns (choose.ns cqs)) restr)
       (do.ns (cq (others.ns cqs))
	 (remark "and" (makeone.ns cq) restr))
       (remark (format nil "~:[were~;was~] derived assuming"
		       (= (cardinality.ns cqs) 1))
	       (makeone.ns (choose.ns aants))
	       restr)
       (do.ns (ant (others.ns aants))
	 (remark "and assuming" (makeone.ns ant) restr))
       (remark "I infer" (makeone.ns *NODE*) restr))))

;
;
; =============================================================================
;
; compute-new-support.intr 
; ------------------------
;
;       arguments     : fns - <fns>
;                       
;       returns       : <support>
;
;       description   : receives as arguments:
;                        'fns' -- a flag node set
;                       Computes a new support based on all supports
;                       present in 'fns'.
;
;       implementation: the structure of the variable allcombinations is as follows:
;                         ((ot ... ot) (ct ... ct) ... (ot ... ot) (ct ... ct))
;                       where each pair '(ot ... ot) (ct ... ct)' has an ot and a ct
;                       of each flag node present in 'fns'.
;       
;
;                                        written :  njm  11/06/88
;                                        modified: 
;
;
(defun compute-new-support.intr (fns)
  (let* ((newsupport (new.sup))
	 (suplist (fns-to-suplist fns))
	 (allcombinations (if (null (rest suplist))
			      (sup-to-combinations (first suplist))  
			      (combinations-of (first suplist) (rest suplist)))))
    (do ((s1 allcombinations (rest (rest s1))))
	((null s1) newsupport)
      (setq newsupport
	    (insert.sup (if (equal *TYPE* 'AND)
			    (combine-and-intr-ots* (first s1) (second s1))
			    (combine-ots* (first s1)))
			(fullbuildcontext (new.ns) (second s1))
			newsupport)))))












    
    




