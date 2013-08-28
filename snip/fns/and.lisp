;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: and.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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


;  and functions
;
(defun rule-handler.and (ant-report cqch)
  ;; if the antecedents have the same set of variables,
  ;; use S-indexing, otherwise use linear ruiset handling    
  (do.set (rui (if (or (null (ants.cqch cqch))
		       (is-all-pat-same-vars (ants.cqch cqch)))
		   (get-rule-use-info-sindexing ant-report cqch)
		   (get-rule-use-info ant-report cqch))
	       t)

      (case (sign.rep ant-report)
	(NEG  (if (equal (signature.rep ant-report)
			 (destination.ch (channel.cqch cqch)))
		  (let ((restr (make.restr (subst.rui rui)))
			(ch (channel.cqch cqch)))
		    (unless-remarkedp.rui
		     rui (remark '"~%I know it is not the case that"
			     (makeone.ns (destination.ch ch)) restr))
		    (send-reports
		     (makeone.repset
		      (make.rep
		       (restrict.sbst (subst.rui rui)
				      (freevars.n *NODE*))
		       (compute-new-support.and ch rui ant-report)
		       'NEG
		       *NODE*
		       nil
		       (context.ch ch)))
		     ch))))
	
	(POS  (if (or (equal (signature.rep ant-report)
			     (destination.ch (channel.cqch cqch)))
		      (equal (match::applysubst *NODE*
						(subst.rep ant-report))
			     (node.rep ant-report)))
		  (let ((restr (make.restr (subst.rui rui)))
			(ch (channel.cqch cqch)))
		    (unless-remarkedp.rui
		     rui (remark '"~%It is the case that"
				 (makeone.ns (destination.ch ch)) restr))
		    (send-reports
		     (makeone.repset
		      (make.rep
		       (restrict.sbst (subst.rui rui)
				      (freevars.n (destination.ch ch)))
		       (compute-new-support.and ch rui ant-report)
		       'POS
		       *NODE*
		       nil
		       (context.ch ch)
		       ))
		     ch)))))))

;
; ==============================================================================
;

(defun usability-test.and (sign)
  (declare (ignore sign))
  (or (isnew.ns (quantified-vars.n *NODE*))
      (not (isnew.ns (nodeset.n *NODE* 'sneps::forall)))))



;
;
; =============================================================================
;
; compute-new-support.and
; -----------------------
;
;       arguments     : ch     - <channel>
;                       rui    - <rule-use-info>
;                       antrep - <report>
;
;       returns       : <support>
;
;       description   : Computes a new support based on:
;                        1- the support of the rule node if it is asserted;
;                        2- the support of the instances (of the rule) which
;                           are asserted in the 'ch' context and has the 
;                           appropriate substitution. 
;
;
;
;                                        written :  njm  12/5/88
;                                        modified: 
;
;
(defun compute-new-support.and (ch rui antrep)

  ;  (cond ((eq (cardinality.ns (nodeset.n *NODE* 'sneps:arg)) 1)
  ;	 (change-tag-support (support.rep antrep)))
  ;	(t ...))
  
  (let ((crntct (context.ch ch))
	(newsupport (new.sup))
	(freevars (freevars.n *NODE*)))
    (when (isassert.n *NODE*)
      (setq newsupport
	    (compute-new-support1.and (support.rep antrep)
				      (sneps:node-asupport *NODE*))))
    (when *KNOWN-INSTANCES*
      (do.set (inst *KNOWN-INSTANCES*)
	(let* ((instnode (match::applysubst *NODE* (subst.inst inst)))
	       (supinstnode (filter.sup (sneps:node-asupport instnode) crntct)))
	  (when (and (not (isnew.sup supinstnode))
		     (match:issubset.sbst (restrict.sbst (subst.rui rui) freevars)
					  (restrict.sbst (subst.inst inst) freevars)))
	    (setq newsupport (merge.sup newsupport
					(compute-new-support1.nor (support.rep antrep)
								  supinstnode)))))))
    newsupport))
 
;
; =============================================================================
;
; compute-new-support1.and
; ------------------------
;
;       arguments     : sup  - <support>
;                       supr - <support>
;
;       returns       : <support>
;
;       description   : receives as arguments:
;                        'sup'  -- the support of the report
;                        'supr' -- the support of the rule node
;                       Computes a new support based on 'sup' and on the support
;                       of the `supr'.
;
;
;
;                                        written :  njm  12/5/88
;                                        modified: 
;
;
(defun compute-new-support1.and (sup supr)
  (let ((newsupport (new.sup)))
    (do* ((s1 supr (others.sup s1))
	  (ot1 (ot.sup s1) (ot.sup s1))
	  (cts1 (ctset.sup s1) (ctset.sup s1)))
	 ((isnew.sup s1) newsupport)
      (dolist (ct1 cts1)
	(do* ((s2 sup (others.sup s2))
	      (ot2 (ot.sup s2) (ot.sup s2))
	      (cts2 (ctset.sup s2) (ctset.sup s2)))
	     ((isnew.sup s2) t)
	  (dolist (ct2 cts2)
	    (setq newsupport
		  (insert.sup (combine-ots ot1 ot2)
			      (fullbuildcontext (new.ns) (make.cts ct1 ct2))
			      newsupport))))))))


;



    
    




