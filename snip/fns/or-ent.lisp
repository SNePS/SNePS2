;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: or-ent.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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


;  or-entailment functions
;
(defun rule-handler.v-ent (ant-report cqch)
  ;; if the antecedents have the same set of variables,
  ;; use S-indexing, otherwise use linear ruiset handling
  (let ((ruis (if (is-all-pat-same-vars (ants.cqch cqch))
		  (get-rule-use-info-sindexing ant-report cqch)
		  (get-rule-use-info ant-report cqch))))
    (do.set (rui ruis t)
      (when (and (eq (sign.rep ant-report) 'POS) (>= (poscount.rui rui) 1))
	(let ((restr (make.restr (subst.rui rui)))
	      (ch (channel.cqch cqch)))
	  (unless-remarkedp.rui
	    rui (remark '"~%Since" (makeone.ns *NODE*) nil)
	    (remark '"and" (makeone.ns (signature.rep ant-report)) restr)
	    (remark '"I infer" (makeone.ns (destination.ch ch)) restr))
	  (send-reports
	    (makeone.repset
	      (make.rep (restrict.sbst (subst.rui rui)
				       (freevars.n (destination.ch ch)))
			(compute-new-support.v-ent ch rui ant-report)
			'POS
			*NODE*
			nil
			(context.ch (channel.cqch cqch))
			))
 	    ch))))))
;
; =============================================================================
;

(defun usability-test.v-ent (sign)
  (declare (special *NODE*))
  (and (eq sign 'POS)
       (or (isnew.ns (quantified-vars.n *NODE*))
           (not (isnew.ns (nodeset.n *NODE* 'sneps::forall))))))


;
;
; =============================================================================
;
; compute-new-support.v-ent
; -------------------------
;
;       arguments     : ch  - <channel>
;                       rui - <rule-use-info>
;                       nd  - <node>
;
;       returns       : <support>
;
;       description   : receives as arguments:
;                        'rui' -- a rule use info
;                        'nd'  -- the antecedent node that has sent a report which
;                                 will permit a derivation with a new support
;                       Computes a new support based on:
;                        1- the support of the rule node if it is asserted;
;                        2- the support of the instances (of the rule) which
;                           are asserted in the 'ch' context and has the 
;                           appropriate substitution. 
;
;
;
;                                        written :  cpf/njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support.v-ent (ch rui ant-report)
  (let ((crntct (context.ch ch))
	(newsupport (new.sup))
	(freevars (freevars.n *NODE*)))
    (when (isassert.n *NODE*)
      (setq newsupport
	    (compute-new-support1.v-ent (fns.rui rui)
					(signature.rep ant-report)
					(sneps:node-asupport *NODE*))))
    (when *KNOWN-INSTANCES*
      (do.set (inst *KNOWN-INSTANCES*)
	(let* ((instnode (match::applysubst *NODE* (subst.inst inst)))
	       (supinstnode (filter.sup (sneps:node-asupport instnode) crntct)))
	  (when (and (not (isnew.sup supinstnode))
		     (match:issubset.sbst (restrict.sbst (subst.rui rui) freevars)
					  (restrict.sbst (subst.inst inst) freevars)))
	    (setq newsupport (merge.sup newsupport
					(compute-new-support1.v-ent (fns.rui rui)
								    (signature.rep ant-report)
								    supinstnode)))))))
    newsupport))

; 
;
; =============================================================================
;
; compute-new-support1.v-ent
; -------------------------
;
;       arguments     : fns - <fns>
;                       nd  - <node>
;                       sup - <support>
;
;       returns       : <support>
;
;       description   : receives as arguments:
;                        'fns' -- a flag node set
;                        'nd'  -- the antecedent node that has sent a report which
;                                 will permit a derivation with a new support
;                        'sup' -- the support of the rule node
;                       Computes a new support based on 'sup' and on the support
;                       of the 'nd' antecedent present in 'fns'
;
;
;
;                                        written :  cpf/njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support1.v-ent (fns nd sup)
  (let ((newsupport (new.sup))
	(supnd (support.fns nd fns)))
    (do* ((s1 supnd (others.sup s1))
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





    
    




