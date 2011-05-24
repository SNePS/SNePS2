;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: and-ent.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;  and-entailment functions
;
(defun rule-handler.&-ent (ant-report cqch)
  ;; if the antecedents have the same set of variables,
  ;; use S-indexing, otherwise use P-tree
  (do* ((ants (ants.cqch cqch))
	(ruis (if (is-all-pat-same-vars ants)
		  (get-rule-use-info-sindexing ant-report cqch)
		  (get-rule-use-info-ptree ant-report cqch))
	      (others.ruis ruis))
	(rui (choose.ruis ruis) (choose.ruis ruis)))
       ((isnew.ruis ruis))
    (when (eql (poscount.rui rui) (cardinality.ns ants))
      (let ((restr (make.restr (subst.rui rui)))
	    (ch (channel.cqch cqch)))
	(unless-remarkedp.rui
	  rui (remark '"~%Since" (makeone.ns *NODE*) restr)
	  (do.ns (ant ants)	
	     (remark '"and" (makeone.ns ant) restr))
	  rui (remark '"I infer" (makeone.ns (destination.ch ch)) restr))
	(send-reports
	  (makeone.repset
	    (make.rep
	      (restrict.sbst (subst.rui rui) (freevars.n (destination.ch ch)))
	      (compute-new-support.&-ent ch rui)
	      'POS
	      *NODE*
	      nil
	      (context.ch ch)
	      ))
	  ch)))))

(defun usability-test.&-ent (sign)
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
;                                        written :  cpf/njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support.&-ent (ch rui)
  (let ((crntct (context.ch ch))
	(newsupport (new.sup))
	(freevars (freevars.n *NODE*)))
    (when (isassert.n *NODE*)
      (setq newsupport
	    (compute-new-support1.&-ent (fns.rui rui)
					(sneps:node-asupport *NODE*))))
    (when *KNOWN-INSTANCES*
      (do.set (inst *KNOWN-INSTANCES*)
	(let* ((instnode (match::applysubst *NODE* (subst.inst inst)))
	       (supinstnode (filter.sup (sneps:node-asupport instnode) crntct)))
	  (when (and (not (isnew.sup supinstnode))
		     (match:issubset.sbst (restrict.sbst (subst.rui rui) freevars)
					  (restrict.sbst (subst.inst inst) freevars)))
	    (setq newsupport (merge.sup newsupport
					(compute-new-support1.&-ent (fns.rui rui)
								    supinstnode)))))))
    newsupport))


;
;
; =============================================================================
;
; compute-new-support1.&-ent 
; -------------------------
;
;       arguments     : fns - <fns>
;                       sup - <support>
;
;       returns       : <support>
;
;       description   : receives as arguments:
;                        'fns' -- a flag node set
;                        'sup' -- the support of the rule node
;                       Computes a new support based on 'sup' and on all supports
;                       present in 'fns'.
;
;       implementation: the structure of the variable allcombinations is as follows:
;                         ((ot ... ot) (ct ... ct) ... (ot ... ot) (ct ... ct))
;                       where each pair '(ot ... ot) (ct ... ct)' has an ot and a ct
;                       of each flag node present in 'fns'.
;       
;
;                                        written :  njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support1.&-ent (fns sup)
  (let ((newsupport (new.sup))
	(allcombinations (combinations-of sup (fns-to-suplist fns))))
    (do ((s1 allcombinations (rest (rest s1))))
	((null s1) newsupport)
      (setq newsupport
	    (insert.sup (combine-ots* (first s1))
			(fullbuildcontext (new.ns) (second s1))
			newsupport)))))




    
    




