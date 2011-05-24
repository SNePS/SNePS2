;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: and-or.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;  and-or functions
;
(defun rule-handler.and-or (ant-report cqch)
  (let* ((min (node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::min))))
	 (max (node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::max))))
	 (tot (cardinality.ns (nodeset.n *NODE* 'sneps::arg)))
	 (ch (channel.cqch cqch))
	 (ants (ants.cqch cqch)))
    
    ;; if the antecedents have the same set of variables,
    ;; use S-indexing, otherwise use linear ruiset handling    
    (do.set (rui (if (is-all-pat-same-vars ants)
		     (get-rule-use-info-sindexing ant-report cqch)
		     (get-rule-use-info ant-report cqch)))
      (cond ((> (poscount.rui rui) max)
	     (report-contradiction.and-or
	      "More true arguments than max"
	      (context.ch ch)  min  max  ants  *node* (select-pos.rui rui)))
	    ((> (negcount.rui rui) (- tot min))
	     (report-contradiction.and-or
	      "More false arguments than tot-min"
	      (context.ch ch)  min  max  ants  *node* (select-neg.rui rui)))
	    ((= (poscount.rui rui) max)  
	     (let ((restr (make.restr (subst.rui rui))))
	       (unless-remarkedp.rui rui
				     (inform-neg.and-or restr ch ants rui))
	       (send-reports
		 (makeone.repset
		   (make.rep (restrict-binding-to-pat (subst.rui rui)
						      (destination.ch ch))
			     (compute-new-support.and-or ch
							 (select-pos.rui rui)
							 (subst.rui rui))
			     'NEG
			     *NODE*
			     nil
			     (context.ch ch)))
		 ch)))
	    ((= (negcount.rui rui) (- tot min)) 
	     (let ((restr (make.restr (subst.rui rui))))
	       (unless-remarkedp.rui rui
				     (inform-pos.and-or restr ch ants rui))
	       (send-reports
		 (makeone.repset
		   (make.rep (restrict-binding-to-pat (subst.rui rui)
						      (destination.ch ch))
			     (compute-new-support.and-or ch
							 (select-neg.rui rui)
							 (subst.rui rui))
			     'POS
			     *NODE*
			     nil
			     (context.ch ch)))
		 ch)))))))


;
;
; =============================================================================
;
; inform-neg.and-or 
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
(defun inform-neg.and-or (restr ch ants rui)
  (remark '"~%Since"
	  (makeone.ns *NODE*)
	  nil)
  (do* ((ant-set ants (others.ns ant-set))
	(next-ant (choose.ns ant-set) (choose.ns ant-set)))
       ((isnew.ns ant-set))
    (if (eq (flag.fns next-ant (fns.rui rui)) 'TRUE)
	(remark '"and"
		(makeone.ns next-ant)
		restr)))
  (remark '"I infer it is not the case that"
	  (makeone.ns (destination.ch ch))
	  restr))

;
;
; =============================================================================
;
; inform-pos.and-or 
; -----------------
;
;       arguments     : restr - <restriction>
;                       ch    - <channel>
;                       ants  - <node set>
;                       rui   - <rule use info>
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
(defun inform-pos.and-or (restr ch ants rui)
  (remark '"~%Since"
	  (makeone.ns *NODE*)
	  nil)
  (do* ((ant-set ants (others.ns ant-set))
	(next-ant (choose.ns ant-set) (choose.ns ant-set)))
       ((isnew.ns ant-set))
    (if (eq (flag.fns next-ant (fns.rui rui)) 'FALSE)
	(remark '"and it is not the case that"
		(makeone.ns next-ant)
		restr)))
  (remark '"I infer"
	  (makeone.ns (destination.ch ch))
	  restr))

;
;
; =============================================================================
;
; report-contradiction.and-or 
; ---------------------------
;
;       arguments     : msg  - <string>
;                       ct   - <context>
;                       min  - <integer>
;                       max  - <integer>
;                       ants - <node set>
;                       nd   - <node>
;                       rui  - <rui>
;
;       returns       : 
;
;       description   : Reports contradictions found in AND-OR
;       
;
;                                        written :  jpm  12/2/82
;                                        modified:  njm  10/27/88
;                                        modified:  scs/flj  6/20/04
;
(defun report-contradiction.and-or (msg ct min max ants nd rui)
  (sneps:sneps-error (format nil "Contradiction in an and-or: ~A in node ~A"
			     msg nd)
		     'SNIP 'report-contradiction.and-or))

;
;
; =============================================================================
;
; compute-new-support.and-or
; --------------------------
;
;       arguments     : ch  - <channel>
;                       fns - <flag-node-set>
;                       subst - <substitution>
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
;                                        written :  cpf/njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support.and-or (ch fns subst)
  (let ((crntct (context.ch ch))
	(newsupport (new.sup))
	(freevars (freevars.n *NODE*)))
    (when (isassert.n *NODE*)
      (setq newsupport
	    (compute-new-support1.and-or fns
					 (sneps:node-asupport *NODE*))))
    (when *KNOWN-INSTANCES*
      (do.set (inst *KNOWN-INSTANCES*)
	(let* ((instnode (match::applysubst *NODE* (subst.inst inst)))
	       (supinstnode (filter.sup (sneps:node-asupport instnode) crntct)))
	  (when (and (not (isnew.sup supinstnode))
		     (match:issubset.sbst (restrict.sbst subst freevars)
					  (restrict.sbst (subst.inst inst) freevars)))
	    (setq newsupport (merge.sup newsupport
					(compute-new-support1.and-or fns
								     supinstnode)))))))
    newsupport))



;
;
; =============================================================================
;
; compute-new-support1.and-or 
; ---------------------------
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
;
;                                        written :  njm  10/25/88
;                                        modified: 
;
;
(defun compute-new-support1.and-or (fns sup)
  (let ((newsupport (new.sup))
	(allcombinations (combinations-of sup (fns-to-suplist fns))))
    (do ((s1 allcombinations (rest (rest s1))))
	((null s1) newsupport)
      (setq newsupport
	    (insert.sup (combine-ots* (first s1))
			(fullbuildcontext (new.ns) (second s1))
			newsupport)))))

;
; =============================================================================
;


(defun usability-test.and-or (sign)
  (and (eq sign 'POS)
       (or (isnew.ns (quantified-vars.n *NODE*))
           (not (isnew.ns (nodeset.n *NODE* 'sneps::forall))))))


;
; =============================================================================
;



    
    




