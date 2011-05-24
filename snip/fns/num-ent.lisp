;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: num-ent.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;  numerical-entailment functions
;
(defun rule-handler.num-ent (ant-report cqch)
  ;; if the antecedents have the same set of variables,
  ;; use S-indexing, otherwise use linear ruiset handling    
  (do* ((ruis (if (is-all-pat-same-vars (ants.cqch cqch))
		  (get-rule-use-info-sindexing ant-report cqch)
		  (get-rule-use-info ant-report cqch))
	      (others.ruis ruis))
	(ants (ants.cqch cqch))
	(rui (choose.ruis ruis) (choose.ruis ruis)))
       ((isnew.ruis ruis))
    (when (not (< (poscount.rui rui)
		  (node-to-number.n
		     (choose.ns (nodeset.n *NODE* 'sneps:thresh)))))
      (let ((restr (make.restr (subst.rui rui)))
	    (ch (channel.cqch cqch)))
	(unless-remarkedp.rui
	  rui (remark '"~%Since" (makeone.ns *NODE*) restr))
	(do.ns (next-ant ants)
	  (when (eq (flag.fns next-ant (fns.rui rui)) 'TRUE)
	    (unless-remarkedp.rui
	      rui (remark '"~%and~%" (makeone.ns next-ant) restr))))
	(unless-remarkedp.rui
	  rui (remark '"~%I infer~%" (makeone.ns (destination.ch ch)) restr))
	(send-reports
	  (makeone.repset
	    (make.rep
	      (restrict.sbst (subst.rui rui) (freevars.n (destination.ch ch)))
	      (compute-new-support.&-ent ch rui)
	      'POS *NODE* nil (context.ch ch)))
	  ch)))))

(defun usability-test.num-ent (sign)
  (and (eq sign 'POS)
       (or (isnew.ns (quantified-vars.n *NODE*))
           (not (isnew.ns (nodeset.n *NODE* 'sneps::forall))))))




    
    




