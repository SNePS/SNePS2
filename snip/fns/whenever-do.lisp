;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: whenever-do.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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


;  WHENEVER-DO functions
;

(defun rule-handler.whenever-do (ant-report cqch)
  ;; if the antecedents have the same set of variables,
  ;; use S-indexing, otherwise use linear ruiset handling
  (when (isassert.n *node*)
    (let ((ruis (if (is-all-pat-same-vars (ants.cqch cqch))
		    (get-rule-use-info-sindexing ant-report cqch)
		  (get-rule-use-info ant-report cqch))))
      (do.set (rui ruis t)
	      (when (and (eq (sign.rep ant-report) 'pos)
			 (>= (poscount.rui rui) 1))
		(let* ((restr (make.restr (subst.rui rui)))
		       (ch (channel.cqch cqch))
		       (reactionset
			(sneps:apply-subst.ns (subst.restr restr) 
					      (makeone.ns (destination.ch ch))))
		       (reaction (choose.ns reactionset))
		       pr)
		  (unless-remarkedp.rui
		   rui
		   (remark '"~%Since" (makeone.ns *node*) restr)
		   (remark '"and" (makeone.ns (signature.rep ant-report)) restr)
		   (remark '"I will perform" reactionset nil))
		  (when (isnew.ns (nodeset.n reaction 'action))
		    (error
		     "~&The supposed ACT node ~A does not have an ACTION."
		     reaction))
		  (activate-act.n reaction)
		  (setq pr (activation.n reaction))
		  (regstore pr '*priority* 'intend)
		  (regstore pr '*agenda* 'start)		
		  (multi:schedule pr multi::*act-queue*)))))))



;
; =============================================================================
;

(defun usability-test.whenever-do (sign)
  (declare (special *NODE*))
  (and (eq sign 'POS)
       (or (isnew.ns (quantified-vars.n *NODE*))
           (not (isnew.ns (nodeset.n *NODE* 'sneps::forall))))))


;
;
; =============================================================================



    
    




