;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: num-quant.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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


;                 Numerical Quantifier Handler
;                =============================
;
;    Representation
;   ----------------
;   type1:  (build emin i emax j etot k pevb (V1, .., Vm)
;                  &ant (P1, ..., Pn) : Qn)
;
;   type2:  (build emin i etot k pevb (V1, ..., Vm)
;                  &ant (P1, ..., Pn) : Qn)
;
;   type3:  (build emax j  pevb (V1, ..., Vm)
;                  &ant (P1, ..., Pn) : Qn)
;
;    Semantics
;   -----------
;     type1:  There are k sequences of individuals, (t1, ..., tm), such that
;             (P1(v) & ... & Pn(v)) [t1/V1, ..., tm/Vm] is true, where v 
;             represents a sequence of variables <V1, ..., Vm>, and that
;             at least i and at most j of these k sequences also satisfy
;             Q(v).
;
;    type2, type3: simplification of type1
;
;    Rules of inference
;   ---------------------
;     When j individuals found satisfying P1(v)& ... &Pn(v)&Q(v), conclude 
;         that the remaining k-j individuals satisfying P1(v)&...&Pn(v) satisfy
;         ~Q(v).
;
;     When k-i individuals found satisfying P1(v)& ... &Pn(v)&~Q(v), conclude 
;         that the remaining i individuals satisfying P1(v)&...&Pn(v) satisfy
;         Q(v).
;
;     Detects Contradictions
;    ------------------------
;      When more that j individuals satisfy P1(v)& ... &Pn(v)&Q(v), and
;         more than k-i individuals satisfy P1(v)& ... &Pn(v)&~Q(v).
;
;
;


(defun rule-handler.num-quant (ant-report cqch)
  (declare (special *NODE*
		    *NUM-QUANT-POS-INSTANCES*
		    *NUM-QUANT-NEG-INSTANCES*))
  (let* ((i (if (nodeset.n *NODE* 'sneps::emin)
		(node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::emin)))
	      nil))
         (j (if (nodeset.n *NODE* 'sneps::emax)
		(node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::emax)))
	      nil))
	 (k (if (nodeset.n *NODE* 'sneps::etot)
		(node-to-number.n (choose.ns (nodeset.n *NODE* 'sneps::etot)))
	      nil))
	 (n (cardinality.ns (nodeset.n *NODE* 'sneps::&ant)))
	 (type1 (and i j k))
	 (type2 (and i (not j) k))
	 (type3 (and (not i) j (not k)))
	 (type4 (and i j (not k)))
;	 (ants (ants.cqch cqch))
	 (ch (channel.cqch cqch))
	 (destn (destination.ch ch)))
    
     ;; use the P-tree method for the numerical quantifiers
    
    (do.set (rui (get-rule-use-info-ptree ant-report cqch))
       (let ((f-sbst (restrict.sbst (subst.rui rui)
				    (freevars.n *NODE*)))
	     (q-sbst (restrict.sbst (subst.rui rui)
				    (quantified-vars.n *NODE*))))
	 (cond ((and (= (poscount.rui rui) (+ n 1))
		     (= (negcount.rui rui) 0))
		(setq *NUM-QUANT-POS-INSTANCES* 
		      (update-instances.num-quant f-sbst
						  q-sbst
						  *NUM-QUANT-POS-INSTANCES*))

		(if (and (or type1 type3 type4) 
			 (= (length
			     (select-instances.num-quant
			      f-sbst *NUM-QUANT-POS-INSTANCES*)) j))
		    (send-reports.num-quant 'NEG n f-sbst rui cqch)))
	 
	       
	       ((and (= (poscount.rui rui) n) (= (negcount.rui rui) 1))
		(when (or (and (eq (signature.rep ant-report) destn)
			       (eq (sign.rep ant-report) 'NEG))
			  (and (not (eq (signature.rep ant-report) destn))
			       (eq (sign.rep ant-report) 'POS)))
		  (setq *NUM-QUANT-NEG-INSTANCES* 
		     (update-instances.num-quant f-sbst
						 q-sbst
						 *NUM-QUANT-NEG-INSTANCES*))

		  (if (and (or type1 type2) 
			   (= (length
			       (select-instances.num-quant
				f-sbst *NUM-QUANT-NEG-INSTANCES*)) 
			      (- k i)))
		      (send-reports.num-quant 'POS n f-sbst rui cqch)))))))))


(defun usability-test.num-quant (sign)
  (and (eq sign 'POS)
       (or (isnew.ns (quantified-vars.n *NODE*))
           (not (isnew.ns (nodeset.n *NODE* 'sneps::pevb))))))



(defun send-reports.num-quant (sign n f-sbst old-rui cqch)
  (let* ((ch (channel.cqch cqch))
	 (ptree-ruiset (ruiset.cqch cqch))
	 (destn (destination.ch ch))
	 (ptree (first ptree-ruiset))
	 (node-ruiset (third ptree-ruiset))
	 (ant-ptree (first ptree))
	 (ruiset (rest (assoc ptree node-ruiset :test 'equal)))
	 (ants-ruiset (rest (assoc ant-ptree node-ruiset :test 'equal))))

   (do* ((ruis ants-ruiset (others.ruis ruis))
	 (rui (choose.ruis ruis) (choose.ruis ruis)))
	((isnew.ruis ruis))
     (let ((restr (make.restr (subst.rui rui)))
	   (q-sbst (restrict.sbst (subst.rui rui)
				  (quantified-vars.n *NODE*)))
	   (pos-instances (select-instances.num-quant
			     f-sbst
			     *NUM-QUANT-POS-INSTANCES*))
	   (neg-instances (select-instances.num-quant
			     f-sbst
			     *NUM-QUANT-NEG-INSTANCES*)))
       (when (and (match::is-compatible.sbst f-sbst (subst.rui rui))
		  (not (match::ismemb.sbst
			  q-sbst
			  (union.Set pos-instances neg-instances))))
	 (cond ((eq sign 'POS)
		(setq *NUM-QUANT-POS-INSTANCES*
		      (update-instances.num-quant f-sbst
						 q-sbst
						 *NUM-QUANT-POS-INSTANCES*))
		(inform.num-quant 'POS f-sbst restr neg-instances destn))
	       (t
		(setq *NUM-QUANT-NEG-INSTANCES*
		      (update-instances.num-quant f-sbst
						 q-sbst
						 *NUM-QUANT-NEG-INSTANCES*))
		(inform.num-quant 'NEG f-sbst restr pos-instances destn)))

	 (send-reports
	   (makeone.repset
	     (make.rep
	       (restrict.sbst (subst.rui rui) (freevars.n destn))
	       (compute-new-support.num-quant ch old-rui rui ruiset n sign)
	       sign *NODE* nil (context.ch ch)))
	   ch))))))



(defun update-instances.num-quant (f-sbst q-sbst list)
  (cond ((isnew.Set list) 
	 (makeone.Set (putin.Set f-sbst (makeone.Set (makeone.Set q-sbst)))))
	((iseq.sbst f-sbst (choose.Set (choose.Set list)))
	 (putin.Set 
	   (putin.Set f-sbst
	       (makeone.Set
		 (union.Set (makeone.Set q-sbst)
			    (choose.Set (others.Set (choose.Set list))))))
	   (others.Set list)))
	(t (putin.Set
	      (choose.Set list)
	      (update-instances.num-quant f-sbst q-sbst (others.Set list))))))


(defun select-instances.num-quant (f-sbst list)
  (cond ((isnew.Set list) nil)
	((iseq.sbst f-sbst (choose.Set (choose.Set list)))
	 (choose.Set (others.Set (choose.Set list))))
	(t (select-instances.num-quant f-sbst (others.Set list)))))



(defun inform.num-quant (sign f-sbst restr instances destn)
  (remark '"~%Since" (makeone.ns *NODE*) (make.restr f-sbst))
  (do.set (instance-sbst instances t)
    (if (eq sign 'POS)
	(remark '"and it is not the case that" 
	    (makeone.ns destn) (make.restr (union.sbst instance-sbst f-sbst)))
	(remark '"and" (makeone.ns destn) (make.restr (union.sbst instance-sbst f-sbst)))))
  (if (eq sign 'POS)
      (remark '"I infer" (makeone.ns destn) restr)
      (remark '"I infer it is not the case that" 
	      (makeone.ns destn) restr)))




(defun report-contradiction.num-quant (ct i j k ants nd rui)
  (format t "Contradiction found in NUM-QUANT ~S ~S ~S ~S ~S ~S ~S"
	  ct i j k ants nd rui)
  (stop))



;
;
; =============================================================================
;
; compute-new-support.num-quant
;-------------------------------
;
;       arguments     : ch  - <channel>
;                       old-rui - <rule-use-info>
;                       rui - <rule-use-info>
;                       rui-set - <rule-use-info-set>
;                       n - <number>
;                       sign - <sign>
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


(defun compute-new-support.num-quant (ch old-rui rui rui-set n sign)
  (let* ((crntct (context.ch ch))
	 (newsupport (new.sup))
	 (freevars (freevars.n *NODE*))
	 (f-sbst (restrict.sbst (subst.rui rui) freevars))
;	 (inst-num-quant (select-instances.num-quant 
;			   f-sbst 
;			   (if (eq sign 'POS) 
;			       *NUM-QUANT-NEG-INSTANCES* 
;			       *NUM-QUANT-POS-INSTANCES*)))
	 (fns (get-fns f-sbst rui-set n sign)))
    (setq fns (union.Set (fns.rui old-rui) (union.Set (fns.rui rui) fns)))
    (when (isassert.n *NODE*)
      (setq newsupport 
	    (compute-new-support1.num-quant fns (sneps:node-asupport *NODE*))))
    (when *KNOWN-INSTANCES*
      (do.set (inst *KNOWN-INSTANCES*)
	(let* ((instnode (match::applysubst *NODE* (subst.inst inst)))
	       (supinstnode (filter.sup (sneps:node-asupport instnode) crntct)))
	  (when (and (not (isnew.sup supinstnode))
		     (match:issubset.sbst (restrict.sbst (subst.rui rui) freevars)
					  (restrict.sbst (subst.inst inst) freevars)))
	    (setq newsupport 
		  (merge.sup newsupport
			     (compute-new-support1.num-quant fns  supinstnode)))))))
    newsupport))


;
;
; =============================================================================
;
; compute-new-support1.num-quant
; -------------------------------
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
;
(defun compute-new-support1.num-quant (fns sup)
  (let ((newsupport (new.sup))
	(allcombinations (combinations-of sup (fns-to-suplist fns))))
    (do ((s1 allcombinations (rest (rest s1))))
	((null s1) newsupport)
      (setq newsupport
	    (insert.sup (combine-ots* (first s1))
			(fullbuildcontext (new.ns) (second s1))
			newsupport)))))

;
;
;==================================================================================
;
; get-fns
;-----------------
;

(defun get-fns (f-sbst rui-set n sign)
  (do* ((ruis rui-set (others.ruis ruis))
	(rui (choose.ruis ruis) (choose.ruis ruis))
	(fns (new.Set) fns))
       ((isnew.ruis ruis) fns)
    (when (and (match::is-compatible.sbst f-sbst (subst.rui rui))
	       (= (poscount.rui rui) (if (eq sign 'POS) n (+ n 1)))
	       (= (negcount.rui rui) (if (eq sign 'POS) 1 0)))
      (setq fns (union.Set (fns.rui rui) fns)))))





    
    




