;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: fcableset.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




(in-package :sneps)


; =============================================================================
; Data Type:   <flat cableset> ::= 
;       (<relation> <node set> <relation> <node set> ... <relation> <node set>)
; =============================================================================
; 
;
; =============================================================================
;
; new.fcs 
; -------
;
;       arguments     : none
;
;       returns       : <flat cable set>
;
;       description   : Creates a <newflatcableset>.
;
;                                        written:  ejm 09/27/83
;                                        modified:
;
;
(defmacro new.fcs ()
   `()) 
;
;
; =============================================================================
;
; is.fcs
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <flat cable set>,
;                       "false" otherwise. 
;
;       implementation: it does not check if all the elements of "u" are
;                       <relation>-<node set> pairs, it just checks the first
;                       one.
;
;                                        written:  ejm 09/27/83
;                                        modified:
;
;
(defmacro is.fcs (u) 
   `(or (null ,u)
        (and (listp ,u)
             (is.r  (first ,u))
             (is.ns (second ,u)))))
;
;
; =============================================================================
;
; isnew.fcs
; ---------
;
;       arguments     : fcs - <flat cable set>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "fcs" is a <newflatcableset>,
;                       "false" otherwise.
;
;
;                                        written:  ejm 09/27/83
;                                        modified:
;
;
(defmacro isnew.fcs (fcs)
  `(null ,fcs))

;; Moved here where it belongs from `sneps;ds;util.lisp':
(defmacro do.fcs ((rvar nsvar fcs &optional resultform) &body forms)
  "Iterator for flat cable sets.
`forms' are evaluated with `rvar' and `nsvsar' set to successive
relations and nodesets in `fcs'. The value of `resultform' is returned."
  ;;                                                written:  scs 02/27/87
  ;;                                                modified: scs 05/12/88
  `(do* ((fcsvar ,fcs (cddr fcsvar))
	 (,rvar (first fcsvar) (first fcsvar))
	 (,nsvar (second fcsvar) (second fcsvar)))
	((null fcsvar) ,resultform)
	,@forms))

;
; 
;;; ==========================================================================
;;;
;;; getnodeset.fcs
;;; --------------
;;;
;;;     arguments     : r - <relation>
;;;                   : fcs - <flat cable set>
;;;
;;;     returns       : <node set>
;;;
;;;     description   : It returns the <node set> corresponding to the 
;;;                     <relation> "r" in the <flat cable set> "fcs".
;;;
;;;                                      written:  ssc 02/21/87
;;;                                      modified: ssc 06/17/87
;;;
(defun getnodeset.fcs (r fcs)
     (cl:getf fcs r))
;
; =============================================================================
;
; insert.fcs 
; ----------
;
;       arguments    : r   - <relation>
;                      ns  - <node set>
;                      fcs - <flat cable set>
;
;       returns      : <flat cable set>
;
;       description  : Looks for the <relation> "r" in the <flat cable set> "fcs"
;                      and if it is there, then it unions the <node set> "ns" with
;                      the nodeset stored under the relation "r" in "fcs".  Otherwise
;                      it inserts the new relation "r" and its nodeset "ns" at the
;                      proper place in the flat cableset "fcs". 
;
;                                        written:  ssc 02/24/87
;                                        modified: scs 05/10/88
;
;
(defun insert.fcs (r ns fcs)
  (if fcs
    (let ((fcs-ns (getnodeset.fcs r fcs)))
      (if fcs-ns
	(substitute (union.ns fcs-ns ns) fcs-ns fcs)
	(if (isless.r r (first fcs))
	  (cons r (cons ns fcs))
	  (do ((tl (cdr fcs) (cddr tl)))
	      ((null (cdr tl))
	       (setf (cdr tl) (list r ns))
	       fcs)
	      (when (isless.r r (cadr tl))
		(setf (cdr tl) (cons r (cons ns (cdr tl))))
		(return fcs))))))
    (list r ns)))


; =============================================================================
;
; delete.fcs
; ----------
;
;       arguments    : r   - <relation>
;                      fcs - <flat cable set>
;
;       returns      : <flat cable set>
;
;       description  : Deletes the R cable in the flat cable set FCS and returns a
;                      copy of the modified cable set.
;
;       written      : hc 6/7/90
;
(defun delete.fcs (r fcs)
  "Deletes the R cable in the flat cable set FCS and returns a
copy of the modified cable set."
  (let ((position (position r fcs :test #'(lambda (x y)
					    (iseq.r x y)))))
    (cond (position
	   (append (subseq fcs 0 position)
		   (subseq fcs (cl:+ position 2))))
	  (t fcs))))
;
; 
; =============================================================================
;
; relation.fcs
; ------------
;
;       arguments     : fcs - <flat cable set>
;
;       returns       : <relation> 
;
;       description   : returns the first <relation> of "fcs".
;
;                                        written:  ejm 09/27/83
;                                        modified:
;
;
(defmacro relation.fcs (fcs)
     `(first ,fcs))
;;;
; =============================================================================
;
; nodeset.fcs
; -----------
;
;       arguments     : fcs - <flat cable set>
;
;       returns       : <node set> 
;
;       description   : returns the first <node set> of "fcs".
;
;                                        written:  ejm 09/27/83
;                                        modified:
;
;
(defmacro nodeset.fcs (fcs)
   `(second ,fcs))
;
;
; =============================================================================
;
; others.fcs 
; ----------
;
;       arguments     : fcs - <flat cable set>
;
;       returns       : <flat cable set> 
;
;       description   : returns a <flat cable set> identical to "fcs" but without
;                       the first <relation>-<node set> pair.
;
;                                        written:  ejm 09/27/83
;                                        modified:
;
;
(defmacro others.fcs (fcs)
   `(rest (rest ,fcs)))
;
;
; =============================================================================
;
; describe.fcs
; ------------
;
;       arguments     : fcs - <flat cable set>
;
;       returns       : <sequence>
;
;       description   : It returns a <sequence> which is a description of 
;                       the <flat cable set> "fcs" to be printed.
;
;                                        written :  ejm 06/05/84
;                                        modified:
;
;(declare (localf describe.fcs))
(defun describe.fcs (fcs)
   (declare (special fcs))
   (cond ((isnew.fcs fcs) nil)
         (t (cons (describe.r (relation.fcs fcs))
		  (cons (describe.ns (nodeset.fcs fcs))
			(describe.fcs (others.fcs fcs)))))))
;;;
;;; ==========================================================================
;;;
;;; replace.fcs
;;; -----------
;;;
;;;     arguments     : r - <rel>
;;;                   : ns - <node set>
;;;                   : fcs - <flat cable set>
;;;
;;;     returns       : <flat cable set>
;;;
;;;     description   : It returns a <flat cable set> where the <relation> "r"
;;;                     is set to the <node set> "ns".  If the new nodeset is 
;;;                     (nil), then the cable is removed.
;;;
;;;
;;;                                      written :  ssc 02/21/87
;;;                                      modified:  ssc 04/24/87
;;;
(defun replace.fcs (r ns fcs)
  (cond ((null (car ns)) (remove r (remove (cl:getf fcs r) fcs)))
	(t
	 (substitute ns (cl:getf fcs r) fcs :start (position r fcs) :count 1))))
;
; =============================================================================
;
; read.fcs
; --------
;
;       arguments     : inunit - <unit>
;
;       returns       : <flat cable set>
;
;       description   : It reads a <flat cable set> from "inunit".
;                       It converts the sets of <node access> to <nodesets>.
;
;
;                                        written :  ejm 10/04/83
;                                        modified:  ssc 02/26/87
;                                                   hc  06/29/93
;
(defun read.fcs (inunit)
  ;; This function should be obsolete
  (read inunit))
;
;
; =============================================================================
;
; Print.fcs
; ---------
;
;       arguments     : fcs - <flat cable set>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : It Prints a <flat cable set> to "outunit".
;
;       side-effects  : It prints the <flat cable set>
;
;                                        written :  ejm 10/04/83
;                                        modified:  ssc 02/13/87
;                                                   hc  06/29/93
;
(defun print.fcs (fcs outunit)
  ;; This function should be obsolete
  (with-readable-nodes-and-contexts
      (print fcs outunit)))

;
;
; =============================================================================

(defun down.fcs (node)
  "Returns the down fcableset of the node."
  (do ((dfcs (node-fcableset node) (others.fcs dfcs)))
      ((or (null dfcs) (isdn.r (relation.fcs dfcs)))
       dfcs)))

(defun relationset.fcs (fcs)
  "Returns the set of relations in the given flat cable set."
  ;; written by: scs 5/12/88
  (let (rs)
    (do.fcs (rel ns fcs rs)
;       (declare (ignore ns))
       (push rel rs))))

(defun downfcs.pbi (tnode downrels)
  "Returns the downfcs of tnode,
   using the relations of downrels and the paths that define them"
  ;; written by: scs 5/12/88
  (let ((resultfcs (new.fcs)))
    (dolist (rel downrels)
      (let ((ns (sneps::pathfrom (sneps::checkpath rel) tnode)))
	(when ns (setq resultfcs (insert.fcs rel ns resultfcs)))))
    resultfcs))

(defun dominatednodes.n (node)
  "Returns a set of nodes dominated by node, other than the free variables of node."
  (let ((dominated (new.ns)))
    (do.fcs (rel ns (down.fcs node) (compl.ns dominated (freevars.n node)))
;       (declare (ignore rel))
       (setq dominated (union.ns ns dominated))
       (do.ns (n ns)
	  (unless (or (isbase.n n) (isvar.n n))
		  (setq dominated (union.ns (dominatednodes.n n) dominated)))))))

(defun dominatingnodes.n (node)
  "Returns a set of nodes dominating node, except patterns"
  (let ((dominating (new.ns)))
    (do.fcs (rel ns (up.fcs node) (compl.ns dominating (sneps::value.sv 'patterns)))
	    (setq dominating (union.ns ns dominating))
	    (do.ns (n ns)
		   (setq dominating (union.ns (dominatingnodes.n n) dominating))))))
	    
(defun upfcs.fcs (fcs)
  "Returns the upcableset of a fclableset"
  (cond ((null fcs) nil)
	((isup.r (relation.fcs fcs))
	 (insert.fcs (relation.fcs fcs)
		     (nodeset.fcs fcs)
		     (upfcs.fcs (others.fcs fcs))))
	(t (upfcs.fcs (others.fcs fcs)))))


(defun up.fcs (node)
  "Returns the up fcableset of a node"
  (upfcs.fcs (node-fcableset node)))


(defun dominating-asupp-nodes.n (node)
  "Returns the set of asupported nodes dominating `node'"
  (let ((dominating (new.ns)))
    (do.fcs (rel ns (up.fcs node) (compl.ns dominating (freevars.n node)))
      (do.ns (n ns)
	(if (node-asupport n)
	    (setq dominating (insert.ns n dominating)))
	(unless (or (isbase.n n) (isvar.n n))
	  (setq dominating (union.ns (dominating-asupp-nodes.n n)
				     dominating)))))))

;
;
; =============================================================================
;
; issubset.fcs
; ------------
;
;       arguments     : fcs1 - <flat cable set>
;                       fcs2 - <flat cable set>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if every flat cable in "fcs1" is in "fcs2",
;                       "false" otherwise.
;
;
;                                        written:  ssc 03/22/89
;                                        modified:
;
;
(defmacro issubset.fcs (fcs1 fcs2)
  `(do.fcs (r ns ,fcs1 t) 
     (unless (iseq.ns ns
		      (getnodeset.fcs r ,fcs2))
       (return nil))))

;
;
; =============================================================================
;
; iseq.fcs
; --------
;
;       arguments     : fcs1 - <flat cable set>
;                       fcs2 - <flat cable set>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "fcs1" is equal to "fcs2",
;                       "false" otherwise.
;
;
;                                        written:  ssc 03/22/89
;                                        modified:
;
;
(defmacro iseq.fcs (fcs1 fcs2)
  `(and (issubset.fcs ,fcs1 ,fcs2)
	(issubset.fcs ,fcs2 ,fcs1)))



    
    




