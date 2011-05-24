;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: remark.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;(defmacro tell-user (msg)
;  `(format sneps:outunit ,msg))

(defun surface-trace-p (infertrace)
  "Returns t if SNePSLOG is running
    or current value of INFERTRACE indicates that tracing
    should be done with SURFACE, i.e., sneps:surface (which could be bound
    to snepslog:surface if SNePSLOG is running)"
  (declare (special snepslog::*SNePSLOGRunning*))
  (or snepslog:*SNePSLOGRunning*
      (eq infertrace :surface)
      (and (symbolp infertrace)
	   (equal (symbol-name infertrace)
		  (symbol-name :surface)))))

;;; Modifications to handle SNePSLOG:
;          1. the test (eq *infertrace* #'sneps:surface)
;             was changed to (surface-trace-p *infertrace*).
;             Snepslog uses a diferent sneps:surface function. This way, the test works 
;             for both surface definitions (see snip:slight-describe-or-surface.ns reasons).
;          2. The second argument of format (in both calls) was changed to sneps:outunit.
;
(defun remark (msg nodeset restriction &optional context)
  (declare (special *infertrace*))
  (cond ((null *infertrace*))
	;; If its a list take it as a form and evaluate it
	((listp *infertrace*) (eval *infertrace*))
	;; If its a surface indication, use sneps:surface
	((surface-trace-p *infertrace*)
	 (format sneps:outunit msg)
	 (eval `(sneps:surface
		 ,@(sneps:apply-subst.ns (subst.restr restriction) nodeset)))
	 (when context (remark-context context)))
	(t (format sneps:outunit msg)
	   (PP-nodetree (describe-nodeset nodeset restriction))
	   (when context (remark-context context)))))


(defun remark-context (context)
  (let ((ct-names (sneps:context-names context)))
    (cond ((sneps:isnew.svs ct-names)
	   (format sneps:outunit
		   "holds within the BS defined by hypotheses ~:A ~%"
		   (snip:slight-describe-or-surface.ns (sneps:context-hyps context) nil)))
	  (t (format sneps:outunit
		     "holds within the BS defined by context~P ~:A~{, ~:A~} ~%"
		     (length ct-names)
		     (car ct-names)
		     (cdr ct-names))))))

;;; given a node, this prints the name of the node depending on the interface
;;; that's currently used (see remark, SNePSLOG)
(defun slight-describe-or-surface (node &optional (stream sneps:outunit))
  (declare (special *infertrace*))
  (cond ((surface-trace-p *infertrace*)
	 (sneps:slight-surface node stream))
	(t (format stream "~A" node))))

(defun slight-describe-or-surface (node &optional (stream sneps:outunit))
  (sneps:slight-surface node stream))

;;; Prints a nodeset via slight-describe-or-surface (SNePSLOG)
(defun slight-describe-or-surface.ns (nodeset &optional (stream sneps:outunit))
   (let ((descrs 
	  (mapcar #'(lambda (node)
		      (slight-describe-or-surface node nil))
		  nodeset)))       
     (format stream
	     "(~A~{ ~A~})" (first descrs) (rest descrs))))

;;; used to describe or surface a node, depending on the 
;;; interface beeing used (SNePSLOG).
(defun describe-or-surface (node &optional (stream sneps:outunit))
  (declare (special *infertrace*))
  (let ((sneps:outunit stream))
    (if (surface-trace-p *infertrace*)
	(format stream "~A" (with-output-to-string (sneps:outunit)
			      (eval `(sneps:surface ,node))))
	(format stream "~A" (describe-one-node node nil)))))

;;; describe-or-surface a nodeset (SNePSLOG).
(defun describe-or-surface.ns (nodeset &optional (stream sneps:outunit))
  (do.ns (node nodeset)
    (describe-or-surface node stream)))

;;; Returns description of node as a string (SNePSLOG).
(defun node-description.n (node)
  (describe-or-surface node nil))


(defun describe-nodeset (nodeset restriction)
  (mapcar #'(lambda (n) (describe-one-node n restriction))
	 nodeset))

(defun describe-one-node (node restriction)
  (let ((sbst (subst.restr restriction))) 
    (cond ((is.n node)
	   (cond ((isbase.n node)
		  (cond ((isnumber.n node) (node-to-number.n node))
			(t (list node))))
		 ((isvar.n node)
		  (cond ((isbound.sbst node sbst)
			 (list node
			       (intern "<--" *package*)
			       (describe.n (mnode.sbst node sbst))))
			(t node)))
		 ((or (ismol.n node) (ispat.n node))
		  (cons (describe.n node)
			(mapcar #'(lambda (c)
				    (sneps:new.c
				      (sneps:relation.c c)
				      (mapcar #'(lambda (n)
						  (describe-one-node n restriction))
					      (sneps:nodeset.c c))))
				(n-to-downcs node))))))
	  (t node))))



    
    




