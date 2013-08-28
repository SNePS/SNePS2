;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: plantrace.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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


(defvar *plantrace* nil)

(defun plantrace (msg nodeset restriction)
  (declare (special *plantrace*))
  (cond ((null *plantrace*))
	((listp *plantrace*) (eval *plantrace*))
	((eql *plantrace* 'snepsul::surface)
	 (format t (format nil "~&~%~a" msg))
	 (mapcar #'(lambda (x) (eval `(snepsul::surface ,x)))
		 (parser::flistify nodeset)))
;	 (eval `(snepsul::surface ,@(sneps:apply-subst.ns (subst.restr restriction) nodeset)))
	(t (format t (format nil "~&~%~a" msg))
	   (cond (nodeset
		  (PP-nodetree (plantrace-desc-ns nodeset restriction)))))))

(defun plantrace-desc-ns (nodeset restriction)
  (mapcar #'(lambda (n) (plantrace-desc-one-n n restriction))
	 nodeset))

(defun plantrace-desc-one-n (node restriction)
  (let ((sbst (subst.restr restriction)))
    (cond ((is.n node)
	   (cond ((isbase.n node)
		  (cond ((isnumber.n node) (node-to-number.n node))
			(t (list node))))
		 ((isvar.n node)
		  (cond ((isbound.sbst node sbst)
			 (list node (intern "<--" *package*)
			       (cond ((eq *plantrace* 'FULL-TRACE)
				      (plantrace-desc-one-n
					(mnode.sbst node sbst)
					restriction))
				     (t (describe.n (mnode.sbst node sbst))))))
			(t node)))
		 ((or (ismol.n node) (ispat.n node))
		  (cons (describe.n node)
			(mapcar #'(lambda (c)
				    (sneps::new.c
				      (sneps::relation.c c)
				      (mapcar #'(lambda (n)
						  (plantrace-desc-one-n n restriction))
					      (sneps::nodeset.c c))))
				(n-to-downcs node))))))
	  (t node))))



    
    




