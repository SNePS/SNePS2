;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: printer.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :snepslog)


;;; Modifies the SNePSLOG printer to include an '!' after the wff # is printed
;;; if the node is asserted. Additionally a number of oerations previously 
;;; performed by the generator were moved to the surface. Modified 6/1/06 - mwk
(defun snepslog-print (nodes &optional (stream sneps:outunit))
  (let ((string 
	 (cond 
	  ((and (consp nodes) (every #'listp nodes) )
	   (let ((result ""))
	     (loop
		 for sub-list in nodes
		 do 
		   (setf result (concatenate 'string result "("))
		   (loop 
			for sub-cons in sub-list
			do 
			  (setf result 
			     (concatenate 'string
			       result
			       "(?"
			       (symbol-name (car sub-cons))
			       " . "
			       (printer (parser  (list (cdr sub-cons))
						 'gen
						 (the-empty-frame) 0) nil)
			       ")")))
		   (setf result (concatenate 'string 
				  result (format nil ")~%"))))
	     result))				             
	  (t      
	   (format nil "~{  ~A~}"
		   (mapcan #'(lambda (node)
			       (declare (special complete-description ))
			       (when (sneps:node-p node)
				 (list
					    (if (not (eq complete-description 
							 'unlabeled))
						(concatenate 'string
						  (slight-surface node) 
						  (if 
						      (sneps:isassert.n node)
						      "!:"
						    ": ")) 
					      "") 
					    (printer (parser  (list node)
							      'gen
							      (The-empty-frame)
							      0)
						     nil)
					    (if (eq complete-description 'expert)
						(printer  (parser 
							  (distribute-ots 
							   (sneps:node-asupport node))
							  'generate-set-of-derivation-histories
							  (the-empty-frame)
							  0) nil) 
					      "")
					    (format nil "~%"))))
				    (flistify nodes)))))))
  (format stream "~A" string)))


(defun printer (snepsloged-node &optional (stream sneps:outunit))
  (format stream
	  "~{~A~}"
	 (mapcan #'(lambda (exp)
		     (if (listp exp)
			 (list (printer (eval exp) nil))
			 (list (format nil "~A" exp))))
		 (flistify snepsloged-node))))



(defun node-to-text (node)
  (printer (parser  (list node)
             'gen
             (the-empty-frame)
             0)
  nil))
   




