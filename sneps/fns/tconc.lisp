;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: tconc.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :sneps)


(defstruct tconc car cdr)

(defun tconc (tconc-cell s-exp)
  (prog ((list-s-exp (list s-exp)))
    (cond ((eql tconc-cell nil)
 	   (setq tconc-cell (make-tconc :car list-s-exp
					:cdr list-s-exp)))
	  ((tconc-p tconc-cell)
	   (rplacd (tconc-cdr tconc-cell) list-s-exp)
	   (setf (tconc-cdr tconc-cell) list-s-exp))
	 (t (error "~a is not a tconc cell. ~%" tconc-cell))))
  tconc-cell)

(defun lconc (tconc-cell list)
   (cond ((eql tconc-cell nil)
	  (setq tconc-cell (make-tconc :car list
				       :cdr (last list))))
	 ((tconc-p tconc-cell)
	  (rplacd (tconc-cdr tconc-cell) list)
	  (setf (tconc-cdr tconc-cell) (last list)))
	 (t (error "~a is not a tconc cell. ~%" tconc-cell)))
   tconc-cell)



    
    




