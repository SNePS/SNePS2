;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: compute-support.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :match)


;============================================================================
;
; compute-support 
; ---------------
;
;       arguments     : tr                - (see unify)
;                     : supporting-nodes  - <node-set>
;
;       returns       : <node-set>
;
;       description   : Computes the support of a node inferred by path-based inference
;
;                                          written:  mrc 10/24/89
;
(defun compute-support (tr supporting-nodes)
  (let (sup-nodes)
    (dolist (one-tr tr)
      (setq sup-nodes (append (get-supporting-nodes (cdr one-tr) supporting-nodes) sup-nodes)))
    (compute-support-1 (apply 'append (mapcar #'combine-supps (get-supports (combine-nodes sup-nodes)))))))


; ===============================================================================================================
; ===============================================================================================================
;
;  The remaining functions in this file are auxiliary functions to the function compute-support
;
; ===============================================================================================================
; ===============================================================================================================
;
;                                         written:  mrc 10/24/89

(defun compute-support-1 (l)         
  (let ((support (snip:new.sup)))    
    (dolist (el l support)
			 (setq support (snip:insert.sup (snip:combine-ots* (first el))
							(sneps:fullbuildcontext (sneps:new.ns) (second el))
							support)))))

; ===============================================================================================================
;
;                                          written:  mrc 10/24/89
;
; If the argument is
;  ( ((M1 M2) (M3 M4))   ((M5 M6))  ((M9) (M10 M11)) )
; returns  - ( (M1 M2 M5 M6 M9) (M1 M2 M5 M6 M10 M11) (M3 M4 M5 M6 M9) (M3 M4 M5 M6 M10 11) )

(defun combine-nodes (l)
  (let ((result (car l)))
    (do ((lst (cdr l) (cdr lst)))
	((null lst) result)
      (setq result 
	    (let (result1)
	      (dolist (res result result1) 
		(setq result1 (dolist (res1 (car lst) result1)
				(setq result1 (cons (sneps:union.ns res1 res)
						    result1))))))))))
	
       
;===============================================================================================================
;
;                                          written:  mrc 10/24/89
;If the argument is
;  ((HYP (C13) DER (C14 C15)) 
;   (HYP (C9)) 
;   (HYP (C5) DER (C1 C3)))
; returns
;(((HYP HYP DER) (C5 C9 C14)) 
; ((DER HYP DER) (C3 C9 C14)) 
; ((DER HYP DER) (C1 C9 C14)) 
; ((HYP HYP DER) (C5 C9 C15))
; ((DER HYP DER) (C3 C9 C15)) 
; ((DER HYP DER) (C1 C9 C15)) 
; ((HYP HYP HYP) (C5 C9 C13)) 
; ((DER HYP HYP) (C3 C9 C13))
; ((DER HYP HYP) (C1 C9 C13)))

(defun combine-supps (l)
  (combine-supps-1 (mapcar #'flatten-supports l)))


(defun combine-supps-1 (l)
  (let ((result (car l)))
    (do ((lst (cdr l) (cdr lst)))
	((null lst) result)
      (setq result 
	    (let (result1)
	      (dolist (res result result1) 
		(setq result1 (dolist (res1 (car lst) result1)
				(setq result1 (cons (list (append(car res1) (car res)) 
							  (append (cadr res1) (cadr res)))
						    result1))))))))))





 
;===============================================================================================================
;
;                                          written:  mrc 10/24/89
; If the argument is 
;  ( (n1 n2 n3) (n1 n5) (n6 n4) .....), where n1, n2,... are of type <node>
; returns 
;     ((sup1 sup2 sup3) (sup1 sup5) (sup6 sup4)), where supi is the support of ni.

(defun get-supports (l)
  (let (result)
    (dolist (el l result)
      (setq result (cons (mapcar #'sneps:node-asupport el)
			 result)))))

;===============================================================================================================
;
;                                          written:  mrc 10/24/89
;If the argument is
; (HYP (C13) DER (C14 C15))
;returns 
;  (((DER) (C14)) ((DER) (C15)) ((HYP) (C13)))

(defun flatten-supports (sup)
  (do ((one sup (cddr one))
       (result nil (append (mapcar #'(lambda (x)
				       (list (list (car one)) (list x)))
				   (cadr one))
			   result)))
      ((null one) result)))

;===============================================================================================================
;
;                                          written:  mrc 10/24/89
; If the arguments are 
;   A
;   (((B (M2! M6!)) (A (M3! M2! M6!) (M2! M7!)) (C (M4! M3! M2! M6!))) ((D (M6!))))
; returns
;   (((M3! M2! M6!) (M2! M7!)))

(defun get-supporting-nodes (node l)
  (do ((el l (cdr el))
       (found nil (let ((aux (get-supporting-nodes-1 node (car el))))
		    (if (null aux)
			found
			(cons aux found)))))
      ((or found (null el)) found)))


(defun get-supporting-nodes-1 (node l)           
  (do ((el l (cdr el))
       (found nil (if (eq node (caar el))
		      (append found (cdar el))
		      found)))
      ((null el) found)))



    
    




