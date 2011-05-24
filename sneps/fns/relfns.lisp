;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: relfns.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


; ==========================================================================
;
; define
; ------
;
;       arguments     : s - <sequence> 
;
;       returns       : <relation set>
;
;       description   : Takes a <sequence> of identifiers to be defined
;                       as <relation>s. 
;                       If the identifier has a '-' as its last character,
;                       then it is rejected as an illegal <relation>.
;                       If the identifier has already been defined as a
;                       <relation>, it is not redefined.
;                       Otherwise, it defines the identifier as a 
;                       <relation> and adds it to the value of the snepsul 
;                       variable "relations".
;                       The function returns the set of non-illegal defined 
;                       <relation>s.
;
;       implementation: The <relation set> "newrels" is reversed by
;                       reverse.rs before the function is exited.
;
;       side-effects  : If the identifier has a '-' as its last character
;                       the identifier followed by the message -
;                       'illegal relation' is printed.
;                       If the identifier has already been defined as a
;                       <relation>, then the identifier followed by the
;                       message - 'already defined' is printed.
;                       There are side-effects caused by new.r.
;
;                                          written : mja 07/28/83
;                                          modified: ejm 10/10/83
;                                          modified: scs 02/07/87
;                                                    hc  07/18/93
;

;;; Change:  Pass the snepsul package to new.r
;;;    so that the converse relation is put in the snepsul package
(defsnepscom define ((&rest s) (top ns bns tbns rs))
  (values
   (mapcan #'(lambda (r)
	       (cond ((char-equal
		       (char (symbol-name r) (1- (length (symbol-name r))))
		       #\-)
		      (beep)
		      (format t "~A:  It is illegal to define a relation ~
                                 ending with \"-\".~%" r))
		     ((is.r r)
		      (beep)
		      (format t "~A is already defined.~%" r)
		      (list r))
		     (t (new.r r (find-package :snepsul))
			(list r))))
	   s)
   (buildcontext (new.ns))))


;;;
;;;  undefine
;;; ---------
;;;
;;;  Undefines a relation.
 
(defsnepscom undefine ((&rest rsf))
  (format t "~%Relations undefined: ~{~a ~}~%"
	  (mapcar #'(lambda (r)
		      (undefine.r r) r)
		  (mklst.rs (rseval rsf))))
  (values))
 
; ==========================================================================
;
; rseval
; ------
;
;       arguments     : rs-exp - <rs-exp>
;
;       returns       : <relation set>
;
;       description   : It evaluates the given <rs-exp> (expression that
;                       evaluates to a <relation set>).
;                       If "rs-exp" is a <sequence> whose first <atom> is a
;                       <relation command> then "rs-exp" is lisp evaluated.
;                       Otherwise it rsevaluates the elements of "rs-exp" 
;                       one by one, returning a <relation set> of their values.
;                       If something evaluates to a non-<rs-exp> it prints
;                       a warning message.
;
;       side-effects  : If a <relation command> is evaluated whatever
;                       side-effects caused by it.
;
;                                        written :  ejm 10/23/83
;                                        modified:  scs 10/29/87
;                                        modified:  scs 5/23/88
;
;
 
(defun rseval (rs-exp)
  (cond ((null rs-exp) (new.rs))
	((atom rs-exp)
	 (cond ((is.r rs-exp) (make.rs rs-exp))
	       (t (sneps-error (format nil "~A is not a relation" rs-exp)
			       'relationset-evaluator
			       'rseval)
		  nil)))
	((and (symbolp (first rs-exp))
	      (isrs.com (first rs-exp)))
	 (protect-eval (eval rs-exp)))
	(t (let ((result (new.rs)))
	     (dolist (exp rs-exp result)
	       (setq result (union.rs result (rseval exp))))))))



    
    




