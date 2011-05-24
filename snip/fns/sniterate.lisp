;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011

;; Research Foundation of State University of New York

;; Version: $Id: sniterate.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;;;
;;; Representation
;;;
;;;  sniterate-act = action sniterate
;;;                  object1 (list of guarded acts)


;;; Previously, chose the successful act based only on the successful guard,
;;;    which could get a snif act if it had the same guard.
;;; Now, also takes into account the actual sniterate node,
;;;   and there can be sets of guards and acts in the if(guard,act).
(defun sniterate (n)
  "Of the set of object1 act nodes,
   nondeterministically performs any one whose condition is true.
   If none are true, does the elseact if there is one, else does nothing."
  (let ((n-act (activation.n n)))
    (case (regfetch n-act '*agenda*)
      (start
       (plantrace "now doing: sniterate " (list n) nil)
       (regstore n-act '*agenda* 'test)
       (initiate n-act)
       (test-conditions n-act))
      (test
       (plantrace
	(format nil "~[no condition is~;this is~:;the following are~] satisfied:"
		(length *reports*)) *reports* nil)
       (regstore n-act '*agenda* 'done)
       (let (elseact)
	 (cond (*reports*
		(regstore n-act '*reports* (new.ns))
		(regstore n-act '*agenda* 'start)
		(schedule-then-iter-acts
		 (sneps:ns-to-lisp-list
		  #!((find then-
			   (find object1- ~n
				 condition ~*reports*))))
		 n))
	       ((setf elseact #!((find (else- object1-) ~n)))
		(schedule-act
		 (choose.ns elseact)))))))))

;

;
(defun schedule-then-iter-acts (acts iter-act)
  (let* ((do-one-act (choose.ns #2!((sneps:build
				     action ~(find-action-node 'do-one)
				     object1 ~acts))))
	 (snseq-act (choose.ns #2!((sneps:build
				    action ~(find-action-node 'snsequence)
				    object1 ~do-one-act
				    object2 ~iter-act))))
	 act-pr)
    (plantrace "I intend to do:" (list snseq-act) nil)
    (activate-act.n snseq-act)
    (setf act-pr (activation.n snseq-act))
    (regstore act-pr '*PRIORITY* 'INTEND)
    (regstore act-pr '*AGENDA* 'START)
    (initiate act-pr)))



    
    




