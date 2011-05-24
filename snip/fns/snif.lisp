;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011
;; Research Foundation of State University of New York

;; Version: $Id: snif.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;;; Representation
;;;
;;;   snif-act = action snif
;;;              object1 (list of guarded acts)

;;; Previously did a do-one of only one act per successful guard.
;;; Now does a do-one of all the acts of all successful guards.
(defun snif (n)
  "Of the set of object1 act nodes,
   nondeterministically performs any one whose condition is true.
   If none are true, does nothing."
  (let ((n-act (activation.n n)))
    (case (regfetch n-act '*agenda*)
      (start
       (plantrace "now doing: snif " (list n) nil)
       (regstore n-act '*agenda* 'test)
       (initiate n-act)
       (test-conditions n-act))
      (test
       (plantrace
	(format nil "~[No condition is~;This is~:;The following are~] satisfied:"
		(length *reports*)) *reports* nil)
       (regstore n-act '*agenda* 'done)
       (let (elseact)
	 (cond (*reports*
		(schedule-then-acts
		 (sneps:ns-to-lisp-list
		  #!((find then-
			   (find object1- ~n
				 condition ~*reports*))))))
	       ((setf elseact #!((find (else- object1-) ~n)))
		(schedule-act
		 (choose.ns elseact)))))))))

;
;
(defun test-conditions (act-process)
  "Initiates a deduction process to determine which (if any) of the
   conditions of the SNIF act are currently believed."
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
		    CRNTCT))
  (let ((conditions (sneps::pathfrom '(snepsul::object1 snepsul::condition)
				     (regfetch act-process '*NODE* )))
	pr)
    (setq *ADDED-NODES* (new.ns))
    (setq *DEDUCTION-RESULTS* (new.ns))
    (setq *USER-PROCESS*
	  (new 'posuser
	       (new.repset)
	       crntct
	       (new.ns)
	       nil
	       nil
	       nil
	       0
	       0
	       act-process
	       'HIGH))
    (do.ns (condishun conditions)
	(activate.n condishun)
	(setq pr (activation.n condishun))
	(regstore pr '*REQUESTS*
		     (insert.chset (make.ch (new.filter)
					    (new.switch)
					    crntct
					    'user
					    'open)
				   (regfetch pr '*REQUESTS*)))
 	(regstore pr '*PRIORITY* 'LOW)
	(initiate pr))))
;
;
(defun schedule-then-acts (reports)
  (let ((act (choose.ns #2!((sneps:build
			     action ~(find-action-node 'do-one)
			     object1 ~reports))))
	act-pr)
    (plantrace "I intend to do:" (list act) nil)
    (activate-act.n act)
    (setf act-pr (activation.n act))
    (regstore act-pr '*PRIORITY* 'INTEND)
    (regstore act-pr '*AGENDA* 'START)
    (initiate act-pr)))



    
    




