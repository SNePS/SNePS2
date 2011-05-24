;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011
;; Research Foundation of State University of New York

;; Version: $Id: achieve.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
;;;   act = action act-node
;;;         objecti ith-argument to act-node


;;; Change so that printing "Already Achieved." and "DEAD END!!"
;;;    are under the control of plantrace.
;;; Previously, they were printed unconditionally.
;;; Also give more information if there are no plans.
(defun achieve (n)
  "Tries to find plans to make the object1 proposition true.
   If it does, chooses one, and schedules it."
  (let ((prop (choose.ns (sneps::pathfrom '(snepsul::object1) n)))
	(n-act (activation.n n))
	plans)
    (case (regfetch n-act '*agenda*)
      (start
       (plantrace "Want to achieve " (list prop) nil)	
       (cond ((isassert.n prop)
	      (plantrace "~&~%Already Achieved.~%" nil nil)
	      (regstore n-act '*agenda* 'done))
	     (t 
	      (regstore n-act '*agenda* 'find-plans)
	      (initiate n-act)
	      (plantrace "Want to find a plan to achieve" (list prop) nil)
	      (find-gplans n prop))))
      (find-plans
       (setf plans (regfetch n-act '*reports*))
       (plantrace
	(format nil	    ;;; altered by flj 2/12/04 (for scs)
		"The goal can~[not~:;~] be achieved.~:*~
                  ~[~:; by the following plan~:p:~] "	
		(length plans))
	plans nil)
       (cond (plans
	      (regstore n-act '*agenda* 'done)
	      (schedule-plans plans))
	     (t (plantrace "No plans found to achieve:" (list prop) nil)))))))

;
;
(defun find-gplans (achieve-act prop)
  "Initiates a deduction process to find plans that decompose the
   currently active complex act process."
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
			    CRNTCT))
  (let (pr deduction-target) 
    (unless (and (snepsul::* 'gplan)
                 (sneps::isvar.n (first (snepsul::* 'gplan))))
	    (snepsul::$ 'gplan))
    (setq deduction-target
          (choose.ns (eval `(sneps:build snepsul::plan (snepsul::* 'gplan)
			     snepsul::goal  ,prop))))
    (setq *ADDED-NODES* (new.ns))
    (setq *DEDUCTION-RESULTS* (new.ns))
    (setq *USER-PROCESS*
	  (new 'user
	       (new.repset)
	       crntct
	       (new.ns)
	       nil
	       nil
	       nil
	       0
	       0
	       (activation.n achieve-act)
	       'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	         (insert.chset (make.ch (new.filter)
					(new.switch)
					(sneps:value.sv crntct)
					;*NODE*
					'user
					'open)
			        (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (initiate pr)
  ))



    
    




