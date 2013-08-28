;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: with-acts.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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


;;;
;;; Representation
;;;
;;;   act = action act-node
;;;         objecti ith-argument to act-node


(defun withsome (n)
  (let ((designator (choose.ns (sneps::pathfrom '(snepsul::suchthat) n)))
	(do-act (choose.ns (sneps::pathfrom '(snepsul::do) n)))
	(with-act (activation.n n)))
    (case (regfetch with-act '*AGENDA*)
      (START
       (plantrace "Now doing: WITHSOME " (list n) nil)
       (regstore with-act '*AGENDA* 'IDENTIFY-OBJECTS)
       (initiate with-act)
       (identify-objects n designator))
      (IDENTIFY-OBJECTS
       (let ((reports (remove-if-not #'isassert.n *REPORTS*)))
	 (regstore with-act '*AGENDA* 'DONE)
	 (cond (reports
		;;(plantrace "The designator(s):" *REPORTS* nil)
		(schedule-withsome-act designator do-act reports))
	       (t (plantrace "No designators found!" nil nil)
		  (let ((acts  #!((find else- ~n))))
		    (when acts
		      (schedule-act (choose.ns acts)))))))))))
;
;
(defun identify-objects (act designator)
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
			    CRNTCT))
  (let (pr)
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
	       (activation.n act)
	       'HIGH))
    (activate.n designator)
    (setq pr (activation.n designator))
    (regstore pr '*REQUESTS*
	         (insert.chset (make.ch (new.filter)
					(new.switch)
					crntct
					;*NODE*
					'user
					'open)
			        (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (initiate pr)))

(defun posuser (*NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
	     *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
		*CLIENT* *PRIORITY*)
  "Like the user process function, but only collects positive answers."
  (declare (special multi::curnt%))
  (let ((reports *REPORTS*))
    (setq *REPORTS* (new.repset))
    (do.repset (report reports)
       (let ((n (node.rep report)))
	 (unless (or (ismemb.ns n *DEDUCED-NODES*)
		     (not (eq (sign.rep report) 'POS)))
		 (tally report)
		 (setq *DEDUCED-NODES* (insert.ns n *DEDUCED-NODES*))))))
  (setq *DEDUCTION-RESULTS* (union.ns *DEDUCED-NODES* *DEDUCTION-RESULTS*))
    (if (not (eq *CLIENT* 'snip::user))            ;; *CLIENT* can be a node
      (send-to-client *CLIENT* *DEDUCED-NODES*))
  (if (enough-answers) (suspend-inference))
  (multi:process-change-slots
   multi::curnt%
   (vector *NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES*
	 *TOTAL-DESIRED* *POS-DESIRED* *NEG-DESIRED* *POS-FOUND*
	 *NEG-FOUND* *CLIENT* *PRIORITY*)))

(setf (get 'posuser 'multi::lregs%)
  '(*NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
    *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
    *CLIENT* *PRIORITY*))
;
;
(defun schedule-withsome-act (designator do-act reports)
  (let ((acts (new.ns))
	substs)
    (plantrace "The designator" (list designator) nil)
    (plantrace "is effective on the following:" reports nil)
    (plantrace "for the act:" (list do-act) nil)
    (do.ns (report reports)
	   (setf substs (first (first (match::unify designator report))))
	   (let ((act (match::applysubst do-act substs)))
	     ;; This conditional is a patch---the test shouldn't succeed.
	     (unless (ispat.n act)
	       (setf acts (cons act acts))))
	   ;(plantrace "The substitutions are:" (list substs) nil)
	   ;(plantrace "The act is:" acts nil)
	   )
    (schedule-then-acts acts)))
;
;
(defun withall (n)
  (let ((designator (choose.ns (sneps::pathfrom '(snepsul::suchthat) n)))
	(do-act (choose.ns (sneps::pathfrom '(snepsul::do) n)))
	(with-act (activation.n n)))
    (case (regfetch with-act '*AGENDA*)
      (START
       (plantrace "Now doing: WITHALL " (list n) nil)
       (regstore with-act '*AGENDA* 'IDENTIFY-OBJECTS)
       (initiate with-act)
       (identify-objects n designator))
      (IDENTIFY-OBJECTS
       (let ((reports (remove-if-not #'isassert.n *REPORTS*)))
	 (regstore with-act '*AGENDA* 'DONE)
	 (cond (reports
		;;(plantrace "The designator(s):" *REPORTS* nil)
		(schedule-withall-act designator do-act reports))
	       (t (plantrace "No designators found!" nil nil)
		  (let ((acts  #!((find else- ~n))))
		    (when acts
		      (schedule-act (choose.ns acts)))))))))))
;
;
(defun schedule-withall-act (designator do-act reports)
  (let ((acts (new.ns))
	substs)
    (plantrace "The designator" (list designator) nil)
    (plantrace "is effective on the following:" reports nil)
    (plantrace "for the act:" (list do-act) nil)
    (do.ns (report reports)
	   (setf substs (first (first (match::unify designator report))))
	   (let ((act (match::applysubst do-act substs)))
	     ;; This conditional is a patch---the test shouldn't succeed.
	     (unless (ispat.n act)
	       (setf acts (cons act acts))))
	   ;(plantrace "The substitutions are:" (list substs) nil)
	   ;(plantrace "The act is:" acts nil)
	   )
    (schedule-doall-acts acts)))
;
;
(defun schedule-doall-acts (reports)
  (let ((act (choose.ns
	      #2!((sneps:build snepsul::action ~(find-action-node 'do-all)
			       snepsul::object1 ~reports))))
	act-pr)
    (plantrace "I intend to do:" (list act) nil)
    (activate-act.n act)
    (setf act-pr (activation.n act))
    (regstore act-pr '*PRIORITY* 'INTEND)
    (regstore act-pr '*AGENDA* 'START)
    (initiate act-pr)))



    
    




