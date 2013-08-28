;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MULTI; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: multi-changes.lisp,v 1.2 2013/08/28 19:07:22 shapiro Exp $

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




(in-package :multi)


;; Contains updated versions of `schedule' and `multip' which indicate
;; the state of a node process by changing the filling style of the
;; according XGinseng node:

(defun is-node-process (process)
  ;; Returns T if PROCESS is a node process.
  ;; Returns NIL for processes such as USER, POSUSER, etc.
  (member 'snip::*NODE* (get (car (eval process)) 'lregs%)))

(defun schedule (event queue)
  (cond ((eq (regfetch event '*PRIORITY*) 'snip::INTEND) ; intended acts
         (dequeue:insert-front event queue))            ; go on front -dk
	((dequeue:in-queue event queue) queue)
	((eq (regfetch event '*NAME*) 'snip:USER)
	 (dequeue:insert-front event queue))
	(t (if (is-node-process event)
               (xginseng::set-fillstyle.n
                (regfetch event 'snip::*NODE*)
                :initiated))
	   (dequeue:insert-rear event queue))))

(defun multip (actq hpq lpq)
  "Evaluates processes"
  (let ((*act-queue* actq)
	(*high-priority-queue* hpq)
	(*low-priority-queue* lpq)
	highest-priority-non-empty-queue
	*NAME*)
    (declare (optimize (speed 3))
	     (special *NAME*))
    (prog (curnt% regvals)
       loop
          (cond (*trace-enabled*
		 (format t "~%>>>>>> MULTI QUEUES> A: ")
		 (dequeue::print *act-queue*)
		 (format t ", H: ")
		 (dequeue::print *high-priority-queue*)
		 (format t ", L: ")
		 (dequeue::print *low-priority-queue*)
		 (format t "~%")))
	  (cond ((not (dequeue:empty *high-priority-queue*))
		 (setq highest-priority-non-empty-queue *high-priority-queue*))
		((not (dequeue:empty *low-priority-queue*))
		 (setq highest-priority-non-empty-queue *low-priority-queue*))
		((not (dequeue:empty *act-queue*))
		 (setq highest-priority-non-empty-queue *act-queue*))
		(t (return)))
	  (setq curnt% (dequeue:front highest-priority-non-empty-queue))
	  (setq regvals (eval curnt%))
	  (setq *NAME* (car regvals))
          (if (is-node-process curnt%)
              (xginseng::set-fillstyle.n
               (regfetch curnt% 'snip::*NODE*)
               :executing))
	  (dequeue:delete-front highest-priority-non-empty-queue)
	  (if *trace-enabled*
	      (cond
		((or (eq *trace-events* t)
		     (member (car regvals) *trace-events*))
		 (print-regs curnt% "Entering"))))
	  (apply (symbol-function (car regvals)) regvals)
          (if (is-node-process curnt%)
              (xginseng::set-fillstyle.n
               (regfetch curnt% 'snip::*NODE*)
               :inactive))
	  (if *trace-enabled*
	      (cond
		((or (eq *trace-events* t)
		     (member (car regvals) *trace-events*))
		 (print-regs curnt% "Leaving"))))
      (go loop))))



    
    




