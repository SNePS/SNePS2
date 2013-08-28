;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: act.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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


; The *USE-DO-ACTS* variable determines if the acts
; ACHIEVE all the preconditions of an act
; Believe all the effects of the act
; Perform one of the plans
; will be scheduled as DO-ALL(ACHIEVE (*PRECONDITIONS*))
;                      DO-ALL(BELIEVE(*EFFECTS*))
;                      DO-ONE(*PLANS*)
; as opposed to separately scheduling all the object acts (more efficient)
;
(defvar *use-do-acts* t)
;
; =============================================================================
;
; act
; ---
;
;       description   : This is the node activation process for act nodes.
;
;                                        written :  dk   4/16/91
;                                        modified:  flj/scs 8/1/04 
;                                                   schedule-believe-effects 
;                                        modified:  
;                                                   
;                                                   
;                                                   
;
					;
;;; If no plans are found for the act,
;;;    this version uses the new surface
;;;    (which uses full-describe if no grammar nor lexicon is loaded).
;;;    to report that no plans were found.
(defun act (*name* *node* *context-name* *reports*
	    *preconditions* *effects* *plans* *agenda* *priority*)
  (declare (ignore *priority* *plans* *effects* *context-name* *name*))
 (let (pr)
  (declare (special multi::curnt%))
  (setq pr (activation.n *node*))
  (if (control-action? *node*)
      (execute-primaction *node*)
      (case *agenda*
	(start 
	       (plantrace "about to do " (list *node*) nil)
	       (regstore pr '*agenda* 'find-preconditions)
	       (initiate pr)
	       (plantrace "i wonder if the act " (list *node*) nil)
	       (plantrace "has any preconditions..." nil nil)
	       (find-preconditions))
	(find-preconditions 
	    (plantrace "the act " (list *node*) nil)
	    (plantrace (format nil 
			       "has ~[no~;a~:;the following~] precondition~:p:"
		       	       (length *reports*))
		       *reports* nil)
            (cond (*reports*
 		   (regstore pr '*preconditions* *reports*)
		   (regstore pr '*reports* (new.ns))
		   (regstore pr '*agenda* 'test-preconditions)
		   (initiate pr)
		   (test-preconditions pr))
		  (t (regstore pr '*agenda* 'find-effects)
		     (initiate pr)
		     (find-effects))))
	(test-preconditions 
	    (cond ((tally-preconditions *preconditions* *reports*)
		   (plantrace (format nil "~[~;it is~:;they are~] satisfied."
				      (length *preconditions*)) nil nil)
		    (regstore pr '*reports* (new.ns))
		    (regstore pr '*agenda* 'find-effects)
		    (initiate pr)
		    (find-effects))
                  (t 
		   (plantrace (format nil "~[~;it is~:;they are~] not satisfied."
				      (length *preconditions*)) nil nil)
;		   (format t "got to schedule achieve")
		   (regstore pr '*agenda* 'start)
		   (regstore pr '*reports* (new.ns))
		   (initiate pr)
		   (schedule-preconditions *preconditions*))))
	(find-effects               ;(format t "found effects")
            (cond (*reports*
 		   (regstore pr '*effects* *reports*)
		   (regstore pr '*reports* (new.ns))
		   (schedule-believe-effects (regfetch pr '*effects*))))
	    (regstore pr '*agenda* 'execute)
	    (initiate pr))
	(execute                     ;(format t "got to execute now!")
	    (cond ((primact? *node*)
		   (execute-primaction *node*)
		   (regstore pr '*agenda* 'done))
		  (t (regstore pr '*reports* (new.ns))
		     (regstore pr '*agenda* 'find-plans)
		     (initiate pr)
		     (find-plans))))
	(find-plans
	    (plantrace "the act " (list *node*) nil)
	    (plantrace (format nil 
			       "has ~[no~;a~:;the following~] plan~:p:"
		       	       (length *reports*))
		       *reports* nil)
	    (cond (*reports*
 		   (regstore pr '*plans* *reports*)
		   (regstore pr '*reports* (new.ns))
		   (regstore pr '*agenda* 'done)
		   (schedule-plans (regfetch pr '*plans*)))
		  (t ;;(format *debug-io* "No plans found for:  ")
		     ;;(eval `(sneps:full-describe ,*node*))
		     (cerror "this act will be ignored."
			     "I don't know how to ~a"
			     (snip::surfaceToString *node*))))
	    (regstore pr '*agenda* 'done))))))


(defun surfaceToString (node)
  "Returns the string that would be printed by (surface node)."
  (let ((strng (make-array '(0) :element-type 'base-char
			   :fill-pointer 0 :adjustable t)))
    (with-output-to-string (strm strng)
      (let ((sneps:outunit strm))
	(declare (special sneps:outunit))
	(cl:eval `(sneps:surface ,node))))
    strng))

;  (set multi::curnt% (list *NAME* *NODE* *CONTEXT-NAME*
;		           *REPORTS* *PRECONDITIONS* *EFFECTS*
;			   *AGENDA* *PRIORITY*))))

(setf (get 'act 'multi::lregs%)
      '(*NAME* *NODE* *CONTEXT-NAME* *REPORTS*
	    *PRECONDITIONS* *EFFECTS* *PLANS* *AGENDA* *PRIORITY*))
;
;
(defun get-preconds (n)
  "Returns the node sitting at the end of PRECONDITION arc of N"
  (choose.ns (nodeset.n n 'SNEPSUL::PRECONDITION)))
;
(defun get-effects (n)
  "Returns the node sitting at the end of EFFECT arc of N"
  (choose.ns (nodeset.n n 'SNEPSUL::EFFECT)))
;
(defun get-plans (n)
  "Returns the node sitting at the end of PLAN arc of N"
  (choose.ns (nodeset.n n 'SNEPSUL::PLAN)))
;


;;; Modified by scs/flj 8/01/04 to do the following...
;;; Added a patch to fix an intermittent error in which,
;;;    where it should have been believing an effect,
;;;    believe was called with no proposition-argument.
;;; It manifested itself with the error message
;;;     "Error: Non-structure argument nil passed to ref of structure slot 3"
(defun schedule-believe-effects (effects-to-be-believed)
  "Activates a BELIEVE act for each effect to be believed,
   and schedules it."
  (let ((believe-act nil)
	do-all-act
	believe-act-process)
    (cond (*use-do-acts*
	   (do.ns (eff (mapcar #'get-effects effects-to-be-believed))
	     (when eff
	       ;; Sometimes a node on the effects-to-be-believed list
	       ;;   is not an act-effect cableset, and so eff is nil.
	       ;; I'm not sure why,
	       ;;    but it may relate to forward inference. scs 7/2/04
	       (setf believe-act
		 (cons (choose.ns 
			#2!((build action ~(find-action-node 'believe)
				   object1 ~eff)))
		       believe-act))))
	   (setf do-all-act
	     (choose.ns 
	      #2!((build action ~(find-action-node 'do-all)
			 object1 ~believe-act))))
				;(plantrace "Intending to do" (list do-all-act) nil)
	   (activate-act.n do-all-act)
	   (setf believe-act-process (activation.n do-all-act))
	   (regstore believe-act-process '*priority* 'intend)
	   (regstore believe-act-process '*agenda* 'start)
	   (initiate believe-act-process))
	  (t (do.ns (eff (mapcar #'get-effects effects-to-be-believed))
		    (setq believe-act
		      (choose.ns
		       #2!((build action ~(find-action-node 'believe)
				  object1 ~eff))))
				;	(format t "believe act is ~S" believe-act)
		    (activate-act.n believe-act)
		    (setq believe-act-process (activation.n believe-act))
		    (regstore believe-act-process '*priority* 'intend)
		    (regstore believe-act-process '*agenda* 'start)
	       (initiate believe-act-process))))))






					;
(defun schedule-preconditions (preconds)
  "Schedules the achieving of preconditions."
  (let ((achieve-act nil)
	do-all-act
	achieve-act-process)
    (cond (*USE-DO-ACTS*
	   (do.ns (pre (mapcar #'get-preconds preconds))
		  (setf achieve-act
		    (cons (choose.ns
			   #2!((build action ~(find-action-node 'achieve)
				      object1 ~pre)))
			  achieve-act)))
	   (setf do-all-act
	     (choose.ns
	      #2!((build action ~(find-action-node 'do-all)
			 object1 ~achieve-act))))
	   (plantrace "Intending to do" (list do-all-act) nil)
	   (activate-act.n do-all-act)
	   (setf achieve-act-process (activation.n do-all-act))
	   (regstore achieve-act-process '*PRIORITY* 'INTEND)
	   (regstore achieve-act-process '*AGENDA* 'START)
	   (initiate achieve-act-process))
	  (t (do.ns (pre (mapcar #'get-preconds preconds))
		    (setq achieve-act
		      (choose.ns
		       #2!((build action ~(find-action-node 'achieve)
				  object1 ~pre))))
		    (plantrace "Intending to do" (list achieve-act) nil)
		    (activate-act.n achieve-act)
		    (setf achieve-act-process (activation.n achieve-act))
		    (regstore achieve-act-process '*PRIORITY* 'INTEND)
		    (regstore achieve-act-process '*AGENDA* 'START)
		    (initiate achieve-act-process))))))
;
(defun schedule-plans (plans)
  (let (do-one-act plan plan-list plan-process)
    (setf plan-list (mapcar #'get-plans plans))
    (cond (*use-do-acts*
	   (setf do-one-act
	     (choose.ns 
	      #2!((build action ~(find-action-node 'do-one)
			 object1 ~plan-list))))
	   (plantrace "Intending to do" (list do-one-act) nil)
	   (activate-act.n do-one-act)
	   (setf plan-process (activation.n do-one-act))
	   (regstore plan-process '*PRIORITY* 'INTEND)
	   (regstore plan-process '*AGENDA* 'START)
	   (initiate plan-process))
	  (t (setf plan (choose-arbitrary-element plan-list))
	     (activate-act.n plan)
	     (setf plan-process (activation.n plan))
	     (regstore plan-process '*PRIORITY* 'INTEND)
	     (regstore plan-process '*AGENDA* 'START)
	     (initiate plan-process)))))
;
(defun test-preconditions (act-process)
  "Initiates a deduction process to see if the preconditions
   of the act represented by ACT-PROCESS are true."
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
			    CRNTCT))
  (let (pr)
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
	       act-process
	       'HIGH))
    (do.ns (precond (mapcar #'get-preconds 
			    (regfetch act-process '*PRECONDITIONS*)))
	(activate.n precond)
	(setq pr (activation.n precond))
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
(defun tally-preconditions (preconditions reports)
  "Tallies to ensure if all preconditions are believed
   to be true."
    (do.ns (precond (mapcar #'get-preconds preconditions) t)
	(if (not (ismemb.ns precond reports))
            (return nil))))
			    
;
(defun execute-primaction (act-node)
  "Applies the given primitive action to the given act node."
  (funcall (gethash (action-of-act act-node) *primitive-action-functions*)
	   act-node))

(defun find-preconditions ()
  "Initiates a deduction process to find the preconditions of the
   currently active act process."
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
			    CRNTCT))
  (let (pr deduction-target) 
    (unless (and (snepsul::* 'precondition)
                 (sneps::isvar.n (first (snepsul::* 'precondition))))
	    (snepsul::$ 'precondition))
    (setq deduction-target
          (choose.ns (sneps::nseval 
		      (sneps:build snepsul::precondition
				   (snepsul::* 'precondition)
				   snepsul::act (snepsul::^ *NODE*)))))
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
	       (activation.n *NODE*)
	       'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	         (insert.chset (make.ch (new.filter)
					(new.switch)
					crntct
					;*NODE*
					'user
					'open)
			        (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (initiate pr)
  ))
;;;
;;;
(defun find-effects ()
  "Initiates a deduction process to find the effects of the
   currently active act process."
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
			    CRNTCT))
  (let (pr deduction-target) 
    (unless (and (snepsul::* 'effect)
                 (sneps::isvar.n (first (snepsul::* 'effect))))
	    (snepsul::$ 'effect))
    (setq deduction-target
          (choose.ns (sneps::nseval 
		      (sneps:build snepsul::effect
				   (snepsul::* 'effect)
				   snepsul::act (snepsul::^ *NODE*)))))
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
	       (activation.n *NODE*)
	       'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	         (insert.chset (make.ch (new.filter)
					(new.switch)
					crntct
					;*NODE*
					'user
					'open)
			        (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (initiate pr)
  ))

(defun find-plans ()
  "Initiates a deduction process to find plans that decompose the
   currently active complex act process."
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS*
			    CRNTCT))
  (let (pr deduction-target) 
    (unless (and (snepsul::* 'plan)
                 (sneps::isvar.n (first (snepsul::* 'plan))))
	    (snepsul::$ 'plan))
    (setq deduction-target
          (choose.ns (sneps::nseval 
		      (sneps:build snepsul::plan
				   (snepsul::* 'plan)
				   snepsul::act (snepsul::^ *NODE*)))))
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
	       (activation.n *NODE*)
	       'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	         (insert.chset (make.ch (new.filter)
					(new.switch)
					crntct
					;*NODE*
					'user
					'open)
			        (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (initiate pr)
  ))
;
;




    
    




