;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: deducetrue.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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


(defun deducetrue* (how-many targetset)
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS* CRNTCT))
  (let ((inference-context (sneps:value.sv crntct))
	deduction-target pr)
    (when-intensional-contexts
     ;; Use the context name
     (setq inference-context crntct))
    (setq *ADDED-NODES* (new.ns))		;special var
    (setq *DEDUCTION-RESULTS* (new.ns))		;special var
;   (clear-lastinfer)
    (if (isnew.ns targetset) *DEDUCTION-RESULTS*)
    (setq deduction-target (choose.ns targetset))
    (setq *USER-PROCESS*
	  (new 'posuser
	       (new.repset)
	       crntct
	       (new.ns)
	       (if (numberp how-many) how-many)
	       (if (not (atom how-many)) (car how-many))
	       (if (not (atom how-many)) (cadr how-many))
	       0
	       0
	       'USER
	       'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	      (insert.chset
		(make.ch (new.filter)
			 (new.switch)
			 inference-context
			 'USER
			 'OPEN)
		(regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (multip (dequeue:new)                               ; act-queue
	    (dequeue:insert-rear pr (dequeue:new))      ; high priority
	    (dequeue:new))                              ; low priority
    *DEDUCTION-RESULTS*))

(defsnepscom deducetrue ((&rest snd) (top ns bns tbns fns))
  (let* ((numberfield (if (isnumbfield (car snd)) (car snd)))
	 (sd  (if numberfield (sneps:getsndescr (cdr snd))
		(sneps:getsndescr snd)))
	 (crntct (if numberfield (sneps:processcontextdescr (cdr snd))
		   (sneps:processcontextdescr snd)))
	 (crntctname crntct)) 
    (declare (special crntct crntctname))
    (values (deducetrue* numberfield
		     (sneps::nseval (cons 'sneps:build sd)))
	    crntctname)))

(defsnepscom deducewh ((&rest snd) (top ns bns tbns fns))
  (let* ((numberfield (if (isnumbfield (car snd)) (car snd)))
	 (sd  (if numberfield (sneps:getsndescr (cdr snd))
		(sneps:getsndescr snd)))
	 (crntct (if numberfield (sneps:processcontextdescr (cdr snd))
		   (sneps:processcontextdescr snd)))
	 (crntctname crntct)) 
    (declare (special crntct crntctname))
    (values (delete-duplicates (deducewh* numberfield
		     (sneps::nseval (cons 'sneps:build sd))) :test #'equal)
	    crntctname)))

(defun deducewh* (how-many targetset)
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS* CRNTCT))
  (let ((inference-context (sneps:value.sv crntct))
	deduction-target pr)
    (when-intensional-contexts
     ;; Use the context name
     (setq inference-context crntct))
    (setq *ADDED-NODES* (new.ns)) ;special var
    (setq *DEDUCTION-RESULTS* '()) ;special var
				;   (clear-lastinfer)
    (if (isnew.ns targetset) *DEDUCTION-RESULTS*)
    (setq deduction-target (choose.ns targetset))
    (setq *USER-PROCESS*
      (new 'whuser
	   (sneps:node-freevars deduction-target)
	   (new.repset)
	   crntct
	   (new.ns)
	   (if (numberp how-many) how-many)
	   (if (not (atom how-many)) (car how-many))
	   (if (not (atom how-many)) (cadr how-many))
	   0
	   0
	   'USER
	   'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	      (insert.chset
	       (make.ch (new.filter)
			(new.switch)
			inference-context
			'USER
			'OPEN)
	       (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (multip (dequeue:new)	; act-queue
	    (dequeue:insert-rear pr (dequeue:new)) ; high priority
	    (dequeue:new))	; low priority
    *DEDUCTION-RESULTS*))

(defun whuser (*NAME* *VARS* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES*
	       *TOTAL-DESIRED* *POS-DESIRED* *NEG-DESIRED*
	       *POS-FOUND* *NEG-FOUND* *CLIENT* *PRIORITY*)
  "Like the user process function, but only collects positive answers."
  (declare (special multi::curnt%))
  (let ((reports *REPORTS*))
    (setq *REPORTS* (new.repset))
    (do.repset (report reports)
	       (let ((n (node.rep report)))
		 (unless (or (member n *DEDUCED-NODES* :key #'cdr)
			     (not (eq (sign.rep report) 'POS)))
		   (tally report)
		   (pushnew
		    (mapcar
		     #'(lambda (v)
			 (cons (or (sneps:node-snepslog v) v)
			       (match::applysubst v (subst.rep report))))
		     *VARS*)
		    *DEDUCED-NODES* :test #'equal)))))
  (setq *DEDUCTION-RESULTS* (append *DEDUCED-NODES* *DEDUCTION-RESULTS*))
  (if (not (eq *CLIENT* 'snip::user))            ;; *CLIENT* can be a node
      (send-to-client *CLIENT* *DEDUCED-NODES*))
  (if (enough-answers) (suspend-inference))
  (multi:process-change-slots multi::curnt%
			      (vector *NAME* *VARS* *REPORTS*
				      *CONTEXT-NAME* *DEDUCED-NODES*
				      *TOTAL-DESIRED* *POS-DESIRED*
				      *NEG-DESIRED* *POS-FOUND*
				      *NEG-FOUND* *CLIENT*
				      *PRIORITY*)))

(setf (get 'whuser 'multi::lregs%)
  '(*NAME* *VARS* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
    *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
    *CLIENT* *PRIORITY*))

(defun deducefalse* (how-many targetset)
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS* CRNTCT))
  (let ((inference-context (sneps:value.sv crntct))
	deduction-target pr)
    (when-intensional-contexts
     ;; Use the context name
     (setq inference-context crntct))
    (setq *ADDED-NODES* (new.ns))		;special var
    (setq *DEDUCTION-RESULTS* (new.ns))		;special var
;   (clear-lastinfer)
    (if (isnew.ns targetset) *DEDUCTION-RESULTS*)
    (setq deduction-target (choose.ns targetset))
    (setq *USER-PROCESS*
	  (new 'neguser
	       (new.repset)
	       crntct
	       (new.ns)
	       (if (numberp how-many) how-many)
	       (if (not (atom how-many)) (car how-many))
	       (if (not (atom how-many)) (cadr how-many))
	       0
	       0
	       'USER
	       'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	      (insert.chset
		(make.ch (new.filter)
			 (new.switch)
			 inference-context
			 'USER
			 'OPEN)
		(regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (multip (dequeue:new)                               ; act-queue
	    (dequeue:insert-rear pr (dequeue:new))      ; high priority
	    (dequeue:new))                              ; low priority
    *DEDUCTION-RESULTS*))

(defsnepscom deducefalse ((&rest snd) (top ns bns tbns fns))
  (let* ((numberfield (if (isnumbfield (car snd)) (car snd)))
	 (sd  (if numberfield (sneps:getsndescr (cdr snd))
		(sneps:getsndescr snd)))
	 (crntct (if numberfield (sneps:processcontextdescr (cdr snd))
		   (sneps:processcontextdescr snd)))
	 (crntctname crntct)) 
    (declare (special crntct crntctname))
    (values (deducefalse* numberfield
		     (sneps::nseval (cons 'sneps:build sd)))
	    crntctname)))

(defun neguser (*NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
	     *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
		*CLIENT* *PRIORITY*)
  "Like the user process function, but only collects positive answers."
  (declare (special multi::curnt%))
  (let ((reports *REPORTS*))
    (setq *REPORTS* (new.repset))
    (do.repset (report reports)
       (let ((n (node.rep report)))
	 (unless (or (ismemb.ns n *DEDUCED-NODES*)
		     (not (eq (sign.rep report) 'NEG)))
		 (tally report)
		 (setq *DEDUCED-NODES* (insert.ns n *DEDUCED-NODES*))))))
  (setq *DEDUCTION-RESULTS* (union.ns *DEDUCED-NODES* *DEDUCTION-RESULTS*))
    (if (not (eq *CLIENT* 'snip::user))            ;; *CLIENT* can be a node
      (send-to-client *CLIENT* *DEDUCED-NODES*))
  (if (enough-answers) (suspend-inference))
  (multi:process-change-slots multi::curnt%
			      (vector *NAME* *REPORTS* *CONTEXT-NAME*
				      *DEDUCED-NODES* *TOTAL-DESIRED*
				      *POS-DESIRED* *NEG-DESIRED*
				      *POS-FOUND* *NEG-FOUND* *CLIENT*
				      *PRIORITY*)))

(setf (get 'neguser 'multi::lregs%)
  '(*NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
    *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
    *CLIENT* *PRIORITY*))


(defsnepscom deducewhnot ((&rest snd) (top ns bns tbns fns))
  (let* ((numberfield (if (isnumbfield (car snd)) (car snd)))
	 (sd  (if numberfield (sneps:getsndescr (cdr snd))
		(sneps:getsndescr snd)))
	 (crntct (if numberfield (sneps:processcontextdescr (cdr snd))
		   (sneps:processcontextdescr snd)))
	 (crntctname crntct)) 
    (declare (special crntct crntctname))
    (values (delete-duplicates (deducewhnot* numberfield
		     (sneps::nseval (cons 'sneps:build sd))) :test #'equal)
	    crntctname)))

(defun deducewhnot* (how-many targetset)
  (declare (special *USER-PROCESS* *ADDED-NODES* *DEDUCTION-RESULTS* CRNTCT))
  (let ((inference-context (sneps:value.sv crntct))
	deduction-target pr)
    (when-intensional-contexts
     ;; Use the context name
     (setq inference-context crntct))
    (setq *ADDED-NODES* (new.ns)) ;special var
    (setq *DEDUCTION-RESULTS* '()) ;special var
				;   (clear-lastinfer)
    (if (isnew.ns targetset) *DEDUCTION-RESULTS*)
    (setq deduction-target (choose.ns targetset))
    (setq *USER-PROCESS*
      (new 'notwhuser
	   (sneps:node-freevars deduction-target)
	   (new.repset)
	   crntct
	   (new.ns)
	   (if (numberp how-many) how-many)
	   (if (not (atom how-many)) (car how-many))
	   (if (not (atom how-many)) (cadr how-many))
	   0
	   0
	   'USER
	   'HIGH))
    (activate.n deduction-target)
    (setq pr (activation.n deduction-target))
    (regstore pr '*REQUESTS*
	      (insert.chset
	       (make.ch (new.filter)
			(new.switch)
			inference-context
			'USER
			'OPEN)
	       (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (multip (dequeue:new)	; act-queue
	    (dequeue:insert-rear pr (dequeue:new)) ; high priority
	    (dequeue:new))	; low priority
    *DEDUCTION-RESULTS*))

(defun notwhuser (*NAME* *VARS* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES*
		  *TOTAL-DESIRED* *POS-DESIRED* *NEG-DESIRED*
		  *POS-FOUND* *NEG-FOUND* *CLIENT* *PRIORITY*)
  "Like the user process function, but only collects positive answers."
  (declare (special multi::curnt%))
  (let ((reports *REPORTS*))
    (setq *REPORTS* (new.repset))
    (do.repset (report reports)
	       (let ((n (node.rep report)))
		 (unless (or (member n *DEDUCED-NODES* :key #'cdr)
			     (not (eq (sign.rep report) 'NEG)))
		   (tally report)
		   (pushnew
		    (mapcar #'(lambda (v)
				(cons (or (sneps:node-snepslog v) v)
				      (match::applysubst v (subst.rep report))))
			    *VARS*)
		    *DEDUCED-NODES* :test #'equal)))))
  (setq *DEDUCTION-RESULTS* (append *DEDUCED-NODES* *DEDUCTION-RESULTS*))
  (if (not (eq *CLIENT* 'snip::user))            ;; *CLIENT* can be a node
      (send-to-client *CLIENT* *DEDUCED-NODES*))
  (if (enough-answers) (suspend-inference))
  (multi:process-change-slots multi::curnt%
			      (vector *NAME* *VARS* *REPORTS*
				      *CONTEXT-NAME* *DEDUCED-NODES*
				      *TOTAL-DESIRED* *POS-DESIRED*
				      *NEG-DESIRED* *POS-FOUND*
				      *NEG-FOUND* *CLIENT*
				      *PRIORITY*)))

(setf (get 'notwhuser 'multi::lregs%)
  '(*NAME* *VARS* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
    *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
    *CLIENT* *PRIORITY*))



    
    




