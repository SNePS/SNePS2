;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: deduce.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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


(defun deduce* (how-many targetset)
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
	  (new 'user
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



;(defun clear-lastinfer ()
;  (progn
;    (setf (get 'lastinfer nil) '%events-queue%)
;    (setf (get 'lastinfer nil) '%user-process%)))

;(defun clear-lastinfer ()
;;  ;; this is how it should be if it was used (hc)
;;  (setf (get 'lastinfer '%events-queue%) nil)
;;  (setf (get 'lastinfer '%user-process%) nil)
;   nil)

(defmacro isnumbfield (x)
  `(or (and (atom ,x) (numberp ,x))
       (and (listp ,x)
            (atom (car ,x)) (numberp (car ,x))
            (atom (cadr ,x)) (numberp (cadr ,x)))))


(defsnepscom deduce ((&rest snd) (top ns bns tbns fns))
  (let* ((numberfield (if (isnumbfield (car snd)) (car snd)))
	 (sd  (if numberfield (sneps:getsndescr (cdr snd))
		(sneps:getsndescr snd)))
	 (crntct (if numberfield (sneps:processcontextdescr (cdr snd))
		   (sneps:processcontextdescr snd)))
	 (crntctname crntct)) 
    (declare (special crntct crntctname))
    (values (deduce* numberfield
		     (sneps::nseval (cons 'sneps:build sd)))
	    crntctname)))



    
    




