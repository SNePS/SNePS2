;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: user.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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


; =============================================================================
;
; tally
; -----
;
;       arguments     : report - <report>
;
;       nonlocal-vars : *POS-FOUND*, *NEG-FOUND* registers of the current process
;
;       description   : increments the appropriate count of reports received,
;                       based on the sign of the report
;
;                                        written :  rgh 11/18/85
;                                        modified:  rgh  3/08/86
;
;
(defmacro tally (report)
  `(cond ((eq (sign.rep ,report) 'POS)
             (setq *POS-FOUND* (1+ *POS-FOUND*)))
         (t
             (setq *NEG-FOUND* (1+ *NEG-FOUND*)))))
;
;
; =============================================================================
;
; enough-answers
; --------------
;
;       returns       : <boolean>
;
;       nonlocal-vars : *TOTAL-DESIRED*, *POS-DESIRED*, *NEG-DESIRED*, *POS-FOUND*,
;                       and *NEG-FOUND* registers of the current process
;
;       description   : returns "true" if the desired number of deduced
;                       results has been found, "false" otherwise
;
;                                        written :  rgh 11/18/85
;                                        modified:
;
;
(defmacro enough-answers ()
  `(cond ((and (null *TOTAL-DESIRED*) (null *POS-DESIRED*)) nil)
         ((numberp *TOTAL-DESIRED*)
             (not (< (+ *POS-FOUND* *NEG-FOUND*) *TOTAL-DESIRED*)))
         (t
             (not (or (< *POS-FOUND* *POS-DESIRED*)
                      (< *NEG-FOUND* *NEG-DESIRED*))))))
;
;
; =============================================================================
;
; user
; ----
;
;       description   : This is the process which represents the SNePS user
;                       who has asked the system to deduce something
;
;                                        written :  rgh 11/18/85
;                                        modified:  rgh 11/27/85
;                                                   rgh  3/09/86
;                                                   rgh  4/13/86
;                                                   njm  4/27/89
;                                                   dk 7/1/92		   
;
;
(defun user (*NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
	     *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
	     *CLIENT* *PRIORITY*)
  (declare (special multi::curnt%))
  (let ((reports *REPORTS*))
    (setq *REPORTS* (new.repset))
    (do.repset (report reports)
       (let ((n (node.rep report)))
	 (unless (ismemb.ns n *DEDUCED-NODES*)
		 (tally report)
		 (setq *DEDUCED-NODES* (insert.ns n *DEDUCED-NODES*))))))
  (setq *DEDUCTION-RESULTS* (union.ns *DEDUCED-NODES* *DEDUCTION-RESULTS*))
    (if (not (eq *CLIENT* 'snip::user))            ;; *CLIENT* can be a node
      (send-to-client *CLIENT* *DEDUCED-NODES*))
  (if (enough-answers) (suspend-inference))
  (multi:process-change-slots
   multi::curnt% (vector *NAME* *REPORTS* *CONTEXT-NAME*
			 *DEDUCED-NODES* *TOTAL-DESIRED* *POS-DESIRED*
			 *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
			 *CLIENT* *PRIORITY*)))

(setf (get 'user 'multi::lregs%)
      '(*NAME* *REPORTS* *CONTEXT-NAME* *DEDUCED-NODES* *TOTAL-DESIRED*
	       *POS-DESIRED* *NEG-DESIRED* *POS-FOUND* *NEG-FOUND*
	       *CLIENT* *PRIORITY*))

;
;
;
(defun send-to-client (*CLIENT* *DEDUCED-NODES*)
  (let ((prior-reports (regfetch *CLIENT* '*REPORTS*)))
    (do.ns (deduced-node *DEDUCED-NODES*)
           (setq prior-reports (insert.ns deduced-node prior-reports)))
  (regstore *CLIENT* '*REPORTS* prior-reports)))
;
; =============================================================================
;
; suspend-inference
; -----------------
;
;       returns       : nil
;
;       nonlocal-vars : evnts - the Multi events queue
;                       USER-PROCESS  - the user process for this deduction
;                       lastinfer
;
;       description   : saves the current state of a deduction on the
;                       property list of the atom 'lastinfer, when the
;                       deduction is halted due to enough answers having
;                       been found
;
;       side-effects  : modifies the property list of 'lastinfer
;
;                                        written :  rgh 11/18/85
;                                        modified:  rgh  3/08/86
;                                                   hc  04/26/89
;
;
;(defun suspend-inference ()
;   (setf (get 'lastinfer evnts) '%events-queue%)
;   (setf (get 'lastinfer *USER-PROCESS*) '%user-process%)
;   (setq evnts nil))

(defun suspend-inference ()
;  ;; this is how it should be if it was used (hc)
;  (setf (get 'lastinfer '%events-queue%) evnts)
;  (setf (get 'lastinfer '%user-process%) *USER-PROCESS*)
  (multi:clear-all-queues))
;
;
; =============================================================================



    
    




