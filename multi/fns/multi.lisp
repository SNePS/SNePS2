;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MULTI; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: multi.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :multi)


;;; THE MULTI PACKAGE
;;;
;;;

;(defvar evnts)
(defvar curnt%)
(defvar *act-queue* (dequeue:new)
  "Holds act processes intended to be performed")

(defvar *high-priority-queue* (dequeue:new)
  "Holds processes with high priority, that have been at the front
of the EVNTS queue in the old scheme")

(defvar *low-priority-queue* (dequeue:new)
  "Holds processes with low priority, that have been at the back
of the EVNTS queue in the old scheme")

(defvar *use-one-queue-only* nil
  "When this flag is T multi does not give priority to processes with reports,
   enabling the correct use of recursive rules.
   Introduced by njm/cpf 06/20/89")

(defun clear-all-queues ()
  "Makes all event queues empty"
  (setq *high-priority-queue* (dequeue:new)
	*low-priority-queue* (dequeue:new)
  	*act-queue* (dequeue:new)))

;;;
;;;
;;; TRACING VARIABLES
;;;
(defvar *trace-initiated-processes* nil
  "Enable trace of initiated processes.
   Values:      NIL ----> Trace is disabled.
                T   ----> Trace all initiated processes.
          (<PROCS>) ----> Trace initiation of specified processes (<PROCS>)")

(defvar *trace-events* nil
  "Enable trace of before-after snapshots of events.
   Values:      NIL ----> Trace is disabled.
                  T ----> Trace all events before & after execution.
          (<PROCS>) ----> Trace specified events before & after execution.")

(defvar *trace-enabled* nil
  "Indicates that some trace (ev-trace/cs-trace) is enabled (value:T).")




;;;DP
;;;--
;;;This macro defines processes for MULTIP. The call expected is
;;;(DP <PROCESS-NAME> (<LIST OF REGISTERS>) <PROCESS-BODY>)
;;;where <PROCESS-NAME>       :is atomic and name of a process template
;;;      <LIST-OF-REGISTERS> :is the list of all local registers used
;;;                           within the <PROCESS-BODY> except *NAME*,
;;;                           which the system manages.
;;;      <PROCESS-BODY>      :consists of forms to be evaluated.
;;;e.g. -- (DP MPLUS (A1 A2 ANS)
;;;           (SETQ ANS (+ A1 A2)))

(defmacro dp (name arg_list &body body)
  "Defines a MULTI process"
  (let ((df (get_def (cons arg_list body)))
	(reg_sym `(*NAME* ,@arg_list)))
    `(prog (retval)
	   (cond ((functionp ',name)
		  (setq retval (list ',name 'redefined)))
		 (t (setq retval ',name)))
	   (setf (get ',name 'lregs%) ',reg_sym)
	   (defun ,name ,@df)
	   
	   (return retval)))) 

(defun has-reg (proc reg)
  "Returns T if the <process> has the given register, NIL otherwise."
  (member reg (get (process-name proc) 'lregs%)))

;;;GET-DEF
;;;-------
;;;This a helper function for DP. It takes the process body template and
;;;adds the system register *NAME* and returns a completed
;;;process template.

(defun get_def (pform)
  "Constructs the body of the process template for the DP macro"
  (prog (arg_list body)
	(setq arg_list `(*NAME* ,@(car pform))
	      body (cdr pform))
	(return `(,arg_list (declare (special curnt%)) ,@body
		  (set curnt% (list ,@arg_list))))))

;;;REGFETCH
;;;--------
;;;This function returns the value of a particular register in a process.
;;;The form of the call is:
;;;       (REGFETCH <PROCESS> <REGISTER-NAME>)
;;;where <PROCESS> is process structure and
;;;      <REGISTER-NAME> is the name of a register as defined by the original\
;;;                      call to DP.
;;;...  
;;;RETURNS: The value of the specified register with the process,
;;;         or calls BREAK if the process has no such register.

(defun regfetch (process register)
  "Returns the value of the specified register in a process"
  (let ((n (position register (process-registers process))))
    (cond (n (svref (process-slots process) n))
	  (t
	   (format *debug-io*
		   "~&multi::regfetch asked for unknown register ~S of process ~S =~%"
		   register (process-id process))
	   (print-regs process)
	   (break)))))

;;;REGSTORE
;;;--------
;;;This function stores a specified value in the register of the process. The
;;;for of the call is:
;;;     (REGFETCH <PROCESS> <REGISTER-NAME> <REGISTER-VALUE>)
;;;where <PROCESS> is a process structure
;;;      <REGISTER-NAME> is the name of a register in the call to DP
;;;                      for the type of process, and
;;;      <REGISTER-VALUE> is the value to be stored in the process.
;;;REGSTORE acts destructively upon the values of the process.
;;;...
;;;RETURNS: <REGISTER-VALUE>
;;;         or calls BREAK if the process has no such register.

(defun regstore (process register value)
  "Stores the VALUE in the REGISTER of the PROCESS"
  (let ((n (position register (process-registers process))))
    (cond (n (setf (svref (process-slots process) n) value)
	     value)
	  (t
	   (format *debug-io*
		   "~&multi::regstore asked to store value ~S in unknown register ~S of process ~S =~%"
		   value register (process-id process))
	   (print-regs process)
	   (break)))))

(defstruct (process (:print-object process-print))
  "A multi process." 
  (id (gensym "p") :read-only t)
  (slots nil :type vector) ; a vector of register values starting with *NAME*
  )

;;;NEW
;;;---
;;;NEW returns a process whose slots are
;;;the register values passed to it. This implementation assumes that the
;;;arguments to new are in the same order as the definition of the process
;;;template. NEW also assumes that the *NAME* (or process template) is the first
;;;argument.
;;;If the number of registers supplied in the call differs from the definition
;;;of the process template, then BREAK is called.

(defun new (&rest arglist)
  "Creates a new process"
  (cond ((= (length arglist) (length (get (first arglist) 'lregs%)))
	 (let ((new-process
		(make-process :slots (apply #'vector arglist))))
	   (when *trace-initiated-processes*
	     (format t "~%** New process ~S~%" new-process ))
	   new-process))
	 (t
	  (format *debug-io* "~%USER-ERR from new~%~S~%HAS WRONG NUMBER OF ARGUMENTS~%" arglist)
	  (break))))

(defun process-registers (p)
  "Returns a list of the register names for the process p."
  (get (process-name p) 'lregs%))

(defun process-name (p)
  "Returns the name of the process p."
  (svref (process-slots p) 0))

(defun process-change-slots (p newslots)
  "Changes the vector of slots to the vector newslots."
  (setf (process-slots p) newslots))

(defun process-print (p s)
  "Print the process by printing its id."
  (princ (process-id p) s))

;
; =============================================================================
;
; INITIATE
; --------
;
;       arguments     : event - <process>
;
;       returns       : <process queue>
;
;       description   : initiate function required by Multi
;
;                                        written :  ???
;                                        modified:  njm/cpf 6/20/89
;
; change: If *use-one-queue-only* flag is true then only the *high-priority-queue*
;         is used. This was implemented to avoid dead cycles generated by
;         recursive rules.
;

(defun initiate (evnt)
  "Schedules the given process (EVNT)."
  (let* ((event-priority (regfetch evnt '*priority*))
	 (queue (cond (*use-one-queue-only* '*high-priority-queue*)
		      (t (case event-priority
			   (snip:high '*high-priority-queue*)
			   (snip:low  '*low-priority-queue*)
   			   (snip::intend '*act-queue*)
			   (t (format *debug-io*
				      "No queue for priority ~s. Can't initiate event ~s ~
                                                  whose name is ~s,and whose registers are:~%"
				      event-priority
				      evnt
				      (process-name evnt))
			      (print-regs evnt)
			      (break)))))))
    (when (and *trace-initiated-processes*
	       (not (dequeue:in-queue evnt (eval queue)))
	       (or (eq *trace-initiated-processes* t)
		   (member (process-name evnt) *trace-initiated-processes*
			   :test #'eq)))
      (format t "~%** Initiate process ~S with id: ~A - on ~S. ~%   Initiated by process: ~A. Length of ~S - ~S~%"
	      (process-name evnt) (process-id evnt) queue (process-id curnt%)
	      queue (1+ (dequeue:queue-length (eval queue)))))
    (set queue (schedule evnt (eval queue)))))


;;;SCHEDULE
;;;--------
;;;This function performs the scheduling of events.

;(defun schedule (evnt evnts)
 ; "Performs the scheduling of events"
  ;(dequeue:insert-rear evnt evnts))
;
;;;MULTIP
;;;------
;;;This function does the evaluation of processes. The list of processes
;;;created by NEW are stored in two queues, *high-priority-queue* and
;;;*low-priority-queue*. These are global variables that will be bound
;;;to the values of HPQ and LPQ on entry to multip. MULTIP loops until 
;;;no more events remain on either of the queues where events on the 
;;;low priority list are not processed at all until the high priority
;;;queue becomes empty. MULTIP expects processes to be represented
;;;as a process structure:
;;;      #S(process :id <ID> :slots #S(<NAME> <OTHER> ... <REGISTERS>))
;;;where ID is an identifier associated with each process;
;;;NAME must be the name of the process template (*NAME*),
;;;and also a function definition (a lambda expression).

(defun multip (actq hpq lpq)
  "Evaluates processes"
  (let ((*act-queue* actq)
	(*high-priority-queue* hpq)
	(*low-priority-queue* lpq)
	highest-priority-non-empty-queue
	*name*)
    (declare ;; (optimize (speed 3)) ; Comment out for debug version
	     (special *name*))
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
		 (setf highest-priority-non-empty-queue *high-priority-queue*))
		((not (dequeue:empty *low-priority-queue*))
		 (setf highest-priority-non-empty-queue *low-priority-queue*))
		((not (dequeue:empty *act-queue*))
		 (setf highest-priority-non-empty-queue *act-queue*))
		(t (return)))
	  (setf curnt% (dequeue:front highest-priority-non-empty-queue)
		regvals (coerce (process-slots curnt%) 'list)
		*name* (process-name curnt%))
	  (dequeue:delete-front highest-priority-non-empty-queue)
	  (if *trace-enabled*
	      (cond
		((or (eq *trace-events* t)
		     (member *name* *trace-events*))
		 (print-regs curnt% "Entering"))))
	  (apply (symbol-function *name*) regvals)
	  (if *trace-enabled*
	      (cond
		((or (eq *trace-events* t)
		     (member *name* *trace-events*))
		 (print-regs curnt% "Leaving"))))
	  (go loop))))

;;;
;;;
;;; TRACING FUNCTIONS
;;;

(defsnepscom ev-trace ((&rest lfrms))
  "Enables event trace for multi (by name)"
  (setq *trace-enabled* t
	*trace-events*
	(record *trace-events*
		(mapcan #'(lambda (p)
			    (cond ((get p 'lregs%) (list p))
				  (t (format t "~&~A is not a process name.~%"
					     p)
				     nil)))
			lfrms))))

(defsnepscom unev-trace ((&rest lfrms))
  "Undoes the effect of EV-TRACE. Returns the current status of
   *trace-events* flag."
  (setq *trace-events* (forget *trace-events* lfrms))
  (when (null *trace-events*)
    (setq *trace-enabled* nil)))

(defsnepscom in-trace ((&rest lfrms))
  "Enables tracing of initiated processes."
  (setq *trace-initiated-processes*
    (record *trace-initiated-processes* lfrms)))

(defsnepscom unin-trace ((&rest lfrms))
  "Undoes the effect of IN-TRACE."
  (setq *trace-initiated-processes*
    (forget *trace-initiated-processes* lfrms)))

(defun record (trace-flag add-list)
  "Inserts ADD-LIST into TRACE-FLAG by union."
  (cond ((and (null add-list) (null trace-flag)) t)
	((and add-list (atom trace-flag)) add-list)
	(add-list (append add-list trace-flag))
	(t trace-flag)))

(defun forget (trace-flag rem-list)
  "Removes REM-LIST from TRACE-FLAG."
  (cond ((null rem-list) nil)
	((atom trace-flag) nil)
	((member (car trace-flag) rem-list) (forget (cdr trace-flag) rem-list))
	(t (cons (car trace-flag) (forget (cdr trace-flag) rem-list)))))

#+explorer
(defun pprin-value (value &optional (stream *standard-output*))
  (let* ((initial-indentation
	   (or (global:send-if-handles stream :read-cursorpos :character)
	       30))
	 (stream-width
	   (or (global:send-if-handles stream :size-in-characters)
	       80))
	 (sys:pp-line-length
	   (cond ((> stream-width 100) 100)
		 (t (- stream-width 5)))))
    (sys:output-pretty-object value nil initial-indentation)))

#-explorer
(defun pprin-value (value &optional (stream *standard-output*))
  (write value :pretty t :stream stream))

(defun print-regs (process &optional msg)
  "Prints the current bindings for PROCESS."
  (let ((ind 5))
    (if msg
	(format t "~%>>>>>> ~A process id = ~A with bindings:"
		       msg (process-id process))
      (format t "~%~V@TProcess id = ~A has bindings:"
	      ind (process-id process)))
    (loop for reg in (process-registers process)
	for val across (process-slots process)
	do (format t "~%~V@T~A = ~A" ind reg val)
	finally (format t "~%"))))
