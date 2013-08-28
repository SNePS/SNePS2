;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: snepstop.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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




(in-package :sneps)

#+allegro
(adjust-for-acl6 :before)

(defvar *sneps-setup-flag* t
  "T as long as the initial SNePS setup has not been performed.")

(defvar *sneps-exit-flag* nil
  "When T, sneps-loop exits SNePS.")

(defvar *sneps-prompt* "* "  ;;; *prompt* -> *sneps-prompt*  FLJ 9/2/02
  "The SNePS prompt.")

(defvar crntct 'default-defaultct
  "The default current context")

(defvar inunit :unbound
  "The current stream where input is read from")

(defvar outunit :unbound
  "The current stream where output is written to")

(defvar *Node-Counter* 0
  "Holds the value of the 'order' field of the last created node.")

;==========================================================================
;
; sneps 
; -----
;
;      arguments     : none
;
;      returns       : "End of SNePS"
;
;      nonlocal-vars : inunit outunit
;
;      description   : This function is the top level function of SNePS.
;                         First, the function calls the "sneps-init" 
;                      function to initialize the system (see "sneps-init").
;                         Then the function calls the sneps read-eval-print 
;                      loop.
;                         The user can exit from the system into lisp by
;                      using the command "(lisp)". This command will set the
;                      "*sneps-exit-flag*" to true; when "sneps" sees it,
;                      it will return from the read-eval-print loop.
;                         The user can exit from the system directly into 
;                      the UNIX shell, by using the command "(exit)" which 
;                      will invoke the lisp function "exit".
;                         The user can also switch momentarily to a pseudo 
;                      lisp environment by using the commands "^" or "^^". 
;                      The command "^^" puts the user in a pseudo
;                      lisp environment where a one or more forms can be
;                      typed in and evaluated one at a time until the form 
;                      "^^" is typed again, at which time control is returned to 
;                      SNePS. The command "^" is identical to "^^" but only
;                      one line of forms can be typed and evaluated (no 
;                      blank character at the end of the line - see function
;                      "pseudolisp1"). Moreover, after the evaluation of the 
;                      form, the system gets automatically back into the 
;                      SNePS environment. The user can also switch momentarily 
;                      to lisp by using an interrupt. 
;
(defun sneps ()
  (let ((*package* (find-package 'snepsul))
	(*print-length* nil)
	(*print-level* nil)
	(*print-pretty* t)
	(inunit *standard-input*)
	(outunit *standard-output*))
    (sneps-init)
    (sneps-loop)
    "End of SNePS"))

;; If you change this make sure the `(sneps)' and `(snepslog)' banners
;; and the `(copyright)' command are still properly formatted:
(defconstant *copyright-years* "1984--2013")

; ==========================================================================
;
; sneps-init
; ----------
;
;       arguments     : none
;
;       returns       : 
;
;       nonlocal-vars : outunit *sneps-setup-flag*
;                       user::*sneps-version*
; 
;       description   : When this function is entered it prints out the sneps
;                       startup message with the current time. The first time
;                       it is called it also initializes the network.
;
;       side-effects  : side effects the non-local variables and prints 
;                       messages.
;
;                                        written : CCC mm/dd/yy
;                                        modified: ejm 10/25/83, 06/01/84
;                                        modified: scs 12/31/87 acc to scottlog
;                                                   hc 07/07/93

(defun sneps-init ()
  (format outunit "~2%   Welcome to ~a" cl-user::*sneps-version*)
  (format
   outunit
   "~2%Copyright (C) ~a by Research Foundation of~
     ~%State University of New York. SNePS comes with ABSOLUTELY NO WARRANTY!~
     ~%Type `(copyright)' for detailed copyright information.~
     ~%Type `(demo)' for a list of example applications."
   *copyright-years*)
  (multiple-value-bind
    (sec min hr day month year) (get-decoded-time)
    (format outunit "~2%   ~A/~A/~2,'0D ~A:~2,'0D:~2,'0D~%~%"
	    month day year hr min sec))  
  (when *sneps-setup-flag*
    (sneps-setup)
    (setq *sneps-setup-flag* nil)))

(defun sneps-setup ()
  (chew-up-output (outunit)
     (resetnet t)))

(defsnepscom copyright (())
  (format outunit
	  "
  Copyright (C) ~a
  Research Foundation of State University of New York

  SNePS is free software; you may redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  SNePS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with SNePS; see the file COPYING.  If not, write to
  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA, or to
  Dr. Stuart C. Shapiro, Department of Computer Science and Engineering, 
  University at Buffalo, The State University of New York, 
  201 Bell Hall, Buffalo, NY 14260, USA"
	  *copyright-years*))


;==========================================================================
;
; sneps-loop 
; ----------
;
;      arguments     : none
;
;      returns       : nil
;
;      nonlocal-vars : inunit outunit *sneps-exit-flag* *sneps-prompt*
;                      demo-start-time   
;                                ;; *prompt* -> *sneps-prompt*  FLJ 9/2/02
;                     
;      description   : This function is the SNePS read-eval-print loop.
;                      Before reading, a prompt will be printed if the sneps
;                      input port is the terminal. It will call the function
;                      "sneps-read" to read in the new <command> and stores
;                      it in the variable "command". Then it sneps-evaluates
;                      the command and stores the result in the the variable
;                      "result".  Finally, it will print out the result, 
;                      the CPU time and GC time for reading and evaluating
;                      the command by calling the function "sneps-timer".
;                     
;                                         written:  CCC 08/02/83
;                                         modified: ejm 10/25/83, 02/13/84,
;                                                       06/13/84
;;;                                                 ssc 06/19/87
;;;                                                 scs 03/29/88
;                                                   njm  4/27/89
;                                                    hc 11/20/91
;                                                    hc 07/07/93

(defun sneps-loop ()
  (let (command demo-start-time oldtime newtime result result-context
	*sneps-exit-flag*)
    (declare (special demo-start-time))
    (loop
      (unwind-protect
	  (catch 'sneps-error
	    (with-demo-control (inunit ((sneps-read (dump))
					(pseudolisp-read NIL)))
	      (loop
	       (princ *sneps-prompt* outunit)  ;;; *prompt* -> *sneps-prompt*  FLJ 9/2/02
	       (setq demo-start-time nil)

	       (setq command (sneps-read))

	       ;; Deal with Explorer's special way to read input
	       ;; Make sure that we are on a fresh line
	       #+explorer(format outunit "~&")
		     
	       ;; Hardwire GC time to 0 until function found.
	       (setq oldtime (or demo-start-time
				 (list (get-internal-run-time) 0)))
	       (protect-eval
		(multiple-value-setq (result result-context)
		  (topsneval command)))
	       (setq newtime (list (get-internal-run-time) 0))
	       
	       (when *sneps-exit-flag*
		 (snepsreadoff)
		 (return-from sneps-loop t))
	       (let ((crntct (or result-context
				 (getcontext (new.ns)))))
		 (declare (special crntct))
		 (format outunit "~&~%")
		 (sneps-print result))
	       (set.sv 'lastcommand command)
	       (sneps-timer oldtime newtime))))
	(sneps-recover))
      (when *sneps-exit-flag* (return t)))))

; ==========================================================================
;
; sneps-print
; -----------
;
;       arguments     : set - <set>
;
;       returns       : nil
;
;       description   : It prints the <set> "set" appending a "!" to all the
;                       members of the <set> which are <assertion>s.
;
;       side-effects  : It prints the <set>
;
;                                          written  : ejm 01/??/84
;                                          modified : scs 10/29/87
;                                                      hc 07/07/93
;                                                     aec 01/22/96

(defun sneps-print (set &optional (stream outunit))
  (cond ((null set) nil)
	((isnew.ns set) (princ "()" stream))
	(t (if (atom set) (setq set (list set)))
	   (princ
	    (mapcar #'(lambda (el)
			(cond ((is.n el) (describe.n el))
			      (t el)))
		    set)
	    stream))))

;==========================================================================
;
; sneps-timer 
; -----------
;
;      arguments     : oldtime - <ptime>
;                      newtime -  <ptime>
;                      stream - <stream>
;
;      returns       : <not-important>
;
;      description   : Compute the CPU time and GC time used from oldtime
;                      to newtime and print them onto outunit.
;
;      side-effects  : Print out the computed CPU time and GC time.
;
;                                         written:  CCC 08/02/83
;                                         modified: ssc 06/19/87
;                                         modified: scs 01/04/88
;                                                   hc  07/07/93

(defun sneps-timer (oldtime newtime &optional (stream outunit))
  (let ((cpu_time (/ (float (cl:- (car newtime) (car oldtime)))
		     internal-time-units-per-second))
        (real_time  (/ (float (cl:- (cadr newtime) (cadr oldtime)))
		     internal-time-units-per-second)))
    (declare (ignore gc_time))
    ;;Until we know how to get GC time it is useless to always print 0:
    ;;(format stream "~&~% CPU time : ~,15f     Real time : ~,15f ~2%"
           ;; cpu_time real_time)
    (format stream "~&~% CPU time : ~,2f ~2%" cpu_time)
    
    ))


; =========================================================================
;
; sneps-error
; -----------
;                                         modified: hc 07/07/93
;                                                   scs/flj 02/12/04
;
(defun sneps-error (msg module fn)
   (cond ((y-or-n-p
	  "SNePS ERROR: ~A~%Occurred in module ~A in function ~A~
            ~2%Do you want to debug it? "
	  msg module fn)
	 (break "SNePS ERROR: ~A" msg)))
   (set.sv 'errorcommand (value.sv 'lastcommand))
   (throw 'sneps-error nil))

;
; ==========================================================================
;

(defsnepscom ^ ((lisp-form) (top ns bns tbns fns rs))
  (values (eval lisp-form) (value.sv 'defaultct)))


; ==========================================================================
;
;  topsneval
;  ---------
;
;       arguments     : snepsul-form - <snepsul-form>
;
;       returns       : <snepsset> or nothing
;
;       description   :    This is the top evaluator of SNePS-2.0.
;                          It remembers "snepsul-form" -- the <snepsul-form>
;                       typed by the user at the top level of SNePS-2.0 --
;                       in the <svar> "command" to enable the user to edit it
;                       in case of error.
;                          Then it evaluates it:
;                       . If "snepsul-form" is a call to a <topcommand>,
;                       it calls the lisp evaluator with it.
;                       . If it is a <node set>, or a <node> it returns the
;                       appropriate <node set>. 
;                       . If it is one of the <rearrange command>s, 
;                       it calls the function "rearrang-command" to handle it.
;                       . If it is the symbol "^" or "^^" 
;                       it calls "pseudolisp1" or "pseudolisp" respectively, 
;                       to enter in some pseudo-lisp  environment 
;                       (see comments of these functions); 
;                       . otherwise, it prints an error message.
;                          If there was no error during the evaluation of 
;                       "snepsul-form" and if it has a value, it remembers 
;                       the value in the <svar> "lastvalue".
;
;       nonlocal-vars : lastvalue - <svar> - the last returned value  
;                       command   - <svar> - the current <snepsul-form>  
;
;       side-effects  : Whatever side-effects are caused by evaluating 
;                       the sneps <command>s.
;                       It side effects the <svar>s "lastvalue" and "command"
;
;                                          written:  CCC mm/dd/yy
;                                          modified: ejm 10/24/83, 02/13/84,
;                                                        05/30/84
;                                          modified: scs 06/08/87
;                                                    njm  4/27/89
;
;
(defun topsneval (snepsul-form)
  (set.sv 'command snepsul-form)
  (catch 'sneps-error 
    (cond ((isnew.ns snepsul-form)
	   (set.sv 'lastvalue (new.ns)))
	  ((listp snepsul-form) (topsneval-seq snepsul-form))
	  (t (topsneval-atom snepsul-form)))))

(defun topsneval-atom (snepsul-form)
    (cond ((isrearrange.com snepsul-form)
           (set.sv 'lastvalue
                   (nseval (rearrange-command snepsul-form))))
          ((numberp snepsul-form) (topsneval-atom (un-ize snepsul-form)))
	  ((stringp snepsul-form)
	   (topsneval-atom (string-to-symbol snepsul-form)))
          ((node snepsul-form)
           (set.sv 'lastvalue (makeone.ns (node snepsul-form))))
          ((null snepsul-form) (values))
          ((eq snepsul-form '^) (pseudolisp1))
          ((eq snepsul-form '^^) (pseudolisp))
          (t (sneps-error (format nil "Invalid top SNePSUL form: ~S" snepsul-form) 
			  'top-evaluator
			  'top-sneval))))

(defun topsneval-seq (snepsul-form)
  (let (result result-context)
    (cond ((istop.com (first snepsul-form))
	   (multiple-value-setq (result result-context)
	     (eval snepsul-form))
           (set.sv 'lastvalue result)
	   (values result result-context))
          ((is.nas snepsul-form)
           (set.sv 'lastvalue (nas-to-ns snepsul-form)))
	  ((stringp (first snepsul-form))
	   (topsneval-seq (mapcar #'string-to-symbol snepsul-form)))
          (t (sneps-error (format nil "Invalid top SNePSUL form: ~S" snepsul-form)
			  'top-evaluator
			  'topsneval)))))

;==========================================================================
;
; nseval 
; ------
;
;      arguments     : snepsul-exp - <snepsul-exp>
;
;      returns       : <node set>
;
;      nonlocal-vars : snfuncs
;
;      description   : Sneps evaluate the given snepsul expression.
;                      If the snepsul expression begins with a sneps
;                      function name, then evaluate that function with the
;                      remaining list elements as arguments for
;                      that function. Otherwise, try to sneps-evaluate
;                      the elements of the snepsul expression one by one.
;
;      side-effects  : Whatever side-effects are caused by evaluating the
;                      sneps functions.
;
;                                         written:  CCC 08/02/83
;                                         modified: SCS 06/08/87
;                                         modified: ??? 02/22/89
(defun nseval (snepsul-exp)
  (declare (special snepsul-exp)) 
  (let ((result
	  (cond ((null snepsul-exp) nil)
		((atom snepsul-exp) (nseval-atom snepsul-exp))
		((isns.com (first snepsul-exp)) (protect-eval (eval snepsul-exp)))
		((eq (second snepsul-exp) '!)
		 (nseval `((! ,(first snepsul-exp)) ,@(cddr snepsul-exp))))
		((isrearrange.com (second snepsul-exp))
		 (cond ((third snepsul-exp)
			(nseval `((,(second snepsul-exp) ,(first snepsul-exp)
				   ,(third snepsul-exp)) ,@(cdddr snepsul-exp))))
		       (t (sneps-error (format nil
					       "Infix operator missing second operand:: ~A"
					       (cdr snepsul-exp))
				       '|evaluation of node description|
				       'nseval))))
		(t (nseval-map snepsul-exp)))))
    (if (listp result) result (makeone.ns result))))

(defun nseval-atom (snepsul-exp)
    (declare (special snepsul-exp))
    (cond ((numberp snepsul-exp) (nseval (un-ize snepsul-exp)))
	  ((stringp snepsul-exp)
	   (nseval (string-to-symbol snepsul-exp)))
          ((is.n snepsul-exp) (makeone.ns snepsul-exp))
	  ((is.ct snepsul-exp) (makeone.cts snepsul-exp))
	  ((node snepsul-exp) (makeone.ns (node snepsul-exp)))
          (t (sneps-error (format nil " Unknown node: ~A" snepsul-exp)
			  'nodeset-evaluator
			  'nseval-atom))))

(defun nseval-map ( snepsul-exp)
  (declare (special result))
  (setq result (new.ns))
  (dolist (exp snepsul-exp result)
    (setq result (union.ns result (nseval exp)))))


(defmacro get-default-stream (&rest stream-variables)
  "Takes a list of STREAM-VARIABLES and returns the value of the first
one that is a bound to a stream."
  `(eval (find-if #'(lambda (stream-variable)
		      (and (boundp stream-variable)
			   (streamp (eval stream-variable))))
	  ',stream-variables)))
  
; ==========================================================================
;
; pseudolisp
; ----------
;
;       arguments     : none
;
;       returns       : <new nodeset>
;
;       description   : This function is invoked by typing a "^^" at the top
;                       level of SNePS and simulates a pseudo-lisp 
;                       environment where a sequence of lisp forms can be 
;                       evaluated one at time until the form "^^" is read, 
;                       at which time control is returned to "sneps".
;
;       side-effects  : Whatever side effects are caused by the evaluation 
;                       of the lisp forms.
;
;       note          : Notice that forms are read using the lisp read 
;                       table, not the sneps read table.
;
;                                          written : ejm 10/25/83
;                                          modified: scs 03/04/87
;                                          modified: scs 11/10/87
;                                                    hc  04/03/90
;                                                    hc  11/20/91
;
(defun pseudolisp (&optional (prompt "--> ")
		             (exit-commands '("^^" "continue" "resume")))
  (declare (special inunit outunit))
  (let (;; Protect SNePS user from in-package side-effects
	(*package* *package*))
    ;; Reevaluate streams at any iterations because they might change
    ;; in the course of a demo. Don't bind them here because resetting
    ;; does not work across multiple levels. Don't assume inunit to be
    ;; bound because pseudolisp is also used in the parser.
    (do* ((in (get-default-stream inunit *standard-input*)
	      (get-default-stream inunit *standard-input*))
	  (out (get-default-stream outunit *standard-output*)
	       (get-default-stream outunit *standard-output*))
	  ;; Do this just once at the beginning:
	  (start (format out  "~%~a" prompt) start)
	  (fm (pseudolisp-read in) (pseudolisp-read in)))
	 ((and (symbolp fm)
	       (member (symbol-name fm) exit-commands
		       :test #'string-equal))
	  ;; Do this at the very end:
	  (terpri out))
      (pseudolisp-printeval fm)
      (princ prompt out))
    (values)))

; ==========================================================================
;
; pseudolisp1
; -----------
;
;       arguments     : none
;
;       returns       : <new nodeset>
;
;       description   : This function is invoked by typing "^" at the top
;                       level of SNePS and simulates a pseudo-lisp 
;                       environment where a single lisp form can be 
;                       entered and evaluated, after which control is 
;                       returned to "sneps".
;       
;       side-effects  : Whatever side effects are caused by the evaluation 
;                       of the lisp form.
;
;       note          : Notice that the form is read using the lisp read 
;                       table, not the sneps read table.
;
;                                          written : ejm 10/23/83
;                                          modified: scs 03/04/87
;                                          modified: scs 11/10/87
;                                                     hc 11/20/91
;
(defun pseudolisp1 (&optional (prompt "--> "))
  (declare (special inunit outunit))
  (let ((*package* *package*)
	(in (get-default-stream inunit *standard-input*))
	(out(get-default-stream outunit *standard-output*)))
    (format out "~%~a" prompt)
    (pseudolisp-printeval (pseudolisp-read in))
    (terpri out)
    (values)))


; ==========================================================================
;
; pseudolisp-printeval
; --------------------
;
;       arguments     : form - <lisp form>
;
;       returns       : nil 
;
;       description   : It evaluates the "form" and prints its value.
;
;       side-effects  : Whatever side-effects are caused by the evaluation 
;                       of the "form".
;
;                                          written : ejm 10/24/83
;                                          modified: hc  04/03/90
;                                                    hc  11/20/91
(defun pseudolisp-printeval (form)
  (declare (special outunit))
  (format (get-default-stream outunit *standard-output*)
	  "~{~&~s~}~%"
	  (multiple-value-list
	      (eval form))))


;==========================================================================
;
; lispeval 
; --------
;
;      arguments     : lisp-forms - (<lisp-form> ... <lisp-form>)
;
;      returns       : <node set>
;
;      description   : It evaluates the lisp-forms and returns a <sequence>
;                      of the results which will be then "snevaluated".
;
;      side-effects  : Whatever side-effects are caused by evaluating the 
;                      lisp-forms.
;          
;
;                                         written:  CCC 08/02/83
;                                         modified: hc  07/18/93

(defsnepscom lispeval ((&rest lisp-forms) ^)
  (mapcar #'eval lisp-forms))


;==========================================================================
;
; lisp 
; ----
;
;      arguments     : none 
;
;      returns       : <boolean>
;
;      nonlocal-vars : *sneps-exit-flag* - <boolean>
;
;      description   : Set the *sneps-exit-flag* to true.
;
;      side-effects  : When the sneps top function sees that the 
;                      *sneps-exit-flag* is true, it will exit from the 
;                      read-eval-print loop.
;
;                                         written:  CCC 08/03/83
;                                         modified: hc  07/18/93
;                                                   hi  03/31/99
(defsnepscom lisp (())
  (setq *sneps-exit-flag* t))

(defsnepscom clear-infer-all (())
  "Clears SNIP by removing all node activations."
  (do.ns (node (value.sv 'nodes))
    (deactivate.n node))
  ;; Unbind any stray multi processes (e.g., user processes)
;;; comment out for new implementation of processes
;;;  (do-symbols (multi-symbol (find-package 'multi))
;;;    (when (multi:is-process-name multi-symbol)
;;;      (makunbound multi-symbol)))
  "All node activations removed.")

(defsnepscom clear-infer (())
  "Modified not to purge some expertise information.
   For rule nodes, information in *KNOWN-INSTANCES* is retained."
  ;; Changed so that clear-infer doesn't deactivate any act nodes.
  ;; Deactivating act nodes was crashing SNeRE
  ;;    when some act nodes were being scheduled multiple times
  ;;    by some complicated agents. -- scs 10/19/04
  (prog1
      (let (node-process type known-instances)
	(do.ns (nde (value.sv 'nodes)
		    "Node activation cleared. Some register information retained.")
	       ;;Don't deactivate act nodes if not DONE! added hi 3/31/99
	       (setq node-process (activation.n nde))
	       (when node-process
		 (setq type (multi::regfetch node-process 'snip::*name*))
		 (cond ((member type '(snip::rule snip::num-quant.rule))
			(setq known-instances
			  (multi::regfetch node-process
					   'snip::*known-instances*))
			(setf (node-activation nde) nil)
			(setq node-process (activate.n nde))
			(multi::regstore node-process
					 'snip::*known-instances*
					 known-instances))
		       ((sneps:is-act.n nde)
			;; Don't deactivate any act nodes.  scs 10/19/2004
			)
		       (t (deactivate.n nde))))))
    (gc-nodes)))

#+allegro
(adjust-for-acl6 :after)
