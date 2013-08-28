;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                         State University of New York

;; Version: $Id: demo.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


;;  demo
;;  ----
;; 
;;         arguments     : &optional file  <filename>
;;                         &optional pause
;;                             one of: (T b before pause bv before-verbose
;;				        a after av after-verbose n NIL)
;; 
;;         returns       : <nothing>
;; 
;;         description   : User function to be called from SNePS.
;;                         It opens FILE  and changes the source
;;                         of the input stream to FILE. All the input
;;                         will be echoed to the terminal. Reading is
;;                         done similar to lisp reading, i.e., comments
;;                         can be put at all places. Depending on PAUSE
;;                         it pauses before reading a SNePSUL form,
;;                         after it has read it but not executed it, or
;;                         does not pause at all (NIL). The verbose forms
;;                         additionally print a pausing message.
;;
;;                         If FILE is a non-existent single character
;;                         filename a menu with available demonstrations
;;                         will be presented (the pity is we can't use
;;                         ? for this purpose because this is a read macro
;;                         that wants to consume the next argument, of course,
;;                         \? works)
;;
;;                         If demo is called without any arguments, a menu
;;                         with available demonstrations will be presented
;;                         and initial pausing will be av (which can be changed
;;                         later at the pause prompt).
;; 
;;         side-effects  : It changes the source of input and it prints
;;                         messages at the beginning and at the end.
;; 
;;                                         written:  jgn 08/07/83
;;                                         modified: ejm 06/01/84
;;                                         modified: scs 11/20/87
;;                                                    hc 11/20/91
;;                                                    hc 07/18/93
;;
(defsnepscom demo ((&optional (file 'x file-supplied-p)
			      (pause nil pause-supplied-p)
			      &rest ignore))
  (declare (ignore ignore))
  (cond (file-supplied-p
	 (if pause-supplied-p
	     (demo-internal file :pause pause)
	   (demo-internal file)))
	(t (demo-internal file :pause 'av)))
  (demo-set input-filter #'demo-input-filter)
  (values))


;;; New changes to demo-internal made allow for the specification of
;;; an output stream. If batach-assert keyword is true the nodes are
;;; asserted into the current context after the demo finishes. -mwk3 6/4/2007 

(defparameter *out-stack* nil
  "Stack of outunit values. Used for nested demos.")

(defparameter *newPropositions* nil
  "Holds a list of propositions to be batch asserted.")

(defparameter *assertLater* 0
  "If greater than 0, assert-nodes will push nodes onto *newPropositions*
       instead of asserting them.")

(defun demo-internal (file &key (pause nil pause-supplied-p)
				(output-stream nil out-supplied-p)
				(use-batch-assert nil)
				(menu (when (initialized-p
					     *available-sneps-demos-menu*)
					*available-sneps-demos-menu*)))
  "Does the main work as described for DEMO. Putting this into a separate
function makes it easier available to other packages such as SNePSLOG (e.g.,
the menu handling couldn't have been easily supported by the DEMO
macro itself). "
  (declare (special outunit))
  (let ((pause (if pause-supplied-p
		   pause
		   ;; Copy pause from outer demo invocation
		   ;; if such a thing exists, i.e., in recursive demos
		   ;; use same pause control as in frame demo unless
		   ;; otherwise specified
		   (demo-get pause)))
	real-file)
    (when use-batch-assert
      (incf *assertLater*))
    (when (if out-supplied-p
	      (progn
		(push outunit *out-stack*)
		(setf outunit output-stream)
		(demo-start file outunit pause menu))
	    (progn
	      (push outunit *out-stack*)
	      (demo-start file outunit pause menu)))
      (setq real-file (truename (demo-get input-stream)))
      (format outunit "~&~%File ~A is now the source of input.~%" real-file)
      (finish-output outunit)
      (when pause
	(format outunit
		"~%  The demo will pause between commands, at that time press~
               ~%  RETURN to continue, or ? to see a list ~
                   of available commands~%")
	(finish-output outunit))
      (demo-set exit-function
		#'(lambda ()
		    (declare (special demo-start-time))
		    (format outunit "~&~%End of ~A demonstration." real-file)
		    (finish-output outunit)
		    (when *out-stack*
		      (setf outunit (pop *out-stack*)))
		    (when use-batch-assert
		      (batchAssert *newPropositions*)
		      (setf *newPropositions* nil)
		      (decf *assertLater*))
		    (setq demo-start-time (demo-get start-time)))))
    (values)))




(defun demo-input-filter (input)
  #+explorer
  ;; When we read from a demo file we have to account for the Explorer's
  ;; special way to read input. Because their read function does not
  ;; consume the #\newline at the end of an s-expression it does not get
  ;; echoed at the end of the read, but rather at the beginning of the
  ;; next read. This leads to the problem that we always get a newline
  ;; at the prompt. To avoid that we check for that case in the input
  ;; filter and read off the newline if necessary.
  (let ((input-stream (demo-get input-stream))
	(demo-stream (demo-get demo-stream)))
    ;; We only have to do this when we read a list
    (and (consp input)
	 ;; If the immediate character after a list is
	 ;; a newline lets read it off and echo it (of
	 ;; course, if there was some whitespace before
	 ;; the newline this will not work)
	 (eql (peek-char nil input-stream nil :eof)
	      #\newline)
	 (read-char demo-stream)))
  input)



    
    




