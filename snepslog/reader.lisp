;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: reader.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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




(in-package :snepslog)


;; Changes (hc Feb-27-90, Mar-12-90)
;;  - Introduce variable line.terminator to allow adaption to
;;    various lisp implementations
;;  - add clause (escape.char? read.char) to read-string
;;    to allow continuation lines (they already were possible
;;    by doing ;\  )
;;  - make comment reading more intelligent (allow empty lines,
;;    expressions starting with comments, inside comments...)
;;  - allow aligned printing of read expressions (good for demo).
;;  - don't close on eof in snepslog-read anymore because demo
;;    uses with-open-file now.
;;  - Change read-string to remember and return position of first character
;;    of a real command (i.e., not whitespace or comment)
;;  - Change snepslog-read, snepslog-read-from-string(list) to take account
;;    of start.of.real.input, so a proper input substring can be given
;;    to execute-snepslog.command to allow use of proper readtables in
;;    ^ and % commands.


;
; the definition of the snepslog read table
;

(let* ((original-read-table *readtable*)
       (snepslog-read-table (copy-readtable nil))
       (*readtable* snepslog-read-table))
      (set-macro-character #\, #'(lambda (s c) (declare (ignore s c)) (quote snepsul:\,)))
      (set-macro-character #\~ #'(lambda (s c) (declare (ignore s c)) (quote snepsul:\~)))
      (set-macro-character #\. #'(lambda (s c) (declare (ignore s c)) (quote snepsul:\.)))
      (set-macro-character #\: #'(lambda (s c) (declare (ignore s c)) (quote snepsul:\:)))
      (set-macro-character #\{ #'(lambda (s c) (declare (ignore s c)) (quote snepsul:\{)))
      (set-macro-character #\} #'(lambda (s c) (declare (ignore s c)) (quote snepsul:\})))
      
      (defun snepslogreadon ()
	"Sets the readtable to the snepslog read table"
	(setq *readtable* snepslog-read-table))
      
      (defun snepslogreadoff ()
	"Sets  the readtable to the original readtable (a copy of the initial readtable)"
	(setq *readtable* original-read-table)))

(defvar line.terminator #\newline
  "Line termination character used by particular Lisp implementation")

(defvar write.read.expression? nil
  "This variable is used to decide if the read input is to be re-written 
(it is supposed to be re-written when the input is beeing read from some 
stream other than  *standard-input*).")

(defvar write.aligned? t
  "If this variable is true each continuation line of an expression echoed
during a demo will be preceded with an appropriate number of blanks to 
balance with the prompt")

(let ((string ())       ;the last string read from stream. Used only by local function next.char.in
      index
      length
      (old.stream ()))
  (defun next.char.in (stream)
    (unless (and (eq stream old.stream)
		 (stringp string))
      (setf string (read-line stream nil nil)
	    old.stream stream)
      (cond ((stringp string)
	     (setf index 0
		   length (length string))
	     (when (string= string "")
	       (setf string ())
	       (return-from next.char.in (next.char.in stream))))
	    (t (return-from next.char.in (values nil t)))))
    (cond ((> index length)	   
	   (setf string ())
	   (next.char.in stream))
	  ((cl:= length index)
	   (incf index)
	   (values line.terminator nil))
	  ((and (cl:=  index (1- length))
		(eql (char string index)
		     #\\))
	   (setf string nil)
	   (next.char.in stream))
	  (t (prog1 (values (char string index) nil)
		    (incf index)))))
  (defun put.char.back (stream)
    (and (eq stream old.stream)
	 (plusp index)
	 (decf index)))
  (defun colapse.reader () (setf string nil)))

(defun read-string (&optional (stream *standard-input*)
			      (eof-error-p t)
			      (eof-warning-p ())
			      (eof-value ()))
  (let (;; a lists of chars that will be coerced to a string
	(result nil)
	;; the last character read
	read.char
	;; t if eof was found when reading read.char
	eof?
	(stack ())
	;; t as soon as any non-comment/non-whitespace input is encountered
	real.input?
	;; position of first real input character
	(start.of.real.input 0))
    (loop
     (multiple-value-setq (read.char eof?)
       (next.char.in stream))
     (when eof?
       (when (and eof-error-p result)
	 (error "Error. Eof found before expected"))
       (when (and eof-warning-p result)
	 (format outunit "Warning: Eof Unexpected"))
       (return-from read-string (values eof-value t start.of.real.input)))
	
     (cond ((eql read.char (first stack))
	    (pop stack)
	    (push read.char result))
	   ((sequence.begginer? read.char)
	    (or real.input? (setq start.of.real.input (length result)))
	    (setq real.input? t)
	    (push (end.of.sequence.begining.with read.char) stack)
	    (push read.char result))
	   ((sequence.ender? read.char)
	    (or real.input? (setq start.of.real.input (length result)))
	    (setq real.input? t)
	    (colapse.reader)
	    (sneps-error "Error in snepslog read. Unexpected eof of sequence found"
			 'snepslog
			 'read))
	   ((comment.char? read.char)
	    (let ((inside.expression? stack))
	      (loop
	       (cond ((comment.ender? read.char)
		      (cond ((or (not real.input?)
				 inside.expression?)
          		     (push read.char result)
			     :keep-reading)
			    (t (put.char.back stream)
			       :terminate-reading))
		      (return))
		     ((escape.char? read.char)
		      (push (setf read.char (next.char.in stream)) result))
		     (t (push read.char result)))
	       (setf read.char (next.char.in stream)))))
	   ((escape.char? read.char)
	    (push (setf read.char (next.char.in stream)) result))
	   ((stringer.char? read.char)
	    (or real.input? (setq start.of.real.input (length result)))
	    (setq real.input? t)
	    (push read.char result)
	    (loop (push (setf read.char (next.char.in stream)) result)
		  (when (stringer.char? read.char)
		    (return))))
	   ((and (eql read.char line.terminator)
		 real.input?
		 (null stack))
	    (push read.char result)
	    (return-from read-string (values (coerce (nreverse result)
						     'string)
					     nil
					     start.of.real.input)))
	   (t (unless (whitespace.char? read.char)
                (or real.input? (setq start.of.real.input (length result)))
		(setq real.input? t))
	      (push read.char result))))))


(defun comment.ender? (char &rest comment.char)
  ;; Previously, this function only checked for end-of-line.
  ;;    If a comment ended with a "-" followed by eof,
  ;;    an infinite loop resulted.
  "Returns t if char marks the end of a comment:
   either end-of-file or end-of-line;
   returns nil otherwise."
  (declare (ignore comment.char))
  (or (null char)			; if eof has been reached
      (eql char line.terminator)))

(defun comment.char? (char)
  (eql char #\;))

(defun whitespace.char? (char)
  (member char `(#\space ,line.terminator #\tab) :test #'char-equal))

(defun escape.char? (char)
  (eql char #\\))

(defun stringer.char? (char)
  (eql char #\"))

(defun sequence.ender? (char)
  (member char '(#\) #\})))

(defun sequence.begginer? (char)
  (member char '(#\( #\{)))

(defun end.of.sequence.begining.with (char)
  (case char
    (#\( #\))
    (#\{ #\})))


(defun snepslog-read (&optional (stream inunit))
  "Snepslog reader.
     Reads a Snepslog expression
        and returns the Snepsul correspondent expression
        or, if the read snepslog expression is a Snepslog command,
               returns the command."
  ;; Previously all curly brackets were changed to parentheses.
  ;; This version does not do that.
  (declare (special line.terminator write.read.expression? write.aligned?))
  (let ((*package* (find-package :snepslog)))
    (multiple-value-bind (snepslog.string eof? start.of.real.input)
	(read-string stream nil t)
      (when eof? (return-from snepslog-read (values nil nil)))
      (when write.read.expression?
	(format outunit "~a" (cond (write.aligned?
				    (make.aligned.expression snepslog.string))
				   (t snepslog.string))))
      (values
       (snepslog-read-from-string snepslog.string start.of.real.input)
       eof?))))

(defun make.aligned.expression (string)
  (with-output-to-string (s)
    (map nil #'(lambda (char)
		 (cond ((char-equal char line.terminator)
			(format s "~%  "))
		       (t (format s "~a" char))))
	 string)))

(defun snepslog-read-from-string (input &optional (start.of.real.input 0))
  "Parses the input string, and returns a parsed expression."
  ;; This is equivalent to snepslog::snepslog-read-from-string
  ;; input ::= snepslogCommand | wffCommand
  (declare (ignore start.of.real.input)) ; was used in the original
  (setf *Input* input)
  (popInput 0)
  (when (not (emptyInput?))
    (catch 'SNePSLOGParseError
      (let ((Expression (or (wffNameCommand)
			    (snepslogCommand)
			    (wffCommand))))
	(cond (Expression
	       (if (emptyInput?)
		   Expression
		 (parseError "Extra text in input: ~S" *Input*)))
	      (t (parseError "Unrecognized input: ~S" *Input*)))))))

(defun snepslog-read-from-list (list &optional command.string)
  (if (snepslog.command? list)
      (execute-snepslog.command list command.string)
      (parser (list list) 'p (the-empty-frame) 0)))

(defun change.brackets! (string)
  (dotimes (i (length string) string)
    (case (char string i)
      (#\{ (setf (char string i) #\())
      (#\} (setf (char string i) #\))))))



    
    




