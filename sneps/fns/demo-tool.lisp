;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: demo-tool.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




;; altered July 1, 2002 for ACL6 (FLJ)

(in-package :sneps)


;; Demo Tool: A package that handles temporary input redirection to read,
;;            print and evaluate input from files within read/eval/print
;;            loops (see comments at macro with-demo-control).
;;
;; Highlights: - Allows recursive demos
;;             - Proper pausing behavior that is consistent across all
;;               demo controlled loops (e.g., sneps, snepslog, parser)
;;             - Allows demo control directives within demo files (e.g.,
;;               pause, set pausing, abort, etc.)
;;             - Menu interface to available demo files
;;             - Easily integratable into existing read/eval/print loops
;;             - Reading from demo files is done with the functions supplied
;;               by the application (e.g., for SNePS reading is done just
;;               like normal Lisp reading, hence comments etc. can be anywhere
;;               they could be in a normal Lisp file)
;;             - Uses an echo stream to echo the read input exactly the way
;;               it appears in the file
;;
;; Author:  Hans Chalupsky
;; Written: Dec, 91


(defvar *demo-original-readfns* nil
  "A-list that holds the original function definitions of read functions
that become demo-controlled. Need this to allow multiple redefinitions of
the same read function in nested with-demo-control's without getting
accumulative control behavior.")

(defvar *demo-pause-string* "~&--- pause ---"
  "String to be displayed for verbose pausing")

(defvar *demo-interactive-io* *query-io*
  "Once in a while during a demo we need to actually interact with the user.
This is the stream on which interactive I/O will be handled.")

(defstruct (dsf (:type list))
  "Describes a demo stack frame which is used to save all the information
necessary to deal with recursive calls to demo."
  (start-time 0)      ; the run time the demo starts.
  (pause nil)         ; describes if and how to pause between commands
  (input-stream nil)  ; The stream from which input is read
  (demo-stream nil)   ; The actual input/echo stream used by demo
  (exit-function nil) ; function to be executed upon termination of a demo

  ;; Because demo has to know whether eof has occured during an execution
  ;; of an application read function, the application has to supply a filter
  ;; function which when applied to the input will return :eof if an
  ;; unexpected eof has occured (e.g., if there was comment at the end of a
  ;; demo file). Needless to say, the application read functions should
  ;; have eof-error-p set to NIL. The input filter will be applied to input
  ;; before eventual demo control is performed, hence the application can
  ;; use the filter function to have its own set of demo control commands
  ;; mapped to actual demo control commands understood by this package.
  (input-filter #'identity)
  
  ;; The next variables are stacks themselves that contain one value
  ;; for every recursive call to with-demo-control
  (input-stream-vars nil)    ; variables bound to demo-stream
  (normal-input-streams nil) ; values of input-stream-vars in nondemo mode
  )

(defvar *demo-stack* nil
  "Variable that holds the demo stack")


(defmacro initialize-demo-stack ()
  `(setq *demo-stack* (list (make-dsf))))

(defmacro make-empty-demo-stack ()
  `(setq *demo-stack* nil))

(defmacro demo-stack-empty-p ()
  `(not (consp *demo-stack*)))

(defmacro demo-save-variables ()
  "Copies the current demo stack frame and pushes it on the stack.
The new frame inherits all the values from the current frame until
they get overwritten."
  `(push (copy-dsf (car *demo-stack*)) *demo-stack*))

(defmacro demo-restore-variables ()
  "Just pops the demo stack"
 `(pop *demo-stack*))

(defmacro dsf-get (slot)
  "Access the value of SLOT in the top demo stack frame"
  `(,(intern 
      (build-namestring  :dsf- slot) 
      :sneps) (car *demo-stack*)))   ;new
  
  
  
(defmacro dsf-set (slot value)
  "Set the value of SLOT in the top demo stack frame to VALUE"
  `(setf (,(intern 
	    (build-namestring  :dsf-  slot) 
	    :sneps) (car *demo-stack*))
     ,value))

(defmacro dsf-push (value slot)
  "Pushes VALUE onto the stack held in SLOT of the top demo stack frame"
  `(dsf-set ,slot (cons ,value (dsf-get ,slot))))

(defmacro dsf-pop (slot)
  "Pops the stack held in SLOT of the top demo stack frame"
  `(dsf-set ,slot (cdr (dsf-get ,slot))))

(defmacro dsf-assoc (key key-slot value-slot)
  "Looks up the position of KEY in the stack held in KEY-SLOT and returns
its corresponding value from the stack held in VALUE-SLOT"
  `(let ((pos (position ,key (dsf-get ,key-slot) :test #'equal)))
    (and pos (nth pos (dsf-get ,value-slot)))))

(defmacro demo-get (slot)
  "This function should be used by applications that use the demo package
to retrieve various values associated with this demo."
  `(dsf-get ,slot))

(defmacro demo-set (slot value)
  "This function should be used by applications that use the demo package
to change various values associated with this demo."
  `(dsf-set ,slot ,value))

(defmacro demo-stack-length ()
  "Returns length of the demo stack"
  `(length *demo-stack*))

(defmacro with-dsf (n &body body)
  "Binds the top of the demo stack to the N'th dsf from the top"
  `(unless (cl:>= ,n (demo-stack-length))
    (let ((*demo-stack* (nthcdr ,n *demo-stack*)))
      ,@body)))

(defmacro do-demo-stack (&body body)
  "Maps over the demo-stack from top to bottom. After every step the top
of the stack gets set to the previous frame. The result of the last
iteration gets returned."
  (let ((result-var (gensym))
	(index-var (gensym)))
    `(let (,result-var)
       (dotimes (,index-var (demo-stack-length) ,result-var)
	 (with-dsf ,index-var (setq ,result-var (progn ,@body)))))))

(defun demo-stream-p (stream)
  "True if STREAM is EQ to the demo-stream slot of some frame
in the demo stack."
  (and (streamp stream)
       (let (found)
	 (do-demo-stack
	     (when (eq stream (dsf-get demo-stream))
	       (setq found t)))
	 found)))

(defun recursive-demo-p ()
  "T if we are in a recursive demo."
  (cl:> (demo-stack-length) 2))


;; The basic idea behind the generic demo package is the following:
;;
;; A normal toplevel loop has a body like this:
;;
;;   print a prompt
;;   read input from INSTREAM via one of a set of read functions
;;   evaluate the read input
;;   print the result to OUTSTREAM
;;
;; INSTREAM and OUTSTREAM are normally bound to terminal I/O.
;; To make the read functions read from a file we have to rebind INSTREAM
;; to a file input stream. One problem is that everything read from
;; the file should get echoed to OUTSTREAM. This can be solved by creating
;; an echo stream with make-echo-stream.
;;
;; A second problem is how to switch back to the original value of INSTREAM
;; once the end of the file has been reached. To solve this we allow the
;; user to define which of the toplevel loop read functions should be
;; ``demo-controlled''. A demo-controlled read function first checks if a
;; demo is in progress. If not it just runs the normal read function.
;; If a demo is in progress and we are not yet at the end of the file
;; some pause/more handling is performed. Then the actual read function
;; is run and the result is returned. If the end of file was reached
;; the file stream gets closed, INSTREAM gets rebound to its old value
;; and a previously specified eof-value is returned from the read function.
;; Additionally, we run some user supplied demo-exit code to do things
;; necessary at the end of a demo.
;;
;; A third problem is control of the output during a demo, such as proper
;; pausing, quitting to the toplevel etc. This can also be handled within
;; a demo-controlled read function. Furthermore, this allows for consistent
;; pausing behavior over all demo-controlled loops.
;;
;; All of the above can be specified in the following macro which is supposed
;; to be wrapped around a toplevel loop:

(defmacro with-demo-control ((instream read-functions)
			     &body body)
  "Demo-control macro that is supposed to be wrapped around toplevel
read/eval/print loops that should be able to read input from demo files.
INSTREAM is the name of the variable which holds the input stream. All
demo relevant reading must be done via this variable. READ-FUNCTIONS is a
list of demo-control specifications. A demo-control specification is
either a symbol naming a read function, or a list with the read function
as its first element, and the s-exp that should get returned if EOF was
reached when the read function was called as the second element."
  (let ((outermost-invocation-var (gensym))
	(function-old-new-list-var (gensym)))
    `(let (;; Record whether this is the outermost
	   ;; invocation of with-demo-control
	   (,outermost-invocation-var (demo-stack-empty-p))
	   (,function-old-new-list-var
	    ;; temporarily rebind functions, memorize their old definitions
	    (mapcar #'(lambda (read-function)
			(let ((readfn (if (consp read-function)
					  (first read-function)
					  read-function))
			      (eof-value (if (listp read-function)
					     (second read-function)
					     nil)))
			  ;; Remember the virginal definitions in a global
			  ;; variable. These definitions have to be available
			  ;; in case of nested calls to with-demo-control to
			  ;; avoid accumulating modifications
			  (pushnew (list readfn (symbol-function readfn))
				   *demo-original-readfns*
				   :test #'(lambda (x y)(eq (car x)(car y))))
			  (list readfn;; the function name
				;; the actual definition
				(symbol-function readfn)
				;; the new definition, it's only interpreted
				;; but it is small
				(eval
				 `#'(lambda (&rest readfnargs)
				      (demo-controlled-read-function
				       ',(second
					  (assoc readfn
						 *demo-original-readfns*))
				       ',eof-value readfnargs))))))
		    ',read-functions))
	   )
      (unwind-protect
	   (progn
	     (when (demo-stack-empty-p)
	       (initialize-demo-stack))
	     ;; Record with-demo-control relevant stuff:
	     ;; First let's find out what the normal value of INSTREAM
	     ;; is for this instance of with-demo-control:
	     (let ((normal-input-stream
		    (cond ((and (demo-stream-p (dsf-get demo-stream))
				(eq ,instream (dsf-get demo-stream)))
			   (dsf-assoc ',instream input-stream-vars
				      normal-input-streams))
			  (t ,instream))))
	       ;; Push these values over the whole demo-stack, because we
	       ;; never know with what dsf we will wind up with when this
	       ;; invocation of with-demo-control exits
	       (do-demo-stack
		 (dsf-push normal-input-stream normal-input-streams)
		 (dsf-push ',instream input-stream-vars)))
	     ;; Redefine functions to demo-controlled mode
	     (dolist (function-old-new ,function-old-new-list-var)
	       (redefine-function (first function-old-new)
				  (third function-old-new)))
	     ;; now execute the actual demo-controlled loop body
	     ,@body)
	(unwind-protect
	     ;; Backdefine functions to previous definition
	     (dolist (function-old-new ,function-old-new-list-var)
	       (redefine-function (first function-old-new)
				  (second function-old-new)))
	  ;; pop the stacks for this instance of with-demo-control
	  (dsf-pop input-stream-vars)
	  (dsf-pop normal-input-streams)
	  ;; If this is the outermost with-demo-control map over the
	  ;; stack and close all input-streams in case something went wrong
	  (when ,outermost-invocation-var
	    (do-demo-stack
		(demo-close (dsf-get input-stream) (dsf-get demo-stream)))
	    ;; Reset these in case we redefine them, otherwise the old
	    ;; definitions will be used all over again.
	    (setq *demo-original-readfns* nil)
	    (make-empty-demo-stack)))
	))))

(defun demo-at-eof (input-stream echo-stream)
  "Returns nonNIL if ECHO-STREAM which is assumed to read from 
INPUT-STREAM is at eof. This function might have to vary for
different Lisps. By no means is it allowed to accidentally
echo something."
  (declare (ignore echo-stream))
  ;; For now let's assume that the input-stream gets
  ;; advanced synchronously with the echo stream
  ;;(not (eq (listen input-stream) t))
  ;;(not (listen input-stream))
  (eq (peek-char nil input-stream nil :eof) :eof)
  )

(defun demo-controlled-read-function (read-function eof-value arguments)
  "This is the actual function that creates demo-controlled behavior of
READ-FUNCTION. Within with-demo-control the definition of each controlled
read-function gets replaced by
     (lambda (&rest args) (demo-controlled-read-function <readfn> <eof> args))
which should work for read functions with all kinds of lambda lists.
If the end of file was reached on entry to this function EOF-VALUE will
be returned (after doing all kind of exit-demo handling - see above).
Otherwise the original READ-FUNCTION will be applied to ARGUMENTS."
  (let ((instream (eval (car (dsf-get input-stream-vars)))))
    (cond ((demo-stream-p instream)
	   (cond (;; Check for end-of-file. Don't use function isendoffile
		  ;; here because peek-char echoes the peeked at character
		  ;; for echo streams on some lisps (clarified in CLtL-II).
		  (demo-at-eof (dsf-get input-stream)
			       (dsf-get demo-stream))
                  ;; Reset streams and demo variables
		  (demo-end 1)
		  ;; Return the specified eof-value for this function
		  eof-value)
		 (t ;; Do pause handling before the actual read
		    (when (eq (demo-pause :before) :exit)
		      (return-from demo-controlled-read-function eof-value))
		    ;; Now apply the actual read function and save its values
		    ;; There is no way to do this without consing if we do no
		    ;; know how many values the readfunction might produce
		    (let* ((read-function-values
			    (multiple-value-list
				(apply read-function arguments)))
			   (input (first read-function-values))
			   ;; Perform demo control 
			   (status (demo-control
				    (funcall (dsf-get input-filter)
					     input))))
		      (case status
			;; In this case act as if we hadn't read anything yet
			;; and perform another read - i.e., don't return
			;; the demo control command as actual input
			(:DONE (return-from demo-controlled-read-function
				 (demo-controlled-read-function
				  read-function eof-value arguments)))
			;; Otherwise act as if we had encountered eof
			(:EXIT (return-from demo-controlled-read-function
				 eof-value)))
		      ;; Check whether an eof has occured during the read
		      (when (and (eq (funcall (dsf-get input-filter) input)
				     :eof)
				 (demo-at-eof (dsf-get input-stream)
					      (dsf-get demo-stream)))
			;; Perform EOF handling by a recursive call
			(return-from demo-controlled-read-function
			  (demo-controlled-read-function
			   read-function eof-value arguments)))
		      ;; Do pause handling after the actual read
		      (when (eq (demo-pause :after) :exit)
			(return-from demo-controlled-read-function eof-value))
		      ;; Return the proper values
		      (values-list read-function-values)
		      ))))
	  (t (apply read-function arguments)))))

(defun demo-end (&optional (n 1))
  "Terminate N demos starting from the top of the stack. This closes
files and resets stream variables"
  ;; Make sure there is always one stack frame left
  (dotimes (n (min n (1- (demo-stack-length))))
    (demo-close (dsf-get input-stream) (dsf-get demo-stream))
    (dolist (input-stream-var (dsf-get input-stream-vars))
      (set input-stream-var
	   (cond ((recursive-demo-p)
		  ;; Reset all stream vars to demo-stream
		  ;; of the previous stack frame
		  (with-dsf 1 (dsf-get demo-stream)))
		 ;; Otherwise reset them all to their normal
		 ;; values.
		 (t (dsf-assoc input-stream-var
			       input-stream-vars
			       normal-input-streams)))))
    ;; Execute user supplied exit forms before variables get
    ;; restored so we can still access them.
    (funcall (dsf-get exit-function))
    (demo-restore-variables)))

(defun demo-close (&rest streams)
  ;; Tries to overcome the problems that some Lisps have with closing streams.
  ;; Some have problem with closing a stream twice, some can't close echo
  ;; streams properly (such as CMU Lisp which also closes the streams from
  ;; which the echo stream is derived of - *standard-output* => disaster).
  (let ((processed-streams nil))
    (dolist (stream streams)
      (when (and (streamp stream)
		 (not (member stream processed-streams)))
	(push stream processed-streams)
	;; don't close bidirectional streams:
	(if (not (and (input-stream-p stream)
		      (output-stream-p stream)))
	    (close stream))))))


(defun demo-control (command)
  "Performs a demo control action specified by COMMAND which can be a symbol
in any package or a string, or a list starting with a symbol or a string.
If it is a list the rest of the list is considered to be arguments to the
command. For a list of available commands look at the case statement below.
If COMMAND was a proper command demo-control will return :DONE, if it was a
demo terminating command it will return :EXIT, otherwise NIL is returned."
  (let ((qio *demo-interactive-io*)
	(cmd (intern 
	      (cond ((consp command)(build-namestring (first command)))
		    (t (build-namestring command))) 				
	      :sneps))
	(args (when (consp command) (rest command))))
    (clear-input qio)
    ;; The DC- prefix of command names stands for Demo Control
    (case cmd
      (DC-PAUSE-HELP
       ;; Print help info about possible commands at a pause point
       (format qio
		 "~%The following commands are available at pause points:~
                 ~%  h,?            Print this help message~
                 ~%  l,^            Enter Lisp read/eval/print loop~
                 ~%  s,%            Enter SNePS toplevel loop~
                 ~%  o,:            Enter SNePSLOG~
                 ~%  c              Continue without pausing~
                 ~%  p              Set pause control~
                 ~%  q              Quit this demo~
                 ~%  a              Quit all demos~
                 ~%  any other key  Continue the demo~
                 ~%")
	     :DONE)
      (DC-LISP
       ;; Start Lisp read/eval-print loop
       (let ((inunit qio)
	     (outunit qio))
	 (declare (special inunit outunit))
	 (format qio
		 "~%Enter Lisp Read/Eval/Print loop. Type ^^ to continue~%")
	 (finish-output qio)
	 (pseudolisp))
       :DONE)
      (DC-SNEPS
       ;; Start SNePS toplevel loop
       (let ((inunit qio)
	     (outunit qio))
	 (declare (special inunit outunit))
	 (sneps))
       :DONE)
      (DC-SNEPSLOG (snepslog) :DONE)
      (DC-NO-PAUSE (dsf-set pause nil) :DONE)
      (DC-SET-PAUSE (dsf-set pause (first args)) :DONE)
      (DC-READ-PAUSE
       (format qio "~%Enter new pause control value (b,bv,a,av,n): ")
       (dsf-set pause (read-word-insist '(b bv a av n) qio)) :DONE)
      (finish-output qio)
      (DC-QUIT (demo-end 1) :EXIT)
      (DC-QUIT-ALL (demo-end (demo-stack-length)) :EXIT)
      (t NIL))))

(defun demo-pause (when)
  "Perform proper pausing depending on the current value of WHEN which
indicates whether demo-pause was called before printing the input or after.
If the current value of pause matches WHEN we will pause verbosely by printing
a pausing message, or briefly without printing."
  (let ((pause (dsf-get pause)))
    (when pause
      (setq pause (intern 
		   (build-namestring pause)
		   :sneps))
      (when (and (eq when :before)
		 (member pause '(a after av after-verbose))
		 (return-from demo-pause)))
      (when (and (eq when :after)
		 (member pause '(b before pause t bv before-verbose))
		 (return-from demo-pause)))
      (when (eq pause 'n) (return-from demo-pause))
      ;; Now we know that we have to pause:
      (demo-query (member pause '(bv before-verbose av after-verbose)))
      )))

(defun demo-query (&optional verbose)
  "Creates the actual pause interrupt and handles commands entered
at a pause point. If VERBOSE is non-NIL a pause message will be printed."
  (let ((qio *demo-interactive-io*)
	(up-case nil))
;;; the next section was added by FLJ for clisp compatibility 9/2/02    
    #+(and allegro-version>= (version>= 6 0))	  
    (if (eq excl:*current-case-mode* :case-insensitive-upper)
	(setf up-case t)
      (setf up-case nil))  
    (clear-input qio)
    (when verbose
      (format qio *demo-pause-string*))
    (case (prog1 (read-single-character 
		  qio 
		  :case (cond 
			 (up-case :upper)
			 (t :lower))
		  :package 'sneps)
	    (clear-input qio))
      ((? |H| |h|) (demo-control :DC-PAUSE-HELP)
		   (demo-query verbose))
      ((^ |L| |l|) (demo-control :DC-LISP))
      ((% |S| |s|) (demo-control :DC-SNEPS))
      ((|:| |O| |o|) (demo-control :DC-SNEPSLOG))
      ((|C| |c|)     (demo-control :DC-NO-PAUSE))
      ((|P| |p|)     (demo-control :DC-READ-PAUSE))
      ((|Q| |q|)     (if (let ((*query-io* *demo-interactive-io*))
			   (yes-or-no-p "Do you really want to quit this demo? "))
			 (demo-control :DC-QUIT)))
      ((a |A| |a|)     (if (let ((*query-io* *demo-interactive-io*))
			     (yes-or-no-p "Do you really want to quit all demos? "))
			   (demo-control :DC-QUIT-ALL)))
      (t NIL))))

(defun demo-start (input-stream-spec &optional output-stream pause menu)
  "Sets up a demo reading from a stream specified by INPUT-STREAM-SPEC. If
OUTPUT-STREAM is non-NIL all input will be echoed to it. The value of PAUSE
determines pausing as defined by demo-pause. This function has to be called
by the actual implementation of a demo command of a particular toplevel. Note
that INPUT-STREAM-SPEC does not necessarily have to be a filename, it could
also be a string stream. (see demo-open-input-stream for a description of
valid stream specifications and a description of MENU). If the demo was started
successfully then the demo stream will be returned; NIL otherwise."
  (let ((input-stream (demo-open-input-stream input-stream-spec menu)))
    (when input-stream
      (demo-save-variables)
      (dsf-set pause pause)
      (dsf-set demo-stream
	       (cond (output-stream
		      (make-echo-stream input-stream output-stream))
		     (t input-stream)))
      ;; If we have an echo stream as a demo stream it is not enough to close
      ;; that, but we also have to explicitly close the input stream from which
      ;; it was made, hence we have to store it in the demo stack frame
      (dsf-set input-stream input-stream)
      (dsf-set start-time (list (get-internal-run-time) 0))
      ;; Set input stream variable of the current invocation of
      ;; with-demo-control to the demo-stream
      (set (car (dsf-get input-stream-vars))
	   (dsf-get demo-stream))
      ;; Return the demo stream
      (dsf-get demo-stream))))


(defvar *demo-read-filename-item*
  (make-menu-item :label "Enter a demo filename"
		  :value '(progn (format *demo-interactive-io*
				  "~&Enter a filename: ")
			   (read-filename *demo-interactive-io*)))
  "Menu item that reads in a demo filename")

(defun demo-open-input-stream (input-stream-spec &optional menu)
  "Opens and returns an input stream as specified by INPUT-STREAM-SPEC. If
INPUT-STREAM-SPEC already is an open input stream that stream is returned.
If INPUT-STREAM-SPEC is a filename of an existing file that file will be
opened. If INPUT-STREAM-SPEC is a symbol or string of length 1 it will
be interpreted as a wildcard and a list of available demo files as specified
by MENU will be presented to the user. If INPUT-STREAM-SPEC is a number or
a symbol/string that represents a number it will be used as an index into
MENU and the corresponding item will be chosen. Once a valid file has been
chosen that file will be opened and returned. In case that INPUT-STREAM-SPEC
is a non-existent file a similar procedure will be followed. If the user quits
the menu by entering q NIL will be returned."
  (cond (;; If we already are given a proper input stream
	 ;; just return it
	 (and (streamp input-stream-spec)
	      (input-stream-p input-stream-spec))
	 input-stream-spec)
	(;; If it is the name of an existing file open it
	 (and (stringp input-stream-spec)
	      (cl-user:sneps-probe-file input-stream-spec))
	 (open (cl-user:sneps-translate input-stream-spec) :direction :input))
	((or (numberp input-stream-spec)
	     (and (or (symbolp input-stream-spec)
		      (stringp input-stream-spec))
		  (every #'digit-char-p (string input-stream-spec))))
	 (setq input-stream-spec
	       (read-from-string (format nil "~a" input-stream-spec)))
	 (cond ((<= 1 input-stream-spec (length menu))
	        (demo-open-input-stream
		 (menu-item-value (nth (1- input-stream-spec) menu)) menu))
	       (t
		(format *demo-interactive-io*
			"~&~%Menu index ~a is not in range [1..~a]. ~
                           ~%Select one of the following demonstrations or ~
                        enter a new demo filename:"
			input-stream-spec (length menu))
		(demo-open-input-stream
		 (menu-selection-insist
		  (append menu (list *demo-read-filename-item*))
		  :quit-key "q"
		  :menu-io *demo-interactive-io*)
		 menu))))
	(;; Check whether we have a single character request for a
	 ;; menu or direct menu-indexing via a number:
	 (and (or (symbolp input-stream-spec)
		  (stringp input-stream-spec))
	      (cl:= (length (string input-stream-spec)) 1))
	 (demo-open-input-stream
	  (menu-selection-insist
	   (append menu (list *demo-read-filename-item*))
	   :title "Available demonstrations:"
	   :quit-key "q"
	   :menu-io *demo-interactive-io*)
	  menu))
	((menu-quit-p input-stream-spec) nil)
	(t (format *demo-interactive-io*
		   "~&~%The file ~a does not exist. ~
                      ~%Select one of the following demonstrations or ~
                        enter a new demo filename:" input-stream-spec)
	   (demo-open-input-stream
	    (menu-selection-insist
	     (append menu (list *demo-read-filename-item*))
	     :quit-key "q"
	     :menu-io *demo-interactive-io*)
	    menu))))



;;; Added for ACL6+ compatibility  (FLJ)
;;; This function should be called by the main file for running/loading
;;;    ANY series of files that are *NOT* ACL6 (+) compatible 
;;; It is called at the beginning of the file run to set the proper case-mode
;;;    depending on what the current case-mode is (use :before)
;;; It is called at the end of the file run to return the case mode 
;;;    to the original setting  (use :after)
;;; It is called only if using ACL 6 or above ...
;;;    #+(and allegro-version>= (version>= 6 0))  (adjust-for-acl6...
;;; ...AND only if your demo cannot run in ACL6 (even after small changes)
;;; Some changes to the might be needed. 
;;; Refer in that case to the SNePS technical note #30

#+(and allegro-version>= (version>= 6 0))
(progn (defvar *case-mode-stack* nil)
       (defvar *print-case-stack* nil))

#+(and allegro-version>= (version>= 6 0))
(defun adjust-for-acl6 (timing) 
  (case timing 
    (:before
     (push excl:*current-case-mode* *case-mode-stack*)
     (push *print-case* *print-case-stack*)
     (case (first *case-mode-stack*)
       (:case-sensitive-lower
	(setf excl::*ignore-package-name-case* t)
	(excl::convert-mixed-case-symbols t) ; nil to preserve mixed-case
	(excl:set-case-mode :case-insensitive-lower)
	(setf *print-case* :downcase))
       (:case-insensitive-lower
	(setf excl::*ignore-package-name-case* t)
	(excl::convert-mixed-case-symbols t)
	(setf *print-case* :downcase))
       (:case-insensitive-upper
	(setf excl::*ignore-package-name-case* t)
	(excl::convert-mixed-case-symbols t)
	(setf *print-case* :upcase))    
       ))				; nil to preserve mixed-case
    (:after
     (case (pop *case-mode-stack*)
       (:case-sensitive-lower
	(excl:set-case-mode :case-sensitive-lower)
	(excl::convert-mixed-case-symbols nil)))
     (setf *print-case* (pop *print-case-stack*)))))

#+(and allegro-version>= (version>= 6 0))
(export 'adjust-for-acl6)


    
    




