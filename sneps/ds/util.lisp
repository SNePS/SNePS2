;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: util.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




;; Altered for ACL 6 by FLJ

(in-package :sneps)


;  beep
;  ----
;
;     this function already exists in the EXPLORER, so now it is a comment!
;
;                                        written :  
;                                        modified: NJM  09/08/88
;                                        modified: 
;
#-(or explorer symbolics)
(defun beep ()
  "Sound the bell."
  ;; I don't know how to get this to work yet.
  (princ "")
  (values))
#+explorer
(defmacro beep()
  `(tv:beep))

;;
;;  repeat
;;  ------
;;
;;  DON'T USE THIS MACRO!!!!
;;  It is an old dinosaur that is only kept for emergencies. The only place
;;  where it still might be used is in an old version of checkqvars1 if it
;;  turns out that the rewritten new version has a bug. (hc, 1/8/92)
;;
;(defmacro repeat (&rest args)
;  (declare (special *untilvalue*))
;    (cond ((member 'begin args :test #'eq)
;	   `(prog ,(cons '*untilvalue* (car args))
;		  (declare (special *untilvalue* ,@(car args)))
;		  ,@(loopmessage (cdr args))
;		  (go begin)))
;	  (t `(prog ,(cons '*untilvalue* (car args))
;		    (declare (special *untilvalue* ,@(car args)))
;		 begin
;		    ,@(loopmessage (cdr args))
;		    (go begin)))))
;
;
;(defun loopmessage (forms)
;    (declare (special *macro*))
;    (cond ((null forms) nil)
;          ((eq (car forms) 'while)
;           (cons (list 'if (list 'null (cadr forms)) '(return nil))
;                 (loopmessage (cddr forms))))
;          ((eq (car forms) 'until)
;           (cons (list 'if (list 'setq '*untilvalue* (cadr forms))
;                          '(return *untilvalue*))
;                      (loopmessage (cddr forms))))
;          (t (cons (car forms)(loopmessage (cdr forms))))))
 

; initialized-p (hc, 9/26/90)
; -------------
;
; Can be used in various places where variables (such as *atn-arcs-hashtable*)
; have to be checked for proper initialization. This should also handle some
; differences in how various Lisps handle DEFVAR (i.e., a value of :unbound
; leaves a var unbound on explorers but sets it to :unbound in other Lisps).
;
(defmacro initialized-p (variable)
  "Returns nonNIL if VARIABLE is bound and has a nonNIL value."
  `(and (boundp ',variable)
	(not (eq ,variable :unbound)) 
	,variable))


(defvar *null-stream* (make-string-output-stream)
   "Variable that holds the only instance of a string-output stream used
in all invocations of chew-up-output (save cons cells).")

(defmacro chew-up-output ((&rest stream-variables) &body body)
  "Chews up any output to any of STREAM-VARIABLES during execution of the forms
supplied in BODY. If no variables are supplied *standard-output* will be used
as default. Be very careful using this macro, in particular, if you rebind
*terminal-io* and an error occurs during execution of the BODY forms you'll
probably loose big time."
  (unless stream-variables
    (setq stream-variables '(*standard-output*)))
  `(let ,(mapcar #'(lambda (stream-var)
		     (list stream-var '*null-stream*))
	  stream-variables)
    (declare (special ,@stream-variables))
    (multiple-value-prog1
	(progn ,@body)
      ;; Free the string for garbage collection
      (get-output-stream-string *null-stream*))))

(defun redefine-function (name definition &optional (type :function))
  "Redefine the function NAME to DEFINITION without printing
redefinition warnings. TYPE should be either :FUNCTION or :MACRO indicating
whether NAME should be defined as a function or a macro. DEFINITION can be
a symbol in which case its symbol or macro function is used as the new
definition of NAME."
  (let (#+lucid (user::*redefinition-action* nil)
	#+allegro (excl:*redefinition-warnings* nil)
	#+(or explorer symbolics)(si:inhibit-fdefine-warnings t)
	;; If non of the above applies chew-up-output might be
	;; an ultimate alternative
	)
    (when (symbolp definition)
      (setq definition
	    (case type
	      (:function (symbol-function definition))
	      (:macro (macro-function definition)))))
    (case type
      (:function (setf (symbol-function name) definition))
      (:macro (setf (macro-function name) definition)))))


;; This macro comes from snepslog utilities. Put it in here instead of the
;; user package and make use of the improved redefine-function functionality.
;;
(defmacro in.environment (&key variables declarations functions eval always.do.this)
  "Creates an environment and evaluates an expression in that environment 
 (the eval arg). Global variables are defined. Local variables can be defined 
 (in the let fashion), local functions may also be defined (also in the let 
 fashion, that is ((<symbol> <function def>) ...). Notice that <symbol>s are 
 not evaluated but <function>s are!). The evaluated expression is
 unwind.protected  by the always.do.this argument. This is a useful macro!"
  (let ((old-definitions-var (gensym)))
    `(let ((,old-definitions-var
	    ;; Remember old definition of functions that will be
	    ;; redefined below
	    (mapcar #'(lambda (function-spec)
			(let ((fnname (first function-spec)))
			  (cons
			   fnname
			   (cond ((fboundp fnname)
				  ;; have to check for macros here because
				  ;; they need to get redefined differently
				  (cond ((macro-function fnname)
					 (list (macro-function fnname)
					       :macro))
					(t (list (symbol-function fnname)
						 :function))))
				 (t nil)))))
		    ',functions))
	   ,@variables)
      (declare ,@declarations)
      (unwind-protect
	   (progn
	     (dolist (function-spec ',functions)
	       (let ((new-definition (eval (second function-spec))))
		 (redefine-function
		  (first function-spec)
		  new-definition
		  ;; Check whether the new thing is a symbol that names a macro
		  (cond ((and (symbolp new-definition)
			      (macro-function new-definition))
			 :macro)
			(t :function))
		  )))
	     ,eval)
	(unwind-protect
	     ,always.do.this
	  ;; Now backdefine functions or undefine them if they
	  ;; were not defined outside the macro call
	  (dolist (function-spec ,old-definitions-var)
	    (cond ((second function-spec)
		   (redefine-function (first function-spec)
				      (second function-spec)
				      (third function-spec)))
		  (t (fmakunbound (first function-spec)))))
	  )))))


(defun un-ize (number)
  ;; Convert a NUMBER into a symbol with that name:
  (intern (build-namestring number)))

(defmacro protect-eval (&body forms)
  #-explorer `(progn . ,forms)
  #+explorer `(let ((sys:inhibit-displacing-flag t))
		(declare (special sys:inhibit-displacing-flag))
		. ,forms))

(defun substitute-tree (subtree-test function tree)
  "Substitutes qualifying subTREEs with the result of FUNCTION(subTREE).
Only proper subtrees are considered, for example, if TREE is `(1 (2 (3)) 4)'
then the subtrees will be `1', `(2 (3))', `2', `(3)', `3' and `4'. Dotted
structures and atomic trees are allowed too. Once a qualifying subtree has
been found its subtrees will not be considered anymore. For example,
`(substitute-tree 'atom 'identity TREE)' generates a copy of TREE."
  (cond ((consp tree)
         (cons (if (funcall subtree-test (car tree))
                   (funcall function (car tree))
                 (if (consp (car tree))
                     (substitute-tree subtree-test function (car tree))
                   (car tree)))
               (substitute-tree subtree-test function (cdr tree))))
        ((funcall subtree-test tree)
         (funcall function tree))
        (t tree)))

(defmacro with-readable-nodes-and-contexts (&body body)
  ;; Within the scope of this macro nodes and contexts will be printed
  ;; readably, i.e., they will generate printed representations that
  ;; will construct actual node and context structures when they are
  ;; read back in. See `n^' and `c^' for more details.
  `(in.environment
    :functions
    ((node-printer
      #'(lambda (node stream depth)
	  ;; Prints NODE such that it will create a node structure when read.
	  (declare (ignore depth))
	  (format stream "#.(~s '~s)" 'n^ (node-na node))))
     (context-printer
      #'(lambda (context stream depth)
	  ;; Prints CONTEXT such that it will create a context structure
	  ;; when read.
	  (declare (ignore depth))
	  (format stream "#.(~s ~d)" 'c^ (context-order context)))))
    :eval (progn ,@body)))


;;; The type character had been misnamed char
(defun build-namestring (&rest symbols)
  "Makes a string of all the elements in symbols concatonated together after being converted to strings in the appropriate way for whatever lisp version is being run."
  (apply #'concatenate 'string
	 (mapcar #'(lambda (s)
		     (etypecase s
		       (string s)
		       (symbol (symbol-name s))
		       (number (princ-to-string s))
		       (character (string s))
		       ))
		 symbols)))




