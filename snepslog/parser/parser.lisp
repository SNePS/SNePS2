;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: parser.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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


;
; defining arc type
;

(defstruct (arc
	     (:constructor make-atn-arc1
			   (name test reference preaction result
				 actions terminal-action arguments)))
  name test reference preaction result actions terminal-action arguments)

(defun make-atn-arc (name test reference preaction result actions terminal-action)
  (make-atn-arc1 name 
		 test 
		 reference 
		 preaction 
		 result 
		 actions 
		 terminal-action
		 (list reference preaction result actions terminal-action)))

(defun buffer-references (reference)
  (flistify (eval-atn (cdr reference))))

(defun reference-to-arc (reference)
  (caddr reference))

(defun node-reference (reference)
   (car reference))

(defun couldnt-find (arc) (null arc))

(defun group-arc? (arc) (if (null arc)
			    nil
			    (eq 'group (arc-name arc))))

(Defun grouped-arcs (arc)
  (arc-test arc))

(Defun actualize-registers ()
  (declare (special registers))
  (prog (new-frame)
	(setf new-frame (get 'registers+ 'registers))
	loop
	(if (empty-frame? new-frame)
	    (return (remprop 'registers+ 'registers)))
	(setf registers (put-binding-frame (register-binding (first-binding new-frame))
					   (value-binding (first-binding new-frame))
					   registers)
	      new-frame (rest-of-frame new-frame))
	(go loop)))



(Defvar Trace? nil)

;; CMU Lisp complains about things like:
;;
;; (eval '(or t registers))
;;
;; with:
;;
;; Warning: This variable is undefined:
;;   REGISTERS
;;
;; So I'll just define the vars to make it happy (hc 07-15-93):

#+cmu (defvar registers nil)
#+cmu (defvar buffer nil)

(Defun Parser (buffer node registers level)
  (declare (special buffer node registers level))
  (prog (* registers+ arcs arc-to-be-taken arc-value path-stack)
        (declare (special * registers+ arcs))
	(setq arcs (arcs-leaving node)
	      path-stack (the-empty-stack)
	      registers+ (the-empty-stack))
	parser-loop
	(push-stack path-stack
		    (make-stack-element (rest-of-arcs arcs) ;when returning from an aborted arc, it will take
					                    ; the next arc
					buffer
					node
					registers
		 			level
					registers+))
	(setf * (first-expression buffer))
	find-arc-loop
	(setq arc-to-be-taken (first-affirmative-arc arcs))
	(If (couldnt-find arc-to-be-taken)
	    (if (one-element? path-stack)
		(return (aborted-arc))
		(progn (pop-stack path-stack)
		       (go find-arc-loop)))
	    (setq arc-value (eval-arc arc-to-be-taken)))
	(cond ((is-a-pop arc-value)
	       (setf (get 'registers+ 'registers)
		     registers+)
	       (return (value-to-be-returned arc-value)))
	      ((is-a-jump-or-to arc-value)
	       (setq node (value-to-be-returned arc-value)
		     arcs (arcs-leaving node))
	       (go parser-loop))
	      ((aborted-arc? arc-value)
	       (reset-parser (pop-stack path-stack))
	       (go parser-loop)))))

(defun eval-arc (arc)
  (declare (special trace?))
  (if trace? (eval-arc-print-trace arc))
  (apply (associated-function 'arc (arc-name arc))
	 (arc-arguments arc)))

(defun eval-group-arc (arc)
  (setq arc (first-affirmative-arc (grouped-arcs arc)))
  (if (not (couldnt-find arc))
      (eval-arc arc)
      (aborted-arc)))

;
;defining the arcs type (a set of arc)
;

(defun no-arcs () nil)

(defun put-arc-arcs (arc arcs) (cons arc arcs))

(defun first-arc (arcs) (car arcs))

(defun rest-of-arcs (arcs) (cdr arcs))

(defun no-more-arcs? (arcs) (null arcs))

(defun  group-arcs (arc) (cadr arc))

;
; high level operations
;
(defun  first-affirmative-arc (arcs)
  (declare (special trace?))
  (if trace? (first-affirmative-arc-print-trace arcs))
  (cond ((no-more-arcs? arcs) (undefined-arc))
	((group-arc? (first-arc arcs))
	 (first-affirmative-arc (grouped-arcs (first-arc arcs))))
	((eval (arc-test (first-arc arcs)))
	 (first-arc arcs))
	(t (first-affirmative-arc (rest-of-arcs arcs)))))

(defun undefined-arc () nil)

;
; stack
;

(defun the-empty-stack () 
"Returns the empty stack"
  (list 'stack))

(defun push-stack (stack element)
"Returns the stack with one more element"
  (rplacd stack (cons element (cdr stack))))

(defun pop-stack (stack)
"Removes the first element of the stack and returns it."
  (let ((result (second stack)))
     (rplacd stack (cddr stack))
     result))

(defun empty-stack? (stack)
"Returns t if the stack is empty"
  (null (cdr stack)))


(defun one-element? (stack)
  " Returns t if stack has only one element"
  (null (cddr stack)))


;
; stack element
;

(defstruct (stack-element
	     (:constructor make-stack-element
			   (arcs buffer node registers level registers+)))
  arcs buffer node registers level registers+)


(defun reset-parser (stack-element)
"Resets the parser to this stack element"
  (declare (special arcs buffer node registers level registers+))
  (setf arcs (stack-element-arcs stack-element)
	buffer (stack-element-buffer stack-element)
	node (stack-element-node stack-element)
	registers (stack-element-registers stack-element)
	level (stack-element-level stack-element)
	registers+ (stack-element-registers+ stack-element)))

(defun make-arc-value (&rest list)
  list)

(defun is-a-pop (arc-value)
  (if (atom arc-value)
      nil
      (eq 'pop (car arc-value))))

(defun is-a-jump-or-to (arc-value)
  (if (atom arc-value)
      nil
      (eq 'jump (car arc-value))))

(defun aborted-arc? (arc-value)
  (eq 'abort arc-value))

(defun value-to-be-returned (arc-value)
  (second arc-value))

(defun aborted-arc () 'abort)

;
; structure node; it is not a lisp structure type
;
(defun make-node (name arcs)
  (setf (get name 'node) arcs))

(defun node-arcs (node) (cdr node))

(defun node-name (node) (first node))

(defun arcs-leaving (node)
  (get node 'node))

(defun node? (node)
  (get node 'node))

;
; buffer
;

(defun make-buffer (val)
  (flistify val))

(defun first-expression (buffer)
  (first buffer))

(defun rest-of-expressions (buffer)
  (rest buffer))

(defun empty-buffer? (buffer)
  (null buffer))


;
;
;
;
;
; actions & arcs
;


(defun associated-function (type name)
  (get name type))

(defun defined-function? (value) value)

;
; actions
;

(defun first-action (a) (first a))

(defun rest-of-actions (a) (rest a))

(defun no-more-actions? (a) (null a))

;
;action
;

(defun register? (action) (atom action))

(defun register-action (action)
  (second action))

(defun form-action (action)
  (third action))

(defun action-name (action)
  (first action))

(defun action-arguments (action)
  (cdr action))

;
; implementation
;
(defun atnsetraction (register form)
  (declare (special registers))
  (if (eq register '*)
      (error "SETR - * can't be assigned" form)
      (setf registers (give-register-value register (eval form) registers))))

(defun atnaddlaction (register form)
  (declare (special registers))
  (setf registers
	(give-register-value register
			     (cons (eval form)
				   (get-register-value register registers))
			     registers)))

(defun atnaddraction (register form)
  (declare (special registers))
  (setf registers
	(give-register-value register
			     (append (get-register-value register registers)
				     (eval form))
			     registers)))

(defun atnsendraction (register form)
  (declare (special registers-))
  (setf registers-
	(give-register-value register (eval form) registers-)))

(defun atnliftraction (register form)
  (declare (special registers+))
  (setf registers+ (give-register-value register (eval form) registers+)))

(defmacro  getr (arg)
  `(if (eq ',arg '*)
       *
       (get-register-value  ',arg  registers)))

(defmacro nullr (arg)
  `(null (getr ,arg)))

(defmacro geta (arc)
   `(get-nodes (getr *) ',arc))

(defmacro endofsentence () '(null buffer))


(setf (get 'setr 'action) (function atnsetraction))  

(setf (get 'addl 'action) (function atnaddlaction))

(setf (get 'addr 'action) (function atnaddraction))

(setf (get 'sendr 'action) (function atnsendraction))

(setf (get 'liftr 'action) (function atnliftraction))

;
; evaluating actions...
;

(defun eval-actions (actions)
  (prog ()
     loop
	(when (null actions) (return))
	(apply (associated-function 'action
				    (action-name (first-action actions)))
	       (action-arguments (first-action actions)))
	(setf actions (rest-of-actions actions))
	(go loop)))

(defun eval-terminal-action (action)
  (declare (special buffer))
  (if (eq (action-name action) 'to)
      (setf buffer (rest-of-expressions buffer)
	    * (first-expression buffer)))
  (make-arc-value 'jump (second action)))

(defun atncallarc (references preactions result actions terminal-action)
  (declare (special registers level))
  (prog (result-value registers-)
        (declare (special registers-))
	(setf registers- (the-empty-frame))
	(eval-actions preactions)
	(setf result-value (parser (buffer-references references)
				   (node-reference references)
				   registers-
				   (1+ level)))
	(if (aborted-arc? result-value)
	    (return (aborted-arc)))
     (actualize-registers)
     (setf registers (give-register-value result result-value registers))
     (eval-actions actions)
     (return (eval-terminal-action terminal-action))))

(setf (get 'call 'arc) (function atncallarc))


(defun atnjumparc (references preactions result actions terminal-action)
  (declare (ignore preactions result terminal-action))
  (eval-actions actions)
  (make-arc-value 'jump references))

(setf (get 'jump 'arc) (function atnjumparc))

(defun atnpoparc (references &rest rest)
  (declare (ignore rest))
  (make-arc-value 'pop (eval-atn references)))

(setf (get 'pop 'arc) (function atnpoparc))

(defun eval-atn (exp)
  (declare (special registers))
  (cond ((eq exp '*) *)
	((atom exp) (get-register-value exp registers))
	(t (eval exp))))

;
; loading and defining nodes
;

(defun define-atn-node (node)
  (make-node (node-name node)
	     (mapcar (function define-arc) (node-arcs node))))

(defun define-arc (arc)
  (cond ((jump-arc? arc)
	 (make-atn-arc 'jump (caddr arc) (cadr arc) nil nil (cdddr arc) nil))
	((call-arc? arc)
	 (make-atn-arc 'call
		       (cadddr arc)
		       (cons (cadr arc)
			     (caddr arc))
		       (preactions arc)
		       (result (cddddr arc))
		       (actions arc)
		       (terminal-action arc)))
	((pop-arc? arc)
	 (make-atn-arc 'pop (caddr arc) (cadr arc) nil nil nil nil))
	((defining-group-arc? arc)
	 (make-atn-arc 'group (mapcar (function define-arc) (cdr arc)) nil nil nil nil nil))))

(defun jump-arc? (arc)
  (eq 'jump (car arc)))

(defun call-arc? (arc)
  (eq 'call (car arc)))

(defun pop-arc? (arc)
  (eq 'pop (car arc)))

(defun defining-group-arc?(arc)
  (eq 'group (car arc)))

(defun preactions (arc)
  (prog (result action)
	(setf arc (cddddr arc))
      loop
	(setf action (car arc))
	(if (preaction? action)
	    (setf result (append result (list action)))
	    (return result))
	(setf arc (cdr arc))
	(go loop)))

(defun preaction? (arc)
  (eq 'sendr (car (flistify arc))))

(defun result (arc)
  (cond ((null arc) nil)
	((register? (first-action arc))
	 (first-action arc))
	(t (result (rest-of-actions arc)))))

(defun actions (arc)
  (prog ()
	(setf arc (cddddr arc))
      remove-pre-actions
	(when (not (register? (first-action arc)))
	  (setf arc (rest-of-actions arc))
	  (go remove-pre-actions))
	(return (remove-terminal-action (rest-of-actions arc)))))

(defun remove-terminal-action (arc)
  (if (null (cdr arc))
      nil
      (cons (car arc) (remove-terminal-action (cdr arc)))))

(defun terminal-action (arc)
  (if (null (cdr arc))
      (car arc)
      (terminal-action (cdr arc))))


;
; registers structure
;

(defun make-binding (register value)
  (cons register value))

(defun register-binding (binding)
  (car binding))

(defun value-binding (binding)
  (cdr binding))

(defun same-register-binding (r b)
  (eq r (car b)))



(defun the-empty-frame () (cons 'frame nil))

(defun put-binding-frame (r v f)
  (setf (cdr f)
	(put-binding-frame1 r v (cdr f)))
  f)

(defun put-binding-frame1 (r v f)
  (let ((place (assoc r f)))
    (cond (place (setf (cdr place) v)
		 f)
	  (t (cons (make-binding r v) f)))))

(defun first-binding (f) (cadr f))

(defun rest-of-frame (f) (cons 'frame (cddr f)))

(defun empty-frame? (f) (null (cdr f)))



(defun give-register-value (register value frame)
  (put-binding-frame  register value frame))

(defun get-register-value (register frame)
  (cond ((empty-frame? frame)
	 (undefined-register))
	((same-register-binding register (first-binding frame))
	 (value-binding (first-binding frame)))
	(t (get-register-value register (rest-of-frame frame)))))

(defun undefined-register () nil)

(defun flistify (l)
  (if (listp l)
      l
      (list l)))


(defun atnin (file)
  (declare (special *package*))
  (do* ((stream (open (cl-user:sneps-translate file) :direction :input))
	(eof (gensym))
	(*package* (find-package 'snepslog))
	(read-exp (read stream nil eof)
		  (read stream nil eof)))
       ((eq read-exp eof) (close stream) t)
    (define-atn-node read-exp)))

;
; tracing atns
;

(defun trace-atn ()
  (declare (special trace?))
  (setf trace? t))

(defun eval-arc-print-trace (arc)
  (declare (special registers buffer level *))
  (format t "
-----------------------------------------------
Executing arc ~A
Registers ~A
Buffer ~A
* ~A
Level ~A
-----------------------------------------------
"
           (arc-name arc) 
           (cdr registers)
           buffer
           *
           level))

(defun first-affirmative-arc-print-trace (arcs)
  (declare (special node * buffer registers))
  (cond ((group-arc? (first-arc arcs))
	 nil)
	((null arcs)
	 (format t "~%----------------------------------------------
In node ~A
Testing arc ~A
*: ~A
Buffer: ~A
Test: ~A
Registers: ~A
----------------------------------------------"
		 node
		 nil
		 *
		 buffer
		 nil))
	 (t (format t "
----------------------------------------------
In node ~A
Testing arc ~A
*: ~A
Buffer: ~A
Test: ~A
Registers: ~A
----------------------------------------------"
		    node
		    (arc-name (first-arc arcs))
		    *
		    buffer
		    (arc-test (first-arc arcs))
		    registers))))



    
    




