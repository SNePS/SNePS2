;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: act-utils.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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


;;; Definitions of SNeRE utilities for acting
;;;
;;; Representation
;;;
;;;   act = action act-node
;;;         objecti ith-argument to act-node


;;; HC: Using function names instead of actual function objects to identify
;;; primitive action functions has the advantage that such functions can
;;; be traced or changed on the fly without having to re-evaluate a
;;; `declare-primaction' form in order to have the change take effect.
;;; Making the mapping bi-directional makes `find-action-node' a lot more
;;; efficient (it used to iterate over the whole hashtable whenever it
;;; tried to find a `believe', `do-all', etc. node).

(defvar *primitive-action-functions*
  (make-hash-table :test #'eql)
  "Stores the mapping between action nodes and associated functions.
The mapping is bi-directional.  If the key is an action node then the
value is the name (a symbol) of the associated primitive action function.
If the key is a symbol (the name of a primitive action function) the
value is the associated action node.  Primitive action functions must
be functions of one argument which they expect to be bound to an act node.")

(defvar *control-actions*
    '(achieve believe snsequence snif sniterate forget do-one
      do-all withsome withall))

;; Top-level SNePSUL interface:


(defmacro define-primaction (primaction vars &body forms)
  "Creates the function definition of the primitive action named PRIMACTION.
     VARS should be a (possibly empty) list of arc relations
        that get bound to the appropriate node sets.
     However, if any VAR is enclosed in parentheses,
        it gets bound to a member of the appropriate node set.
     FORMS syntax is just as it is for `defuns'."
  (let ((act-node-var (gensym))
	(strippedvars 
	 (mapcar #'(lambda (v) (if (atom v) v (first v)))
		 vars)))
    `(prog1
	 (defun ,primaction (,act-node-var)
	   ,@(when (null vars)
	       `((declare (ignore ,act-node-var))))
	   ((lambda ,strippedvars ,@forms)
	    ,@(mapcar #'(lambda (rel)
			  (if (atom rel)
			      `(sneps:nodeset.n ,act-node-var ',rel)
			    `(sneps:choose.ns
			      (sneps:nodeset.n ,act-node-var
					       ',(first rel)))))
		      vars)))
       (when (member :compiler *features*) (compile ',primaction))
       )))

(defmacro attach-primaction (&rest nodeset-fnct)
  "NODESET-FNCT must be a list of the form
        '( ... nodesetform functionname ...),
   where NODESETFORM evaluates to a singleton nodeset,
   and FUNCTIONNAME is a symbol that names a primitive action function.
   This function will store each action function in
   *primitive-action-functions* with the node as key."
  (declare (special *primitive-action-functions*))
  (do ((nsflist (cddr nodeset-fnct) (cddr nsflist))
       (ns (first nodeset-fnct) (first nsflist))
       (fn (second nodeset-fnct) (second nsflist)))
      ((and (null ns) (null fn)) t)
    (when (null ns)
      (error "~&There is an extra function name: ~A~%" fn))
    (when (null fn)
      (error "~&There is an extra nodeset: ~A~%" ns))
    (unless (symbolp fn)
      (error "~&~A must be a symbol, but isn't.~%" fn))
    (unless (fboundp fn)
      (error "~&~A must name a function, but doesn't.~%" fn))
    (setf ns (sneps::buildeval ns t))
    (when (sneps:others.ns ns)
      (error "~&Ambiguous action node: ~A~%" ns))
    (setf (gethash fn *primitive-action-functions*) (sneps:choose.ns ns))
    (setf (gethash (sneps:choose.ns ns) *primitive-action-functions*) fn)
    (sneps:set.sv 'SNePSUL::PRIMITIVE-ACTIONS
		  (insert.ns (choose.ns ns)
			     (sneps:value.sv 'SNePSUL::primitive-actions)))))

(defmacro declare-primitive (&rest nodeset-fnct)
  ;; For backward compatibility:
  `(attach-primaction ,@nodeset-fnct))

(defun find-action-node (action)
  "Returns an action node connected to the function
named by the symbol ACTION."
  (let ((action-node (gethash action *primitive-action-functions*)))
    (or action-node
        ;; We didn't find anything, create a new entry:
        (and (eval `(declare-primitive ,action ,action))
             (sneps:node action)))))

(defun action-of-act (act-node)
  "Returns the action node of the given ACT-NODE."
  (choose.ns (nodeset.n act-node 'action)))

(defun control-action? (act)
  (member (gethash (action-of-act act) *primitive-action-functions*)
	  *control-actions*))

(defun primact? (act-node)
  "Returns T if ACT-NODE represents an act with a primitive action."
  (gethash (choose.ns #!((find action- ~act-node)))
	   *primitive-action-functions*))



(defun schedule-act (act-node)
  "Schedule the performance of ACT-NODE."
  ;; SCS 10/28/94
  (check-type act-node sneps:node) ; added scs 11/2/06
  (activate-act.n act-node)
  (let ((activation (activation.n act-node)))
    (regstore activation '*priority* 'intend)
    (regstore activation '*agenda* 'start)
    (initiate activation)))

(defvar *choose-randomly* t
  "Set this to T for random choosing from a set of alternatives.
Set it to NIL for deterministic choosing (useful for debugging).")

(defun choose-arbitrary-element (list)
  (if *choose-randomly*
      (nth (random (length list)) list)
    (first list)))



    
    




