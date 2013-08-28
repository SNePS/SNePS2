;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: generator.lisp,v 1.4 2013/08/28 19:07:26 shapiro Exp $

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




;; altered for ACL 6 compatibility (FLJ)
	 

(in-package :snepslog)


; Modifications:
;   Choi 2/13/92: Included handling of numerical quantifiers
;   Choi 4/28/92: Included handling of set arguments


(defun sneps-atomic-node? (node) 
  (eq (sneps:node-type node) :base))

(defun sneps-relation? (node)
  (and (not (sneps-quantified-node? node))
       (relation-predicate node)))

(defun sneps-connected-node? (node)
  (and (not (sneps-quantified-node? node))
       (or (get-nodes node 'cq)
	   (get-nodes node 'arg))))

(defun sneps-simple-entailment? (node)
  (and (cl:= 1 (length (get-nodes node 'ant)))
       (cl:= 1 (length (get-nodes node 'cq)))))

(defun sneps-or-entailment? (node)
  (get-nodes node 'ant))

(defun sneps-and-entailment? (node)
  (get-nodes node '&ant))

(defun sneps-entailment? (node)
  (get-nodes node 'cq))

(defun sneps-and? (node)
  (let ((min (sneps:choose.ns (get-nodes node 'min)))
	(max (sneps:choose.ns (get-nodes node 'max))))
    (and (eql min max)
	 (eql (length (get-nodes node 'arg))
	    (numberize max)))))

(defun sneps-or? (node)
  (let ((min (sneps:choose.ns (get-nodes node 'min)))
	(max (sneps:choose.ns (get-nodes node 'max))))
    (and (eql (numberize min) 1)
	 (eql (length (get-nodes node 'arg))
	    (numberize max)))))


(defun sneps-andor? (node)
  (get-nodes node 'min))

(defun sneps-not? (node)
  (and (get-nodes node 'min)
       (zerop (numberize (sneps:choose.ns (get-nodes node 'min))))
       (zerop (numberize (sneps:choose.ns (get-nodes node 'max))))
       (cl:= 1 (length (get-nodes node 'arg)))))

(defun sneps-nor? (node)
  (and (get-nodes node 'min)
       (zerop (numberize (sneps:choose.ns (get-nodes node 'min))))
       (zerop (numberize (sneps:choose.ns (get-nodes node 'max))))
       (> (length (get-nodes node 'arg)) 1)))

(defun sneps-nand? (node)
  (and (get-nodes node 'min)
       (zerop (numberize (sneps:choose.ns (get-nodes node 'min))))
       (> (length (get-nodes node 'arg)) 1)
       (cl:= (sneps:node-to-lisp-object
	      (sneps:choose.ns (get-nodes node 'max)))
	     (1- (length (get-nodes node 'arg))))))

(defun sneps-xor? (node)
  (and (get-nodes node 'min)
       (cl:= (numberize (sneps:choose.ns (get-nodes node 'min))) 1)
       (cl:= (numberize (sneps:choose.ns (get-nodes node 'max))) 1)
       (> (length (get-nodes node 'arg)) 1)))

(defun sneps-thresh? (node)
  (and (not (sneps-quantified-node? node))
       (or (sneps-general-thresh? node)
	   (sneps-equivalence? node))))

(defun sneps-equivalence? (node)
  (let ((thresh (sneps:choose.ns (get-nodes node 'thresh))))
    (and thresh
	 (cl:= (numberize thresh) 1))))

(defun sneps-general-thresh? (node)
  (let ((thresh (sneps:choose.ns (get-nodes node 'thresh))))
    (and thresh
	 (/= (numberize thresh) 1))))


(defun sneps-forall? (node)
  (get-nodes node 'forall))

(defun sneps-exists? (node)
  (get-nodes node 'exists))

(defun sneps-num-quant? (node)
  (get-nodes node 'pevb))

(defun sneps-quantified-node? (node)
  (declare (special registers))
  (and (nullr quantified)
       (or (sneps-forall? node)
	   (sneps-exists? node)
	   (sneps-num-quant? node))))

;
;
;
;
;
;
;
;
;
;
;

(defun get-nodes (node arc) 
  "Given a node and an arc name, returns the node-set that is connected to node by the arc" 
  (getf (sneps:node-fcableset node) (intern (build-namestring arc) 'snepsul)))

(defun get-node-name (node)
  (and (sneps:node-p node)
       (sneps:node-na node)))


(defun relation-predicate (node)
  "Given a sneps node as argument (it should be a relation node), returns the relation predicate."
  (funcall (get 'mode 'relation-predicate) node))
;(get 'mode 'relation-predicate) is one of relation-predicate_mode.1 or relation-predicate_mode.2 . See
; make.snepslog1 and make.snepslog2 .


(defun relation-argument.list (node)
  "Given a sneps node as argument (it should be a relation node), returns the relation arguments (the arcs)."
  (funcall (get 'mode 'relation-argument.list) node))

(defun relation-predicate_mode.1 (node)
  "Given a sneps node as argument (it should be a relation node and generated by snepslog version 1), returns the relation
   predicate."
  (get-nodes node 'r))


;;; Modified to make sure that mode 1 arc relations are in the snepsul package.
;;; 8/31/06 - scs
(defun relation-argument.list_mode.1 (node)
  "Given a relation node, returns a node list containing the argument nodes.
   Works only for snepslog version 1."
  (do* ((counter 1 (+ 1 counter))
	(arc 
	 (intern (build-namestring :a  counter) :snepsul)
	 (intern (build-namestring :a  counter) :snepsul)) 
	(node-listed (get-nodes node arc) (get-nodes node arc))
	(arguments (list node-listed) (append (list node-listed) arguments)))
       ((null node-listed) (reverse (cdr arguments)))))

(defun relation-predicate_mode.2 (node)
  "Given a sneps node as argument (it should be a relation node), returns the relation predicate.
   Works only in snepslog version 2."
  (get-nodes node (predicate-arc node)))

(defun relation-argument.list_mode.2 (node)
  "Given a sneps node as argument (it should be a relation node), returns the relation arguments.
   Works only in snepslog version 2."
  (apply #'append
	 (mapcar #'(lambda (arc)
		     (get-nodes node arc))
		 (relation-argument.arcs  (predicate-arc node)))))

(defun relation-argument.arcs (arc)
  "Given an arc-name, returns the list of the associated-arcs. Snepslog version 2.
   Used by relation-argument.list_mode.2"
  (get arc 'snepslog-associated-arcs))


(defun predicate-arc (node)
  "Given a node generated by snepslog version 2, returns the predicate arc.
   This is the alphabeticly lowest named arc."
  (do* ((plist (sneps:node-fcableset node) (cddr plist)))
       ((null plist) nil)
    (if (eql (char (symbol-name (first plist)) 0) #\ )
	(return (first plist)))))



(defun numberize (node)
  "Returns the atomic version of the node."
  (and (sneps:node-p node)
       (read-from-string (princ-to-string (sneps:node-na node)))))

(defun make-tuple (list separator open-parentesis close-parentesis)
  (cons open-parentesis
	(make-tuple1 list
		     separator
		     close-parentesis)))

(defun make-tuple1 (list separator close-parentesis)
  (if (null (cdr list))
      (append (flistify (car list))
	      (list close-parentesis))
      (append (flistify (car list))
	      (cons separator
		    (make-tuple1 (cdr list) separator close-parentesis)))))


(defun set-snepslog-version (node snepslog-version)
  (setf (sneps:node-snepslog node) (list 'snepslog-version snepslog-version))
  snepslog-version)
 
;;; COMMENTED OUT BECAUSE IT IS DEFINED IN SNEPSLOG/MODE3 FILE
;;;(defun get-snepslog-version (node)
;;;  (let ((node-snepslog (sneps:node-snepslog node)))
;;;    (if (listp node-snepslog)
;;;	(getf node-snepslog 'snepslog-version)
;;;	node-snepslog)))

;;; Modified to include the grammar state the function was called from
;;; 5/22/06 - mwk
(defun set-snepslog-complete-version (node snepslog-version state)
  "This function sets the snepslog complete description. Returns the snepslog version being set."
  (setf (sneps:node-snepslog node) 
	(append (list state snepslog-version)
		(sneps:node-snepslog node)))
  snepslog-version)

;;; Modified to include the grammar state the function was called from
;;; 5/22/06 - mwk
(defun get-snepslog-complete-version (node state)
  "returns the snepslog complete description of a node"
  (if (eq (sneps:node-type node) :var)
      (sneps:node-snepslog node)
    (getf (sneps:node-snepslog node) state)))

;;; altered for ACL 6.X  by FLJ
;;; from "WFF" to proper case depending on lisp version 
;;;     by using build-namestring
(defun m->wff (node)
  "Returns the node's short name (a string)"
  (cond ((eq :mol (sneps:node-type node)) 
	 (format nil 
		 (build-namestring 
		  :wff
		  (subseq (symbol-name (sneps:node-na node)) 1))))
	(t (string-trim '(#\!) (symbol-name (sneps:node-na node))))))

(defun distribute-cons (plist)
  (declare (special plist))
  (if (null plist)
      nil
      (do ((result (nreverse (mapcar #'(lambda (element)
					 (cons (first plist) element))
				     (second plist)))
		   (nconc (nreverse (mapcar #'(lambda (element)
						(cons (first plist) element))
					    (second plist)))
			  result)))
	  ((null plist) (reverse result))
	(setq plist (cddr plist)))))
  
(defun distribute-ots (asupport)
  (distribute-cons asupport))

(defun ot-pair.ot.ct (pair)
  (car pair))

(defun context-pair.ot.ct (pair)
  (cdr pair))

(defun higher-priority? (node operation)
  (let ((ops (list #'sneps-entailment? #'sneps-thresh? #'sneps-or?  #'sneps-and? #'sneps-not?)))
    (some #'(lambda (f) (funcall f node)) (member operation ops))))



    
    




