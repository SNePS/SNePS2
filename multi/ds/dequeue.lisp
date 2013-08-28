;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DEQUEUE; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: dequeue.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :dequeue)


;;;DEQUEUE PACKAGE
;;;
;;; A QUEUE is represented as a list with a header.
;;; The value of a QUEUE is the header, viz.
;;;      a cons cell whose CDR points to the first cons cell of the QUEUE,
;;;      and whose CAR points to the last cons cell of the QUEUE.
;;; The CDR of each cons cell in the QUEUE points to the next cons cell,
;;;      and the CDR of the last cons cell contains NIL.
;;; An empty QUEUE is a header whose CAR points to itself, and whose CDR contains NIL.
;;; All manipulations are destructive.
;;;
;;; The Queue can now be used as a doubly-ended list (Dequeue)
;;; Operations on Queue can now be used to simulate FIFO and/or LIFO policies.

  

;;; CONSTRUCTORS

(defun new ()
  "Creates and returns a new QUEUE."
  (let ((queue (list nil)))
    (rplaca queue queue)
    queue))

(defun insert-rear (elt queue)
  "Inserts ELT onto the rear of QUEUE, and returns QUEUE."
  (rplacd (car queue) (list elt))
  (rplaca queue (cdar queue))
  queue)

(defun insert-front (elt queue)
  "Inserts ELT onto the front of the QUEUE, and returns QUEUE."
  (cond ((empty queue) (insert-rear elt queue))
	(t (setq elt (list elt))
	   (rplacd elt (cdr queue))
	   (rplacd queue elt)
	   queue)))

;;; SELECTORS

(defun front (queue)
  "Returns the first element on the QUEUE."
  (if (empty queue) (break "Trying to get the front of an empty QUEUE.")
      (cadr queue)))

(defun rear (queue)
  "Returns the last element on the QUEUE."
  (if (empty queue) (break "Trying to get the rear of an empty QUEUE.")
      (caar queue)))

(defun delete-front (queue)
  "Returns the QUEUE with the first element deleted."
  (cond ((empty queue) (break "Trying to remove an element from an empty QUEUE.~%"))
	((eq (car queue) (cdr queue))		; only one element in queue
	 (rplaca queue queue)
	 (rplacd queue nil))
	(t (rplacd queue (cddr queue)))))

(defun delete-rear (queue)
  "Returns the QUEUE with last element deleted."
  (cond ((empty queue) (break "Trying to remove an element from an empty QUEUE.~%"))
	((eq (car queue) (cdr queue))		; only one element in queue
	 (rplaca queue queue)
	 (rplacd queue nil))
	(t (delete-rear-help queue queue))))

(defun delete-rear-help (head queue)
  "Traverses the QUEUE (QUEUE has more than one element) to delete the last element."
  (cond ((eq (cddr queue) (car head))		;now sitting at the penultimate element
	 (rplacd (cdr queue) nil)
	 (rplaca head (cdr queue)))
	(t (delete-rear-help head (cdr queue)))))

;;; RECOGNIZERS

(defun empty (queue)
  "Returns T if QUEUE is an empty QUEUE."
  (null (cdr queue)))

(defun in-queue (elt queue &key (test #'eql))
  "Returns T if ELT is in QUEUE. Elements are compared with TEST"
  (member elt (cdr queue) :test test))

;;; TRANSFORMERS

(defun print (queue)
  "Prints the QUEUE."
  (cond ((empty queue) (format t "()") (values))
      (t (format t "(~S" (cadr queue))
	 (do ((cell (cddr queue) (cdr cell)))
	     ((null cell) (format t ")") (values))
	   (format t " ~S" (car cell))))))

(defun queue-length (queue)
  "Returns number of elements in QUEUE"
  (1- (length queue)))



    
    




