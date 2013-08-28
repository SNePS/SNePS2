;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSUL; Base:10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: arcinfo.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :snepsul)


;;; Definitions of Primitive Arc/Info Actions

(defvar *just-fake-it* nil
  "If T actions are not actually sent to ARC/INFO.")

(defun just-do-it (command &optional (print-result nil))
  "Takes an AML COMMAND (a string) and executes it, i.e., sends it to
ARC/INFO unless *just-fake-it* is T."
  (format t "~&~% Now  doing: ~a~%" command)
  (unless *just-fake-it*
    (execute-command command)
    (when print-result (print-result))))

(defun coerce-node (node type)
  "Coerces NODE into TYPE."
  (case type
    (:node node)
    (:symbol (sneps:node-na node))
    (:string (format nil "~a" node))
    (t (format nil "~a" node))))

(defun lex-node (object-node-set)
  ;; Returns node at the head of the `lex' arc of a singleton OBJECT-NODE-SET.
  (first (ns-to-lisp-list #!((find lex- ~object-node-set)))))

(define-primaction typeout (object1)
  (just-do-it (format nil "&type ~s" (lex-node object1))))

(define-primaction clear ()
  (just-do-it "clear"))

(define-primaction start (object1)
  (just-do-it (coerce-node (lex-node object1) :string)))

(define-primaction stop ()
  (just-do-it "quit"))

(define-primaction tell-fun (object1)
  (format t "~&~%*** ~a ***~3%" (lex-node object1)))

(define-primaction interpret (object2)
  "Does an ad hoc interpretation of a record that describes
a region on the Landuse coverage. HACK!"
  (let* ((result (get-file-as-string *comout*))
	 (lu-code-value
	  ;; Return 300 as a default in case actions are faked!!
	  (or (with-input-from-string (s result)
		(read-line s nil nil)
		(dotimes (i 5) (read s nil nil))
		(read s nil nil))
	      300))
	 (translation
          (first
           (ns-to-lisp-list
	    #!((find translated-value-
                     (& (find translations-
                              (find relation landuse attribute lu-code))
                        (find value ~lu-code-value))))))))
    (format t "~2%This area on the coverage ~a is ~(~a~).~2%"
	    (lex-node object2) translation)))


;; Support for pseudo NL declaration of primactions: Using the three
;; commands SAY, ISSUE and SEND one can assemble a complex ARC/INFO
;; command in a pseudo natural language style and send the constructed
;; command. SAY corresponds to a write, ISSUE to a write-line and SEND
;; to a write plus actual sending of the assembled command to the
;; ARC/INFO process.

(defvar *arc-command-stack* nil
  "A list of strings that together define a complex ARC command.")

(define-primaction say (object1)
  "Push OBJECT1 literally on the stack"
  (cl:push (coerce-node (lex-node object1) :string) *arc-command-stack*))

(define-primaction issue (object1)
  "Push OBJECT1 and a terminating carriage return on the stack"
  (cl:push (coerce-node (lex-node object1) :string) *arc-command-stack*)
  (cl:push (format nil "~%") *arc-command-stack*))

(define-primaction send (object1)
  "Push OBJECT1 on the stack, concatenate all the items on the stack into
a single command string and send the string to the ARC/INFO process."
  (cl:push (coerce-node (lex-node object1) :string) *arc-command-stack*)
  (just-do-it (format nil "~{~a ~}" (reverse *arc-command-stack*)))
  (setq *arc-command-stack* nil))



    
    




