;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: snepslog.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;; 
;; The contents of this file are subject to the University at
;; Buffalo Public License Version 1.0 (the "License"); you may
;; not use this file except in compliance with the License. You
;; may obtain a copy of the License at http://www.cse.buffalo.
;; edu/sneps/Downloads/ubpl.pdf.
;; 
;; Software distributed under the License is distributed on an
;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;; or implied. See the License for the specific language gov
;; erning rights and limitations under the License.
;; 
;; The Original Code is SNePS 2.7.
;; 
;; The Initial Developer of the Original Code is Research Foun
;; dation of State University of New York, on behalf of Univer
;; sity at Buffalo.
;; 
;; Portions created by the Initial Developer are Copyright (C)
;; 2011 Research Foundation of State University of New York, on
;; behalf of University at Buffalo. All Rights Reserved.
;; 
;;  
;; 
;; 


;; $END LICENSE$


(in-package :snepslog)


(defvar complete-description 'normal
  "This variable is used to select the mode (unlabeled, normal or expert) used to describe the nodes")

(defparameter *SNePSLOGRunning* nil
  "If value is True, indicates that SNePSLOG is running.
     Primarily to determine how SNePS terms should be printed.")

(defmacro surface (&rest nodes)
  `(snepslog-print ',nodes))

(defun slight-surface (node &optional (stream nil))
  "Prints in stream the representation of a node"
  (format stream "~A" (m->wff node)))
  
(defun node-intern (string)
  "Returns the node represented by string."
  (sneps:node (wff->m (intern string))))

;;; altered for acl6.X by FLJ... "WFF" -> the appropriate case for 
;;;    the current lisp version (using "build-namestring")
;;; allows input of wffs as "wff##" .vs "WFF##"
(defun sneps-node? (string)
  "given a string, returns t if the string represents the short name of a proposition"
  (and (stringp string)
       (string-equal string (build-namestring :wff) :start1 0 :end1 2 :start2 0 :end2 2)
       (parse-integer (subseq string 3) :junk-allowed t)))

(defun make.snepslog1 ()
  "changes the way snepslog relations are represented in sneps. Makes the arcs to the arguments not to be indexed on the predicate"
  (declare (special *br-tie-mode* *br-auto-mode*))
  ;; This is a new 'canonical' relation put onto sneps:*initial-relations*
  (define-if-not-yet-defined 'snepsul:r)
  (setf (get 'mode 'relation-argument.list) #'relation-argument.list_mode.1)
  (setf (get 'mode 'relation-predicate) #'relation-predicate_mode.1)
  (setf (get 'mode 'make-relation) #'make-relation.1)
  (setf *br-auto-mode* nil)
  (setf *br-tie-mode* nil)
  (setf (symbol-function 'snebr::br-totalorder) (symbol-function 'null-order)))

(defun make.snepslog2 ()
  "changes the way snepslog relations are represented in sneps. Makes the arcs to the arguments be indexed on the predicate"
  (setf (get 'mode 'relation-argument.list) #'relation-argument.list_mode.2)
  (setf (get 'mode 'relation-predicate) #'relation-predicate_mode.2)
  (setf (get 'mode 'make-relation) #'make-relation.2)
  (setf *br-auto-mode* nil)
  (setf *br-tie-mode* nil)
  (setf (symbol-function 'snebr::br-totalorder) (symbol-function 'null-order)))

(make.snepslog1)

