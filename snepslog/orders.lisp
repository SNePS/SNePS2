;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: orders.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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

;;; Predefined epistemic ordering functions
;;; by Ari Fogel
;;; 2011/05/04

(in-package :snepslog)

;;; *fluents*
;;;   list of symbols corresponding to names of functions that are fluents
(defparameter *fluents* nil)


;;; An order in which all propositions are equally entrenched
(defun null-order (lhs rhs)
  (declare (ignore lhs rhs))
  t)

;;; Description:  An ordering function that causes fluent propositions to be
;;;               less epistemically entrenched than non-fluent propositions
;;;               is a fluent or the rhs argument is not.
(defun fluent (lhs rhs)
  (or (is-fluent lhs)
    (not (is-fluent rhs))))

;;; Description:  Returns t iff the function symbol for n is a fluent
;;; Arguments:    n - a node
(defun is-fluent (n)
  (let ((pred (relation-predicate n)))
    (when (and pred (listp pred))
      (setf pred (get-node-name (car pred))))
    (member pred *fluents*)))

;;;; Description:  An ordering function that causes propositions with more
;;;               reliable sources to be more epistemically entrenched than
;;;               propositions with less reliable sources. Also, unsourced
;;;               propositions are more entrenched than sourced ones.
;;; [HasSource(x,y)] = The source of [x] is [y]
;;; [IsBetterSource(x,y)] = [x] is a better source than [y]
(defun source (lhs rhs)
  (or
    (and
      (let ((source-lhs (tell "askwh HasSource(~A, ?x)" lhs))
            (source-rhs (tell "askwh HasSource(~A, ?x)" rhs)))
        (cond ((and source-lhs source-rhs)
            (let ((source-lhs-term
                    (cdr (assoc 'x (first source-lhs))))
                  (source-rhs-term
                    (cdr (assoc 'x (first source-rhs)))))
              (not (tell "ask IsBetterSource(~A, ~A)"
                      source-lhs-term source-rhs-term))))
          (source-rhs nil)
          (t t))))))


;;; Description: An ordering function relying on explicit statements of
;;;              relative entrenchment of propositions, using the
;;;              IsLessEntrenched predicate for checks
;;; [IsLessEntrenched(x.y)] = [x] is strictly less entrenched than [y]
(defun explicit (lhs rhs)
  (not (tell "ask IsLessEntrenched(~A, ~A)" rhs lhs)))

