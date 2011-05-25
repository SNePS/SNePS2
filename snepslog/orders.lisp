;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: orders.lisp,v 1.2 2011/05/25 20:03:22 shapiro Exp $

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

(defun source (p1 p2)
  "Returns t iff p1 <= p2 in the epistemic entrenchment ordering.
   Uses assertions:
        HasSource(p,s) to mean that proposition p's source is s;
        IsBetterSource(s1,s2) to mean that s1 is a more credible source than s2.
   If neither p1 nor p2 has a source, then they're epistemically tied.
   If only one of p1 or p2 has a source,
      then the one without the source is more epistemically entrenched than the other.
   If they both have sources,
      then p1 <= p2
           iff for every source of p1 
                  there is a source of p2 that is more credible than p1's source."
  (let ((p1sources (mapcar #'(lambda (sub) (match:value.sbst 'x sub))
			   (tell "askwh HasSource(~A, ?x)" p1)))
	(p2sources (mapcar #'(lambda (sub) (match:value.sbst 'x sub))
			   (tell "askwh HasSource(~A, ?x)" p2))))
    (if (and p1sources p2sources) 
	(every #'(lambda (s1)
		   (some #'(lambda (s2) (tell "ask IsBetterSource(~A, ~A)" s2 s1))
			 p2sources))
	       p1sources)
      (not p2sources))))


;;; Description: An ordering function relying on explicit statements of
;;;              relative entrenchment of propositions, using the
;;;              IsLessEntrenched predicate for checks
;;; [IsLessEntrenched(x.y)] = [x] is strictly less entrenched than [y]
(defun explicit (lhs rhs)
  (not (tell "ask IsLessEntrenched(~A, ~A)" rhs lhs)))

