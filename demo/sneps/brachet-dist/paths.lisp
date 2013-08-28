;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;;; Copyright (C) 1984--2013
;;; Research Foundation of State University of New York

;;; Version: $Id: paths.lisp,v 

;;; This file is part of SNePS.

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


;;; Composition of certain paths

;;; Make Before Transitive
(define-path before (compose before (kstar (compose after- ! before))))

;;; Make After Transitive
(define-path after (compose after (kstar (compose before- ! after))))

;; If X is a member of the class Y and Y is a subclass of Z then
;;   X is a member of the class Z.
;; Example:  If Fido is a brachet and all brachets are hounds then 
;;;  Fido is a hound.
(define-path class (compose class (kstar (compose subclass- ! superclass))))

;; Make subclass transitive
(define-path subclass (compose subclass (kstar (compose superclass- ! subclass))))


