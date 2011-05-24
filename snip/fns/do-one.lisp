;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011
;; Research Foundation of State University of New York

;; Version: $Id: do-one.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


(in-package :snip)

;;;
;;; Representation
;;;
;;;   act = action act-node
;;;         objecti ith-argument to act-node

;;; Added the check for no acts to be done.  scs 11/2/06
(defun do-one (n)
  "Randomly picks one of a set of actions and intends it to be performed."
  (let* ((possible-actions (sneps:pathfrom '(object1) n))
	 (chosen-act (choose-arbitrary-element possible-actions))
	 (pr (activation.n n)))
    (plantrace "Now doing: DO-ONE " possible-actions nil)
    (cond (chosen-act
	   (plantrace "Chose to do the act " (list chosen-act) nil)
	   (schedule-act chosen-act))
	  (t (plantrace "Nothing to do" (new.ns) nil)))
    (regstore pr '*AGENDA* 'DONE)))
