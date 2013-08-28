;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: matchingset.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :match)


;;; -----------------------------------------------------------------------------
;;;       Ported from Franz Lisp to Common Lisp:   KEB  Summer 1987
;;; -----------------------------------------------------------------------------
;;;
;;;
;;;
;;; =============================================================================
;;; Data Type:  <matching set> ::=  { <matching>  <matching> ... <matching> }
;;;
;;; =============================================================================
;;;
;;; makeone.matchingset
;;; -------------------
;;;
;;;       arguments     : matching - <matching> 
;;;
;;;       returns       : <matching set> 
;;;
;;;       description   : makes a singleton <matching set> from 'matching'. 
;;;
;;;                                        written :  vhs 06/08/85
;;;                                        modified:
;;;
;;;
#|
(defmacro makeone.matchingset (matching)

  "Makes a singleton <matching set> from 'matching'."

  `(list ,matching))
|#
;;;
;;; ============================================================================= 
;;;
;;; make.matchingset
;;; ----------------
;;;
;;;       arguments     : uniset - <unification set>
;;;                       tnode - <target node>     <- added:  KEB 06/20/87
;;;
;;;       returns       : <matching set>
;;;
;;;       description   : Builds a <matching set> from <matching>s made
;;;                       by combining each <unification> from uniset with
;;;                       the one <target node> tnode.
;;;
;;;                                        written   :   vhs [I guess]
;;;                          converted and commented :   KEB 06/20/87
#|
(defun make.matchingset (uniset tnode)
  "Builds a <matching set> from <matching>s made
   by combining each <unification> from uniset with
   the one <target node> tnode."
  (declare (special *RENAMESUB* *CHANGED* *MNRS* *SUB*))
  (mapcan
   (function
    (lambda (uni)
      (let ((*SUB* (substitution.uni uni))
	    (*MNRS* (mnoderepset.uni uni))
	    (*CHANGED* (mnodeset.uni uni)))
	(makeone.matchingset 
		(make.matching tnode
		     (make.srcsub *RENAMESUB*)
		     (complete.targetsub *SUB*))))))
   uniset))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; choose.matchingset
;;; ------------------
;;;
;;;       arguments     : matchingset - <matching set> 
;;;
;;;       returns       : <matching> 
;;;
;;;       description   : returns a <matching> from <matching set> 
;;;
;;;                                        written :  vhs 06/08/85
;;;                                        modified:
;;;
;;;
#|
(defmacro choose.matchingset (matchingset)

  "Returns a <matching> from <matching set>."

  `(first ,matchingset))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; others.matchingset
;;; ------------------
;;;
;;;       arguments     : matchingset - <matching set> 
;;;
;;;       returns       : <matching set> 
;;;
;;;       description   : returns a <matching set> like 'matchingset', except
;;;                       that the element that would be chosen by
;;;                       choose.matchingset is removed.
;;;
;;;                                        written :  vhs 06/08/85
;;;                                        modified:
;;;
;;;
#|
(defmacro others.matchingset (matchingset)

  "Returns a <matching set> like 'matchingset', except
   that the element that would be chosen by
   choose.matchingset is removed."

  `(rest ,matchingset))
|#
;;;
;;;
;;; =========================================================================== 

(defmacro do.matchingset ((var matchingsetform &optional resultform) &body forms)
  `(dolist (,var ,matchingsetform ,resultform) ,@forms))

; =============================================================================
;
; isnew.matchingset
; -----------------
;
;       arguments     : ms - <matching set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "ms" is a <new matching set>,
;                               "false" otherwise
;
;                                        written :  rgh 07/30/85
;                                        modified:
;
;
(defmacro isnew.matchingset (ms)
  `(null ,ms))
;
;
;================================================================================



    
    




