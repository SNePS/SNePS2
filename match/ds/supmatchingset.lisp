;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: supmatchingset.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :match)


;;; -----------------------------------------------------------------------------
;;;       Ported from Franz Lisp to Common Lisp:   KEB  Summer 1987
;;; -----------------------------------------------------------------------------
;;;
;;;
;;; =============================================================================
;;; Data Type:  <supmatching set> ::=  { <supmatching>  <supmatching> ... <supmatching> }
;;;
;;; =============================================================================
;;;
;;; new.supmatchingset
;;; ------------------
;;;
;;;       arguments     : none 
;;;
;;;       returns       : <supmatching set> 
;;;
;;;       description   : makes a new <supmatching set> 
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:
;;;
;;;
(defmacro new.supmatchingset ()
  "Makes a new <supmatching set>"
  `())
;;;
;;; ============================================================================= 
;;;
;;; makeone.supmatchingset
;;; ----------------------
;;;
;;;       arguments     : supmatching - <supmatching> 
;;;
;;;       returns       : <supmatching set> 
;;;
;;;       description   : makes a singleton <supmatching set> from 'supmatching'. 
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:
;;;
;;;
(defmacro makeone.supmatchingset (supmatching)
  "Makes a singleton <supmatching set> from 'supmatching'."
  `(list ,supmatching))
;;;
;;; ============================================================================= 
;;;
;;; putin.supmatchingset
;;; --------------------
;;;
;;;       arguments     : supmatching    - <supmatching>
;;;                       supmatchingset - <supmatching set>
;;;
;;;       returns       : <supmatching set>
;;;
;;;       description   : Returns a new <supmatching set> with 'supmatching' 
;;;                       inserted in 'supmatchingset'.
;;;
;;;                                        written   :  cpf/njm 10/19/88 
;;;
;;;
(defmacro putin.supmatchingset (supmatching supmatchingset)
  "Returns a new <supmatching set> with 'supmatching' inserted in
   'supmatchingset'"
  `(cons ,supmatching ,supmatchingset))   
;;;
;;;
;;; =============================================================================
;;;
;;; choose.supmatchingset
;;; ---------------------
;;;
;;;       arguments     : supmatchingset - <supmatching set> 
;;;
;;;       returns       : <supmatching> 
;;;
;;;       description   : returns a <supmatching> from <supmatching set> 
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:
;;;
;;;
(defmacro choose.supmatchingset (supmatchingset)
  "Returns a <supmatching> from <supmatching set>."
  `(first ,supmatchingset))
;;;
;;;
;;; =============================================================================
;;;
;;; others.supmatchingset
;;; ---------------------
;;;
;;;       arguments     : supmatchingset - <supmatching set> 
;;;
;;;       returns       : <supmatching set> 
;;;
;;;       description   : returns a <supmatching set> like 'supmatchingset', except
;;;                       that the element that would be chosen by
;;;                       choose.supmatchingset is removed.
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:
;;;
;;;
(defmacro others.supmatchingset (supmatchingset)
  "Returns a <supmatching set> like 'supmatchingset', except
   that the element that would be chosen by
   choose.supmatchingset is removed."
  `(rest ,supmatchingset))
;;;
;;;
;;; =========================================================================== 

(defmacro do.supmatchingset ((var supmatchingsetform &optional resultform) &body forms)
  `(dolist (,var ,supmatchingsetform ,resultform) ,@forms))

; =============================================================================
;
; isnew.supmatchingset
; --------------------
;
;       arguments     : ms - <supmatching set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "ms" is a <new supmatching set>,
;                               "false" otherwise
;
;                                        written :  cpf/njm 10/19/88
;                                        modified:
;
;
(defmacro isnew.supmatchingset (ms)
  `(null ,ms))
;
;
; =============================================================================
;
; matchingset-to-supmatchingset
; -----------------------------
;
;       arguments     : ms - <matching set>
;                       ct - <context>
;
;       returns       : <supmatching>
;
;       description   : returns "true" if "ms" is a <new supmatching set>,
;                               "false" otherwise
;
;                                        written :  njm 10/19/88
;                                        modified:
;
;
(defmacro matchingset-to-supmatchingset (ms ct)
  `(let ((newsupmatch (new.supmatchingset)))
     (do.matchingset (m ,ms newsupmatch)
       (setq newsupmatch
	     (putin.supmatchingset
	       (make.supmatching (tnode.matching m) 
				 (target-sub.matching m)
				 (snip:filter.sup
				   (sneps:node-asupport (tnode.matching m))
				   ,ct)
				 (source-sub.matching m))
	       newsupmatch)))))
;
;
; =============================================================================



    
    




