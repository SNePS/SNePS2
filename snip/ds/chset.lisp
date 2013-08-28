;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: chset.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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




(in-package :snip)


; =============================================================================
;
; <channel set>  ::=  { <channel> ... <channel> }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.chset     :  --> <channel set>
;  ELEMENTS
;
; RECOGNIZERS    is.chset      :  <universal> --> <boolean>
;                isnew.chset   :  <channel set> --> <boolean>
;
; CONSTRUCTORS   putin.chset   :  <channel> x <channel set> --> <channel set>
;                insert.chset  :  <channel> x <channel set> --> <channel set>
;
; SELECTORS      choose.chset  :  <channel set> --> <channel>
;                others.chset  :  <channel set> --> <channel set>
;                remove.chset  :  <channel> x <channel set> --> <channel set>
;
; =============================================================================
;
; new.chset
; ---------
;
;       returns       : <channel set>
;
;       description   : returns a "new" <channel set>
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro new.chset ()
  `(new.Set))
;
;
; =============================================================================
;
; is.chset
; --------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <channel set>,
;                               "false" otherwise
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro is.chset (u)
  `(and (is.Set ,u)
        (is.ch (choose.Set ,u))))
;
;
; =============================================================================
;
; isnew.chset
; -----------
;
;       arguments     : chset - <channel set>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "chset" is a "new" <channel set>,
;                               "false" otherwise
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro isnew.chset (chset)
  `(isnew.Set ,chset))
;
;
; =============================================================================
;
; putin.chset
; -----------
;
;       arguments     : ch - <channel>
;                       chset - <channel set>
;
;       returns       : <channel set>
;
;       description   : returns "chset" with the <channel> "ch" inserted.
;                       (it assumes that "ch" is not already in "chset")
;
;                                        written :  rgh 08/29/85
;                                        modified:
;
;
(defmacro putin.chset (ch chset)
  `(putin.Set ,ch ,chset))
;
; =============================================================================
;
; choose.chset
; ------------
;
;       arguments     : chset - <channel set>
;
;       returns       : <channel>
;
;       description   : returns the first <channel> in "chset"
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro choose.chset (chset)
  `(choose.Set ,chset))
;
;
; =============================================================================
;
; others.chset
; ------------
;
;       arguments     : chset - <channel set>
;
;       returns       : <channel set>
;
;       description   : returns a <channel set> consisting of all of the
;                        <channel>s in "chset" except the first
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro others.chset (chset)
  `(others.Set ,chset))
;
;
; =============================================================================
;
; insert.chset
; ------------
;
;       arguments     : ch - <channel>
;                       chset - <channel set>
;
;       returns       : <channel set>
;
;       description   : returns "chset" with the <channel> "ch" inserted, if
;                       it was not already there
;
;                                        written :  rgh 08/25/85
;                                        modified:  rgh 11/30/85
;
;
(defmacro insert.chset (ch chset)
  `(prog (c cs)
	 (setq cs ,chset)
      begin
	 (if (isnew.chset cs) (return (putin.Set ,ch ,chset)))
	 (setq c (choose.chset cs))
	 (cond ((equivalent.ch c ,ch)
		(setf (valve.ch c) (valve.ch ,ch))
		(return ,chset)))
	 (setq cs (others.chset cs))
	 (go begin)))
;
;
;;; =============================================================================
;;;
;;; do.chset
;;; --------
;;;
;;;       arguments     : var - a local variable
;;;                       chsetform - a form evaluating to a <channel set>
;;;                       resultform - a form evaluating to a <channel set>
;;;                       forms - a sequence of Lisp forms
;;;
;;;       returns       : <channel set> or nil
;;;
;;;       description   : Evaluates the sequence of forms in order once
;;;                       for val bound to sucessive elements of the value of chsetform
;;;                       Then returns the value of resultform.
;;;                       If resultform is omitted, returns nil.
;;;
;;;                                        written:  scs 03/14/88
;;;

(defmacro do.chset ((var chsetform &optional resultform) &body forms)
  `(do.set (,var ,chsetform ,resultform) ,@forms))
;;;
;;;
; =============================================================================
;
; remove.chset
; ------------
;
;       arguments     : ch - <channel>
;                       chset - <channel set>
;
;       returns       : <channel set>
;
;       description   : returns "chset" with "ch" removed, if it was there
;
;                                        written :  rgh 08/25/85
;                                        modified:  rgh 11/30/85
;
;
(defmacro remove.chset (ch chset)
  `(remove.Set ,ch ,chset equivalent.ch))
;
;
; =============================================================================



    
    




