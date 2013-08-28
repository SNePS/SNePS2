;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: repset.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; <report set>  ::=  { <report> ... <report> }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.repset    :  --> <report set>
;  ELEMENTS
;
; RECOGNIZERS    is.repset     : <universal> --> <boolean>
;                isnew.repset  : <report set> --> <boolean>
;
; CONSTRUCTORS   makeone.repset: <report> --> <report set>
;                insert.repset : <report> x <report set> --> <report set>
;                putin.repset  : <report> x <report set> --> <report set>
;                union.repset  : <report set> x <report set> --> <report set>
;
; SELECTORS      choose.repset : <report set> --> <report>
;                others.repset : <report set> --> <report set>
;                remove.repset : <report> x <report set> --> <report set>
;
; TESTS          ismemb.repset : <report> x <report set> --> <boolean>
;
; UTILITY        addbinding.repset : <mbind> x <report set> --> <report set>
;
; =============================================================================
;
; new.repset
; ----------
;
;       returns       : <report set>
;
;       description   : returns a "new" <report set>
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro new.repset ()
  `(new.Set))
;
;
; =============================================================================
;
; is.repset
; ---------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <report set>,
;                               "false" otherwise
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro is.repset (u)
  `(and (is.Set ,u)
        (is.rep (choose.Set ,u))))
;
;
; =============================================================================
;
; isnew.repset
; ------------
;
;       arguments     : rs - <report set>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "rs" is a "new" <report set>,
;                               "false" otherwise
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro isnew.repset (rs)
  `(isnew.Set ,rs))
;
;
; =============================================================================
;
; makeone.repset
; --------------
;
;       arguments     : r - <report>
;
;       returns       : <report set>
;
;       description   : returns a <report set> containing the single element
;                        "r"
;
;                                        written :  rgh 07/30/85
;                                        modified:
;
;
(defmacro makeone.repset (r)
  `(makeone.Set ,r))
;
;
; =============================================================================
;
; insert.repset
; -------------
;
;       arguments     : r - <report>
;                       repset - <report set>
;
;       returns       : <report set>
;
;       description   : returns the <report set> "repset" with the <report> "r"
;                        inserted
;
;                                        written :  rgh 07/30/85
;                                        modified:  rgh 11/30/85
;
;
(defmacro insert.repset (r repset)
  `(insert.Set ,r ,repset 'iseq.rep))
;
;
; =============================================================================
;
; putin.repset
; ------------
;
;       arguments     : rep - <report>
;                       repset - <report set>
;
;       returns       : <report set>
;
;       description   : returns "repset" with "rep" added to it, whether it
;                       was there before or not
;
;                                        written :  rgh  4/24/86
;                                        modified:
;
;
(defmacro putin.repset (rep repset)
  `(putin.Set ,rep ,repset))
;
;
; =============================================================================
;
; union.repset
; ------------
;
;       arguments     : s1 - <report set>
;                       s2 - <report set>
;
;       returns       : <report set>
;
;       description   : returns the set union of two <report set>s
;
;                                        written :  rgh 07/29/85
;                                        modified:  rgh 11/30/85
;
;
(defmacro union.repset (s1 s2)
  `(union.Set ,s1 ,s2 'iseq.rep))
;
;
; =============================================================================
;
; choose.repset
; -------------
;
;       arguments     : repset - <report set>
;
;       returns       : <report>
;
;       description   : returns the first <report> in "repset"
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro choose.repset (repset)
  `(choose.Set ,repset))
;
;
; =============================================================================
;
; others.repset
; -------------
;
;       arguments     : repset - <report set>
;
;       returns       : <report set>
;
;       description   : returns a <report set> consisting of all of the
;                        <report>s in "repset" except the first
;
;                                        written :  rgh 07/29/85
;                                        modified:
;
;
(defmacro others.repset (repset)
  `(others.Set ,repset))
;
;
;;; =============================================================================
;;;
;;; do.repset
;;; ---------
;;;
;;;       arguments     : var - a local variable
;;;                       repsetform - a form evaluating to a <report set>
;;;                       resultform - a form evaluating to a <report set>
;;;                       forms - a sequence of Lisp forms
;;;
;;;       returns       : <report set> or nil
;;;
;;;       description   : Evaluates the sequence of forms in order once
;;;                       for val bound to sucessive elements of the value of repsetform
;;;                       Then returns the value of resultform.
;;;                       If resultform is omitted, returns nil.
;;;
;;;                                        written:  scs 03/03/88
;;;

(defmacro do.repset ((var repsetform &optional resultform) &body forms)
  `(do.set (,var ,repsetform ,resultform) ,@forms))
;;;
;;;
; =============================================================================
;
; remove.repset
; -------------
;
;       arguments     : report - <report>
;                       repset - <report set>
;
;       returns       : <report set>
;
;       description   : returns a <report set> identical to "repset" with
;                        "report" removed if it was there
;
;                                        written :  rgh 11/10/85
;                                        modified:  rgh 11/30/85
;
;
(defmacro remove.repset (report repset)
  `(remove.Set ,report ,repset 'iseq.rep))
;
;
; =============================================================================
;
; ismemb.repset
; -------------
;
;       arguments     : report - <report>
;                       repset - <report set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "report" is in "repset",
;                               "false" otherwise
;
;                                        written :  rgh 11/10/85
;                                        modified:  rgh 11/30/85
;
;
(defmacro ismemb.repset (report repset)
  `(ismemb.Set ,report ,repset 'iseq.rep))
;
;
; =============================================================================
;
; addbinding.repset
; -----------------
;
;       arguments     : mb - <mbind>
;                       repset - <report set>
;
;       returns       : <report set>
;
;       description   : inserts the binding "mb" into the <substitution> of
;                        each of the <report>s in "repset"
;
;                                        written :  rgh 08/05/85
;                                        modified:
;
;
(defmacro addbinding.repset (mb repset)
  `(let ((result (new.repset)))
     (do.repset (next-rep ,repset result)
	(setq result (insert.repset (addbinding.rep ,mb next-rep) result)))))
;
;
; =============================================================================



    
    




