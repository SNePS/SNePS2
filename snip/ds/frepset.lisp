;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: frepset.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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
; <freportset>  ::=  { <freport> ... <freport> }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.frepset    :  --> <freportset>
;  ELEMENTS
;
; RECOGNIZERS    is.frepset     : <universal> --> <boolean>
;                isnew.frepset  : <freportset> --> <boolean>
;
; CONSTRUCTORS   makeone.frepset: <freport> --> <freportset>
;                insert.frepset : <freport> x <freportset> --> <freportset>
;                putin.frepset  : <freport> x <freportset> --> <freportset>
;                union.frepset  : <freportset> x <freportset> --> <freportset>
;
; SELECTORS      choose.frepset : <freportset> --> <freport>
;                others.frepset : <freportset> --> <freportset>
;                remove.frepset : <freport> x <freportset> --> <freportset>
;
; TESTS          ismemb.frepset : <freport> x <freportset> --> <boolean>
;
; UTILITY        addbinding.frepset : <mbind> x <freportset> --> <freportset>
;
; =============================================================================
;
; new.frepset
; -----------
;
;       returns       : <freportset>
;
;       description   : returns a "new" <freportset>
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro new.frepset ()
  `(new.Set))
;
;
; =============================================================================
;
; is.frepset
; ----------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <freportset>,
;                               "false" otherwise
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro is.frepset (u)
  `(and (is.Set ,u)
        (is.rep (choose.Set ,u))))
;
;
; =============================================================================
;
; isnew.frepset
; ------------
;
;       arguments     : frs - <freportset>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "rs" is a "new" <freportset>,
;                               "false" otherwise
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro isnew.frepset (frs)
  `(isnew.Set ,frs))
;
;
; =============================================================================
;
; makeone.frepset
; --------------
;
;       arguments     : fr - <freport>
;
;       returns       : <freportset>
;
;       description   : returns a <freportset> containing the single element
;                        "fr"
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro makeone.frepset (fr)
  `(makeone.Set ,fr))
;
;
; =============================================================================
;
; insert.frepset
; --------------
;
;       arguments     : fr - <freport>
;                       frepset - <freportset>
;
;       returns       : <freportset>
;
;       description   : returns the <freportset> "frepset" with the <freport> "fr"
;                        inserted
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;
;
(defmacro insert.frepset (fr frepset)
  `(insert.Set ,fr ,frepset 'iseq.frep))
;
;
; =============================================================================
;
; putin.frepset
; -------------
;
;       arguments     : frep - <freport>
;                       frepset - <freportset>
;
;       returns       : <freportset>
;
;       description   : returns "frepset" with "frep" added to it, whether it
;                       was there before or not
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro putin.frepset (frep frepset)
  `(putin.Set ,frep ,frepset))
;
;
; =============================================================================
;
; union.frepset
; ------------
;
;       arguments     : s1 - <freportset>
;                       s2 - <freportset>
;
;       returns       : <freportset>
;
;       description   : returns the set union of two <freportset>s
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;
;
(defmacro union.frepset (s1 s2)
  `(union.Set ,s1 ,s2 'iseq.frep))
;
;
; =============================================================================
;
; choose.frepset
; --------------
;
;       arguments     : frepset - <freportset>
;
;       returns       : <freport>
;
;       description   : returns the first <freport> in "frepset"
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro choose.frepset (frepset)
  `(choose.Set ,frepset))
;
;
; =============================================================================
;
; others.frepset
; --------------
;
;       arguments     : frepset - <freportset>
;
;       returns       : <freportset>
;
;       description   : returns a <freportset> consisting of all of the
;                        <freport>s in "frepset" except the first
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro others.frepset (frepset)
  `(others.Set ,frepset))
;
;
;;; =============================================================================
;;;
;;; do.frepset
;;; ----------
;;;
;;;       arguments     : var - a local variable
;;;                       frepsetform - a form evaluating to a <freportset>
;;;                       resultform - a form evaluating to a <freportset>
;;;                       forms - a sequence of Lisp forms
;;;
;;;       returns       : <freportset> or nil
;;;
;;;       description   : Evaluates the sequence of forms in order once
;;;                       for val bound to sucessive elements of the value of frepsetform
;;;                       Then returns the value of resultform.
;;;                       If resultform is omitted, returns nil.
;;;
;;;                                        written:  cpf 10/25/88
;;;

(defmacro do.frepset ((var frepsetform &optional resultform) &body forms)
  `(do.set (,var ,frepsetform ,resultform) ,@forms))
;;;
;;;
; =============================================================================
;
; remove.frepset
; --------------
;
;       arguments     : freport- <freport>
;                       frepset - <freportset>
;
;       returns       : <freportset>
;
;       description   : returns a <freportset> identical to "frepset" with
;                        "report" removed if it was there
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;
;
(defmacro remove.frepset (freport frepset)
  `(remove.Set ,freport,frepset 'iseq.frep))
;
;
; =============================================================================
;
; ismemb.frepset
; -------------
;
;       arguments     : freport- <freport>
;                       frepset - <freportset>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "report" is in "repset",
;                               "false" otherwise
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;
;
(defmacro ismemb.frepset (freport frepset)
  `(ismemb.Set ,freport ,frepset 'iseq.frep))
;
;
; =============================================================================
;
; addbinding.frepset
; ------------------
;
;       arguments     : mb - <mbind>
;                       frepset - <freportset>
;
;       returns       : <freportset>
;
;       description   : inserts the binding "mb" into the <substitution> of
;                        each of the <freport>s in "repset"
;
;                                        written :  cpf 10/25/88
;                                        modified:
;
;
(defmacro addbinding.frepset (mb frepset)
  `(let ((result (new.frepset)))
     (do.frepset (next-frep ,frepset result)
	(setq result (insert.frepset (addbinding.frep ,mb next-frep) result)))))
;
;
; =============================================================================



    
    




