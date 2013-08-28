;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: ruiset.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; <rule-use-info set> ::=  { <rule-use-info> ... }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.ruis :  --> <rule-use-info set>
;  ELEMENTS
;
; RECOGNIZERS    isnew.ruis : <rule-use-info set> --> <boolean>
;
; CONSTRUCTORS   makeone.ruis : <rule-use-info> --> <rule-use-info set>
;                putin.ruis   : <rule-use-info> x <rule-use-info set>
;                                  --> <rule-use-info set>
;                update.ruis  : <rule-use-info> x <rule-use-info set>
;                                  --> <rule-use-info set>
;
; SELECTORS      choose.ruis : <rule-use-info set> --> <rule-use-info>
;                others.ruis : <rule-use-info set> --> <rule-use-info set>
;
; =============================================================================
;
; new.ruis
; --------
;
;       returns       : <rule-use-info set>
;
;       description   : returns a "new" <rule-use-info set>
;
;                                        written :  rgh  2/13/86
;                                        modified:
;
;
(defmacro new.ruis ()
  `(new.Set))
;
;
; =============================================================================
;
; isnew.ruis
; ----------
;
;       arguments     : ruis - <rule-use-info set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "ruis" is a "new" <rule-use-info
;                       set>
;
;                                        written :  rgh  2/13/86
;                                        modified:
;
;
(defmacro isnew.ruis (ruis)
  `(isnew.Set ,ruis))
;
;
; =============================================================================
;
; makeone.ruis
; ------------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <rule-use-info set>
;
;       description   : returns a singleton <rule-use-info set> containing
;                       "rui"
;
;                                        written :  rgh  4/03/86
;                                        modified:
;
;
(defmacro makeone.ruis (rui)
  `(makeone.Set ,rui))
;
;
; =============================================================================
;
; putin.ruis
; ----------
;
;       arguments     : rui - <rule-use-info>
;                       ruis - <rule-use-info set>
;
;       returns       : <rule-use-info set>
;
;       description   : returns a <rule-use-info set> consisting of "rui"
;                       added to the elements of "ruis".  Assumes that "rui"
;                       is not already in "ruis".
;
;                                        written :  rgh  4/03/86
;                                        modified:
;
;
(defmacro putin.ruis (rui ruis)
  `(putin.Set ,rui ,ruis))
;
;
; =============================================================================
;
; choose.ruis
; -----------
;
;       arguments     : ruis - <rule-use-info set>
;
;       returns       : <rule-use-info>
;
;       description   : returns an element of "ruis"
;
;                                        written :  rgh  2/13/86
;                                        modified:
;
;
(defmacro choose.ruis (ruis)
  `(choose.Set ,ruis))
;
;
; =============================================================================
;
; others.ruis
; -----------
;
;       arguments     : ruis - <rule-use-info set>
;
;       returns       : <rule-use-info set>
;
;       description   : returns a <rule-use-info set> exactly like "ruis"
;                       except with the element chosen by choose.ruis
;                       removed
;
;                                        written :  rgh  2/13/86
;                                        modified:
;
;
(defmacro others.ruis (ruis)
  `(others.Set ,ruis))
;
;
; =============================================================================
;
; update.ruis
; -----------
;
;       arguments     : updated-rui - <rule-use-info>
;                       ruiset - <rule-use-info set>
;
;       returns       : <rule-use-info set>
;
;       description   : updates "ruiset" by adding "updated-rui" in place of
;                       an already existing <rule-use-info> with the same
;                       <substitution> and not icompatible with it.
;                       "Updated-rui" is merged with the
;                       the already existing <rule-use-info> if one is found,
;                       in that the poscount, negcount and flagged node set
;                       are updated to contain all information contained
;                       in both <rule-use-info>s.
; 
;
;                                        written :  rgh  4/03/86
;                                        modified:  rgh  4/26/86
;                                        modified:  njm 11/09/88
;
;
(defun update.ruis (updated-rui ruiset)
  (cond ((isnew.ruis ruiset) (makeone.ruis updated-rui))
        ((and (iseq.sbst (subst.rui updated-rui)
			 (subst.rui (choose.ruis ruiset)))
	      (compatible.rui updated-rui (choose.ruis ruiset)))
	 (putin.ruis
	   (merge.rui updated-rui (choose.ruis ruiset))
	   (others.ruis ruiset)))
        (t (putin.ruis (choose.ruis ruiset)
		       (update.ruis updated-rui (others.ruis ruiset))))))
;
;
; =============================================================================



    
    




