;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: setnorm.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
;;;
;;; =============================================================================
;;; Data Type:  <set> : (<setelem> <setelem> ... <setelem>)
;;;
;;; NB: In general, <setelem>s will be non-atomic
;;;     Here, sets are represented as lists without duplicates.
;;; =============================================================================
;;;
;;; new.set 
;;; -------
;;;
;;;       returns       : <set>
;;;
;;;       description   : creates a <newset>
;;;
;;;                                        written:  rmr 06/18/84 
;;;                                        modified:
;;;

(defmacro new.set ()
   "Creates a <newset>."
   `()) 
;;;
;;;
;;; =============================================================================
;;;
;;; is.set
;;; ------
;;;
;;;       arguments     : u - <universal>
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : returns 'true' if 'u' is a <set>, 'false' otherwise.
;;;
;;;                                        written : rmr 06/18/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro is.set (u)
   "Returns 'true' if 'u' is a <set>, 'false' otherwise."
   `(listp ,u))
;;;
;;;
;;; =============================================================================
;;;
;;; isnew.set 
;;; ---------
;;;
;;;       arguments     : s - <set>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : returns 'true' if 's' is a <newset>,
;;;                       'false' otherwise.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro isnew.set (s)
   "Returns 'true' if 's' is a <newset>, 'false' otherwise."
   `(null ,s))
;;;
;;;
;;; =============================================================================
;;;
;;; insert.set 
;;; ----------
;;;
;;;       arguments     : se - <setelem>
;;;                       s  - <set>
;;;                       eqfn - an equality predicate function
;;;
;;;       returns       : <set> 
;;;
;;;       description   : returns a <set> identical to 's' but with 'se' as
;;;                       a new <set> if 'se' was not yet in 's'. 
;;;                       If 'se' was in 's' it just returns 's' unchanged.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;                                        modified: scs 03/03/88
;;;                                        modified: scs 03/09/88
;;;

(defmacro insert.set (se s &optional (eqfn '#'equal))
  "Returns a <set> identical to 's' but with 'se' as
    a new <setelem> if 'se' was not yet in 's'. 
    If 'se' was in 's' it just returns 's' unchanged.
    An equality test function is optional."
  `(adjoin ,se ,s :test ,eqfn))
;;;
;;;
;;; =============================================================================
;;;
;;; make.set
;;; --------
;;;
;;;       arguments     : se ... se - <setelem>s
;;;
;;;       returns       : <set>
;;;
;;;       description   : returns a <set> composed of the <setelem>s passed as
;;;                       arguments.
;;;
;;;       implementation: It assumes there is no repetitions of the same <setelem>
;;;                       in the argument list in order to avoid extra checking.
;;;
;;;                                        written :  rmr 06/25/84 
;;;                                        modified:
;;;

(defmacro make.set (&rest s)
   "Returns a <set> composed of the <setelem>s passed as arguments.
    (It assumes there is no repetitions of the same <setelem>
     in the argument list in order to avoid extra checking.)"
   `',s)
;;;
;;;
;;; =============================================================================
;;;
;;; makeone.set
;;; -----------
;;;
;;;       arguments     : se - <setelem>
;;;
;;;       returns       : <set>
;;;
;;;       description   : returns a <set> composed of the <setelem>.
;;;
;;;       note          : This function should be used whenever one wants to
;;;                       create a <set> with just one <setelem>.
;;;                       Don't use the alternative forms
;;;
;;;                                 (make.set se)
;;;                       or
;;;
;;;                                 (insert.set se (new.set))
;;;
;;;                       in order to improve efficiency.
;;;
;;;                                        written :  rmr 06/25/84 
;;;                                        modified:
;;;

(defmacro makeone.set (se)
   "This function should be used whenever one wants to
    create a <set> with just one <setelem>."
   `(list ,se))
;
;
; =============================================================================
;
; putin.Set 
; ----------
;
;       arguments     : u - <universal>
;                       s - <Set>
;
;       returns       : <Set> 
;
;       description   : returns a <Set> identical to "s" but with "u"
;                       inserted.  It does not check to see if "u" was already
;                       in the <Set> "s".
;
;                                        written:  rgh 08/29/85
;                                        modified:
;
;
(defmacro putin.Set (u s)
   `(cons ,u ,s))
;;;
;;;
;;; =============================================================================
;;;
;;; compl.set 
;;; ---------
;;;
;;;       arguments     : s1 - <set>
;;;                       s2 - <set> 
;;;
;;;       returns       : <set>
;;;
;;;       description   : returns the set difference s1 - s2. 
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro compl.set (s1 s2)
   "Returns the set difference s1 - s2."
   `(set-difference ,s1 ,s2 :test #'equal))     
;;;
;;; =============================================================================
;;;
;;; union.set 
;;; ---------
;;;
;;;       arguments     : s1 - <set>
;;;                       s2 - <set>
;;;                       eqfn - optional equality function
;;;
;;;       returns       : <set>
;;;
;;;       description   : returns the union of 's1' and 's2'.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;                                        modified: scs 03/03/88
;;;

(defmacro union.set (s1 s2 &optional (eqfn '#'equal))
  "Returns the union of 's1' and 's2'."
  `(union ,s1 ,s2 :test ,eqfn))
;;;
;;;
;;; =============================================================================
;;;
;;; remove.set 
;;; ----------
;;;
;;;       arguments     : se - <setelem>
;;;                       s  - <set>
;;;                       eqfn - an optional equality function
;;;
;;;       returns       : <set> 
;;;
;;;       description   : returns a <set> identical to 's' but with 'se'
;;;                       removed if it was there. If 'se' was not i n's',
;;;                       it just returns 's' unchanged.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;                                        modified: scs 03/03/88
;;;

(defmacro remove.set (se s &optional (eqfn '#'equal))
   "Returns a <set> identical to 's' but with 'se'
    removed if it was there. If 'se' was not in 's',
    it just returns 's' unchanged."
   `(remove ,se ,s :test ,eqfn))
;;;
;;;
;;; =============================================================================
;;;
;;; choose.set 
;;; ----------
;;;
;;;       arguments     : s - <set>
;;;
;;;       returns       : <setelem> 
;;;
;;;       description   : returns a <setelem> of 's'.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro choose.set (s)
   "Returns a <setelem> of 's'."
   `(first ,s))
;;;
;;;
;;; =============================================================================
;;;
;;; others.set 
;;; ----------
;;;
;;;       arguments     : s - <set>
;;;
;;;       returns       : <set>
;;;
;;;       description   : returns a <set> identical to 's' but without
;;;                       the element that would be chosen by choose.set.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro others.set (s)
   "Returns a <set> identical to 's' but without
    the element that would be chosen by choose.set."
   `(rest ,s))
;;;
;;;
;;; =============================================================================
;;;
;;; do.set 
;;; ------
;;;
;;;       arguments     : var - a local variable
;;;                       setform - a form evaluating to a <set>
;;;                       resultform - a form evaluating to a <set>
;;;                       forms - a sequence of Lisp forms
;;;
;;;       returns       : <set>
;;;
;;;       description   : Evaluates the sequence of forms in order once
;;;                       for val bound to sucessive elements of the value of setform
;;;                       Then returns the value of resultform.
;;;                       If resultform is omitted, returns nil.
;;;
;;;                                        written:  scs 03/03/88
;;;

(defmacro do.set ((var setform &optional resultform) &body forms)
  `(dolist (,var ,setform ,resultform) ,@forms))
;;;
;;;
;;; =============================================================================
;;;
;;; intersect.set   
;;; -------------
;;;
;;;       arguments     : s1 - <set>
;;;                       s2 - <set>
;;;
;;;       returns       : <set>
;;;
;;;       description   : returns the intersection of 's1' and 's2'.
;;;
;;;
;;;                                        written:  rm 06/25/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro intersect.set (s1 s2)
  "Returns the intersection of 's1' and 's2'."
  `(intersection ,s1 ,s2 :test #'equal))
;;;
;;;
;;; =============================================================================
;;;
;;; ismemb.set    
;;; ----------
;;;
;;;       arguments     : se - <setelem>
;;;                       s  - <set>
;;;                       eqfn - optional equality function
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : returns 'true' if 'se' is an element of 's',
;;;                       'false' otherwise.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;                                        modified: scs 03/03/88
;;;

(defmacro ismemb.set (se s &optional (eqfn '#'equal))
  "Returns 'true' if 'se' is an element of 's', 'false' otherwise."
  `(member ,se ,s :test ,eqfn))
;;;
;;;
;;; =============================================================================
;;;
;;; issubset.set
;;; ------------
;;;
;;;       arguments     : s1 - <set>
;;;                       s2 - <set>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : returns 'true' if 's1' is a subset of 's2', 
;;;                       'false' otherwise.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified: scs 02/01/88
;;;

(defmacro issubset.set (s1 s2)
  "Returns 'true' if 's1' is a subset of 's2', 'false' otherwise."
  `(subsetp ,s1 ,s2 :test #'equal))
;;;
;;;
;;; =============================================================================
;;;
;;; iseq.set 
;;; --------
;;;
;;;       arguments     : s1 - <set>
;;;                       s2 - <set>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : Compares 's1' and 's2' as sets : returns 'true' if
;;;                       's1' and 's2' have exactly the same elements,
;;;                       'false' otherwise.
;;;
;;;                                        written:  rmr 06/25/84 
;;;                                        modified:
;;;

(defmacro iseq.set (s1 s2)
   "Compares 's1' and 's2' as sets : returns 'true' if
    's1' and 's2' have exactly the same elements,
    'false' otherwise."
   `(and (issubset.set ,s1 ,s2)
         (issubset.set ,s2 ,s1)))
;;;
;;; =============================================================================
;;;
;;; makeset  
;;; -------
;;;
;;;       arguments     : bag - <univ. list>
;;;
;;;       returns       : <set>
;;;
;;;       description   : converts bag to a set, i.e., removes duplicate elts.
;;;
;;;
;;;                                        written: 
;;;                                        modified: scs 02/01/88
;;;

(defmacro makeset (bag)
  `(remove-duplicates ,bag :test #'equal))
;;;
;;; =============================================================================




    
    




