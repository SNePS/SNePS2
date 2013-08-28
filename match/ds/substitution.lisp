;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: substitution.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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
;;; =============================================================================
;;; Data Type:  <substitution> ::= {<mbind>  <mbind> ... <mbind>}
;;;
;;;         
;;; R[<substitution>] ::= ( R[<mbind>] R[<mbind>]  ... R[<mbind>] )
;;;
;;; CHANGE
;;; =====
;;;
;;; For sake of efficiency, generality, and elimination of loops of macros
;;; defined in terms of each other, this is being changed to:
;;;
;;; Data Type:  <substitution> ::= <Lisp association list>
;;;                            ::= (... (<key> . <datum>) ...)
;;;
;;; Note that the <key> could be a <node> so #'equal is the appropriate test.
;;;
;;;                     -- SCS 2/9/88
;;;
;;; =============================================================================
;;;
;;; new.sbst 
;;; --------
;;;
;;;       returns       : <substitution>
;;;
;;;       description   : creates a <new substitution>.
;;;
;;;                                        written:  vhs 11/19/84
;;;                                        modified:
;;;
;;;
(defmacro new.sbst ()

   "Creates a <new substitution>."

   `())
;;;
;;;
;;; =============================================================================
;;;
;;; is.sbst 
;;; -------
;;;
;;;       arguments     : u - <universal>
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : Returns 'true' if 'u' is a <substitution>, 
;;;                       'false' otherwise.
;;;
;;;       implementation: It does not check if all elements of 'u' are <consp>s,
;;;                       it checks just the first one.
;;;
;;;                                        written:  vhs 11/19/84
;;;                                        modified: scs 02/09/88
;;;
;;;
(defmacro is.sbst (u)
  "Returns 'true' if 'u' is a <substitution>, 'false' otherwise.
    (It does not check if all elements of 'u' are <mbind>s,
     it checks just the first one.)"
  `(let ((iu ,u))
     (and (listp iu)
	  (or (null iu)
	      (consp (first iu))))))
;;;
;;;
;;; =============================================================================
;;;
;;; isnew.sbst 
;;; ----------
;;;
;;;       arguments     : sbst - <substitution> 
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : Returns 'true' if 'sbst' is a <new substitution>,
;;;                       'false' otherwise.
;;;
;;;                                        written:  vhs 11/19/84
;;;                                        modified: scs 02/09/88
;;;
(defmacro isnew.sbst (sbst)
  "Returns 'true' if 'sbst' is a <new substitution>, 'false' otherwise."
  `(null ,sbst))
;;;
;;;
;;; =============================================================================
;;;
;;; insert.sbst
;;; -----------
;;;
;;;        arguments        : qsbst - <substitution>
;;;                           mb    - (<key> . <datum>)
;;;                           sbst  - <substitution>
;;;
;;;        returns          : <substitution>
;;;
;;;        description      : inserts 'mb' into 'sbst' and the sets the value of 
;;;                           'qsbst' to that.
;;;
;;;                                        written:  vhs  12/09/84
;;;                                        modified: scs  02/09/88
;;;                                        modified: scs  02/11/88
;;;
(defmacro insert.sbst (qsbst mb sbst)
  "Inserts 'mb' into 'sbst' and the sets the value of 'qsbst' to that."
  `(set ,qsbst (cons ,mb ,sbst)))
;;;
;;;
;;; ==============================================================================
;;;
;;; update.sbst
;;; -----------
;;;
;;;       arguments      : qsbst - <substitution>
;;;                        mn1   - <datum>
;;;                        mn2   - <datum>
;;;                        sbst  - <substitution>
;;;
;;;       returns        : <substitution>
;;;
;;;       description    : changes an occurrence of mn1 to mn2 as the <datum> of
;;;                        a (<key> . <datum>) pair in <sbst>.
;;;
;;;       side-effects   : the value of qsbst                       
;;;
;;;                                        written:  vhs  06/14/85
;;;                                        modified: scs  02/09/88
;;;
;;;
(defmacro update.sbst (qsbst mn1 mn2 sbst)
  "Changes an occurrence of mn1 to mn2 as the <mnode> of an <mbind> in <sbst>."
  `(set ,qsbst (subst ,mn2 ,mn1 ,sbst :test #'equal)))
;;;
;;; ==============================================================================
;;;
;;; choose.sbst
;;; -----------
;;;
;;;        arguments        : sbst - <substitution>
;;;
;;;        returns          : (<key> . <datum>) pair
;;;
;;;        description      : returns a pair of 'sbst'.
;;;
;;;                                        written:  vhs  12/09/84
;;;                                        modified: scs  02/09/88
;;;
;;;
(defmacro choose.sbst (sbst)
  "Returns a pair of 'sbst'."
  `(first ,sbst))
;;;
;;;
;;; ==============================================================================
;;;
;;; others.sbst
;;; -----------
;;;
;;;        arguments         : sbst - <substitution>
;;;
;;;        returns           : <substitution>
;;;
;;;        description       : returns a <substitution> identical to 'sbst' but
;;;                            without the element that would be chosen by
;;;                            first.sbst.
;;;
;;;                                        written:  vhs  12/09/84
;;;                                        modified: scs  02/09/88
;;;
;;;
(defmacro others.sbst (sbst)
  "Returns a <substitution> identical to 'sbst' but
   without the element that would be chosen by
   first.sbst."
  `(rest ,sbst))
;;;
;;;
;;; =============================================================================
;;;
;;; do.sbst
;;; -------
;;;
;;;       arguments     : var - a local variable
;;;                       sbstform - a form evaluating to a <substitution>
;;;                       resultform - a form evaluating to a <substitution>
;;;                       forms - a sequence of Lisp forms
;;;
;;;       returns       : <substitution> or nil
;;;
;;;       description   : Evaluates the sequence of forms in order once
;;;                       for val bound to sucessive elements of the value of sbstform
;;;                       Then returns the value of resultform.
;;;                       If resultform is omitted, returns nil.
;;;
;;;                                        written:  scs 03/03/88
;;;

(defmacro do.sbst ((var sbstform &optional resultform) &body forms)
  `(dolist (,var ,sbstform ,resultform) ,@forms))
;;;
;;; ==============================================================================
;;;
;;; isbound.sbst
;;; ------------
;;;
;;;       arguments     : mv   - <mvar>
;;;                       sbst - <substitution>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : It returns t if there is a pair in 'sbst' which 
;;;                       has 'mv' as the <key>. If there is no such pair
;;;                       it returns 'false'.
;;;
;;;                                        written :  vhs 11/19/84
;;;                                        modified:  rgh  3/28/86
;;;                                        modified:  scs 02/09/88
;;;
;;;
(defmacro isbound.sbst (mv sbst)
  "It returns t if there is a pair in 'sbst' which 
   has 'mv' as the <key>. If there is no such pair
   it returns 'false'."
  `(assoc ,mv ,sbst :test #'equal))
;;;
;;;
;;; =============================================================================
;;;
;;; isvalue.sbst
;;; ------------
;;;
;;;       arguments     : mn   - <datum>
;;;                       sbst - <substitution>
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : it returns t if there is a pair in 'sbst' which
;;;                       has 'mn' as its <datum>, false otherwise.
;;;
;;;                                        written :  vhs 04/24/85
;;;                                        modified:  rgh  3/28/86
;;;                                        modified:  scs 01/29/88
;;;                                        modified:  scs 02/09/88
;;;
(defmacro isvalue.sbst (mn sbst)
  "It returns t if there is a pair in 'sbst' which
   has 'mn' as its <datum>, false otherwise."
  `(rassoc ,mn ,sbst :test #'equal))
;;;
;;;
;;;
;;; =============================================================================
;;;
;;; value.sbst
;;; ----------
;;;
;;;       arguments     : mn   - <key>
;;;                       sbst - <substitution>
;;;
;;;       returns       : <datum> 
;;;
;;;       description   : if 'mn' is a <key> which is bound in 'sbst', then 
;;;                       it returns the <datum> to which 'mn' is bound',
;;;                       otherwise it just returns 'mn'.
;;;
;;;                                        written :  vhs 12/31/84
;;;                                        modified: scs 2/9/88
;;;
;;;
(defmacro value.sbst (mn sbst)
  "If 'mn' is a <key> which is bound in 'sbst', then 
   it returns the <datum> to which 'mn' is bound',
   otherwise it just returns 'mn'."
  `(let ((key ,mn))
     (or (cdr (assoc key ,sbst :test #'equal)) key)))
;;;
;;;
;;; =============================================================================
;;;
;;; mnode.sbst
;;; ----------
;;;
;;;       arguments     : mv   - <key>
;;;                       sbst - <substitution>
;;;
;;;       returns       : <datum>
;;;
;;;       description   : It returns the <datum> which is associated
;;;                       with 'mv' in 'sbst'.
;;;
;;;       implementation: It will only be used after it has already been 
;;;                       checked that 'mv' is bound in 'sbst'.
;;; 
;;;                                        written :  vhs 11/19/84
;;;                                        modified:  scs 02/09/88
;;;                                        modified:  scs 02/10/88
;;;
;;;
(defmacro mnode.sbst (mv sbst)
  "It returns the <datum> which is associated with 'mv' in 'sbst'.
   (It will only be used after it has already been 
    checked that 'mv' is bound in 'sbst'.)"
  `(cdr (assoc ,mv ,sbst :test #'equal)))
;;;
;;;
;;; =============================================================================
;;;
;;; srcnode.sbst
;;; ------------
;;;
;;;       arguments     : sub - <substitution>
;;;                       mn  - <datum>
;;;
;;;       returns       : <key> 
;;;
;;;       description   : It returns the <key> of the pair of 'sub'
;;;                       whose <datum> is 'mn'.
;;;
;;;                                        written :  vhs 04/25/85
;;;                                        modified:  scs 01/29/88
;;;                                        modified:  scs 02/09/88
;;;
;;;
(defmacro srcnode.sbst (sub mn)
  "It returns the <key> of the pair of 'sub' whose <datum> is 'mn'."
  `(car (rassoc ,mn ,sub :test #'equal)))
;;;
;;;
;;; =============================================================================
;;;
;;; mbind.sbst
;;; ----------
;;;
;;;       arguments     : mv   - <key>
;;;                       sbst - <substitution>
;;;
;;;       returns       : pair | nil 
;;;
;;;       description   : returns the pair of 'sub' whose <key> is 'mv'
;;;                       if there is one, returns 'nil' otherwise.
;;;
;;;                                        written :  vhs 04/25/85
;;;                                        modified:  scs 02/09/88
;;;
;;;
(defmacro mbind.sbst (mv sbst)
  "Returns the pair of 'sub' whose <key> is 'mv'
   if there is one, returns 'nil' otherwise."
  `(assoc ,mv ,sbst :test #'equal))
;;;
;;;
;;;
;;; =============================================================================
;;;
;;; ismemb.sbst    
;;; -----------
;;;
;;;       arguments     : mb   - pair
;;;                       sbst - <substitution>
;;;
;;;       returns       : <boolean>   
;;;
;;;       description   : returns 'true' if 'mb' is one of the pairs in 'sbst',
;;;                       'false' otherwise.
;;;
;;;                                        written : vhs 11/19/84
;;;                                        modified: scs 02/09/88
;;;
(defmacro ismemb.sbst (mb sbst)
  "Returns 'true' if 'mb' is one of the pairs in 'sbst', 'false' otherwise."
  `(member ,mb ,sbst :test #'equal))
;;;
;;;
;;; =============================================================================
;;;
;;; issubset.sbst  
;;; -------------
;;;
;;;       arguments     : sbst1 - <substitution>
;;;                       sbst2 - <substitution>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : returns 'true' if 'sbst1' is a subset of 'sbst2',
;;;                       'false' otherwise.
;;;
;;;
;;;                                        written:  vhs 11/19/84   
;;;                                        modified: rgh 11/30/85
;;;                                        modified: scs 02/09/88
;;;
;;;
(defmacro issubset.sbst (sbst1 sbst2)
   "Returns 'true' if 'sbst1' is a subset of 'sbst2', 'false' otherwise."
   `(do ((current-sbst1 ,sbst1 (others.sbst current-sbst1)))
	((isnew.sbst current-sbst1) t)
	(when (not (ismemb.sbst (choose.sbst current-sbst1) ,sbst2))
	  (return nil))))
;;;
;;;
;;; =============================================================================
;;;
;;; iseq.sbst   
;;; ---------
;;;
;;;       arguments     : sbst1 - <substitution>
;;;                       sbst2 - <substitution>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : Compares 'sbst1' and 'sbst2' as sets :
;;;                       returns 'true' if 'sbst1' and 'sbst2' have 
;;;                       identical pairs, 'false' otherwise.  
;;;
;;;
;;;                                        written:  vhs 11/19/84
;;;                                        modified: scs 02/09/88
;;;
;;;
(defmacro iseq.sbst (sbst1 sbst2)
  "Compares 'sbst1' and 'sbst2' as sets :
    returns 'true' if 'sbst1' and 'sbst2' have 
    identical pairs, 'false' otherwise." 
  `(let ((s1 ,sbst1) (s2 ,sbst2))
     (and (issubset.sbst s1 s2)
	  (issubset.sbst s2 s1))))
;;;
;;;
;;; =============================================================================
;
; putin.sbst
; ----------
;
;       arguments     : mb - <mbind>
;                       sbst - <substitution>
;
;       returns       : <substitution>
;
;       description   : returns "sbst" with "mb" inserted
;
;       implementation: this is a standard "insert" function, as opposed to
;                        "insert.ds" which side-effects one of its arguments
;
;                                        written :  rgh 08/05/85
;                                        modified:  rgh 08/25/85
;
(defmacro putin.sbst (mb sbst)
  `(adjoin ,mb ,sbst :test #'equal))
;
;
; =============================================================================
;
; union.sbst
; ----------
;
;       arguments     :  s1  -  <substitution>
;                        s2  -  <substitution>
;
;       returns       :  <substitution>
;
;       description   :  returns the union of the two substitutions, s1
;                        and s2
;
;                                        written :  rgh 06/12/85
;                                        modified:  
;
;
(defun union.sbst (s1 s2)
   (cond ((isnew.sbst s2) s1)
         ((isbound.sbst (mvar.mb (choose.sbst s2)) s1)
            (union.sbst s1 (others.sbst s2)))
         (t (putin.sbst (choose.sbst s2)
                     (union.sbst s1 (others.sbst s2))))))
;
;
; =============================================================================
;
; restrict.sbst
; -------------
;
;       arguments     : subst - <substitution>
;                       ns - <nodefun set>
;
;       returns       : <substitution>
;
;       description   : returns a <substitution> consisting of only those
;                       <bind>s whose <var>s are in "ns"
;
;                                        written :  rgh  2/08/86
;                                        modified:  scs  3/09/88
;                                                   ssc 10/24/88
;
;
(defmacro restrict.sbst (subst ns)
  `(remove-if-not #'(lambda (mb) (ismemb.ns (mvar.mb mb) ,ns))
		  ,subst))
;
;
; =============================================================================
;
; term.sbst
; ---------
;
;       arguments     : mvar   -  <mvar>
;                       subst  -  <substitution>
;
;       returns       : <mnode> | nil
;
;       description   : if "mvar" is an <mvar> which is bound in "subst", then
;                       this function returns the <mnode> to which "mv" is
;                       bound;  otherwise it returns nil
;
;                                        written :  rgh 06/12/85
;                                        modified:
;
;
(defmacro term.sbst (mvar subst)
  `(let ((mbind (mbind.sbst ,mvar ,subst)))
      (if mbind (mnode.mb mbind))))
;
;
; =============================================================================
;
; is-compatible.sbst
; ------------------
;
;       arguments     : s1,s2 - <substitution>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "s1" and "s2" do not contain 
;                       inconsistent bindings, and have no two different
;                       variables bound to the same term, "false" otherwise
;
;                                        written :  rgh  4/24/86
;                                        modified:  rgh  4/26/86
;
;
(defun is-compatible.sbst (s1 s2)
  (do* ((mb1 (choose.sbst s1)
	     (choose.sbst s1))
	(mb2 (mbind.sbst (mvar.mb mb1) s2)
	     (mbind.sbst (mvar.mb mb1) s2)))
       ((or (isnew.sbst s1)
	    (and mb2 (not (eq (mnode.mb mb1) (mnode.mb mb2))))
	    (and (null mb2) (isvalue.sbst (mnode.mb mb1) s2)))
	(isnew.sbst s1))
       (setq s1 (others.sbst s1))))
;
;
; =============================================================================
;
; cardinality.sbst
; ----------------
;
;       arguments     : subst - <substitution>
;
;       returns       : <integer>
;
;       description   : returns the number of <mbinds> in "subst"
;
;                                        written :  rgh 12/02/85
;                                        modified:
;
;
(defmacro cardinality.sbst (subst)
  `(length ,subst))
;
;
; =============================================================================



    
    




