;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: restriction.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


; =============================================================================
;
; <restriction>  ::=  ( <substitution> . <mnoderep set> )
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.restr    :  --> <restriction>
;  ELEMENTS
;
; RECOGNIZERS    is.restr     :  <universal> --> <boolean>
;                isnew.restr  :  <restriction> --> <boolean>
;
; CONSTRUCTORS   make.restr   :  <substitution> x <mnoderep set>
;                                        --> <restriction>
;
; SELECTORS      subst.restr  :  <restriction> --> <substitution>
;                mnrs.restr   :  <restriction> --> <mnoderep set>
;
; TESTS          equivalent.restr : <restriction> x <restriction> --> <boolean>
;
; =============================================================================
;
; new.restr
; ---------
;
;       returns       : <restriction>
;
;       description   : creates a <new restriction>
;
;                                        written :  rgh 08/17/85
;                                        modified:
;
;
(defmacro new.restr ()
  `(new.sbst))
;
;
; =============================================================================
;
; is.restr
; ---------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "u" is a <restriction>,
;                               "false" otherwise
;
;                                        written :  rgh 08/17/85
;                                        modified:  rgh 08/21/85
;
;
(defmacro is.restr (u)
  `(and (consp ,u)
        (is.sbst (car ,u))
        (is.mnrs (cdr ,u))))
;
;
; =============================================================================
;
; subst.restr
; ------------
;
;       arguments     : r - <restriction>
;
;       returns       : <substitution>
;
;       description   : returns the <substitution> of the <restriction> "r"
;
;                                        written :  rgh 08/17/85
;                                        modified:
;
;
(defmacro subst.restr (r)
  `(car ,r))
;
;
; =============================================================================
;
; isnew.restr
; ------------
;
;       arguments     : r - <restriction>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "r" is a <new restriction>,
;                               "false" otherwise
;
;       implementation: simply tests to see if the <substitution> of the
;                        <restriction> is "new", since the <mnoderep set>
;                        would never be looked at anyway if this is true.
;
;                                        written :  rgh 08/17/85
;                                        modified:
;
;
(defmacro isnew.restr (r)
  `(isnew.sbst (subst.restr ,r)))
;
;
; =============================================================================
;
; make.restr
; -----------
;
;       arguments     : subst - <substitution>
;                       mnrs - <mnoderep set>
;
;       returns       : <restriction>
;
;       description   : returns a <restriction> composed of the arguments
;
;                                        written :  rgh 08/17/85
;                                        modified:  ssc 10/24/88
;
;
(defmacro make.restr (subst)
  `(list ,subst))
;
;
; =============================================================================
;
; mnrs.restr
; -----------
;
;       arguments     : r - <restriction>
;
;       returns       : <mnoderep set>
;
;       description   : returns the <mnoderep set> of the <restriction> "r"
;
;                                        written :  rgh 08/17/85
;                                        modified:
;
;
(defmacro mnrs.restr (r)
  `(cdr ,r))
;
;
; =============================================================================
;
; equivalent.restr
; ----------------
;
;       arguments     : r1 - <restriction>
;                       r2 - <restriction>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "r1" and "r2" are equivalent
;                       <restriction>s in the sense that each <instance>
;                       which satisfies one will satisfy the other.
;
;       implementation: two <restriction>s are equivalent if their
;                       <substitution>s match exactly, with the possible
;                       exception that two i-nodes (or u-nodes) with the
;                       exact same structure may have different names
;                       (<msyms>).
;
;                                        written :  rgh 08/18/85
;                                        modified:  ssc 11/28/88
;
;
(defun equivalent.restr (r1 r2)
  (let (term
	(s1 (subst.restr r1))
	(s2 (subst.restr r2)))
    (when (eql (cardinality.sbst s1) (cardinality.sbst s2))
      (do.sbst (next-mb s1 t)
	 (unless (and (setq term (term.sbst (mvar.mb next-mb) s2))
		      (sneps::iseq.n (mnode.mb next-mb) term))
		 (return nil))))))
;
;
; =============================================================================




    
    




