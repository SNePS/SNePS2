;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: iset.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <instance set> ::= { <instance> ... }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.iset : --> <instance set>
;  ELEMENTS
;
; RECOGNIZERS    is.iset    : <universal> --> <boolean>
;                isnew.iset : <instance set> --> <boolean>
;
; TESTS          ismemb.iset : <instance> x <instance set> --> <boolean>
;
; SELECTORS      choose.iset : <instance set> --> <instance>
;                others.iset : <instance set> --> <instance set>
;
; CONSTRUCTORS   insert.iset     : <instance> x <instance set>
;                                         --> <instance set>
;                makeone.iset    : <instance> --> <instance set>
;                union.iset      : <instance set> x <instance set>
;                                         --> <instance set>
;                addbinding.iset : <mbind> x <instance set> --> <instance set>
;
; =============================================================================
;
; new.iset
; -----------
;
;       returns       : <instance set>
;
;       description   : returns a "new" <instance set>
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defmacro new.iset ()
  `(new.Set))
;
;
; =============================================================================
;
; is.iset
; ----------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <instance set>,
;                               "false" otherwise
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defmacro is.iset (u)
  `(and (is.Set ,u)
        (is.inst (choose.Set ,u))))
;
;
; =============================================================================
;
; isnew.iset
; -------------
;
;       arguments     : iset - <instance set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "iset" is a "new" <instance set>
;                               "false" otherwise
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defmacro isnew.iset (iset)
  `(isnew.Set ,iset))
;
;
; =============================================================================
;
; choose.iset
; --------------
;
;       arguments     : iset - <instance set>
;
;       returns       : <instance>
;
;       description   : returns the first <instance> in "iset"
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defmacro choose.iset (iset)
  `(choose.Set ,iset))
;
;
; =============================================================================
;
; others.iset
; --------------
;
;       arguments     : iset - <instance set>
;
;       returns       : <instance set>
;
;       description   : returns a <instance set> consisting of all of the
;                       <instance>s in "iset" except the first
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defmacro others.iset (iset)
  `(others.Set ,iset))
;
;
; =============================================================================
;
; ismemb.iset
; -----------
;
;       arguments     : inst - <instance>
;                       iset - <instance set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "inst" is a member of "iset",
;                               "false" otherwise
;
;                                        written :  rgh 08/21/85
;                                        modified:  rgh 11/30/85
;                                        modified:  njm/cpf 10/21/88
;
(defmacro ismemb.iset (inst iset)
  `(do* ((is ,iset (others.iset is))
	 (i (choose.iset is) (choose.iset is))
	 (result nil))
	((or (isnew.iset is) result) result)
     (if (and (iseq.sbst (subst.inst ,inst) (subst.inst i))
	      (isincluded.sup (support.inst ,inst) (support.inst i))
	      (iseq.sign (sign.inst ,inst) (sign.inst i)))
	 (setq result t))))

;
;
; =============================================================================
;
; insert.iset
; -----------
;
;       arguments     : inst - <instance>
;                       iset - <instance set>
;
;       returns       : <instance set>
;
;       description   : returns "iset" with "inst" inserted if it was not
;                       already there
;
;                                        written :  rgh 11/24/85
;                                        modified:  rgh 11/30/85
;                                        modified:  njm/cpf 10/21/88
;
(defmacro insert.iset (inst iset)
  `(let ((result nil)
	 (finiset (new.iset)))
     (cond ((ismemb.iset ,inst ,iset) ,iset)
	   ((do.Set (i ,iset result)
	      (if (and (iseq.sbst (subst.inst i) (subst.inst ,inst))
		       (iseq.sign (sign.inst i) (sign.inst ,inst)))
		  (progn (setq finiset (putin.Set (merge.inst i ,inst)
						  finiset))
			 (setq result t))
		  (setq finiset (putin.Set i finiset))))
	    finiset)
	   (t (putin.Set ,inst ,iset)))))
  
;
;
; =============================================================================
;
; makeone.iset
; ------------
;
;       arguments     : inst - <instance>
;
;       returns       : <instance set>
;
;       description   : returns an <instance set> containing the one element
;                       "inst"
;
;                                        written :  rgh  3/30/86
;                                        modified:
;
;
(defmacro makeone.iset (inst)
  `(makeone.Set ,inst))
;
;
; =============================================================================
;
; union.iset
; ----------
;
;       arguments     : iset1, iset2 - <instance set>
;
;       returns       : <instance set>
;
;       description   : returns the set union of "iset1" and "iset2"
;
;                                        written :  rgh  3/30/86
;                                        modified:  njm/cpf 10/18/88
;
;

(defmacro union.iset (iset1 iset2)
  `(let ((result1 ,iset2))
    (do.Set (ii ,iset1 result1)
      (setq result1 (insert.iset ii result1)))))
      

;
; =============================================================================
;
; addbinding.iset
; ---------------
;
;       arguments     : mb - <mbind>
;                       inst-set - <instance set>
;
;       returns       : <instance set>
;
;       description   : returns an <instance set> with "mb" added to the
;                       substitutions of each of the instances in "inst-set"
;
;                                        written :  rgh  3/30/86
;                                        modified:  scs  3/15/88
;
;
(defun addbinding.iset (mb inst-set)
  (let ((result (new.iset)))
    (do.set (inst inst-set result)
       (setq result (insert.iset (addbinding.inst mb inst) result)))))
;
;
; =============================================================================



    
    




