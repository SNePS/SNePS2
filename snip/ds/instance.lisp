;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: instance.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; <instance> ::=  ( <substitution> <support> <sign> )
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.inst         : <universal> --> <boolean>
;
; CONSTRUCTORS   make.inst       : <substitution> x <support> x <sign>
;                                                       --> <instance>
;                addbinding.inst : <mbind> x <instance> --> <instance>
;                merge.inst      : <instance> x <instance> --> <instance>
;
; SELECTORS      subst.inst      : <instance> --> <substitution>
;                support.inst    : <instance> --> <support>
;                sign.inst       : <instance> --> <sign>
;
; TESTS          unknown.inst    : <instance> --> <boolean>
;                iseq.inst       : <instance> x <instance> --> <boolean>
;
; =============================================================================
;
; is.inst
; -------
;
;       arguments     : u -- <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is an <instance>,
;                               "false" if not
;
;                                        written :  rgh 08/13/85
;                                        modified:  rgh 08/21/85
;                                        modified:  cpf 10/07/88
;
(defmacro is.inst (u)
  `(and (listp ,u)
        (is.sbst (first ,u))
	(is.sup (second ,u))
        (is.sign (third ,u))))
;
;
; =============================================================================
;
; make.inst
; ---------
;
;       arguments     : subst -- <substitution>
;                       sup   -- <support>
;                       sign  -- <sign>
;
;       returns       : <instance>
;
;       description   : returns a <instance> consisting of the given
;                        <substitution>, <support>, and <sign>
;
;                                        written :  rgh 08/13/85
;                                        modified:  cpf 10/07/88
;
;
(defmacro make.inst (subst sup sign)
  `(list ,subst ,sup ,sign))
;
;
; =============================================================================
;
; subst.inst
; ----------
;
;       arguments     : inst -- <instance>
;
;       returns       : <substitution>
;
;       description   : selects the <substitution> of "inst"
;
;                                        written :  rgh 08/13/85
;                                        modified:  cpf 10/07/88
;
;
(defmacro subst.inst (inst)
  `(first ,inst))

;
;
; =============================================================================
;
; support.inst
; ------------
;
;       arguments     : inst -- <instance>
;
;       returns       : <support>
;
;       description   : selects the <support> of "inst"
;
;                                        written :  rgh 08/13/85
;                                        modified:  cpf 10/07/88
;
;
(defmacro support.inst (inst)
  `(second ,inst))
;
;
; =============================================================================
;
; sign.inst
; ---------
;
;       arguments     : inst -- <instance>
;
;       returns       : <sign>
;
;       description   : selects the <sign> of "inst"
;
;                                        written :  rgh 08/13/85
;                                        modified:
;
;
(defmacro sign.inst (inst)
  `(third ,inst))
;
;
; =============================================================================
;
; addbinding.inst
; ---------------
;
;       arguments     : mb - <mbind>
;                       inst - <instance>
;
;       returns       : <instance>
;
;       description   : returns an <instance> exactly like "inst" but with
;                       "mb" added to the substitution
;
;                                        written :  rgh  3/30/86
;                                        modified:
;
;
(defmacro addbinding.inst (mb inst)
  `(make.inst (putin.sbst ,mb (subst.inst ,inst))
	      (support.inst ,inst)
	      (sign.inst ,inst)))
;
;
; =============================================================================
;
; merge.inst
; ----------
;
;       arguments     : inst1 - <instance>
;                       inst2 - <instance>
;
;       returns       : <instance>
;
;       description   : returns an <instance> which has a support corresponding
;                       to the merge of the supports of "inst1" and "inst2".
;
;                                        written :  cpf  10/18/88
;                                        modified: 
;
;
(defmacro merge.inst (inst1 inst2)
  `(make.inst (subst.inst ,inst1)
	      (merge.sup (support.inst ,inst1) (support.inst ,inst2))
	      (sign.inst ,inst1)))
;
;
; =============================================================================
;
; unknown.inst
; ------------
;
;       arguments     : inst -- <instance>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *KNOWN-INSTANCES* -- a register of the node-activation
;                        process which calls this function
;
;       description   : returns "true" if "inst" is a previously unknown
;                        instance of the node whose activation calls the
;                        function, "false" otherwise.
;
;       modification  : The original test only checked the node instance, so
;                        failed when node has two free variables and they are
;                        bound differently in the two cases.
;
;                                        written :  rgh 08/13/85
;                                        modified:  scs 05/25/88
;                                        modified:  njm/cpf 10/21/88
;                                        modified:  scs 10/19/88
;                                        modified:  njm/cpf 11/02/88 
;
;
;
(defun unknown.inst (inst)
  (not (member (subst.inst inst)
	       *KNOWN-INSTANCES*
	       :test #'(lambda (x y)
			 (and (dolist (pair x t)
				(unless (iseq.n (cdr pair)
						(match:bindingOf (car pair)
								 (subst.inst y)))
				  (return nil)))
			      (isincluded.sup (support.inst inst)
					      (support.inst y)))))))
;
;
;
; =============================================================================
;
; iseq.inst
; ---------
;
;       arguments     : i1 - <instance>
;                       i2 - <instance>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "i1" and "i2" are equal
;
;                                        written :  rgh 11/30/85
;                                        modified:  scs 4/6/88
;
;
(defun iseq.inst (i1 i2)
  (and (iseq.sbst (subst.inst i1) (subst.inst i2))
       (iseq.sup (support.inst i1) (support.inst i2))
       (iseq.sign (sign.inst i1) (sign.inst i2))))
;
;
; =============================================================================



    
    




