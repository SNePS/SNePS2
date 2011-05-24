;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: fnode.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <flagged node> ::= ( <node> <support> < truth-flag > )
;
;     where    <truth-flag> ::=  'TRUE | 'FALSE | 'UNKNOWN | 'REQUESTED
;              <support>    ::=  (<ot> <os> ... <ot> <os>)
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.fn      : <universal> --> <boolean>
;
; CONSTRUCTORS   make.fn    : <node> x <support> x <truth-flag> --> <flagged node>
;                merge.fn   : <flagged node> x <flagged node> --> <flagged node>
;
; SELECTORS      node.fn    : <flagged node> --> <node>
;                support.fn : <flagged node> --> <support>
;                flag.fn    : <flagged node> --> <truth-flag>
;
; TESTS          iseq.fn    : <flagged node> x <flagged node> --> <boolean>
;
; =============================================================================
;
; is.fn
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <flagged node>, "false"
;                       otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  4/03/86
;                                        modified:  cpf 10/06/88
;
(defun is.fn (u)
  (and (vectorp u)
       (is.n (node.fn u))
       (is.sup (support.fn u))
       (member (flag.fn u) '(TRUE FALSE UNKNOWN REQUESTED) :test #'eq)))
;
;
; =============================================================================
;
; make.fn
; -------
;
;       arguments     : n - <node>
;                       sup - <support>
;                       flag - <truth-flag>
;
;       returns       : <flagged node>
;
;       description   : returns a <flagged node> constructed from a given
;                       node, support and truth-flag
;
;                                        written :  rgh  2/08/86
;                                        modified:  cpf 10/06/88
;
;
(defun make.fn (n sup flag)
  (vector n sup flag))
;
;
; =============================================================================
;
; node.fn
; -------
;
;       arguments     : fn - <flagged node>
;
;       returns       : <node>
;
;       description   : returns the <node> of "fn"
;
;                                        written :  rgh  2/08/86
;                                        modified:  cpf 10/06/88
;
;
(defun node.fn (fn)
  (svref fn 0))
;
;
; =============================================================================
;
; support.fn
; ----------
;
;       arguments     : fn - <flagged node>
;
;       returns       : <support>
;
;       description   : returns the <support> of "fn"
;
;                                        written :  cpf 10/06/88
;                                        modified:
;
;
(defun support.fn (fn)
  (svref fn 1))
;
;
; =============================================================================
;
; flag.fn
; -------
;
;       arguments     : fn - <flagged node>
;
;       returns       : <truth-flag>
;
;       description   : returns the <truth-flag> of "fn"
;
;                                        written :  rgh  2/08/86
;                                        modified:  cpf 10/06/88
;
;
(defun flag.fn (fn)
  (svref fn 2))
;
;
;
; =============================================================================
;
; iseq.fn
; -------
;
;       arguments     : fn1, fn2 - <flagged node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "fn1" and "fn2" are equal,
;                       "false" otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:  scs  5/10/88
;
;
(defun iseq.fn (fn1 fn2)
  (and (iseq.n (node.fn fn1) (node.fn fn2))
       (iseq.sup (support.fn fn1) (support.fn fn2))
       (eq (flag.fn fn1) (flag.fn fn2))))
;
;
; =============================================================================
;
; merge.fn
; --------
;
;       arguments     : fn1, fn2 - <flagged node>
;
;       returns       : <flagged node>
;
;       description   : returns a flagged node which has a support corresponding
;                       to the merge of the supports of "fn1" and "fn2".
;
;                                        written :  cpf  10/18/88
;                                        modified: 
;
;
(defun merge.fn (fn1 fn2)
  (make.fn (node.fn fn1)
	   (merge.sup (support.fn fn1) (support.fn fn2))
	   (flag.fn fn1)))



; =============================================================================



    
    




