;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: fnodeset.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <flagged node set> ::= { <flagged node> ... }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.fns    :  --> <flagged node set>
;  ELEMENTS
;
; RECOGNIZERS    is.fns     : <universal> --> <boolean>
;                isnew.fns  : <flagged node set> --> <boolean>
;
; CONSTRUCTORS   insert.fns : <flagged node> x <flagged node set>
;                                 --> <flagged node set>
;                putin.fns  : <flagged node> x <flagged node set>
;                                 --> <flagged node set>
;                update.fns : <flagged node set> x <node> x <flag>
;                                 --> <flagged node set>
;
; SELECTORS      choose.fns : <flagged node set> --> <flagged node>
;                others.fns : <flagged node set> --> <flagged node set>
;                flag.fns   : <node> x <flagged node set> --> <flag>
;                support.fns: <node> x <flagged node set> --> <support>
;
; TESTS          ismemb.fns : <flagged node> x <flagged node set>
;                                 --> <boolean>          
;
; =============================================================================
;
; new.fns
; -------
;
;       arguments     : 
;
;       returns       : <flagged node set>
;
;       description   : returns a "new" <flagged node set>
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro new.fns ()
  `(new.Set))
;
;
; =============================================================================
;
; choose.fns
; ----------
;
;       arguments     : fns - <flagged node set>
;
;       returns       : <flagged node>
;
;       description   : returns an element of "fns"
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro choose.fns (fns)
  `(choose.Set ,fns))
;
;
;;; =============================================================================
;;;
;;; do.fns 
;;; ------
;;;
;;;       arguments     : var - a local variable
;;;                       fnsform - a form evaluating to a <flagged node set>
;;;                       resultform - a form evaluating to a <flagged node set>
;;;                       forms - a sequence of Lisp forms
;;;
;;;       returns       : <flagged node set> or nil
;;;
;;;       description   : Evaluates the sequence of forms in order once
;;;                       for val bound to sucessive elements of the value of fnsform
;;;                       Then returns the value of resultform.
;;;                       If resultform is omitted, returns nil.
;;;
;;;                                        written:  scs 03/03/88
;;;
(defmacro do.fns ((var fnsform &optional resultform) &body forms)
  `(do.set (,var ,fnsform ,resultform) ,@forms))
;;;
;;;
; =============================================================================
;
; is.fns
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <flagged node set>,
;                       "false" otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro is.fns (u)
  `(and (is.Set ,u)
        (or (isnew.Set ,u)
            (is.fn (choose.Set ,u)))))
;
;
; =============================================================================
;
; isnew.fns
; ---------
;
;       arguments     : fns - <flagged node set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "fns" is a "new" <flagged node set>,
;                       "false" otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro isnew.fns (fns)
  `(isnew.Set ,fns))
;
;
; =============================================================================
;
; others.fns
; ----------
;
;       arguments     : fns - <flagged node set>
;
;       returns       : <flagged node set>
;
;       description   : returns all of "fns" except for the element that
;                       would be chosen by choose.fns
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro others.fns (fns)
  `(others.Set ,fns))
;
;
; =============================================================================
;
; ismemb.fns
; ----------
;
;       arguments     : fn  - <flagged node>
;                       fns - <flagged node set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "fn" is member of "fns", 
;                       "false" otherwise
;
;                                        written :  njm/cpf 10/18/88
;                                        modified:
;
;

(defmacro ismemb.fns (fn fns)
  `(do* ((fs ,fns (others.fns fs))
	 (f (choose.fns fs) (choose.fns fs))
	 (result nil))
	((or (isnew.fns fs) result) result)
     (if (and (iseq.n (node.fn ,fn) (node.fn f))
	      (isincluded.sup (support.fn ,fn) (support.fn f))
	      (eq (flag.fn ,fn) (flag.fn f)))
	 (setq result t))))

;
; =============================================================================
;
; insert.fns
; ----------
;
;       arguments     : fn - <flagged node>
;                       fns - <flagged node set>
;
;       returns       : <flagged node set>
;
;       description   : returns a <flagged node set> consisting of "fns",
;                       but with "fn" also included if it was not already
;
;                                        written :  rgh  2/08/86
;                                        modified:  njm/cpf 10/20/88
;
;
(defmacro insert.fns (fn fns)
  `(let ((result nil)
	 (finfns (new.fns)))
     (cond ((ismemb.fns ,fn ,fns) ,fns)
	   ((do.Set (f ,fns result)
	      (if (and (iseq.n (node.fn f) (node.fn ,fn))
		       (eq (flag.fn f) (flag.fn ,fn)))
		  (progn (setq finfns (putin.Set (merge.fn f ,fn)
						 finfns))
			 (setq result t))
		  (setq finfns (putin.Set f finfns))))
	    finfns)
	   (t (putin.Set ,fn ,fns)))))


;
; =============================================================================
;
; putin.fns
; ---------
;
;       arguments     : fn - <flagged node>
;                       fns - <flagged node set>
;
;       returns       : <flagged node set>
;
;       description   : returns a <flagged node set> consisting of "fns" with
;                       "fn" added to it.  No check is made to see if it was
;                       already there.
;
;                                        written :  rgh  2/13/86
;                                        modified:
;
;
(defmacro putin.fns (fn fns)
  `(putin.Set ,fn ,fns))
;
;
; =============================================================================
;
; update.fns
; ----------
;
;       arguments     : fns - <flagged node set>
;                       n - <node>
;                       sup - <support>
;                       flag - <flag>
;
;       returns       : <flagged node set>
;
;       description   : returns a <flagged node set> similar to "fns" but
;                       with the flag corresponding to "n" set to "flag";
;                       This function also updates the node a-support.
;
;                                        written :  rgh  4/03/86
;                                        modified:  cpf 10/07/88
;
;
(defun update.fns (fns n sup flag)
  (let ((fn (choose.fns fns)))
    (cond ((isnew.fns fns) fns)
	  ((iseq.n n (node.fn fn))
	   (putin.fns (make.fn n (merge.sup sup (support.fn fn)) flag)
		      (others.fns fns)))
	  (t
	   (putin.fns fn (update.fns (others.fns fns) n sup flag))))))
;
;
; =============================================================================
;
; flag.fns
; --------
;
;       arguments     : n - <node>
;                       fnset - <flagged node set>
;
;       returns       : <flag> | nil
;
;       description   : returns the <flag> associated with the <node> "n"
;                       in "fns" 
;
;       implementation: actually should only be used when it is known that
;                       "n" is a node in "fnset", so "nil" should neve be
;                       returned
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defmacro flag.fns (n fnset)
  `(prog (fns fn)
	 (setq fns ,fnset)
      begin
	 (if (isnew.fns fns) (return))
	 (setq fn (choose.fns fns))
	 (if (iseq.n ,n (node.fn fn)) (return (flag.fn fn)))
	 (setq fns (others.fns fns))
	 (go begin)))
;
;
; =============================================================================
;
; support.fns
; -----------
;
;       arguments     : n - <node>
;                       fnset - <flagged node set>
;
;       returns       : <support> | nil
;
;       description   : returns the <support> associated with the <node> "n"
;                       in "fns" 
;
;       implementation: actually should only be used when it is known that
;                       "n" is a node in "fnset", so "nil" should neve be
;                       returned
;
;                                        written :  cpf 10/18/88
;                                        modified:
;
;
(defmacro support.fns (n fnset)
  `(prog (fset fn)
	 (setq fset ,fnset)
      begin
	 (if (isnew.fns fset) (return))
	 (setq fn (choose.fns fset))
	 (if (iseq.n ,n (node.fn fn)) (return (support.fn fn)))
	 (setq fset (others.fns fset))
	 (go begin)))
;
;
; =============================================================================



    
    




