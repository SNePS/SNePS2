;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: context3.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :sneps)


; =============================================================================
;
; assert.n
; --------
;
;       arguments     : n - <node>
;                       crntname - <svar>  (optional)
;
;       returns       : <node> | ---
;
;       description   : it sets <node> "n" as an hypothesis, performing the 
;                       following operations:
;                          1 - creates a new context with "n" as hyps;
;                          2 - creates a new context which hyps are the union 
;                              of the current context's hyps with "n", and 
;                              computes its restriction;
;                          3 - If the second argument is supplied then:
;                                a) the context "crntct is no more called
;                                   crntname;
;                                b) names the context created in 
;                                   step 2 as "crntname";
;                                c) the value of the special variable "crntct"
;                                   is the context created in step 2
;                          4 - If the new context is already known
;                              to be inconsistent, issues a warning.
;
;       side-effects  : it side-effects the node "n", and the system 
;                       svar "contexts" and the special variable "crntct"
;
;
;
;                                        written :  ejm 10/03/83
;                                        modified:  scs 02/13/87 
;                                        modified:  scs 10/07/87
;                                        modified:  njm 09/20/88 
;                                        modified:  njm 10/03/88 
;                                        modified:  njm 10/13/88
;                                        modified:  mrc 10/26/88
;                                        modified:  scs 05/28/07
;
;
;
(defun assert.n (n &optional (crntname crntct))
  (declare (special crntct))
  (let* ((ct (get-context crntname))
	 (newassrt (buildcontext (makeone.ns n)))
	 (newct    (fullbuildcontext (makeone.ns n) (makeone.cts ct))))
    (setf (node-asupport n) (insert.ctcs 'hyp
					 newassrt
					 (node-asupport n)))
    (name.ct newct crntname)
    (if (and (isinconsis.ct newct) (isokinconsis.ct newct)) 
	    (when *infertrace* (format outunit
		    "The context named ~{~A~#[~;, and ~:;, ~]~} is inconsistent.~%"
		    (context-names newct)))
      (snebr:ck-contradiction n newct 'assertion))))

;
;
; ==============================================================================
;
; isassert.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <context> | NIL
;
;       description   : It returns the context justifying "n" if "n" is an 
;                       Hypothesis and is believed in the current context.
;                       Returns "false" otherwise.
;
;
;                                        written :  ejm 10/03/83
;                                        modified:  scs 02/11/87
;                                        modified:  njm 09/20/88
;                                        modified:  njm 10/03/88
;                                        modified:  njm  4/27/89
;
;
(defun isassert.n (n &optional cntx)
  (declare (special crntct))
  (let ((ctcs (node-asupport n))
	(ct (if cntx
		(if (is.ct cntx) cntx (value.sv cntx))
		(if (is.ct crntct) crntct (value.sv crntct)))))
    (declare (special ct))
    (or (isassert1.n (getcontextset.ctcs 'hyp ctcs))
	(isassert1.n (getcontextset.ctcs 'der ctcs)) 
	(isassert1.n (getcontextset.ctcs 'ext ctcs)))))

;
;
; ==============================================================================
;
; isassert1.n
; -----------
;
;       arguments     : cts - <context set>
;
;       returns       : <boolean>
;
;       description   : If any of the contexts of the context set "cts" is a 
;                       subset of the current context "crntct" then returns
;                       "true". Returns "false" otherwise.
;
;
;                                        written :  njm 10/03/88
;                                        modified:  njm 02/13/89
;                                        modified:  njm  4/27/89
;
;
(defun isassert1.n (cts)
  (declare (special ct))
  (cond ((isnew.cts cts) nil)
	((issubset.ct (choose.cts cts) ct) t)
	(t (isassert1.n (others.cts cts)))))


;
;
; ==============================================================================
;
; isassert-print.n
; ----------------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true if the node "n" has a non empty 
;                       node-asupport. Returns "false" otherwise.
;
;
;       ATTENTION     : This function is only used in the print of a
;                       node structure, elsewhere the "isassert.n"
;                       function must be used.
;
;
;                                        written :  njm 10/03/88
;                                        modified:  
;
;
(defun isassert-print.n (n)
  (if (node-asupport n) t))

  
;
;
; ==============================================================================
;
;remove-hyp-itself
; ----------------
;
;       arguments     : n   - <node>
;                       cts - <context set>
;
;       returns       : <context set>
;
;       description   : removes 'n' from the contexts in 'cts'.
;
;
;
;
;                                        written :  mrc 11/28/88
;                                        modified:  
;
;  
(defun remove-hyp-itself (n cts)
  (let ((newcts (new.cts)))
    (do.cts (ct cts)
	    (setq newcts
	      (insert.cts (fullbuildcontext (compl.ns (context-hyps ct)
						      (makeone.ns n))
					    (new.cts))
			  newcts)))
    newcts))
