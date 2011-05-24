;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: updatect.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
;
; flatten-lcts
; ------------
;
;
;       arguments     : lcts - list of <context set>                       
;
;       returns       : <context set>
;
;       description   : Flattens the list 'lcts' to a context set.
;                              
;
;                                  written:   mrc  10/17/88
; 
;
(defun flatten-lcts (lcts)
  (cond ((null lcts) (new.cts))
	(t (union.cts (first lcts)
		      (flatten-lcts (rest lcts))))))
; =============================================================================
;
;
; update-contexts 
; ---------------
;
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set>
;
;       returns       : NIL
;
;       description   : Updates the restriction ......
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc 10/14/88
;                                  modified:  scs 06/19/08
; 
;

(defun update-contexts (cts1 cts2)
  "Record the fact that every context in the context set cts1
       is inconsistent with every context in the context set cts2."
  (declare (special *nogoods*))
  (do.cts (c1 cts1)
	  (let ((hyps1 (context-hyps c1)))
	    (do.cts (c2 cts2)
		    ;; c1 and c2 are inconsistent
		    (let ((uhyps
			   (union.ns hyps1 (context-hyps c2))))
		      (unless (member uhyps *nogoods*
				      :test #'(lambda (uh hypset)
						(issubset.ns hypset uh))) 
			(recordInconsistent uhyps)))))))

(defun mark-inconsistent (ct)
  "Records the context ct to be inconsistent."
  (setf (%context-kinconsistent (get-context ct)) t))

(defun ok-update-contexts (cts1 cts2)
  (declare (special *oknogoods*))
  (do.cts (c1 cts1)
	  (let ((hyps1 (context-hyps c1)))
	    (do.cts (c2 cts2)
		    (let ((uhyps
			   (union.ns hyps1 (context-hyps c2))))
		      (unless (member uhyps *oknogoods*
				      :test #'(lambda (uh hypset)
						(issubset.ns hypset uh))) 
			(recordOKInconsistent uhyps)))))))

(defun mark-okinconsistent (ct)
  (setf (%context-okinconsistent (get-context ct)) t))
;
; =============================================================================
;
;
; ishyp  
; -----
;
;
;       arguments     : ct - <context>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun ishyp (ct)
  (eql (cardinality.ns (context-hyps ct)) 1))

;
; =============================================================================
;
;
; insert-rs 
; ---------
;
;
;       arguments     : newrs - <context>
;                       ct    - <context>
;
;       returns       : <context>
;
;       description   : returns CT
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
;;;(defun insert-rs (newrs ct) 
;;;  (setf (context-restriction ct)
;;;	(sigma (insert.cts newrs (context-restriction ct))))
;;;  ct)

;
; =============================================================================
;
;
; remove-rs 
; ---------
;
;
;       arguments     : newrs - <context>
;                       ct    - <context>
;
;       returns       : <context>
;
;       description   : returns CT
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
;;;(defun remove-rs (newrs ct) 
;;;  (setf (context-restriction ct)
;;;	(remove.cts newrs (context-restriction ct)))
;;;  ct)



;
; =============================================================================
;
;
; supperfulus?  
; ------------
;
;
;       arguments     : oldrestr - <context set>
;                       restr    - <context>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
;;;(defun supperfulus? (oldrestr restr)
;;;  (cond ((isnew.cts oldrestr) nil)
;;;	((issubset.ct (choose.cts oldrestr) restr) t)
;;;	(t (supperfulus? (others.cts oldrestr) restr))))



;
; =============================================================================
;
;
; simple-addition?  
; ----------------
;
;
;       arguments     : oldrestr - <context set>
;                       restr    - <context>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
;;;(defun simple-addition? (oldrestr restr)
;;;  (cond ((isnew.cts oldrestr) t)
;;;	((issubset.ct restr (choose.cts oldrestr)) nil)
;;;	(t (simple-addition? (others.cts oldrestr) restr))))



;
; =============================================================================
;
; known-incompat  
; --------------
;
;
;       arguments     : ct1   - <context>
;                       ct2rs - <context set>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun known-incompat (ct1 ct2rs)
  (ismemb.cts ct1 ct2rs))



;(trace known-incompat re-structure-rs-1 re-structure-rs insert-rs remove-rs update-this-hyp-1 update-hypotheses ck-contradiction implement-sneps-option) 



    
    




