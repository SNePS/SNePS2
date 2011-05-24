;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: filterhelp.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
(defun filter (subst filter)
  "Returns subst if it passes the filter, else NIL"
  ;; written by: scs 5/24/88, modified: hi 8/18/99
  ;; subst passes filter iff every variable in filter, bound to a term tf
  ;; is also a variable in subst, bound to a term ts,
  ;; and ts is a specialization of tf.
  (dolist (pair filter t)
    (let* ((var (car pair))
	   (tf (cdr pair))
	   (ts (match::bindingOf var subst)))
      (when (or (null ts)
		(not (iseq.n ts tf)) ; overly strict test for now
		)
	(return nil)))))
;
; =============================================================================
;
(defun compatible (subst filter)
  "Returns T if it is compatible with the filter, else NIL"
  ;; written by: scs 6/17/88
  ;; subst is compatible with the filter iff every variable that is in both subst and filter
  ;; is bound to a term, ts, in subst
  ;; that is compatible with the term, tf, it is bound to in subst.
  ;; modified by scs 12/13/94
  ;; so subst is not compatible with filter if a term, t, is in both
  ;; subst and filter, but bound to different variables.
  (dolist (pair subst t)
    (let* ((var (car pair))
	   (ts (cdr pair))
	   (tf (match::bindingOf var filter))
	   (varf (car (rassoc ts filter  :test #'iseq.n))))
      (when (or (and tf
		     (not (iseq.n ts tf)) ; overly strict test for now
		     )
		(and varf (not (iseq.n var varf))))
	(return nil)))))





;;;
;;;
;;;
;;;
;;;    The following functions seems not be called by any other function or macro
;;;
;;;
;;;                                  njm 11/08/88
;;;
;;;
;;;
;;;;
;;;; =============================================================================
;;;;
;;;; test-repset
;;;; -----------
;;;;
;;;;       arguments     : repset - <report set>
;;;;                       mb - <mbind>
;;;;                       mnrs - <mnoderep set>
;;;; 
;;;;       returns       : <report set>
;;;;
;;;;       description   : tests each of the <reports> in "repset" to see which
;;;;                       have bindings compatible with "mb".  Returns a
;;;;                       <report set> of all those <report>s which pass,
;;;;                       possibly updating their <substitution>s.
;;;;
;;;;                                        written :  rgh  7/29/85
;;;;                                        modified:  rgh  8/02/85
;;;;                                                   rgh  3/28/86
;;;;                                        modified:  scs  3/3/88
;;;;                                        modified: njm/cpf 10/19/88
;;;;
;;;(defun test-repset (repset mb mnrs)
;;;  (declare (ignore mnrs))
;;;  (let ((updated-repset (new.repset)))
;;;    (do.repset (nextrep repset updated-repset)
;;;       (setq updated-repset
;;;	     (union.repset updated-repset (test-rep nextrep mb))))))
;;;;
;;;;
;;;; =============================================================================
;;;;
;;;; test-rep
;;;; --------
;;;;
;;;;       arguments     : r  - <report>
;;;;                       mb - <mbind>
;;;;
;;;;       returns       : <report set>
;;;;
;;;;       description   : tests the <report> "r" to see if it is compatible
;;;;                       with the <mbind> "mb".  Returns a <report set> of
;;;;                       those updatings of "r" (with possibly updated
;;;;                       <substitution>s) which are.  If "r" is not
;;;;                       compatible with "mb", a <new report set> is
;;;;                       returned.
;;;;
;;;;                                        written :  rgh  7/29/85
;;;;                                        modified:  rgh  8/05/85
;;;;                                                   rgh  3/28/86
;;;;                                                   rgh  4/06/86
;;;;                                                   scs  5/24/88
;;;;                                                   cpf 10/17/88
;;;;
;;;(defun test-rep (r mb)
;;;  (let* ((s (subst.rep r))
;;;	 (mv (mvar.mb mb))
;;;	 (mn (mnode.mb mb))
;;;	 (rmn (term.sbst mv s)))
;;;    (cond ((null rmn)
;;;	   (makeone.repset (make.rep (putin.sbst mb s)
;;;				     (support.rep r)
;;;				     (sign.rep r)
;;;				     (signature.rep r)
;;;				     (node.rep r)
;;;				     (context.rep r))))
;;;	  ((isbase.n mn)
;;;	   (cond ((iseq.n mn rmn) (makeone.repset r))
;;;		 (t (new.repset))))
;;;	  ((ismol.n mn)
;;;	   (cond ((issubset.cs (n-to-downcs mn) (n-to-downcs rmn))
;;;		  (makeone.repset r))
;;;		 (t (new.repset))))
;;;	  ((isbound.sbst mn s)
;;;	   (cond ((iseq.n rmn (mnode.sbst mn s))
;;;		  (makeone.repset r))
;;;		 (t (new.repset))))
;;;	  (t (match-test-rep rmn mn (n-to-downcs mn) nil r ct)))))	;cpf nil
;;;;
;;;;
;;;; =============================================================================
;;;;
;;;; match-test-rep
;;;; --------------
;;;;
;;;;       arguments     : source-mn  -  <mnode>
;;;;                       target-mn  -  <mnode>
;;;;                       mcs        -  <mcable set>
;;;;                       mnrs       -  <mnode rep set>
;;;;                       report     -  <report>
;;;;
;;;;       returns       : <report set>
;;;;
;;;;       description   : helper function to test-report which takes care of
;;;;                       cases where match must be called to see if the
;;;;                       report passes the filter
;;;;
;;;;                                        written :  rgh  3/28/86
;;;;
;;;;
;;;(defun match-test-rep (source-mn target-mn mcs mnrs report ct)
;;;  (let ((new-mtch (matchingset-to-supmatchingset
;;;		    (match-mcs-against-filter source-mn mcs mnrs)
;;;		    ct)))
;;;    (cond ((isnew.supmatchingset new-mtch)
;;;	   (new.repset))
;;;	  (t (addbinding.repset
;;;	       (new.mb target-mn source-mn)
;;;	       (supmatchingset-to-repset new-mtch
;;;					 (sign.rep report)
;;;					 (signature.rep report)))))))
;;;;
;;;;
;;;; =============================================================================



    
    




