;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: contextset.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; Data Type:  <context set> : (<context> <context> ... <context>)  
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    isnew.cts :    <universal> --> <boolean>
;                isall.cts :    <universal> --> <boolean>
;
; CONSTRUCTORS   new.cts :                --> <context set>
;                makeone.cts :  <context> --> <context set>
;                make.cts :     <context> ... <context> --> <context set>
;
; SELECTORS      choose.cts :   <context set> --> <context>
;                others.cts :   <context set> --> <context set>
;
; TESTS          ismemb.cts :   <context> x <context set> --> <boolean>
;
;
; UTILITY        insert.cts :   <context> x <context set> --> <context set>
;                union.cts :    <context> x <context set> --> <context set>
;                remove.cts :   <context> x <context set> --> <context set>
;                intersect.cts :<context set> x <context set> --> <context set>
;                compl.cts :    <context set> x <context set> --> <context set>
;                cardinality.cts :  <context set> --> <integer>
;                remove-if-not.cts : <context set> x <function>
;                                        --> <context set>
;
;                describe.cts :
;
; =============================================================================
;

;
; new.cts 
; ----------
;
;       returns       : <context set>
;
;       description   : creates a <new context set>
;
;                                        written :  njm 09/17/88
;                                        modified: 
;
;
(defmacro new.cts ()
  "Returns a new, empty context set."
  nil)
;
;
; =============================================================================
;
; isnew.cts 
; --------
;
;       arguments     : cts - <context set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if CTS is a new <context set>,
;                       "false" otherwise.
;
;                                        written :  njm 09/17/88  
;                                        modified:
;
;
(defmacro isnew.cts (cts)
  "Returns T if CTS is a new, empty context set, NIL otherwise."
   `(null ,cts))
;
;
; =============================================================================
;
; is.cts
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "u" is a <context set>, "false" otherwise
;
;       implementation: it does not check if all the elements of "u" are <context>s
;                       it checks just the first one.
;
;                                        written :  njm 09/17/88  
;                                        modified: 
;
;
(defun is.cts (u)
  "Returns T if U is a context set, NIL otherwise.  Only checks first member."
  (and (listp u)
       (or (isnew.cts u)
	   (is.ct (first u)))))
;
;
; =============================================================================
;
; isall.cts
; --------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if "u" is a <context set>,
;                       "false" otherwise.
;
;       implementation: Unlike "is.cts", "isall.cts" tests whether all the 
;                       elements of "u" are <context>s.
;
;                                        written :  njm 09/17/88  
;                                        modified: 
;
;
(defun isall.cts (u)
  "Returns T if U is a context set, NIL otherwise. Checks everything."
  (and (listp u) 
       (isall1.cts u)))
;
;
; =============================================================================
;
; isall1.cts
; ---------
;
;       arguments     : s - <sequence>
;
;       returns       : <boolean>
;
;       description   : Auxiliary function to isall.cts. It tests whether
;                       "s" is a <context set> by checking whether all
;                       the elements of "s" are <context>s.
;
;                                        written :  njm 09/17/88  
;                                        modified:
;
;

(defun isall1.cts (s)
  "Returns T if the non-empty list S is a context set, NIL otherwise."
  (cond ((null s) t)
	((not (is.ct (first s))) nil)
	((null (rest s)) t)
	((and (is.ct (second s)) (isless.ct (first s) (second s)))
	 (isall1.cts (rest s)))
	(t nil)))
;
;
; =============================================================================
;
; makeone.cts
; -----------
;
;       arguments     : c - <context>
;
;       returns       : <context set>
;
;       description   : returns a <context set> composed of the <context>.
;
;       note          : This function should be used whenever one wants to
;                       create a <context set> with just one <context>.
;                       Don't use the alternative forms
;
;                                 (make.cts c)
;                       or
;
;                                 (insert.cts c (new.cts))
;
;                       in order to improve efficiency.
;
;                                        written :  njm 09/17/88  
;                                        modified:
;
;
(defmacro makeone.cts (c)
  "Returns a context set containing just the context C."
  `(list ,c))
;
;
; =============================================================================
;
; insert.cts 
; ----------
;
;       arguments     : c - <context>
;                       cts - <context set>
;
;       returns       : <context set> 
;
;       description   : returns a <context set> identical to "cts" but with "c" as
;                       a new <context> if "c" was not yet in "cts". 
;                       If "c" was in "cts" it just returns "cts" unchanged.
;
;                                        written :  njm 09/17/88  
;                                        modified:
;
;
(defun insert.cts (c cts)
  "Returns a context set like CTS, but with C inserted."
  (cond ((isnew.cts cts) (makeone.cts c))
	((isless.ct c (first cts)) (cons c cts))
	((iseq.ct c (first cts)) cts)
	(t (let ((last (last cts)))
	     (cond ((iseq.ct c (first (last cts))) cts)
		   ((isless.ct (first (last cts)) c)
		    (append cts (list c)))
		   (t (let ((tail (member c cts
					  :test #'(lambda (x y)
						    (or (iseq.ct x y)
							(isless.ct x y))))))
			(if (iseq.ct c (first tail))
			    cts
			  (nconc (ldiff cts tail) (list c) tail)))))))))

					;
;
; =============================================================================
;
; make.cts
; --------
;
;       arguments     : ct ... ct - <context>s
;
;       returns       : <context set>
;
;       description   : returns a <context set> composed of the <context>s passed as
;                       arguments.
;
;                                        written :  njm 09/17/88  
;                                        modified: 
;
;
(defun make.cts (&rest ct)
  "Returns a context set composed of the argument contexts."
  (cond ((null ct) (new.cts))
	(t (insert.cts (first ct) (apply #'make.cts (rest ct))))))
;
;
; =============================================================================
;
; union.cts 
; ---------
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set>
;
;       returns       : <context set>
;
;       description   : returns the union of "cts1" and "cts2"
;
;                                        written :  njm 09/17/88  
;                                        modified: 
;
;
(defun union.cts (cts1 cts2)
  "Returns the union of the two context sets, CTS1 and CTS2."
  (let* ((result (list nil)) (last result))
    (loop (cond ((null cts1) (rplacd last cts2) (return))
		((null cts2) (rplacd last cts1) (return))
		((isless.ct (first cts1) (first cts2))
		 (rplacd last (list (first cts1)))
		 (setq last (rest last))
		 (setq cts1 (rest cts1)))
		((isless.ct (first cts2) (first cts1))
		 (rplacd last (list (first cts2)))
		 (setq last (rest last))
		 (setq cts2 (rest cts2)))
		(t (rplacd last (list (first cts1)))
		   (setq last (rest last))
		   (setq cts1 (rest cts1))
		   (setq cts2 (rest cts2)))))
    (rest result)))

;
;
; =============================================================================
;
; remove.cts 
; ----------
;
;       arguments     : c - <context>
;                       cts - <context set>
;
;       returns       : <context set> 
;
;       description   : returns a <context set> identical to "cts" but with "c"
;                       removed if it was there. If "c" was not in "cts"
;                       it just returns "cts" unchanged.
;
;                                        written :  njm 09/17/88  
;                                        modified: 
;
;
(defun remove.cts (c cts)
  "Returns a context set like CTS, but with the context C absent."
  (cond ((isnew.cts cts) cts)
	((isless.ct c (first cts)) cts)
	((iseq.ct c (first cts)) (rest cts))
	(t (cons (first cts) (remove.cts c (rest cts))))))
;
;
; =============================================================================
;
; choose.cts 
; ----------
;
;       arguments     : cts - <context set>
;
;       returns       : <context> 
;
;       description   : returns a <context> of "cts"
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defun choose.cts (cts)
  "Returns an arbitrary, but deterministic member of the context set CTS."
  (first cts))
;
;
; =============================================================================
;
; others.cts 
; ----------
;
;       arguments     : cts - <context set>
;
;       returns       : <context set>
;
;       description   : returns a <context set> identical to "cts" but without
;                       the element that would be chosen by choose.cts
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defun others.cts (cts)
  "Returns a context set like CTS, but with (CHOOSE.CTS CTS) deleted."
   (rest cts))
;
;
; =============================================================================

(defmacro do.cts ((var ctsform &optional resultform) &body forms)
  `(dolist (,var ,ctsform ,resultform) ,@forms))

; =============================================================================
;
; intersect.cts   
; -------------
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set>
;
;       returns       : <context set>
;
;       description   : returns the intersection of "cts1" and "cts2"
;
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defun intersect.cts (cts1 cts2)
  "Returns the intersection of the context sets CTS1 and CTS2."
 (let* ((result (list nil)) (last result))
    (loop (cond ((null cts1) (return))
		((null cts2) (return))
		((isless.ct (first cts1) (first cts2))
		 (setq cts1 (rest cts1)))
		((isless.ct (first cts2) (first cts1))
		 (setq cts2 (rest cts2)))
		(t (rplacd last (list (first cts1)))
		   (setq last (rest last))
		   (setq cts1 (rest cts1))
		   (setq cts2 (rest cts2)))))
    (rest result)))

;
;
; =============================================================================
;
; compl.cts 
; ---------
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set> 
;
;       returns       : <context set>
;
;       description   : returns the set difference cts1 - cts2. 
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defun compl.cts (cts1 cts2)
  "Returns the relative complement of context set CTS1 and context set CTS2."
 (let* ((result (list nil)) (last result))
    (loop (cond ((null cts1) (return))
		((null cts2) (rplacd last cts1) (return))
		((isless.ct (first cts1) (first cts2))
		 (rplacd last (list (first cts1)))
		 (setq last (rest last))
		 (setq cts1 (rest cts1)))
		((isless.ct (first cts2) (first cts1))
		 (setq cts2 (rest cts2)))
		(t (setq cts1 (rest cts1))
		   (setq cts2 (rest cts2)))))
    (rest result)))

;
;
; =============================================================================
;
; ismemb.cts    
; ----------
;
;       arguments     : ct - <context>
;                       cts - <context set> 
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "ct" is an element of "cts",
;                       "false" otherwise.
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defun ismemb.cts (ct cts)
  "Returns T if context CT is in the context set CTS."
  (cond ((null cts) nil)
	((isless.ct ct (first cts)) nil)
	((iseq.ct ct (first cts)) t)
	(t (ismemb.cts ct (rest cts)))))
;
;
; =============================================================================
;
; issubset.cts
; ------------
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "cts1" is a subset of "cts2", 
;                       "false" otherwise.
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defun issubset.cts (cts1 cts2)
  "Returns T if context set CTS1 is a subset of context set CTS2; NIL otherwise."
  (loop
    (cond ((null cts1) (return t))
	  ((null cts2) (return nil))
	  ((isless.ct (first cts1) (first cts2)) (return nil))
	  ((isless.ct (first cts2) (first cts1)) (setq cts2 (rest cts2)))
	  (t (setq cts1 (rest cts1)
		   cts2 (rest cts2))))))
	
;
;
; =============================================================================
;
; iseq.cts 
; --------
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set>
;
;       returns       : <boolean>
;
;       description   : Compares "cts1" and "cts2" as sets : returns "true" if
;                       "cts1" and "cts2" have exactly the same elements,
;                       "false" otherwise.
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
;
(defmacro iseq.cts (cts1 cts2)
  "Returns T if context sets CTS1 and CTS2 are equal; NIL otherwise."
  `(equal ,cts1 ,cts2))
;
;
; =============================================================================
;
; describe.cts
; ------------
;
;       arguments     : cts - <context set>
;
;       returns       : <sequence>
;
;       description   : It returns a <sequence> which is a description of 
;                       the <context set> "cts" to be printed.
;
;                                        written :  njm 09/17/88  
;                                        modified:  
;
(defmacro describe.cts (cts)
  "Returns a sequence which is a printable version of the context set CTS."
  cts)


;
; =============================================================================
;
; cardinality.cts
; ---------------
;
;       arguments     : cts - <context set>
;
;       returns       : <non-negative integer>
;
;       description   : returns the number of <context>s in CTS
;
;                                        written :  njm 09/17/88 
;                                        modified:
;
;
(defmacro cardinality.cts (cts)
  `(length ,cts))

;
; =============================================================================
;
; remove-if-not.cts
; -----------------
;
;       arguments     : test - <function>
;                       cts - <context set>
;
;       returns       : <context set>
;
;       description   : Returns a <context set> just like CTS, but with all 
;                       <context>s failing the TEST removed.
;
;                                        written :  njm 09/17/88 
;                                        modified:
;
(defmacro remove-if-not.cts (test cts)
  "Returns a <context set> just like CTS, but with all <contexts> failing the TEST removed."
  `(remove-if-not ,test ,cts))



    
    




