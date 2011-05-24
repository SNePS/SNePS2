;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: svarset.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


;=============================================================================
; Data Type  <svar set> ::= (<svar> <svar> ... <svar>)  
; =============================================================================
;
; 
; =============================================================================
;
; new.svs 
; -------
;
;       returns       : <svar set>
;
;       description   : creates a <newsvarset>
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
(defmacro new.svs ()
   `())
;
;
; =============================================================================
;
; is.svs
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "u" is a <svar set>,
;                       "false" otherwise.
;
;       implementation: it does not check if all the elements of "u" are
;                       <svar>s, it just checks the first one.
;
;
;                                        written : ejm 10/18/83 
;                                        modified: 
;
;
(defmacro is.svs (u)
   `(and (listp ,u)
         (or (null ,u)
             (is.sv (first ,u)))))
;
;
; =============================================================================
;
; isnew.svs 
; ---------
;
;       arguments     : svs - <svar set> 
;
;       returns       : <boolean>
;
;       description   : returns "true" if "svs" is a <svar set>,
;                       "false" otherwise.
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
(defmacro isnew.svs (svs)
   `(null ,svs))
;
;
; =============================================================================
;
; insert.svs 
; ----------
;
;       arguments     : sv - <svar>
;                       svs - <svar set>
;
;       returns       : <svar set> 
;
;       description   : returns a <svar set> identical to "svs" but with "sv"
;                       as a new  <svar> if "sv" was not yet in "svs".
;                       if "sv" was in "svs" it just returns "svs" unchanged.
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
;(declare (localf insert.svs))
(defun insert.svs (sv svs)
   (declare (special sv svs))
   (cond ((member sv svs) svs)
         (t (cons sv svs))))
;
;
; =============================================================================
;
; make.svs
; --------
;
;       arguments     : sv ... sv - <svar>s
;
;       returns       : <svar set>
;
;       description   : returns a <svar set> composed of the <svar>s
;                       passed as arguments.
;
;       implementation: It assumes there is no repetitions of the same
;                       <svar> in the argument list in order to avoid
;                       extra checking.
;
;                                        written :  ejm 09/19/83
;                                        modified:
;
;
(defmacro make.svs (&rest sv)
   `',sv)
;
;
; =============================================================================
;
; compl.svs 
; ---------
;
;       arguments     : svs1 - <svar set>
;                       svs2 - <svar set> 
;
;       returns       : <svar set>
;
;       description   : returns the set differense svs1 - svs2. 
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
;(defmacro compl.svs (svs1 svs2)
;   `(cond ((and ,svs1 ,svs2)
;           (mapcon
;              '(lambda (svs)
;                 (if (not (member (first svs) ,svs2))
;                     (list (first svs))))
;              ,svs1))
;          (t ,svs1)))
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defmacro compl.svs (svs1 svs2)
   `(cond ((and ,svs1 ,svs2)
           (apply #'nconc (maplist 
              '(lambda (svs)
                 (if (not (member (first svs) ,svs2))
                     (list (first svs))))
              ,svs1)))
          (t ,svs1)))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;
; =============================================================================
;
; union.svs 
; ---------
;
;       arguments     : svs1 - <svar set>
;                       svs2 - <svar set>
;
;       returns       : <svar set>
;
;       description   : returns the union of "svs1" and "svs2"
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
(defmacro union.svs (svs1 svs2)
   `(nconc (compl.svs ,svs1 ,svs2) ,svs2))
;
;
; =============================================================================
;
; remove.svs     
; ----------
;
;       arguments     : sv - <svar>
;                       svs - <svar set>
;
;       returns       : <svar set>
;
;       description   : returns a <svar set> identical to "svs" but with "sv"
;                       removed if it was there. If "sv" was not in "svs"
;                       it just returns "svs" unchanged.
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
;(declare (localf remove.svs))
(defun remove.svs (sv svs)
   (declare (special sv svs))
   (remove sv svs)) 
;
;
; =============================================================================
;
; choose.svs 
; ----------
;
;       arguments     : svs - <svar set>
;
;       returns       : <svar> 
;
;       description   : returns a <svar> of the <svar set> 
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
(defmacro choose.svs (svs)
   `(first ,svs))
;
;
; =============================================================================
;
; others.svs 
; ----------
;
;       arguments     : svs - <svar set>
;
;       returns       : <svar set>
;
;       description   : returns a <svar set> identical to "svs" but without
;                       the element that would be chosen by choose.svs
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
(defmacro others.svs (svs)
   `(rest ,svs))
;
;
; =============================================================================
;
; ismemb.svs    
; ----------
;
;       arguments     : sv - <svar>
;                       svs - <svar set> 
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "sv"  is an element of "svs",
;                       "false" otherwise.
;
;                                        written:  ejm 10/18/83
;                                        modified:
;
;
(defmacro ismemb.svs  (sv svs)
   `(member ,sv ,svs))
;
;
; =============================================================================
;
; do.svs
; ------
;
;                                        written:  ssc  5/10/89
;                                        modified:
;
;
;
(defmacro do.svs ((var svsform &optional resultform) &body forms)
  "iterator for SNePS variable sets."
  `(dolist (,var ,svsform ,resultform) ,@forms))
;
;
; =============================================================================
;
; issubset.svs
; ------------
;
;       arguments     : svs1 - <svar set>
;                       svs2 - <svar set>
;
;       returns       : <boolean>
;
;       description   : Returns "true" if "svs1" is a subset of "svs2",
;                       "false" otherwise.  
;
;                                        written:  ejm 10/18/83
;                                        modified: ssc  5/10/89
;
;
(defmacro issubset.svs (svs1 svs2)
  `(do.svs ((svs ,svs1 t))
     (unless (ismemb.svs svs ,svs2)
       (return nil))))
;
;
; =============================================================================
;
; iseq.svs 
; -------
;
;       arguments     : svs1 - <svar set>
;                       svs2 - <svar set>
;
;       returns       : <boolean>
;
;       description   : Compares "svs1" and "svs2" as sets : returns "true" if
;                       "svs1" and "svs2" have exactly the same elements,
;                       "false" otherwise.
;
;                                        written:  ejm 10/18/83 
;                                        modified:
;
;
(defmacro iseq.svs (svs1 svs2)
   `(and (issubset.svs ,svs1 ,svs2)
         (issubset.svs ,svs2 ,svs1)))
;
;
; =============================================================================



    
    




