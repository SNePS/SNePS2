;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: relset.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




(in-package :sneps)


; =============================================================================
; Data Type:  <relation set> ::=
;                  (<relation> <relation> ... <relation>) | () | <relation>  
; =============================================================================
;
; 
; =============================================================================
;
; new.rs 
; ------
;
;       returns       : <relation set>
;
;       description   : creates a <newrelationset>
;
;                                        written:  rmo 07/28/83
;                                        modified:
;
;
(defmacro new.rs ()
   `())  
;
;
; =============================================================================
;
; is.rs
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "u" is a <relation set>,
;                       "false" otherwise.
;
;       implementation: it does not check if all the elements of "u" are
;                       <relation>s, it just checks the first one.
;
;
;                                        written : rmo 07/28/83 
;                                        modified: ejm 08/30/83
;
;
(defmacro is.rs (u)
  `(or (and (listp ,u) (not (null (rest ,u))) (or (null ,u) (is.r (first ,u))))
       (is.r ,u)))
;
;
; =============================================================================
;
; isall.rs
; --------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if "u" is a <relation set>,
;                       "false" otherwise.
;
;       implementation: Unlike "is.rs", "isall.rs" tests whether all the 
;                       elements of "u" are <relation>s.
;
;                                        written :  ejm 06/19/84
;                                        modified:
;
;
(defmacro isall.rs (u)
   `(and (listp ,u) 
         (isall1.rs ,u)))
;
;
; =============================================================================
;
; isall1.rs
; ---------
;
;       arguments     : s - <sequence>
;
;       returns       : <boolean>
;
;       description   : Auxiliary function to isall.rs. It tests wether
;                       "s" is a <relation set> by checking whether all
;                       the elements of "s" are <relation>s.
;
;                                        written :  ejm 06/19/84
;                                        modified:
;
;(declare (localf isall1.rs))
;
(defun isall1.rs (s)
   (declare (special s))
   (cond ((null s) t)
         ((is.r (first s)) (isall1.rs (rest s)))
         (t nil)))
;
;
; =============================================================================
;
; isnew.rs 
; --------
;
;       arguments     : rs - <relation set> 
;
;       returns       : <boolean>
;
;       description   : returns "true" if "rs" is a <newrelationset>,
;                       "false" otherwise.
;
;                                        written:  rmo 07/28/83
;                                        modified:
;
;
(defmacro isnew.rs (rs)
   `(null ,rs))
;
;
; =============================================================================
;
; insert.rs 
; ---------
;
;       arguments     : r - <relation>
;                       rs - <relation set>
;
;       returns       : <relation set> 
;
;       description   : returns a <relation set> identical to "rs" but with "r"
;                       as a new  <relation> if "r" was not yet in "rs".
;                       if "r" was in "rs" it just returns "rs" unchanged.
;
;                                        written:  rmo 07/28/83
;                                        modified:
;
;
(defmacro insert.rs (r rs)
  `(cond((listp ,rs) (cond ((null ,rs) ,r)
			   ((member ,r ,rs) ,rs)
			   (t (cons ,r ,rs))))
	((null ,r) ,rs) 
	((eq ,r ,rs) ,rs)
	(t (list ,r ,rs))))
;
; =============================================================================
;
; singlechk.rs 
; ------------
;
;       arguments     : L - a list 
;
;       returns       : a <relation set> 
;
;       description   : singlechk.rs checks the list for length. if it is of 
;                       length one,it returns the atom. if > 1 then the list is
;                       returned. 
;                                        written :  RMR 11/2?/83 
;                                        modified:
;
;
(defmacro singlechk.rs (rs)
     `(cond ((null (rest ,rs)) (first ,rs))
            (t ,rs)))
 
; =============================================================================
;
; make.rs
; -------
;
;       arguments     : r ... r - <relation>(s)
;
;       returns       : <relation set>
;
;       description   : returns a <relation set> composed of the <relation>(s)
;                       passed as argument(s).
;
;       implementation: It assumes there is no repetitions of the same
;                       <relation> in the argument list in order to avoid
;                       extra checking.
;
;                                        written :  ejm 09/19/83
;                                        modified:
;
;
(defmacro make.rs (&rest r)
   `(singlechk.rs (list ,@r)))         ; works with both
;  `(singlechk.rs ',r))                ; works only  with MaCrO = nil
;  `(singlechk.rs (list ,@r)))       ; works only  with MaCrO = t
;
; =============================================================================
;
; mklst.rs 
; --------
;
;       arguments     : rs - a <relation set> 
;
;       returns       : rs if rs is a list, (rs) otherwise
;
;       description   : if rs is a list it is returned unchanged. if it is an 
;                       atom, it is returned contained in a list 
;
;                                        written : RMR 11/2?/83 
;                                        modified:
;
;
(defmacro mklst.rs (rs)
     `(cond ((listp ,rs) ,rs) (t (list ,rs))))
;
; =============================================================================
;
; compl.rs 
; -------
;
;       arguments     : rs1 - <relation set>
;                       rs2 - <relation set> 
;
;       returns       : <relation set>
;
;       description   : returns the set differense rs1 - rs2. 
;
;       implementation: lrs1 - local rs1
;                       lrs2 - local rs2
;                       necessary  because "dem" does not let ,rs1  and
;                       ,rs2 to be assigned (BUG)
;
;                                        written:  rmo 07/28/83
;                                        modified: ejm 06/19/84
;
;
;(defmacro compl.rs (rs1 rs2)
;   `(cond ((and ,rs1 ,rs2)
;           (let ((lrs1 (mklst.rs ,rs1)) (lrs2 (mklst.rs ,rs2)))
;                (singlechk.rs
;                   (mapcon (function
;                               (lambda (rs)
;                                    (if (not
;                                         (member (first rs) lrs2))
;                                        (list (first rs)))))
;                           lrs1))))   
;          (t ,rs1)))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defmacro compl.rs (rs1 rs2)
   `(cond ((and ,rs1 ,rs2)
           (let ((lrs1 (mklst.rs ,rs1)) (lrs2 (mklst.rs ,rs2)))
                (singlechk.rs
                    (apply #'nconc (maplist (function
                               (lambda (rs)
                                    (if (not
                                         (member (first rs) lrs2))
                                        (list (first rs)))))
                           lrs1))))   )
          (t ,rs1)))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
; =============================================================================
;
; union.rs 
; -------------
;
;       arguments     : rs1 - <relation set>
;                       rs2 - <relation set>
;
;       returns       : <relation set>
;
;       description   : returns the union of "rs1" and "rs2"
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro union.rs (rs1 rs2)
  `(singlechk.rs
	     (nconc (mklst.rs (compl.rs ,rs1 ,rs2))
		    (mklst.rs ,rs2)))) 
;
;
; =============================================================================
;
; remove.rs     
; ---------
;
;       arguments     : r - <relation>
;                       rs - <relation set>
;
;       returns       : <relation set>
;
;       description   : returns a <relation set> identical to "rs" but with "r"
;                       removed if it was there. If "r" was not in "rs"
;                       it just returns "rs" unchanged.
;
;                                        written:  rmo 08/15/83
;                                        modified:
;
;
(defmacro remove.rs (r rs)
               `(cond ((listp ,rs) (singlechk.rs (remove ,r ,rs))) 
                      ((eq ,r ,rs) nil)
                      (t ,rs)))
;
;
; =============================================================================
;
; choose.rs 
; ---------
;
;       arguments     : rs - <relation set>
;
;       returns       : <relation> 
;
;       description   : returns the first <relation> of the <relation set> 
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro choose.rs (rs)
   `(cond ((listp ,rs) (first ,rs)) (t ,rs)))
;
;
; =============================================================================
;
; others.rs 
; ---------
;
;       arguments     : rs - <relation set>
;
;       returns       : <relation set>
;
;       description   : returns a <relation set> identical to "rs" but without
;                       the first relation 
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro others.rs (rs)
   `(cond ((listp ,rs) (rest ,rs)) (t nil)))
;
;
;; =============================================================================
;
; ismemb.rs    
; ---------
;
;       arguments     : r - <relation>
;                       rs - <relation set> 
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "r"  is an element of "rs",
;                       "false" otherwise.
;
;                                        written:  rmo 07/28/83
;                                        modified:
;
;
(defmacro ismemb.rs  (r rs)
   `(cond ((listp ,rs) (member ,r ,rs))
          ((eq ,r ,rs) t)
          (t nil)))

;
;
; =============================================================================
;
; do.rs
; -----
;                                        written:  ssc  5/10/89
;                                        modified:  hc 07/07/93
;
;
;
(defmacro do.rs ((var rsform &optional resultform) &body forms)
  "iterator for relation sets."
  `(dolist (,var (mklst.rs ,rsform) ,resultform) ,@forms))
;
;
; =============================================================================
;
; issubset.rs
; -----------
;
;       arguments     : rs1 - <relation set>
;                       rs2 - <relation set>
;
;       returns       : <boolean>
;
;       description   : Returns "true" if "rs1" is a subset of "rs2",
;                       "false" otherwise.  
;
;                                        written:  rmo 07/28/83
;                                        modified: ssc  5/10/89
;
;
(defmacro issubset.rs (rs1 rs2)
  `(do.rs (current-r ,rs1 t) 
     (unless (ismemb.rs current-r ,rs2) (return nil))))
;
;
; =============================================================================
;
; iseq.rs 
; -------
;
;       arguments     : rs1 - <relation set>
;                       rs2 - <relation set>
;
;       returns       : <boolean>
;
;       description   : Compares "rs1" and "rs2" : returns "true" if
;                       "rs1" and "rs2" have exactly the same elements,
;                       "false" otherwise.
;
;                                        written:  rmo 07/28/83 
;                                        modified:
;
;
(defmacro iseq.rs (rs1 rs2)
   `(and (issubset.rs ,rs1 ,rs2)
         (issubset.rs ,rs2 ,rs1)))
 
; =============================================================================
;
; reverse.rs 
; ----------
;
;       arguments     : rs - a <relation set>
;
;       returns       : rs reversed
;
;       description   : reverse.rs reverses the relations in a relset 
;
;                                        written :  EJM 11/2?/83 
;                                        modified:
;
;
(defmacro reverse.rs (rs)
     `(cond ((atom ,rs) ,rs) (t (reverse ,rs))))
 
; =============================================================================
;
; describe.rs
; ------------
;
;       arguments     : rs - <relation set>
;
;       returns       : <sequence>
;
;       description   : It returns a <sequence> which is a description of 
;                       the <relation set> "rs" to be printed.
;
;                                        written :  ejm 06/05/84
;                                        modified:
;
(defmacro describe.rs (rs)
   `(mapcar (function (lambda (r) 
                         (describe.r r)))
            ,rs))
;
;
; =============================================================================



    
    




