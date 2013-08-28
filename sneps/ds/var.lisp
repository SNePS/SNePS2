;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: var.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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
; Data Type:  <var>
; =============================================================================
;
;
; =============================================================================
;
; is.v
; -----
;
;       arguments     : sexpr - <any type>
;
;       returns       : <boolean>
;
;       description   : If sexpr is a <var> the function returns t, 
;                       otherwise it returns nil.
;
;                                        written  : ejm 8/11/83
;                                        modified : ejm 10/11/83
;
;
(defmacro is.v (sexpr)
   `(or (and (is.n ,sexpr)
             (isvar.n ,sexpr))
        (is.sv ,sexpr)))
;
;
; =============================================================================
;
; isnew.v
; --------
;
;       arguments     : var - <var>
;
;       returns       : <boolean>
;
;       description   : If var is a <newvar> the function returns t, 
;                       otherwise it returns nil.
;
;                                        written:  ejm 8/11/83 
;                                        modified:
;
;
(defmacro isnew.v (var)
   `(cond ((is.v ,var) (not (get ,var ':val)))
          (t (error "function isnew.v -- argument not a variable"))))
;
;
; =============================================================================
;
; value.v
; --------
;
;       arguments     : var - <var>
;
;       returns       : <value>
;
;       description   : It returns the <value> of var.
;
;                                        written:  ejm 8/11/83
;                                        modified:
;
;
(defmacro value.v (var)
   `(get ,var ':val))
;
;
; =============================================================================
;
; set.v
; ------
;
;       arguments     : var - <var>
;                       seqval  - <value>
;
;       returns       : <value>
;
;       description   : Given the <var> "var" and the sequence <value> 
;                       "seqval", it sets the <value> of var to seqval.
;
;       side-effects  : It side effects the plist of var.
;
;                                        written:  ejm 8/11/83
;                                        modified:
;
;       note: this fnct calls new.v, which is undefined!
;                              -- scs 10/14/87
;
;
(defmacro set.v (var seqval)
   `(cond ((is.v ,var) (setf (get ,var ':val) ,seqval))
          (t (new.v ,var) (setf (get ,var ':val) ,seqval))))
;
;
; =============================================================================
;
; update.v
; ---------
;
;       arguments     : var - <var>
;                       val  - <value>
;
;       returns       : <value>
;
;       description   : Given the <var> var and the individual <value> val
;                       it adds val to the <value> of var.
;
;       side-effects  : It side effects the plist of var.
;
;                                        written:  ejm 8/11/83
;                                        modified:
;
;
(defmacro update.v (var indval)
   `(cond ((and (is.n ,indval) (is.ns (value.v ,var)))
           (set.v ,var (insert.ns ,indval (value.v ,var))))
          ((and (is.r ,indval) (is.rs (value.v ,var)))
           (set.v ,var (insert.rs ,indval (value.v ,var))))
          (t (error "function update.v -- illegal type"))))
;
;
; =============================================================================
;
; iseq.v
; -------
;
;       arguments     : var1 - <var>
;                       var2 - <var>
;
;       returns       : <boolean>
;
;       description   : It compares two <var>s and returns t if they are 
;                       equal, nil otherwise.
;
;       implementation: Since the implementation of the values of a <svar>
;                       and a <variable node> are identical, this function
;                       uses the <svar> test.
;
;                                        written:  ejm 8/11/83
;                                        modified:
;
;
(defmacro iseq.v (var1 var2)
   `(iseq.sv ,var1 ,var2))
;
;
; =============================================================================




    
    




