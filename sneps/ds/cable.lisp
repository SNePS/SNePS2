;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: cable.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; Data Type:  <cable> ::= (<relation> . <node set>)     
; =============================================================================
;
; 
; =============================================================================
;
; new.c
; -----
;
;       arguments     : r - <relation>
;                       ns - <node set>
;
;       returns       : <cable>
;
;       description   : Creates a <newcable> from "r" and "ns".
;
;                                        written : rmo 07/28/63
;                                        modified:
;
;
(defmacro new.c (r ns)
   `(cons ,r ,ns))
;
;
; =============================================================================
;
; is.c
; ----
;
;       arguments     : u - <universal> 
;
;       returns       : <boolean>
;
;       description   : returns "true" if it is a <cable>, "false"
;                       otherwise. 
;
;                                        written : rmo 07/28/83
;                                        modified:
;
;
(defmacro is.c (u)
   `(and (consp ,u)
         (is.r (car ,u))
         (is.ns (cdr ,u))))
;
; =============================================================================
;
; update.c 
; --------
;
;       arguments     : ns - <node set>
;                       c - <cable>
;
;       returns       : <cable>
;
;       description   : returns a <cable> with the same <relation> of "c"
;                       and with a <node set> which is the union of "ns" with
;                       the <node set> of "c".
;
;                                        written : rmo 07/28/83
;                                        modified:
;
;
(defmacro update.c (ns c)
   `(cons (car ,c)
          (union.ns (cdr ,c) ,ns)))
;
;
; =============================================================================
;
; relation.c 
; ----------
;
;       arguments     : c - <cable>
;
;       returns       : <relation>       
;
;       description   : Returns the <relation> of "c".
;
;                                        written : rmo 07/28/83
;                                        modified:
;
;
(defmacro relation.c (c)
   `(car ,c))
;
;
; =============================================================================
;
; nodeset.c 
; ---------
;
;       arguments     : c - <cable>
;
;       returns       : <node set> 
;
;       description   : returns the <node set> of "c".
;
;                                        written : rmo 07/28/83
;                                        modified:
;
;
(defmacro nodeset.c (c)
   `(cdr ,c))
;
;
; =============================================================================
;
; iseq.c
; ------
;
;       arguments     : c1 - <cable>
;                       c2 - <cable>
;
;       returns       : <boolean>
;
;       description   : Returns "true" if "c1" and "c2" are equal as <cable>s,
;                       that is, if they have the same <relation> and the same
;                       nodeset, "false" otherwise.
;
;                                        written : rmo 07/28/83
;                                        modified: ejm 08/30/83
;
;
(defmacro iseq.c (c1 c2)
   `(and (iseq.r (relation.c ,c1) (relation.c ,c2))
         (iseq.ns (nodeset.c ,c1) (nodeset.c ,c2))))
;
;
; =============================================================================
;
; describe.c
; ----------
;
;       arguments     : c - <cable>
;
;       returns       : <dotted pair>
;
;       description   : It returns a <dotted pair> which is a description of 
;                       the <cable> "c" to be printed.
;
;                                        written :  ejm 06/05/84
;                                        modified:
;
(defmacro describe.c (c)
   `(cons (describe.r  (relation.c ,c))
          (describe.ns (nodeset.c ,c))))
;
;
; =============================================================================

 
;;;
;;;  Functions used in Match
;;;
;;;
;;; =============================================================================
;;;
;;; rename.c
;;; --------
;;;
;;;       arguments     : c - <cable> 
;;;
;;;       returns       : <mcable> 
;;;
;;;       description   : renames the <node>s in the <node set> of 'c' 
;;;
;;;       side-effects  : *RENAMESUB*, *MNRS* 
;;;
;;;                                        written :  vhs 06/17/85
;;;                                        modified:
;;;
;;;
#|
(defmacro rename.c (c)

   "Renames the <node>s in the <node set> of 'c'."

  `(new.mc (relation.mc ,c) (rename.ns (nodeset.c ,c))))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; changed.c
;;; ---------
;;;
;;;       arguments     : c - <cable> 
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : returns true if any <node> in 'c' is bound in *SUB*,
;;;                       or dominates a <node> which is bound, 'false' ow.
;;;
;;;                                        written :  vhs 06/17/85
;;;                                        modified:
;;;
;;;
#|
(defun changed.c (c)
   "Returns true if any <node> in 'c' is bound in *SUB*,
    or dominates a <node> which is bound, 'false' ow."
  (changed.ns (nodeset.c c)))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; all-instances.c
;;; ---------------
;;;
;;;       arguments     : c - <cable> 
;;;
;;;       description   : Finds all <pattern node>s in 'c' which dominate
;;;                       variables bound in *SUB*, and binds them to their
;;;                       instances.
;;;
;;;       side-effects  : *FINAL-TARGET-SUB*, *CHANGED*, *FINALMNRS*
;;;
;;;                                        written :  vhs 06/17/85
;;;                                        modified:
;;;
;;;
#|
(defun all-instances.c (c)
  "Finds all <pattern node>s in 'c' which dominate
   variables bound in *SUB*, and binds them to their instances."
  (mapc
   (function
    (lambda (n) (match::add-instance.n n)))
   (nodeset.c c)))
|#
;;;
;;;
;;; =============================================================================



    
    




