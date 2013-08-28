;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: snd.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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
; Data Type:  <snepsul node description> ::=
;                   ( <relation form> <nodeset form> 
;                                  . . .
;                     <relation form> <nodeset form> )
; =============================================================================
;
;
; =============================================================================
;
; isnew.snd
; ---------
;
;       arguments     :  snd - <snepsul node description>
;
;       returns       :  <boolean>
;
;       description   :  Returns t if "snd" is a new <snepsul node 
;                        description>, nil otherwise.
;
;       implementation: 
;
;                                        written :  jms 9/20/83
;                                        modified:
;
;
(defmacro isnew.snd (snd)
   `(null ,snd))
;
;
; =============================================================================
;
; nodesetform.snd
; ---------------
;
;       arguments     : snd - <snepsul node description>
;                             Assumed to be represented as a list.
;
;       returns       : <nodeset form>
;
;       description   :   It returns the first <nodeset form> from snd.
;                         But first, if the first <nodeset form> in snd is followed by
;                         a postfix operator (!) or an infix operator, snd is changed
;                         by side-effect to incorporate the Cambridge Prefix version
;                         into the first <nodeset form>.
;
;       side-effects  : The <snepsul node description> which is the value of snd.
;
;                                        written :  jms 9/20/83
;                                        modified:  scs 2/24/87
;
;
(defun nodesetform.snd (snd)
  (loop
    (cond ((eq (third snd) '!)
	   (setf (cdr snd)
		 (cons (list '! (second snd)) (cdddr snd))))
	  ((isrearrange.com (third snd))
	   (cond ((fourth snd)
		  (setf (cdr snd)
			(cons (list (third snd) (second snd) (fourth snd)) (cddddr snd))))
		 (t (sneps-error (format nil
					 "Infix operator missing second operand: ~A"
					 (cddr snd))
				 '|Evaluation of node description|
				 'nodesetform.snd))))
	  (t (return (second snd))))))
;
;
; =============================================================================
;
; relationform.snd
; ----------------
;
;       arguments     : snd - <snepsul node description>
;
;       returns       :  <relation form>
;
;       description   :   It returns the first <relation form> from snd.
;
;                                        written :  jms 9/20/83
;                                        modified:
;
;
(defmacro relationform.snd (snd)
   `(first ,snd))
;
;
; =============================================================================
;
; rest.snd
; --------
;
;       arguments     : snd - <snepsul node description>
;
;       returns       : <snepsul node description>
;
;       description   :   It returns snd without its first <relation> -
;                       <nodeset form> pair.
;
;                                        written :  jms 9/20/83
;                                        modified:
;
;
 
(defmacro rest.snd (snd)
   `(rest (rest ,snd)))
;
; =============================================================================



    
    




