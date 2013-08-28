;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: rel1.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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
; Data Type:  <relation>
; =============================================================================
;
; 
; =============================================================================
;
; is.r         
; ----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <relation>, otherwise
;                       "false".
;
;                                        written:  mja 07/28/83
;                                        modified: ejm 06/05/84
;                                        modified: scs 12/31/87 acc to scottlog
;
;
(defmacro is.r (u)
  `(if (symbolp ,u) (get ,u '=conv)))
;
;
; =============================================================================
;
; iseq.r
; ------
;
;       arguments     : r1 - <relation>
;                       r2 - <relation>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "r1" and "r2" are the same
;                       <relation>, "false" otherwise.
;
;                                        written:  ejm 08/19/83
;                                        modified:
;
;
(defmacro iseq.r (r1 r2)
   `(eq ,r1 ,r2))
;
;
; =============================================================================
;
; describe.r
; ----------
;
;       arguments     : r - <relation>
;
;       returns       : <atom>
;
;       description   : It returns an  <atom> which is a description of
;                       the  <relation> "r" to be printed.
;                       The description is identical to "r".
;
;                                        written :  ejm 06/05/84
;                                        modified:
;
(defmacro describe.r (r)
   `,r)
;
;
; =============================================================================



    
    




