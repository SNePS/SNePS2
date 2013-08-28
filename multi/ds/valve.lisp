;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MULTI; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: valve.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :multi)


; =============================================================================
;
; <valve>  ::=  OPEN | CLOSED
;
; -----------------------------------------------------------------------------
; RECOGNIZERS    is.valve   : <universal> --> <boolean>
;
; =============================================================================
;
; is.valve
; --------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <valve>, "false" otherwise
;
;                                        written :  rgh 11/18/85
;                                        modified:
;
;
(defmacro is.valve (u)
  `(and (atom ,u)
        (or (eq ,u 'OPEN)
            (eq ,u 'CLOSED))))
;
;
; =============================================================================



    
    




