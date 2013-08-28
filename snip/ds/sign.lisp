;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: sign.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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




(in-package :snip)


; =============================================================================
;
; <sign>  ::=  POS | NEG
;
; =============================================================================
;
; RECOGNIZERS    is.sign   : <universal> --> <boolean>
;
; TESTS          iseq.sign : <sign> x <sign> --> <boolean>
;
; =============================================================================
;
; is.sign
; -------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <sign>, "false" otherwise
;
;                                        written :  rgh 11/10/85
;                                        modified:
;
;
(defmacro is.sign (u)
  `(and (atom ,u)
        (or (eq ,u 'POS)
            (eq ,u 'NEG))))
;
;
; =============================================================================
;
; iseq.sign
; ---------
;
;       arguments     : s1 - <sign>
;                       s2 - <sign>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "s1" and "s2" are equal
;
;                                        written :  rgh 11/30/85
;                                        modified:
;
;
(defmacro iseq.sign (s1 s2)
  `(eq ,s1 ,s2))
;
;
; =============================================================================



    
    




