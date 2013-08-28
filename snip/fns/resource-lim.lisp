;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: resource-lim.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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
; decrease-resources
; ------------------
;
;       returns       : <integer>
;
;       nonlocal-vars : REMAINING-RESOURCES
;
;       description   : reduces the available resources whenever match or
;                       forward-match is called
;
;       side-effects  : changes the value of REMAINING-RESOURCES
;
;       implementation: tempororily de-activated for initial tests
;
;                                        written :  rgh 10/05/85
;                                        modified:
;
;
(defmacro decrease-resources ()
  `t)
;
;
; =============================================================================
;
; enough-resources
; ----------------
;
;       returns       : <boolean>
;
;       nonlocal-vars : REMAINING-RESOURCES
;
;       description   : checks to see if there are enough resources available
;                       to perform a match
;
;       implementation: tempororily de-activated for initial tests
;
;                                        written :  rgh 10/05/85
;                                        modified:
;
;
(defmacro enough-resources ()
  `t)
;
;
; =============================================================================



    
    




