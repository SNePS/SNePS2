;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: destination.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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
; <destination> ::=  <node> | 'USER
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.dest      : <universal> --> <boolean>
;                iseq.dest    : <destination> x <destination> --> <boolean>
;                is-user.dest : <destination> --> <boolean>
;
; =============================================================================
;
; is.dest
; -------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <destination>,
;                               "false" otherwise
;
;                                        written :  rgh  3/08/86
;                                        modified:
;
;
(defun is.dest (u)
  (or (eq u 'USER) (is.n u)))
;
;
; =============================================================================
;
; iseq.dest
; ---------
;
;       arguments     : d1, d2 - <destination>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "d1" and "d2" are equivalent
;                       <destination>s, "false" otherwise
;
;                                        written :  rgh  3/22/86
;                                        modified:
;
;
(defun iseq.dest (d1 d2)
  (or (and (eq d1 'USER) (eq d2 'USER))
      (and (is.n d1) (is.n d2) (iseq.n d1 d2))))
;
;
; =============================================================================
;
; is-user.dest
; ------------
;
;       arguments     : d - <destination>
;
;       returns       : <boolean>
;
;       description   : Returns "true" if "d" is 'USER, "false" otherwise
;
;                                        written :  rgh  3/08/86
;                                        modified:
;
;
(defun is-user.dest (d)
  (eq d 'USER))
;
;
; =============================================================================



    
    




