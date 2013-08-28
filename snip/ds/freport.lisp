;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: freport.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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
; <freport>  ::=  ( <report> . <context> )
;
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.frep      :  <universal> --> <boolean>
;
;
; CONSTRUCTORS   make.frep    :  <report> x <context> --> <freport>
;                                  
;
; SELECTORS      report.frep  :  <freport> --> <report>
;                context.frep   :  <freport> --> <context>
;      
;
; TESTS          iseq.frep  :  <freport> x <freport> --> <boolean>
;
;
; =============================================================================
;
; is.frep
; -------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "u" is a <freport>,
;                               "false" otherwise
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;                                        modified:  
;
(defmacro is.frep (u)
  `(and (consp ,u)
        (is.rep (car ,u))
	(is.ct (cdr ,u))))
;
; =============================================================================
;
; make.frep
; ---------
;
;       arguments     : rep - <report>
;                       ct  - <context>                                        
;
;       returns       : <freport>
;
;       description   : returns a <freport> consisting of the components
;                       passed as arguments
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;                                        modified:  
;
(defmacro make.frep (rep ct)
  `(cons ,rep ,ct))
;
;
; =============================================================================
;
; report.frep
; -----------
;
;       arguments     : freport  -  <freport>
;
;       returns       : <report>
;
;       description   : selects the <report> of the freport
;
;                                        written :  cpf 10/25/88;                                   
;                                        modified: 
;
(defmacro report.frep (freport)
   `(car ,freport))
;
;
; =============================================================================
;
; context.frep
; ------------
;
;       arguments     : freport  -  <freport>
;
;       returns       : <context>
;
;       description   : selects the <context> of the freport
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;
;
(defmacro context.frep (freport)
   `(cdr ,freport))
;
;
; =============================================================================
;
; iseq.frep
; ---------
;
;       arguments     : fr1 - <freport>
;                       fr2 - <freport>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "fr1" and "fr2" are equal
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;                                        modified:
;
(defmacro iseq.frep (fr1 fr2)
  `(and (iseq.ct (context.frep ,fr1) (context.frep ,fr2))
	(iseq.rep (report.frep ,fr1) (report.frep ,fr2))))
;
;
; =============================================================================
;
; addbinding.frep
; ---------------
;
;       arguments     : mb - <mbind>
;                       frep - <report>
;
;       returns       : <freport>
;
;       description   : adds the binding "mb" to the <substitution> of "frep"
;
;                                        written :  cpf 10/25/88
;                                        modified:  
;
;
(defmacro addbinding.frep (mb frep)
  `(addbinding.rep ,mb (report.frep ,frep)))
;
;
; =============================================================================



    
    




