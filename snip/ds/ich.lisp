;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: ich.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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
; <i-channel> ::= ( <channel> <context> <node set> <rule-use-info set> )
;
;     The <node set> is a set of all consequents of this rule
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      
;  ELEMENTS
;
; RECOGNIZERS    
;
; CONSTRUCTORS   make.ich : <channel> x <context> x <node set> x <rule-use-info set>
;                                 --> <i-channel>
;
; SELECTORS      channel.ich     : <i-channel> --> <channel>
;                context.ich     : <i-channel> --> <context>
;                consequents.ich : <i-channel> --> <node set>
;                ruiset.ich      : <i-channel> --> <rule-use-info set>
;
; =============================================================================
;
; make.ich
; --------
;
;       arguments     : ch - <channel>
;                       ct - <context>
;                       cqts - <node set>
;                       ruis - <rule-use-info set>
;
;       returns       : <i-channel>
;
;       description   : returns a <i-channel> consisting of the arguments
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro make.ich (ch ct cqts ruis)
  `(list ,ch ,ct ,cqts ,ruis))
;
;
; =============================================================================
;
; channel.ich
; ------------
;
;       arguments     : ich - <i-channel>
;
;       returns       : <channel>
;
;       description   : returns the <channel> of "ich"
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro channel.ich (ich)
  `(first ,ich))
;
;
; =============================================================================
;
; context.ich
; -----------
;
;       arguments     : ich - <i-channel>
;
;       returns       : <context>
;
;       description   : returns the <context> of the <i-channel> `ich'.
;                   
;                   
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro context.ich (ich)
  `(second ,ich))
;
;
; =============================================================================
;
; consequents.ich
; ---------------
;
;       arguments     : ich - <i-channel>
;
;       returns       : <node set>
;
;       description   : returns the <node set> of all rule consequents 
;               
;         
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro consequents.ich (ich)
  `(third ,ich))
;
;; =============================================================================
;
; ruiset.ich
; ----------
;
;       arguments     : ich - <i-channel>
;
;       returns       : <rule-use-info set>
;
;       description   : returns the <rule-use-info set> of "ich"
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro ruiset.ich (ich)
  `(fourth ,ich))
;
;
; =============================================================================



    
    




