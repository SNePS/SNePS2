;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: cqch.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :snip)


; =============================================================================
;
; <cq-channel> ::= ( <channel> <node set> <rule-use-info set> )
;
;     The <node set> is a set of all antecedents for this consequent.
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      
;  ELEMENTS
;
; RECOGNIZERS    
;
; CONSTRUCTORS   make.cqch : <channel> x <node set> x <rule-use-info set>
;                                 --> <cq-channel>
;
; SELECTORS      channel.cqch : <cq-channel> --> <channel>
;                ants.cqch    : <cq-channel> --> <node set>
;                ruiset.cqch  : <cq-channel> --> <rule-use-info set>
;
; =============================================================================
;
; make.cqch
; ---------
;
;       arguments     : ch - <channel>
;                       ants - <node set>
;                       ruis - <rule-use-info set>
;
;       returns       : <cq-channel>
;
;       description   : returns a <cq-channel> consisting of the arguments
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defun make.cqch (ch ants ruis)
  (list ch ants ruis))
;
;
; =============================================================================
;
; channel.cqch
; ------------
;
;       arguments     : cqch - <cq-channel>
;
;       returns       : <channel>
;
;       description   : returns the <channel> of "cqch"
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defun channel.cqch (cqch)
  (first cqch))
;
;
; =============================================================================
;
; ants.cqch
; ---------
;
;       arguments     : cqch - <cq-channel>
;
;       returns       : <node set>
;
;       description   : returns the <node set> of all rule antecedents for
;                       the consequent which is the destination of the
;                       channel of "cqch"
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defun ants.cqch (cqch)
  (second cqch))
;
;
; =============================================================================
;
; ruiset.cqch
; -----------
;
;       arguments     : cqch - <cq-channel>
;
;       returns       : <rule-use-info set>
;
;       description   : returns the <rule-use-info set> of "cqch"
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defun ruiset.cqch (cqch)
  (third cqch))
;
;
; =============================================================================



    
    




