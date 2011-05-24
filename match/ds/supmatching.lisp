;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: supmatching.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :match)


;;; -----------------------------------------------------------------------------
;;;       Ported from Franz Lisp to Common Lisp:   KEB  Summer 1987
;;; -----------------------------------------------------------------------------
;;;
;;;
;;; =============================================================================
;;; Data Type:  <supmatching> ::= (<target node> <target substitution>
;;;                                <target support> <source substitution>)
;;;
;;; =============================================================================
;;;
;;; make.supmatching
;;; ----------------
;;;
;;;       arguments     : tnode - <target node>
;;;                       srcsub - <source substitution>
;;;                       tsub - <target substitution>
;;;                        
;;;       returns       : <matching> 
;;;
;;;       description   : makes a <matching> from 'tnode', 'srcsub', and 'tsub'
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:  
;;;
;;;
(defmacro make.supmatching (tnode  tsub tsup srcsub)
  "Makes a <matching> from 'tnode', 'tsub', 'tsup' and 'srcsub'."
  `(list ,tnode ,tsub ,tsup ,srcsub))
;;;
;;;
;;; =============================================================================
;;;
;;; tnode.supmatching
;;; --------------
;;;
;;;       arguments     : supmatching - <supmatching> 
;;;
;;;       returns       : <node> 
;;;
;;;       description   : returns the <tnode> of 'supmatching'
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:
;;;
;;;
(defmacro tnode.supmatching (supmatching)
  "Returns the <tnode> of 'supmatching'."
  `(first ,supmatching))
;;;
;;;
;;; =============================================================================
;;;
;;; source-sub.supmatching
;;; -------------------
;;;
;;;       arguments     : supmatching - <supmatching> 
;;;
;;;       returns       : <source substitution> 
;;;
;;;       description   : returns the <source substitution> of 'supmatching'. 
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:  
;;;
;;;
(defmacro source-sub.supmatching (supmatching)
  "Returns the <source substitution> of 'supmatching'."
  `(fourth ,supmatching))
;;;
;;;
;;; =============================================================================
;;;
;;; target-sub.supmatching
;;; -------------------
;;;
;;;       arguments     : supmatching - <supmatching>
;;;
;;;       returns       : <target substitution> 
;;;
;;;       description   : returns the <target substitution> of 'supmatching'. 
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:
;;;
;;;
(defmacro target-sub.supmatching (supmatching)
  "Returns the <target substitution> of 'supmatching'."
  `(second ,supmatching))
;;;
;;; ==========================================================================
;;;
;;; target-sup.supmatching
;;; ----------------------
;;;
;;;       arguments     : supmatching - <supmatching>
;;;
;;;       returns       : <target substitution> 
;;;
;;;       description   : returns the <target substitution> of 'supmatching'. 
;;;
;;;                                        written :  cpf/njm 10/19/88
;;;                                        modified:  
;;;
;;;
(defmacro target-sup.supmatching (supmatching)
  "Returns the <target substitution> of 'supmatching'."
  `(third ,supmatching))
;;;
;;; ==========================================================================



    
    




