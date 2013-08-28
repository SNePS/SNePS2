;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: matching.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :match)


;;; --------------------------------------------------------------------------
;;;       Ported from Franz Lisp to Common Lisp:   KEB  Summer 1987
;;; --------------------------------------------------------------------------
;;;
;;;
;;; =============================================================================
;;; Data Type:  <matching> ::= (<target node> <target substitution> <source substitution>)
;;;
;;; =============================================================================
;;;
;;; make.matching
;;; -------------
;;;
;;;       arguments     : tnode - <target node>
;;;                       srcsub - <source substitution>
;;;                       tsub - <target substitution>
;;;                        
;;;       returns       : <matching> 
;;;
;;;       description   : makes a <matching> from 'tnode', 'srcsub', and 'tsub'
;;;
;;;                                        written :  vhs 04/09/85
;;;                                        modified:  scs 02/11/88
;;;                                        modified:  scs 05/12/88
;;;
;;;
#|
(defmacro make.matching (tnode srcsub tsub)
  "Makes a <matching> from 'tnode', 'srcsub', and 'tsub'."
  `(list ,tnode ,tsub ,srcsub))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; tnode.matching
;;; --------------
;;;
;;;       arguments     : matching - <matching> 
;;;
;;;       returns       : <node> 
;;;
;;;       description   : returns the <tnode> of 'matching'
;;;
;;;                                        written :  vhs 06/08/85
;;;                                        modified:
;;;
;;;
(defmacro tnode.matching (matching)
  "Returns the <tnode> of 'matching'."
  `(first ,matching))
;;;
;;;
;;; =============================================================================
;;;
;;; source-sub.matching
;;; -------------------
;;;
;;;       arguments     : matching - <matching> 
;;;
;;;       returns       : <source substitution> 
;;;
;;;       description   : returns the <source substitution> of 'matching'. 
;;;
;;;                                        written :  vhs 06/08/85
;;;                                        modified:  rgh 07/10/85
;;;
;;;
(defmacro source-sub.matching (matching)
  "Returns the <source substitution> of 'matching'."
  `(third ,matching))
;;;
;;;
;;; =============================================================================
;;;
;;; target-sub.matching
;;; -------------------
;;;
;;;       arguments     : matching - <matching>
;;;
;;;       returns       : <target substitution> 
;;;
;;;       description   : returns the <target substitution> of 'matching'. 
;;;
;;;                                        written :  vhs 06/08/85
;;;                                        modified:  rgh 07/10/85
;;;
;;;
(defmacro target-sub.matching (matching)
  "Returns the <target substitution> of 'matching'."
  `(second ,matching))
;;;
;;; ==========================================================================



    
    




