;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: imports.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :snepslog)


;;;  Import the following symbols from the SNePS package:

(import '(sneps:outunit sneps:inunit sneps:sneps-error))

(import '(sneps:assertions sneps:defaultct sneps:default-defaultct
	  sneps:sneps-error))
(import '(sneps:resetnet))
(import '( sneps:node sneps:node-asupport sneps:node sneps:topsneval sneps:value.sv
	  sneps:set.sv sneps:issubset.ns))
(import '(parser:nl-tell))

;;; SNePS relations
(import '(sneps:act sneps:action
	  sneps:effect
	  sneps:emin sneps:emax sneps:etot
	  sneps:goal
	  snepsul:nexists sneps:object1 sneps:object2
	  sneps:pevb sneps:plan sneps:precondition
	  sneps:suchthat
	  sneps:then sneps:vars 
	  sneps:whenever
	  ))

;;; Import symbols from SNePS which are SNePSUL commands and also SNePSLOG commands
;;; (so the definiton will be attached to the right symbol):
(import '(sneps:|%| sneps:|^| sneps:add-to-context
	  sneps:define-path sneps:describe-context sneps:erase
	  sneps:lisp sneps:list-context-names
	  sneps:set-default-context sneps:set-context
	  sneps:undefine-path))

(shadowing-import '(sneps:=))

;;; For path-based inference
(import
 '(sneps:! sneps:converse sneps:compose sneps:kstar sneps:kplus
   sneps:relative-complement sneps:irreflexive-restrict
   sneps:exception sneps:domain-restrict sneps:range-restrict
	  ))

;;; For attached primitive actions and attached functions:
(import '(snip:*depthCutoffBack* snip:*depthCutoffForward*
	  snip:define-primaction
	  snip:attach-primaction snip:define-attachedfunction
	  snip:attach-function snip:attachedfunction))

;;; Import predefined primitive action function names.
(import '(snip:achieve snip:adopt snip:believe snip:disbelieve snip:do-all
	  snip:do-one sneps:else snip:snif snip:sniterate
	  snip:snsequence snip:unadopt snip:withall snip:withsome))

;;; To set automatic belief revision parameters:
(import '(snebr:*br-auto-mode* snebr:*br-tie-mode* snebr:set-order
          snebr:br-mode snebr:br-tie-mode))

(import 'sneps:build-namestring)  ;; for acl6 (FLJ)
