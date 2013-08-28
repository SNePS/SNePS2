;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: exports.lisp,v 1.2 2013/08/28 19:07:26 shapiro Exp $

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


(export '(perform deduce add dynamic-add *infertrace* *plantrace*
	  *depthCutoffBack* *depthCutoffForward* user high low plantrace
	  describe-one-node addsupport.n filter.sup new.sup insert.sup
	  combine-ots* crntctname
	  Stop-Handled-by-Contradiction-Handler
	  *use-intensional-contexts*


	  slight-describe-or-surface slight-describe-or-surface.ns
	  describe-or-surface describe-or-surface.ns node-description.n

	  act define-primaction attach-primaction 
          declare-primitive schedule-act

	  achieve believe disbelieve adopt unadopt snsequence snif
	  sniterate forget do-one do-all withsome withall
	  
	  pos neg define-attachedFunction attach-function attachedfunction))

;;; 23 Feb 2011 ABCL-specific code to fix bug:
;;; When SNePS commands were later defined for these symbols via
;;; defsnepscom, ABCL 0.24.0 was crashing when defsnepscom called
;;; shadowing-import. Importing them with shadowing-import initially
;;; solves the problem. - JPB
#+abcl (shadowing-import '(perform deduce add dynamic-add)
			 (find-package 'snepsul))
#-abcl (shadowing-import '(perform deduce add dynamic-add)
			 (find-package 'snepsul))

(import '( *infertrace* *plantrace*
	  *use-intensional-contexts*
	  *depthCutoffBack* *depthCutoffForward* 

	  achieve believe disbelieve adopt unadopt snsequence snif
	  sniterate forget do-one do-all withsome withall schedule-act
	  define-primaction attach-primaction declare-primitive
	  
	  define-attachedFunction attach-function attachedfunction)
	(find-package 'snepsul))



    
    




