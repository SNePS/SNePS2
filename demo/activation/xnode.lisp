;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XGINSENG; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: xnode.lisp,v 1.1 2011/05/24 17:59:36 mwk3 Exp $

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


(in-package :xginseng)


(defvar *fillstyle-update-wait-function*
  #'(lambda ()(sleep 1.2))
  "Function to be called before the fillstyle of an XGinseng node gets
updated")

;; An access function:
(defun set-fillstyle.n (sneps-node activation)
  (let ((gi-node (intern (stringify.n sneps-node) 'xginseng)))
    ;; check whether we have an XGinseng node for this Sneps node
    (when (boundp gi-node)
      (setq gi-node (eval gi-node))
      (funcall *fillstyle-update-wait-function*)
      (case activation
	(:initiated
	 (s-value (g-value gi-node :frame)
		  :filling-style opal:light-gray-fill))
	(:executing
	 (s-value (g-value gi-node :frame)
		  :filling-style opal:dark-gray-fill))
	(t (s-value (g-value gi-node :frame)
		    :filling-style opal:white-fill)))
      (opal:update display-window)
      )))



    
    




