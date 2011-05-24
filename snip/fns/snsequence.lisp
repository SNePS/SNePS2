;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011
;; Research Foundation of State University of New York

;; Version: $Id: snsequence.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;; altered for ACL 6 compatibility (FLJ)


(in-package :snip)


;;;
;;; Representation
;;;
;;;   act = action snsequence
;;;         object1 act1
;;;	    object2 act2
;;;         ...


(defun snsequence (n)
  "Performs the objecti acts in order of i."
  (let ((i 0) acts all-acts (n-act (activation.n n)))
    ;; Use CLtL-I-style `loop':
    (regstore n-act '*AGENDA* 'DONE)
    (loop
      (incf i)
      (setq acts
        (sneps::nodeset.n n (intern 
			     (build-namestring :object i) :snepsul)))
      (if (isnew.ns acts)
          (return)
        (setf all-acts (append acts all-acts))))
    (dolist (act all-acts)
      (activate-act.n act)   
      (let ((pr (activation.n act)))
	(regstore pr '*PRIORITY* 'INTEND)
	(regstore pr '*AGENDA* 'START)
	(initiate pr)))))



    
    




