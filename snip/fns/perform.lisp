;;; ===========================================================================
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2011
;; Research Foundation of State University of New York

;; Version: $Id: perform.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


;;; Definitions of SNACTOR
;;;
;;; Representation
;;;
;;;   act = action act-node
;;;         objecti ith-argument to act-node


(defsnepscom perform ((nodeset &rest context-specifier) add)
  (sneps:clear-infer)
  (let* ((crntct (sneps:processcontextdescr context-specifier))
         (crntctname crntct)
         (intended-act (choose.ns (sneps::nseval nodeset)))
         pr)
    (declare (special crntctname))
    (activate-act.n intended-act)
    (setq pr (activation.n intended-act))
    (regstore pr '*PRIORITY* 'INTEND)
    (regstore pr '*AGENDA* 'START)
    (multip (dequeue:insert-front pr (dequeue:new)) 
	    (dequeue:new)
	    (dequeue:new))))



    
    




