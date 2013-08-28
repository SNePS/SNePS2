;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: dynamic-add.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; dynamic-add
; -----------
;
;       arguments     : snd - <snepsul node description>
;
;       returns       : <node set>
;
;       description   : top-level SNePSUL function which adds a node to the
;                       network and then tries to do forward inference
;                       using it.
;
;       side-effects  : may build new nodes in the network.
;                       Initiates the variable 'crntctname' with the name given
;                       by the user to the context where the inference must be 
;                       done.
;
;                                        written :  dk: 6/93
;
(defsnepscom dynamic-add ((&rest snd) add)
   (let* ((crntct (sneps:processcontextdescr snd))
	  (crntctname crntct))
     (declare (special crntct crntctname))
     (values (dynamic-add* (sneps::nseval (cons 'sneps:assert snd)))
	     crntctname)))

;
;
; =============================================================================
;
; dynamic-add*
; ----
;
;       arguments     : new-nodes - <nodefun set>
;
;       returns       : <nodefun set>
;
;       description   : see dynamic-add
;
;       side-effects  : see dynamic-add
;
;                                        written :  rgh  4/20/86
;                                        modified:  scs  4/20/88
;                                        modified:  njm/cpf 10/19/88
;                                                   hc/njm  04/26/89
;                                                   dk 6/2/93
; added act-queue to the call to MULTIP -: dk
;
(defun dynamic-add* (new-nodes)
  (declare (special *ADDED-NODES* crntct))
  (let ((inference-context (sneps:value.sv crntct))
	new-nodefun pr)
    (when-intensional-contexts
     ;; Use the context name
     (setq inference-context crntct))
    (setq *ADDED-NODES* new-nodes)	;special var
    (if (isnew.ns new-nodes) *ADDED-NODES*)
    (setq new-nodefun (choose.ns new-nodes))
    (when (ismol.n new-nodefun)
      (activate.n new-nodefun)
      (setq pr (activation.n new-nodefun))
      (regstore pr '*REPORTS*
		(insert.repset
		 (make.rep (new.sbst)
			   (filter.sup (sneps:node-asupport new-nodefun)
				       (sneps:value.sv crntct))
			   'POS
			   'SENSOR
			   new-nodefun
			   inference-context)
		 (regfetch pr '*REPORTS*)))
      (regstore pr '*PRIORITY* 'HIGH)
      (initiate pr))
    *ADDED-NODES*))
;
;
; =============================================================================









    
    




