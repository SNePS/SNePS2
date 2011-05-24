;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MULTI; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: sched.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


(in-package :multi)


; =============================================================================
;
; schedule
; --------
;
;       arguments     : event - <process>
;                       events - <process queue>
;
;       returns       : <process queue>
;
;       description   : the scheduling function required by Multi
;
;                                        written :  rgh 11/18/85
;                                        modified:  rgh 12/02/85
;                                                   rgh  4/13/86
;                                                   rgh  4/20/86
;                                                   njm  1/19/89
;                                                   njm  3/22/89
;                                                   njm/hc 4/26/89
;					 	    dk 4/17/91
;
; This is the new, streamlined version that doesn't have to
; deal with high-priority and low-priority processes on the
; same queue anymore.
;
(defun schedule (event queue)
  (cond ((eq (regfetch event '*PRIORITY*) 'snip::INTEND) ; intended acts
         (dequeue:insert-front event queue))            ; go on front -dk
	((dequeue:in-queue event queue) queue)
	((eq (regfetch event '*NAME*) 'snip:USER)
	 (dequeue:insert-front event queue))
	(t (dequeue:insert-rear event queue))))






    
    




