;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: feeder.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <feeder> ::= ( <restriction> <context> <node> <valve> )
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.feeder  : <universal> --> <boolean>
;
; CONSTRUCTORS   make.feeder : <restriction> x <context> x <node> x <valve>
;                                          --> <feeder>
;
; SELECTORS      restriction.feeder : <feeder> --> <restriction>
;                context.feeder     : <feeder> --> <context>
;                source.feeder      : <feeder> --> <node>
;                valve.feeder       : <feeder> --> <valve>
;
; TESTS          equivalent.feeder  : <feeder> x <feeder> --> <boolean>
;                isopen.feeder      : <feeder> --> <boolean>
;                isclosed.feeder    : <feeder> --> <boolean>
;
; UTILITY        open.feeder        : <feeder> -->
;                close.feeder       : <feeder> -->
;
; =============================================================================
;
; is.feeder
; ---------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "u" is a <feeder>, "false" otherwise
;
;                                        written :  rgh 08/21/85
;                                        modified:  cpf 10/06/88
;
;
(defun is.feeder (u)
  (and (listp u)
        (is.restr (restriction.feeder u))
	(sneps:is.ct (context.feeder u))
        (is.n (source.feeder u))
        (is.valve (valve.feeder u))))
;
;
; =============================================================================
;
;  make.feeder
; ------------
;
;       arguments     : restr - <restriction>
;                       ct - <context>
;                       source - <node>
;                       v - <valve>
;
;       returns       : <feeder>
;
;       description   : returns a <feeder> made up of the elements passed as
;                       arguments
;
;                                        written :  rgh 08/21/85
;                                        modified:  cpf 10/06/88
;
;
(defun make.feeder (restr ct source v)
  (vector restr ct source v))
;
;
; =============================================================================
;
; restriction.feeder
; ------------------
;
;       arguments     : f - <feeder>
;
;       returns       : <restriction>
;
;       description   : returns the <restriction> of "f"
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun restriction.feeder (f)
  (svref f 0))
;
;
; =============================================================================
;
; context.feeder
; --------------
;
;       arguments     : f - <feeder>
;
;       returns       : <context>
;
;       description   : returns the source context of "f"
;
;                                        written :  rgh 08/21/85
;                                        modified:  cpf 10/06/88
;
;
(defun context.feeder (f)
  (svref f 1))
;
;
; =============================================================================
;
; source.feeder
; -------------
;
;       arguments     : f - <feeder>
;
;       returns       : <node>
;
;       description   : returns the source <node> of "f"
;
;                                        written :  rgh 08/21/85
;                                        modified:  cpf 10/06/88
;
;
(defun source.feeder (f)
  (svref f 2))
;
;
; =============================================================================
;
; valve.feeder
; ------------
;
;       arguments     : f - <feeder>
;
;       returns       : <valve>
;
;       description   : returns the <valve> of "f"
;
;                                        written :  rgh 08/21/85
;                                        modified:  cpf 10/06/88
;
;
(defun valve.feeder (f)
  (svref f 3))

(defsetf valve.feeder (f) (newvalvevalue)
  `(setf (svref ,f 3) ,newvalvevalue))
;
;
; =============================================================================
;
; equivalent.feeder
; -----------------
;
;       arguments     : f1 - <feeder>
;                       f2 - <feeder>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "f1" and "f2" are equivalent -- i.e.
;                       their restrictions are equivalent, and their source
;                       nodes and contexts are equal;  "false" otherwise
;
;                                        written :  rgh 11/30/85
;                                        modified:  cpf 10/06/88
;
;
(defun equivalent.feeder (f1 f2)
  (and (sneps:iseq.ct (context.feeder f1) (context.feeder f2))
        (iseq.n (source.feeder f1) (source.feeder f2))
        (equivalent.restr (restriction.feeder f1) (restriction.feeder f2))))
;
;
; =============================================================================
;
; isopen.feeder
; -------------
;
;       arguments     : f - <feeder>
;
;       returns       : <boolean>
;
;       description   : returns "true" if the <valve> of "f" is OPEN
;
;                                        written :  rgh 11/30/85
;                                        modified:
;
;
(defun isopen.feeder (f)
  (eq (valve.feeder f) 'OPEN))
;
;
; =============================================================================
;
; isclosed.feeder
; ---------------
;
;       arguments     : f - <feeder>
;
;       returns       : <boolean>
;
;       description   : returns "true" if the <valve> of "f" is CLOSED
;
;                                        written :  rgh 11/30/85
;                                        modified:
;
;
(defun isclosed.feeder (f)
  (eq (valve.feeder f) 'CLOSED))
;
;
; =============================================================================
;
; open.feeder
; -----------
;
;       arguments     : f - <feeder>
;
;       description   : sets the <valve> of "f" to 'OPEN
;
;       side-effects  : destructively modifies "f"
;
;                                        written :  rgh 11/30/85
;                                        modified:  cpf 10/06/88
;
;
(defun open.feeder (f)
  (setf (valve.feeder f) 'OPEN))
;
;
; =============================================================================
;
; close.feeder
; ------------
;
;       arguments     : f - <feeder>
;
;       description   : sets the <valve> of "f" to 'CLOSED
;
;       side-effects  : destructively modifies "f"
;
;                                        written :  rgh 11/30/85
;                                        modified:  cpf 10/07/88
;
;
(defun close.feeder (f)
  (setf (valve.feeder f) 'CLOSED))
;
;
; =============================================================================



    
    




