;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: feederset.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <feeder set> ::= { <feeder> ... }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.feedset : --> <feeder set>
;  ELEMENTS
;
; RECOGNIZERS    is.feedset    : <universal> --> <boolean>
;                isnew.feedset : <feeder set> --> <boolean>
;
; SELECTORS      choose.feedset : <feeder set> --> <feeder>
;                others.feedset : <feeder set> --> <feeder set>
;
; CONSTRUCTORS   insert.feedset : <feeder> x <feeder set> --> <feeder set>
;
; =============================================================================
;
; new.feedset
; -----------
;
;       returns       : <feeder set>
;
;       description   : returns a "new" <feeder set>
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun new.feedset ()
  (new.Set))
;
;
; =============================================================================
;
; is.feedset
; ----------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <feeder set>,
;                               "false" otherwise
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun is.feedset (u)
  (and (is.Set u)
        (is.feeder (choose.Set u))))
;
;
; =============================================================================
;
; isnew.feedset
; -------------
;
;       arguments     : fs - <feeder set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "fs" is a "new" <feeder set>
;                               "false" otherwise
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun isnew.feedset (fs)
  (isnew.Set fs))
;
;
; =============================================================================
;
; choose.feedset
; --------------
;
;       arguments     : fs - <feeder set>
;
;       returns       : <feeder>
;
;       description   : returns the first <feeder> in "fs"
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun choose.feedset (fs)
  (choose.Set fs))
;
;
; =============================================================================
;
; others.feedset
; --------------
;
;       arguments     : fs - <feeder set>
;
;       returns       : <feeder set>
;
;       description   : returns a <feeder set> consisting of all of the
;                       <feeder>s in "fs" except the first
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun others.feedset (fs)
  (others.Set fs))
;
;
; =============================================================================
;
; insert.feedset
; --------------
;
;       arguments     : feeder - <feeder>
;                       feederset - <feeder set>
;
;       returns       : <feeder set>
;
;       description   : returns a <feeder set> identical to "feederset", but
;                       with "feeder" inserted if there was not already an
;                       equivalent one there
;
;                                        written :  rgh 11/30/85
;                                        modified:  cpf 10/07/88
;
;
(defun insert.feedset (feeder feederset)
  (prog (f fs)
	 (setq fs feederset)
      begin
         (if (isnew.feedset fs) (return (putin.Set feeder feederset)))
	 (setq f (choose.feedset fs))
         (cond ((equivalent.feeder f feeder)
		(setf (valve.feeder f) (valve.feeder feeder))
		(return feederset)))
	 (setq fs (others.feedset fs))
	 (go begin)))
;
;
; =============================================================================



    
    




