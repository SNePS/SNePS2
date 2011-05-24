;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: cqchset.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <cq-channel set> ::= { <cq-channel> ... }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.cqchset :  --> <cq-channel set>
;  ELEMENTS
;
; RECOGNIZERS    isnew.cqchset : <cq-channel set> --> <boolean>
;
; CONSTRUCTORS   putin.cqchset   : <cq-channel> x <cq-channel set>
;                                     --> <cq-channel set>
;                insert.cqchset  : <cq-channel> x <cq-channel set>
;                                           --> <cq-channel set>
;                makeone.cqchset : <cq-channel> --> <cq-channel set>
;                update.cqchset  : <cq-channel> x <cq-channel set>
;                                           --> <cq-channel set>
;
; SELECTORS      choose.cqchset : <cq-channel set> --> <cq-channel>
;                others.cqchset : <cq-channel set> --> <cq-channel set>
;
; TESTS          ismemb.cqchset : <cq-channel> x <cq-channel set> --> <boolean>
;
; UTILITIES      get-updated-cqch : <cq-channel> x <cq-channel set> --> <cq-channel>
;
; =============================================================================
;
; new.cqchset
; -----------
;
;       returns       : <cq-channel set>
;
;       description   : returns a "new" <cq-channel set>
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro new.cqchset ()
  `(new.Set))
;
;
; =============================================================================
;
; isnew.cqchset
; -------------
;
;       arguments     : cqchs - <cq-channel set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "cqchs" is a "new" <cq-channel set>,
;                       "false" otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro isnew.cqchset (cqchs)
  `(isnew.Set ,cqchs))
;
;
; =============================================================================
;
; putin.cqchset
; -------------
;
;       arguments     : cqch - <cq-channel>
;                       cqchs - <cq-channel set>
;
;       returns       : <cq-channel set>
;
;       description   : returns a <cq-channel set> consisting of "cqchs" with
;                       "cqch" inserted.  It is assumed that "cqch" was not
;                       already in "cqchs".
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro putin.cqchset (cqch cqchs)
  `(putin.Set ,cqch ,cqchs))
;
;
; =============================================================================
;
; makeone.cqchset
; ---------------
;
;       arguments     : cqch - <cq-channel>
;
;       returns       : <cq-channel set>
;
;       description   : returns a <cq-channel set> with the single element
;                       "cqch"
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defmacro makeone.cqchset (cqch)
  `(makeone.Set ,cqch))
;
;
; =============================================================================
;
; choose.cqchset
; --------------
;
;       arguments     : cqchs - <cq-channel set>
;
;       returns       : <cq-channel>
;
;       description   : returns and element of "cqchs"
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro choose.cqchset (cqchs)
  `(choose.Set ,cqchs))
;
;
; =============================================================================
;
; others.cqchset
; --------------
;
;       arguments     : cqchs - <cq-channel set>
;
;       returns       : <cq-channel set>
;
;       description   : returns "cqchs" with the element chosen by
;                       choose.cqchset eliminated
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro others.cqchset (cqchs)
  `(others.Set ,cqchs))
;
;
; =============================================================================
;
; insert.cqchset
; --------------
;
;       arguments     : cqch - <cq-channel>
;                       cqchset - <cq-channel set>
;
;       returns       : <cq-channel set>
;
;       description   : returns a <cq-channel set> similar to "cqchset" but
;                       with "cqch" inserted if it was not already there
;
;       implementation: "cqch" is considered to be in "cqchset" already if
;                       there is already a <cq-channel> is "cqchset" with
;                       the same <channel>
;
;                                        written :  rgh  2/12/86
;                                        modified:  rgh  3/30/86
;                                                   rgh  4/03/86
;
;
(defun insert.cqchset (cqch cqchset)
  (cond ((isnew.cqchset cqchset) (makeone.cqchset cqch))
        ((equivalent.ch (channel.cqch cqch)
                        (channel.cqch (choose.cqchset cqchset)))
	 cqchset)
        (t (putin.cqchset (choose.cqchset cqchset)
			  (insert.cqchset cqch (others.cqchset cqchset))))))
;
;
; =============================================================================
;
; update.cqchset
; --------------
;
;       arguments     : cqch - <cq-channel>
;                       cqchset - <cq-channel set>
;
;       returns       : <cq-channel set>
;
;       description   : returns a <cq-channel set> similar to "cqchset" but
;                       with "cqch" replacing a <cq-channel> in "cqchset"
;                       that has an equivalent <channel>, or with "cqch"
;                       inserted if no matching <cq-channel> is found.
;
;                                        written :  rgh  4/03/86
;                                        modified:
;
;
(defun update.cqchset (cqch cqchset)
  (cond ((isnew.cqchset cqchset) (makeone.cqchset cqch))
        ((equivalent.ch (channel.cqch cqch)
			(channel.cqch (choose.cqchset cqchset)))
	 (putin.cqchset cqch (others.cqchset cqchset)))
        (t (putin.cqchset (choose.cqchset cqchset)
			  (update.cqchset cqch (others.cqchset cqchset))))))
;
;
; =============================================================================
;
; ismemb.cqchset
; --------------
;
;       arguments     : cqch - <cq-channel>
;                       cqchs - <cq-channel set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if there is a <cq-channel> in "cqchs"
;                       which has a <channel> equivalent to the <channel> of
;                       "cqch", "false" otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:
;
;
(defmacro ismemb.cqchset (cqch cqchs)
  `(prog (cqchset)
	 (setq cqchset ,cqchs)
      begin
         (if (isnew.cqchset cqchset) (return nil))
         (if (equivalent.ch (channel.cqch ,cqch)
			    (channel.cqch (choose.cqchset cqchset)))
	     (return t))
	 (setq cqchset (others.cqchset cqchset))
	 (go begin)))
;
;
; =============================================================================
;
; get-updated-cqch
; ----------------
;
;       arguments     : cqch - <cq-channel>
;                       cqchs - <cq-channel set>
;
;       returns       : <cq-channel>
;
;       description   : Returns the <cq-channel> equivalent to 'cqch' that
;                       is present in 'cqchs'
;                       
;                       
;
;                                        written : njm/cpf 12/14/88 
;                                        modified:
;
;
(defun get-updated-cqch (cqch cqchs)
  (let ((result nil))
    (dolist (firstcqch cqchs result)
      (when (equivalent.ch (channel.cqch cqch) (channel.cqch firstcqch))
	(setq result firstcqch)))))






  
;
;
; =============================================================================



    
    




