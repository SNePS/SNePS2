;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: ichset.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; <i-channel set> ::= { <i-channel> ... }
;
; -----------------------------------------------------------------------------
;
; PRIMITIVE      new.ichset :  --> <i-channel set>
;  ELEMENTS
;
; RECOGNIZERS    isnew.ichset : <i-channel set> --> <boolean>
;
; CONSTRUCTORS   putin.ichset   : <i-channel> x <i-channel set>
;                                     --> <i-channel set>
;                insert.ichset  : <i-channel> x <i-channel set>
;                                           --> <i-channel set>
;                makeone.ichset : <i-channel> --> <i-channel set>
;                update.ichset  : <i-channel> x <i-channel set>
;                                           --> <i-channel set>
;
; SELECTORS      choose.ichset : <i-channel set> --> <i-channel>
;                others.ichset : <i-channel set> --> <i-channel set>
;
; TESTS          ismemb.ichset : <i-channel> x <i-channel set> --> <boolean>
;
; =============================================================================
;
; new.ichset
; ----------
;
;       returns       : <i-channel set>
;
;       description   : returns a "new" <i-channel set>
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro new.ichset ()
  `(new.Set))
;
;
; =============================================================================
;
; isnew.ichset
; ------------
;
;       arguments     : ichs - <i-channel set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "ichs" is a "new" <i-channel set>,
;                       "false" otherwise
;
;                                        written :  cpf 11/04/88 
;                                        modified:
;
;
(defmacro isnew.ichset (ichs)
  `(isnew.Set ,ichs))
;
;
; =============================================================================
;
; putin.ichset
; -------------
;
;       arguments     : ich - <i-channel>
;                       ichs - <i-channel set>
;
;       returns       : <i-channel set>
;
;       description   : returns a <i-channel set> consisting of "ichs" with
;                       "ich" inserted.  It is assumed that "ich" was not
;                       already in "ichs".
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro putin.ichset (ich ichs)
  `(putin.Set ,ich ,ichs))
;
;
; =============================================================================
;
; makeone.ichset
; ---------------
;
;       arguments     : ich - <i-channel>
;
;       returns       : <i-channel set>
;
;       description   : returns a <i-channel set> with the single element
;                       "ich"
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro makeone.ichset (ich)
  `(makeone.Set ,ich))
;
;
; =============================================================================
;
; choose.ichset
; --------------
;
;       arguments     : ichs - <i-channel set>
;
;       returns       : <i-channel>
;
;       description   : returns and element of "ichs"
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro choose.ichset (ichs)
  `(choose.Set ,ichs))
;
;
; =============================================================================
;
; others.ichset
; --------------
;
;       arguments     : ichs - <i-channel set>
;
;       returns       : <i-channel set>
;
;       description   : returns "ichs" with the element chosen by
;                       choose.ichset eliminated
;
;                                        written :  cpf 11/04/88
;                                        modified:
;
;
(defmacro others.ichset (ichs)
  `(others.Set ,ichs))
;
;
; =============================================================================
;
; insert.ichset
; --------------
;
;       arguments     : ich - <i-channel>
;                       ichset - <i-channel set>
;
;       returns       : <i-channel set>
;
;       description   : returns a <i-channel set> similar to "ichset" but
;                       with "ich" inserted if it was not already there
;
;       implementation: "ich" is considered to be in "ichset" already if
;                       there is already a <i-channel> is "ichset" with
;                       the same <channel>
;
;                                        written :  cpf  2/12/86
;                                        modified:  cpf  3/30/86
;                                                   cpf  4/03/86
;
;
(defun insert.ichset (ich ichset)
  (cond ((isnew.ichset ichset) (makeone.ichset ich))
        ((equivalent.ch (channel.ich ich)
                        (channel.ich (choose.ichset ichset)))
            ichset)
        (t
            (putin.ichset (choose.ichset ichset)
                     (insert.ichset ich (others.ichset ichset))))))
;
;
; =============================================================================
;
; update.ichset
; --------------
;
;       arguments     : ich - <i-channel>
;                       ichset - <i-channel set>
;
;       returns       : <i-channel set>
;
;       description   : returns a <i-channel set> similar to "ichset" but
;                       with "ich" replacing a <i-channel> in "ichset"
;                       that has an equivalent <channel>, or with "ich"
;                       inserted if no matching <i-channel> is found.
;
;                                        written :  cpf  4/03/86
;                                        modified:
;
;
(defun update.ichset (ich ichset)
  (cond ((isnew.ichset ichset) (makeone.ichset ich))
        ((and (equivalent.ch (channel.ich ich)
			     (channel.ich (choose.ichset ichset)))
	      (sneps:iseq.ns (context.ich ich) (context.ich (choose.ichset ichset)))
	      (sneps:iseq.ns (consequents.ich ich) (consequents.ich (choose.ichset ichset))))
	 (putin.ichset ich (others.ichset ichset)))
        (t
	 (putin.ichset (choose.ichset ichset)
		       (update.ichset ich (others.ichset ichset))))))
;
;
; =============================================================================
;
; ismemb.ichset
; --------------
;
;       arguments     : ich - <i-channel>
;                       ichs - <i-channel set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if there is a <i-channel> in "ichs"
;                       which has a <channel> equivalent to the <channel> of
;                       "ich", "false" otherwise
;
;                                        written :  cpf  2/08/86
;                                        modified:
;
;
(defmacro ismemb.ichset (ich ichs)
  `(prog (ichset)
	 (setq ichset ,ichs)
      begin
         (if (isnew.ichset ichset) (return nil))
         (if (equivalent.ch (channel.ich ,ich)
			    (channel.ich (choose.ichset ichset)))
	     (return t))
	 (setq ichset (others.ichset ichset))
	 (go begin)))
;
;
; =============================================================================



    
    




