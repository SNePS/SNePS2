;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: rui.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; <rule-use-info> ::= ( <substitution> <non-neg int> <non-neg int>
;                                                       <flagged node set> )
;
;      The first <non-neg int> is a count of the antecedents which are known
;   to be true in this substitution, and the second is the count of
;   antecedents known to be false.
;
; -----------------------------------------------------------------------------
;
; CONSTRUCTORS   make.rui : <substitution> x <non-neg int> x <non-neg int> x
;                              <flagged node set> --> <rule-use-info>
;                update.rui : <rule-use-info> x <node> x <sign>
;                                 --> <rule-use-info>
;                merge.rui  : <rule-use-info> x <rule-use-info>
;                                 --> <rule-use-info>
;
; SELECTORS      subst.rui    : <rule-use-info> --> <substitution>
;                poscount.rui : <rule-use-info> --> <non-neg int>
;                negcount.rui : <rule-use-info> --> <non-neg int>
;                fns.rui      : <rule-use-info> --> <flagged node set>
;
; UTILITIES      select-neg.rui   : <rule-use-info> --> <flagged node set>
;                select-pos.rui   : <rule-use-info> --> <flagged node set>
;                select-known.rui : <rule-use-info> --> <flagged node set>
;
; =============================================================================
;
; make.rui
; --------
;
;       arguments     : sub - <substitution>
;                       pos - <non-neg int>
;                       neg - <non-neg int>
;                       fns - <flagged node set>
;
;       returns       : <rule--use-info>
;
;       description   : returns a <rule-use-info> composed of the arguments
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  2/09/86
;
;
(defun make.rui (sub pos neg fns remarkedp)
  (vector sub pos neg fns remarkedp))
;
;
; =============================================================================
;
; subst.rui
; ---------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <substitution>
;
;       description   : returns the <substitution> of "rui"
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  2/09/86
;
;
(defun subst.rui (rui)
  (svref rui 0))
;
;
; =============================================================================
;
; poscount.rui
; ------------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <non-neg int>
;
;       description   : returns the count of the known positive antecedents
;                       for "rui"
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  2/09/86
;
;
(defun poscount.rui (rui)
  (svref rui 1))
;
;
; =============================================================================
;
; negcount.rui
; ------------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <non-neg int>
;
;       description   : returns the count of the known negative antecedents
;                       for "rui"
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  2/09/86
;
;
(defun negcount.rui (rui)
  (svref rui 2))
;
;
; =============================================================================
;
; fns.rui
; -------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <flagged node set>
;
;       description   : returns the <flagged node set> of "rui"
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  2/09/86
;
;
(defun fns.rui (rui)
  (svref rui 3))

(defun remarkedp.rui (rui)
  (svref rui 4))

(defmacro unless-remarkedp.rui (rui &body body)
  `(unless (remarkedp.rui ,rui)
     (setf ,rui
       (make.rui (subst.rui ,rui) (poscount.rui ,rui)
		 (negcount.rui ,rui) (fns.rui ,rui) t))
     ,@body))
;
;
; =============================================================================
;
; update.rui
; ----------
;
;       arguments     : rui  - <rule-use-info>
;                       node - <node>
;                       sup  - <support>
;                       sign - <sign>
;
;       returns       : <rule-use-info>
;
;       description   : returns a <rule-use-info> similar to "rui" but with
;                       the flag of "node" set corresponding to "sign" and
;                       merging the <support> of the node with "sup".
;                       It only increments the apropriate counter if the old
;                       flag was unkown or requested.
;                     
;
;                                        written :  rgh  4/03/86
;                                        modified:  njm/cpf 10/18/88
;
;
(defun update.rui (rui node sup sign)
  (cond
   ((eq sign 'pos)
    (make.rui (subst.rui rui) (1+ (poscount.rui rui)) (negcount.rui rui)
	      (update.fns (fns.rui rui) node sup 'true) (remarkedp.rui rui)))
   (t
    (make.rui (subst.rui rui) (poscount.rui rui) (1+ (negcount.rui rui))
	      (update.fns (fns.rui rui) node sup 'false) (remarkedp.rui rui)))))
;
;
; =============================================================================
; merge.rui
; ---------
;
;       arguments     : rui1, rui2 - <rule-use-info>
;
;       returns       : <rule-use-info>
;
;       description   : merges the information (poscount, negcount, flagged
;                       node set) of "rui1" and "rui2" and returns the
;                       result.  Assumes that "rui1" and "rui2" have the
;                       same <substitution>.
;                       It assumes that "rui1" and "rui2" are not
;                       incompatible (tested by "compatible.rui").
;
;                                        written :  rgh  4/26/86
;                                        modified:  scs  3/3/88
;                                                   njm  10/19/88
;
;
(defun merge.rui (rui1 rui2) 
  (let ((fns2 (fns.rui rui2))
	(newfns (new.fns))
	(newposcount (poscount.rui rui1))
	(newnegcount (negcount.rui rui1)))
    (do.fns (fn1 (fns.rui rui1) (make.rui (subst.rui rui1)
					  newposcount newnegcount
					  newfns t))
      (let* ((n1 (node.fn fn1))
	     (f1 (flag.fn fn1))
	     (f2 (flag.fns n1 fns2))
	     (s1 (support.fn fn1))
	     (s2 (support.fns n1 fns2)))
	(cond ((or (eq f1 'TRUE) (eq f1 'FALSE))
	       (setq newfns (putin.fns (make.fn n1 (merge.sup s1 s2) f1)
				       newfns)))      
	      ((eq f2 'TRUE)
	       (setq newfns (putin.fns (make.fn n1 s2 f2) newfns))
	       (setq newposcount (1+ newposcount)))
	      ((eq f2 'FALSE)
	       (setq newfns (putin.fns (make.fn n1 s2 f2) newfns))
	       (setq newnegcount (1+ newnegcount)))
	      ((eq f2 'REQUESTED)
	       (setq newfns (putin.fns (make.fn n1 s2 f2) newfns)))
	      (t (setq newfns (putin.fns fn1 newfns))))))))

;
;
; =============================================================================
;
; compatible.rui
; --------------
;
;       arguments     : rui1 - <rule-use-info>
;                       rui2 - <rule-use-info>
;
;       returns       : <boolean>
;
;       description   : returns TRUE if both <rule-use-info>s do not have
;                       contradictory information.
;
;                                        written :  njm  11/09/88
;                                        modified:  
;                                                   
;
;
(defun compatible.rui (rui1 rui2) 
  (let ((fns2 (fns.rui rui2)))
    (do.fns (fn1 (fns.rui rui1) t)
      (let* ((n1 (node.fn fn1))
	     (f1 (flag.fn fn1))
	     (f2 (flag.fns n1 fns2)))
        (when (or (and (equal f1 'TRUE)  (equal f2 'FALSE))
		  (and (equal f1 'FALSE) (equal f2 'TRUE)))
	  (return nil))))))

;
; =============================================================================
;
; select-neg.rui 
; --------------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <flag node set>
;
;       description   : returns a flag node set which includes only the
;                       flag nodes with a 'FALSE flag.
;
;                                        written :  njm  10/27/88
;                                        modified:  
;                                                   
;
(defun select-neg.rui (rui)
  (let ((new-fns (new.fns)))
    (do.fns (fn (fns.rui rui) new-fns)
	    (when (eq (flag.fn fn) 'FALSE)
	      (setf new-fns (putin.fns fn new-fns))))))


; =============================================================================
;
; select-pos.rui 
; --------------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <flag node set>
;
;       description   : returns a flag node set which includes only the
;                       flag nodes with a 'TRUE flag.
;
;                                        written :  njm  10/27/88
;                                        modified:  
;                                                   
;
(defun select-pos.rui (rui)
  (let ((new-fns (new.fns)))
    (do.fns (fn (fns.rui rui) new-fns)
	    (when (eq (flag.fn fn) 'TRUE)
	      (setf new-fns (putin.fns fn new-fns))))))

;
;
; =============================================================================
;
; select-known.rui 
; ----------------
;
;       arguments     : rui - <rule-use-info>
;
;       returns       : <flag node set>
;
;       description   : returns a flag node set which includes only the
;                       flag nodes with a 'TRUE flag or a 'FALSE flag.
;
;                                        written :  njm  10/27/88
;                                        modified:  
;                                                   
;
(defun select-known.rui (rui)
  (let ((new-fns (new.fns)))
    (do.fns (fn (fns.rui rui) new-fns)
	    (when (or (eq (flag.fn fn) 'TRUE)
		      (eq (flag.fn fn) 'FALSE))
	      (setf new-fns (putin.fns fn new-fns))))))

;
;
; =============================================================================
