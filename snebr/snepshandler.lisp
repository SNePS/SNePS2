;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEBR; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: snepshandler.lisp,v 1.2 2013/06/17 15:20:34 shapiro Exp $

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


(in-package :snebr)



;;; *br-auto-mode*
;;;   when true, indicates that automatic belief revision will always be
;;;     performed when a contradiction is encountered
;;;   when false, indicates that automatic belief revision must be selected
;;;     manually as a belief-revision strategy when a contradiction is
;;;     encountered
(defparameter *br-auto-mode* nil)

;;; br-mode
;;;   sets *br-auto-mode* to flag
(defsnepscom br-mode ((flag))
  (setf *br-auto-mode* flag))

;;; *br-tie-mode*
;;;   when true, indicates that entrenchment ties will automatically be broken
;;;     without requiring input from the user
;;;   when false, indicates that the user will be asked to make some choice(s)
;;;     in order to break entrenchment ties
(defparameter *br-tie-mode* nil)

;;; br-tie-mode
;;;   sets *br-tie-mode* to flag
(defsnepscom br-tie-mode ((flag))
  (setf *br-tie-mode* flag))

;;; *most-entrenched-props*
;;;   list of propositions that always have minimal entrenchment during belief
;;;   revision
(defparameter *most-entrenched-props* nil)


(defparameter *order-cache* nil)
(defparameter *order-cache-complement* nil)

;;; Description:  Sets the epistemic ordering function to the one specified
;;;               by order-function-symbol
;;; Arguments:    order-function-symbol - a symbol corresponding to an ordering
;;;               function
(defsnepscom set-order ((order-function-symbol))
    (setf (symbol-function 'br-totalorder)
          (symbol-function order-function-symbol)))

;
; =============================================================================
;
; br-total-order 
; -------------------
; 
;
;       arguments     : lhs - <node> 
;                       rhs - <node>
;
;       returns       : if lhs is strictly less entrenched than rhs, t
;                       if lhs is equally entrenched as rhs, t
;                       if lhs is strictly more entrenched than rhs, nil
;
;       description   : This function takes two arguments lhs and rhs and
;                       determines their relative epistemic entrenchment. In
;                       order to use a custom ordering, redefine this function
;                       appropriately. Pay close attention to the 'returns'
;                       portion of these comments.
;                       
;                                  written :  aif 01/31/11 
;

(defun br-totalorder (lhs rhs)
  (declare (ignore lhs rhs))
  t)


;
; =============================================================================
;
; sneps-contr-handler 
; -------------------
; 
;
;       arguments     : newnode - <node> 
;                       contrnd - <node>
;
;       returns       : 
;
;       description   : This function is called whenever a contradiction is detected,
;                       as the result of the assertion of a new node.
;                       'newnode' is the newly asserted node and 'contrnd' is the
;                       node that contradicts 'newnode' (it is assumed that 'contrnd'
;                       is asserted in the BS defined by the context in which 'newnode'
;                       was asserted - see the function ck-contradiction).
;                       The function sneps-contr-handler i) warns the user of the
;                       detected contradiction, ii) gives the user several choices to
;                       handle the contradiction (see the function options-in-sneps-h),
;                       and iii) implements the user's choice (see the function
;                       implement-sneps-option).
;                       
;                                  written :  njm  10/11/88
;                                  modified:  mrc  10/28/88
;                                  modified:  mrc  12/27/88
;                                  modified:  flj   3/22/99
;                                  modified:  aif  02/03/11
;                                                
;

(defun sneps-contr-handler (newnode contrnd)
  (declare (special sneps:outunit sneps:crntct *br-auto-mode*))
  (if *br-auto-mode*
    (implement-sneps-option 'snepsul:A newnode contrnd)
    (progn
      (format sneps:outunit
        "~%~%~T A contradiction was detected within context ~A.~
         ~%~T The contradiction involves the proposition you want to assert:~
         ~%~T~T~T ~A ~
         ~%~T and the previously existing proposition:~
         ~%~T~T~T ~A"
	      sneps:crntct
	      (snip:describe-or-surface newnode nil)
	      (snip:describe-or-surface contrnd nil))
      (options-in-sneps-h)
      (implement-sneps-option (read-sneps-option) newnode contrnd))))
 
;
; =============================================================================
;
;
; options-in-sneps-h 
; ------------------
;
;
;       arguments     : ----
;
;       returns       : ----
;
;       description   : This function informs the user of the possibilities he
;                       has to handle a contradiction detected upon the assertion
;                       of a new node.
;
;       
;
;
;                                  written :  njm 10/06/88 
;                                  modified:  flj 3/22/99
;                                  modified:  aif 02/03/11
;
;
(defun options-in-sneps-h ()
  (declare (special sneps:outunit))
  (format sneps:outunit
     "~%~%~%~T You have the following options:~
    ~%~T~T 1. [a] to attempt to resolve the contradiction automatically ~
    ~%~T~T 2. [c] to continue anyway, knowing that a contradiction is ~
                     derivable;~
    ~%~T~T 3. [r] to revise the inconsistent part of the context manually~
    ~%~T~T 4. [d] to discard this contradictory new assertion from the context~
      ~%~T (please type a, c, r or d)")
  (values))  
;
; =============================================================================
;
;
; implement-sneps-option
; ----------------------
;
;       arguments     : option  - <char>
;                       newnode - <node>
;                       contrnd - <node>
;
;       returns       : <node> |  NIL
;
;       description   : This function implements the option chosen by the user
;                       (see the function  options-in-sneps-h).
;                       'newnode' is the newly asserted node and 'contrnd' is
;                       the node that contradicts 'newnode'.
;                       'option' may have the following values:
;                       'c' - meaning that the user chose to continue in spite
;                             of the contradiction. In this case nothing is done.
;                       'd' - meaning that the user chose to remove the newly 
;                             asserted node from the current context. In this
;                             case 'newnode' is removed from the current context. 
;                       'r' - meaning that the user choses to revise the part of
;                             the context that is inconsistent - this is done by 
;                             calling change-context which looks at all contexts
;                             that support BOTH 'newnode' and 'contrnd'. 
;
;
;                                  written :  njm 10/06/88 
;                                  modified:  mrc 12/18/88
;                                  modified:  flj  3/22/99
;                                  modified:  aif 02/03/11
;                                  modified:  scs 06/14/13
;
;
;
(defun implement-sneps-option (option newnode contrnd)
  (declare (special sneps:crntct sneps:outunit))
  (cond ((eqans option 'd) 
	 (name.ct (buildcontext
		   (remove.ns newnode
			      (context-hyps
			       (value.sv sneps:crntct))))
		  sneps:crntct))
	((eqans option 'a)
    (let ((contrsup (ctcs-to-cts (sneps:node-asupport contrnd)))
          (newsup (ctcs-to-cts (sneps:node-asupport newnode))))
      (autobr newsup contrsup (value.sv sneps:crntct) nil)))
	((eqans option 'r)
	 (let ((contrsup (ctcs-to-cts (sneps:node-asupport contrnd)))
	       (contr-otlst (sneps:ctcs-to-ots (sneps:node-asupport contrnd)))
	       (newsup (ctcs-to-cts (sneps:node-asupport newnode)))
	       (new-otlst (sneps:ctcs-to-ots (sneps:node-asupport newnode))))
	   (change-context newsup contrsup new-otlst contr-otlst 
			   (value.sv sneps:crntct) sneps:crntct)))
  ((eqans option 'c)
   (setf (sneps::%context-okinconsistent (value.sv sneps:crntct)) t))))


;
; =============================================================================
;
; read-sneps-option 
; -----------------
;
;
;       arguments     : -------
;
;       returns       : 'A | 'C | 'R | 'D
;
;       description   : This function reads the option typed by the user.
;
;       
;
;
;                                  written :  njm 10/11/88 
;                                  modified:  hc 10/19/88 (get rid of repeat)
;                                  modified:  flj  3/22/99
;                                  modified:  aif 02/03/11
;                                  modified:  scs 06/14/13
;
;
(defun read-sneps-option ()
  (declare (special sneps:outunit sneps:inunit))
  (let (ans)
    (loop (ct-prompt)
	  (setq ans (read sneps:inunit))
	  (if (or (eqans ans 'A)
		  (eqans ans 'C)
		  (eqans ans 'R)
		  (eqans ans 'D))
	      (return ans))
	  (format sneps:outunit "Please type a, c, r or d"))))

;
; =============================================================================
;
;
; make-consistent
; ---------------
;
;
;       arguments     : hyps       - <node set>
;                       context    - <context>
;                       new-ct     - <context>
;                       contr-ct   - <context>
;                       new-ot     - <otag>
;                       contr-ot   - <otag>
;                       
;
;       returns       : <node set>
;
;       description   : This function is called by change-context when either
;                        1 - a contradiction is detected as the result
;                            of the assertion of a new node (see implement-
;                            sneps-option), and the user wants to
;                            resolve the contradiction by revising the
;                            inconsistent part of the context. 
;                        2 - a contradiction is detected during inference,
;                            and the user wants to re-start the same inference
;                            in a new context (see change-context). 
;                        In both situations, 'hyps' is the set of
;                        incompatible hypotheses, from which the user
;                        must remove at least one, in order to make the context
;                        consistent.
;
;                       In both situations we have:
;
;                        'new-ct'   - context that supports the new node (this node 
;                                     may have been either derived or asserted).
;
;                        'contr-ct' - context that supports the node that 
;                                     contradicts the new node.
;
;                        'new-ot'   - origin tag associated with 'new-ct'.
;
;                        'contr-ot' - origin tag associated with 'contr-ct'.  
;
;                       Returns the set of hypotheses that remain in 'hyps'
;                       after the interaction with the user.
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc 11/03/88
;                                             hc  10/19/88 (get rid of repeat)
;                                  modified:  flj  3/22/99 
;
;
;
(defun make-consistent (hyps context new-ct contr-ct new-ot contr-ot)
  (declare (special sneps:outunit))
  (let (ct)
    (loop (setq ct (make-consistent-1 hyps context))
	  (when  (not (sneps:iseq.ns ct hyps)) 
	    (negate-hyps (compl.ns hyps ct)
			 (fullbuildcontext ct (new.cts))
			 new-ct contr-ct new-ot contr-ot)
	    (return ct)))))

;
; =============================================================================
;
;
; make-consistent-1
; -----------------
;
;
;       arguments     : inc-ct   - <node set>
;                       context - <context>
;
;       returns       : <node set>
;
;       description   : This function takes as argument an inconsistent 
;                       set of hypotheses and interacts with the user 
;                       until he has removed at least one hypothesis
;                       from this set.
;
;                       Returns the remaining hypotheses in 'inc-ct'.
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;                                  modified:  flj  3/22/99 
;
;
;
(defun make-consistent-1 (inc-ct context)
  (options-in-make-consistent-1)
  (browse-through-hyps inc-ct (context-hyps context) 'T))


;
; =============================================================================
;
;
; options-in-make-consistent-1
; ----------------------------
;
;
;       arguments     : ---
;
;       returns       : ---
;
;       description   : Notifies user that at least one hypothesis from
;                       an upcoming list must be deleted in order to make
;                       the context consistent.
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;                                  modified:  flj  3/22/99 
;
;
;
(defun options-in-make-consistent-1 ()
  (declare (special sneps:outunit))
  (format sneps:outunit
      "~% In order to make the context consistent you must delete ~
       ~%~T at least one hypothesis from the set listed below.~%"))



;
; =============================================================================
;
;
; autobr
; ------------------
;
;
;       arguments     : newnd-supps contrnd-supps context
;
;       returns       : the revised context
;
;       description   : This function attempts to perform automated belief
;                       revision once a contradiction has been detected.
;
;
;
;                                  written :  aif 01/23/11
;
;
;
(defun autobr (newnd-supps contrnd-supps context
                &optional (forward-inference-flag t) (assert-negation-flag t))
  (declare (special sneps:outunit snip::crntct sneps:crntct
    *most-entrenched-props*))
  (let ((hyp-sets nil))
    (sneps:do.cts (newsupp newnd-supps)
      (if (null contrnd-supps)
       (let ((union-hyps (context-hyps newsupp)))
          (when (sneps::issubset.ns union-hyps (context-hyps context))
            (setf hyp-sets (cons union-hyps hyp-sets))))
       (sneps:do.cts (contrsupp contrnd-supps)
        (let ((union-hyps 
                (union.ns
                  (context-hyps newsupp)
                  (context-hyps contrsupp))))
          (when (sneps::issubset.ns union-hyps (context-hyps context))
            (setf hyp-sets (cons union-hyps hyp-sets)))))))
    (let ((culprits (get-culprits hyp-sets)))
      #3!((remove-from-context ~culprits ~snip::crntct))
      (when (not forward-inference-flag)
        (sneps:clear-infer))
      (when assert-negation-flag (dolist (culprit culprits)
        (let ((neg-culprit (sneps::negate.n culprit)))
          (dolist (hyp-set hyp-sets)
            (when (and (member culprit hyp-set)
                       (isnew.ns
                        (sneps::intersect.ns
                          (remove.ns culprit culprits) hyp-set))
                       (not (member neg-culprit hyp-set)))
              (let* ((support-list (remove culprit hyp-set))
                     (support-ct (sneps::buildcontext support-list)))
                (sneps::newjust neg-culprit 'sneps:ext support-ct))))
          (snip::add* (list neg-culprit)))))
      (setf *most-entrenched-props* nil))
    sneps:crntct))




;
; =============================================================================
;
;
; get-culprits
; ------------------
;
;
;       arguments     : no-goods - a set of sets of propositions, from each of
;                       which a proposition must be retracted in order to
;                       restore a consistent context
;
;       returns       : a nodeset containing propositions to be retracted
;                       from the current context
;
;       description   : This function gets the culprits from the set of
;                       no-goods provided. TODO: explain more
;
;
;
;
;                                  written :  aif 01/24/11
;
;
;

(defun get-culprits (no-goods)
  (declare (special *br-tie-mode* br-totalorder))
  (reset-order-cache)
  (let* ((culprits nil)
         (sno-goods nil)
         (order<= (auto-order #'br-totalorder))
         (order< (lex-order< (auto-order #'br-totalorder)))
         (cached-order<
          (lambda (lhs rhs) (order-cache<= lhs rhs order<)))
         (cached-order>
          (lambda (lhs rhs) (not (order-cache<= lhs rhs order<)))))
    (unless *br-tie-mode*
      (let ((returnval (assist-get-culprits no-goods order<=)))
        (return-from get-culprits returnval)))
    (dolist (no-good no-goods)
      (setf sno-goods (cons (min-to-front no-good cached-order<) sno-goods)))
    (setf sno-goods (sort sno-goods cached-order> :key 'car))
    (loop while (not (null sno-goods)) do
      (let ((currentculprit (car (car sno-goods)))
            (cno-goods (copy-list sno-goods)))
        (setf culprits (cons currentculprit culprits))
        (dolist (no-good cno-goods)
          (when (member currentculprit no-good)
            (setf sno-goods (set-difference sno-goods (cons no-good nil)))))))
    culprits))



(defun reset-order-cache ()
  (declare (special *order-cache* *order-cache-complement*))
  (setf *order-cache* nil)
  (setf *order-cache-complement* nil))

(defun order-cache<= (lhs rhs order<=)
  (declare (special *order-cache* *order-cache-complement*))
  (if (member-equal (list lhs rhs) *order-cache*)
    t
    (if (member-equal (list lhs rhs) *order-cache-complement*)
      nil
      (if (funcall order<= lhs rhs)
        (setf *order-cache* (cons (list lhs rhs) *order-cache*))
        (progn
          (setf *order-cache-complement*
            (cons (list lhs rhs) *order-cache-complement*))
          nil)))))

(defun member-equal (item itemlist)
  (cond ((null itemlist) nil)
        ((equal item (car itemlist)) t)
        (t (member-equal item (cdr itemlist)))))

(defun remove-equal (item itemlist)
  (let ((listcopy nil))
    (dolist (elem itemlist)
      (when (not (equal item elem))
        (setf listcopy (cons elem listcopy))))
    listcopy))

; =============================================================================
;
;
; assist-get-culprits
; ------------------
;
;
;       arguments     : no-good - a set propositions, from which a proposition
;                       must be retracted in order to restore a consistent
;                       context
;                       order< - a total order that returns t iff lhs is less
;                       entrenched than rhs
;
;       returns       : a nodeset of which the first element is minimally
;                       entrenched
;
;       description   : This function helps choose a minimally entrenched
;                       proposition from a nodeset when an entrenchment tie
;                       occurs
;                       
;
;
;
;
;                                  written :  aif 02/03/11
;
;
;
(defun assist-get-culprits (no-goods order<=)
 (declare (special *order-cache* *order-cache-complement*))
 (let ((culprits nil))
  (loop
    (let ((possible-mins nil))
      (dolist (no-good no-goods)
        (let ((mins nil))
          (dolist (lhs no-good)
            (let ((minimal t))
              (block findmins
                (dolist (rhs no-good)
                  (when (not (order-cache<= lhs rhs order<=))
                    (setf minimal nil)
                    (return-from findmins))))
              (when minimal (setf mins (cons lhs mins)))))
          (setf possible-mins (append possible-mins (list mins)))))
      (let ((done))
        (loop
          (setf done t)
          (block autoremove
            (do* ((i 0 (+ i 1))
                  (possible-min-group (nth i possible-mins) (nth i possible-mins))
                  (no-good (nth i no-goods) (nth i no-goods)))
                 ((null possible-min-group))
              (when (eq (list-length possible-min-group) 1)
                (let* ((candidate (car possible-min-group))
                       (rest-mins (remove candidate no-good))
                       (other-mins nil))
                  (dolist (other-min-group (remove possible-min-group possible-mins))
                    (setf other-mins (union other-mins other-min-group)))
                  (when (null (intersection other-mins rest-mins))
                    (setf done nil)
                    (setf culprits (cons candidate culprits))
                    (let ((possible-mins-copy (copy-list possible-mins)))
                      (dolist (min-group possible-mins-copy)
                        (when (member candidate min-group)
                          (setf possible-mins (remove min-group possible-mins)))))
                    (let ((no-goods-copy (copy-list no-goods)))
                      (dolist (current-no-good no-goods-copy)
                        (when (member candidate current-no-good)
                          (setf no-goods (remove current-no-good no-goods)))))
                    (when (null no-goods) (return-from assist-get-culprits culprits)))))))
          (when done (return))))
      (let ((i 0))
        (loop
          (when (not (eq (list-length (nth i possible-mins)) 1))
            (let ((chosen-min (assist-choose-min (nth i possible-mins))))
              (dolist (rest-rhs (remove chosen-min (nth i possible-mins)))
                (when (order-cache<= rest-rhs chosen-min order<=)
                  (setf *order-cache*
                    (remove-equal (list rest-rhs chosen-min) *order-cache*))
                  (setf *order-cache-complement*
                    (cons (list rest-rhs chosen-min)
                      *order-cache-complement*)))))
            (return))
          (setf i (+ i 1))))))
  culprits))

(defun assist-choose-min (mins)
  (declare (special sneps:outunit sneps:inunit))
  (when (eq (list-length mins) 1)
    (return-from assist-choose-min (car mins)))
  (format sneps:outunit
    "~% Please choose from the following hypotheses the one that you would ~
     ~% least like to keep:~%")
  (print-num-lst mins mins)
  (ct-prompt)
  (let (( ans (- (read sneps:inunit) 1)))
    (nth ans mins)))
 


;
; =============================================================================
;
;
; lex-order<
; ------------------
;
;
;       arguments     : comp-func - a total order <=
;
;
;       returns       : a well-order < based on comp-func
;
;       description   : This creates a well-order from a total-order by
;                       deterministically ordering elements with the same
;                       rank in the total order. In order to break ties,
;                       lexicographic order is used. The total-order comp-func
;                       should be a function that takes two string-arguments lhs
;                       and rhs, and returns the following:
;                        if lhs < rhs, return t
;                             (the semantics for < are defined by comp-func)
;                        else, return nil
;
;
;                                  written :  Ari Fogel 01/23/2011
;
;
(defun lex-order< (comp-func)
  (lambda (lhs rhs)
    (let ((forward-result (funcall comp-func lhs rhs)))
      (if (and forward-result (funcall comp-func rhs lhs))
        (string< (write-to-string lhs) (write-to-string rhs)) 
        forward-result))))

(defun auto-order (comp-func)
  (lambda (lhs rhs)
    (or (member rhs *most-entrenched-props*)
      (and (not (member lhs *most-entrenched-props*))
           (funcall comp-func lhs rhs)))))


;
; =============================================================================
;
; browse-through-hyps 
; -------------------
;
;
;       arguments     : hyplst     - <node set>
;                       fullcthyps - <node set>
;                       inc        - T or nil(default) - (optional)
;
;       returns       : <node set>
;
;       description   : Shows the user a numbered list of the hypotheses in 
;                        'hyplst' and a menu which helps the user to make 
;                        decisions about which nodes to remove from the list.
;                        The '99 modification altered the interface to a list  
;                        with menu options for more user control and quicker
;                        revision. The flag 'inc' indicates whether the list
;                        being examined is inconsistent at the start.
;
;
;                       Returns the set of nodes that the user
;                       did not choose to discard. 
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  hc  10/19/88 (remove repeat)
;                                  modified:  flj  3/22/99 
;                                  modified:  scs  8/17/10
;                                  modified:  scs  06/14/13
;
;
(defun browse-through-hyps (hyplst fullcthyps &optional inc)       
  (declare (special sneps:outunit))
  (let (ans1 ans2 hyp (oldhyps hyplst) (removed (new.ns)))    
    (loop
      (format sneps:outunit "~%")
      (cond 
         ((zerop (cardinality.ns hyplst))
	  (if inc 
	      (format sneps:outunit 
		      "The set is now empty, but consistent.")
	    (format sneps:outunit "The set is now empty.")))
	 ((sneps::iseq.ns oldhyps hyplst)
	  (if inc
	      (format sneps:outunit "An inconsistent set of hypotheses: ")
	    (format sneps:outunit "An unaltered set of hypotheses: ")))
	 (t 
	  (if inc
	      (format sneps:outunit "The consistent set of hypotheses: ")
	    (format sneps:outunit "The revised set of hypotheses: "))))
      (format sneps:outunit "~%")
      (print-num-lst hyplst hyplst)
      (let ((n (cardinality.ns hyplst)))
	(setf ans1 (offer-choice n))
	(setf ans2 nil)
	(cond ((numberp ans1)
	       (setf hyp (nth ans1 hyplst))
	       (setf ans2 (lookat-hyp hyp fullcthyps)))
	      ((eqans ans1 'q) (RETURN hyplst))
	      ((eqans ans1 'i) (menu-instructions n))
	      ((eqans ans1 'r) 
	       (setf ans2 (see-removed removed fullcthyps)))
	      ((eqans ans1 'a) 
	       (print-fullct (compl.ns fullcthyps removed)))
	      ((eqans ans1 'd) 
	       (setf ans2 (delete-which n))))
	(cond ((numberp ans2) 
	       (setf hyp (nth ans2 hyplst))
	       (setf hyplst (remove.ns hyp hyplst))
	       (setf removed (insert.ns hyp removed)))
	      ((is.n ans2) 
	       (setf hyplst (insert.ns ans2 hyplst))
	       (setf removed (remove.ns ans2 removed)))
	      ((eqans ans2 'q) (RETURN hyplst))
	      ((eqans ans2 'd) 
	       (setf hyplst (remove.ns hyp hyplst))
	       (setf removed (insert.ns hyp removed))))))))

;
; =============================================================================
;
;
; offer-choice
; ------------
;
;
;       arguments     : n   - <integer>
;
;       returns       : ---
;
;       description   : Called from browse-through-hyps which has just 
;                       printed a numbered list of hypotheses (of length 'n')
;                       for examination.  This function offers the user menu 
;                       choices to assist in choosing whether to remove a 
;                       hypothesis -- and if so, which one(s). 
;                                              
;
;                                  written :  flj  3/22/99 
;                                  modified:  scs 06/14/13
;
;

(defun offer-choice (n)
  (declare (special sneps:outunit sneps:inunit))
  (if (zerop n)
      (format sneps:outunit "~%~TChoose from the menu below:")
    (format sneps:outunit 
	    "~%~T Enter the list number of a hypothesis to examine or~
             ~%~T [d] to discard some hypothesis from this list,"))
  (format sneps:outunit 
      "~%~T [a] to see ALL the hypotheses in the full context,~
       ~%~T [r] to see what you have already removed,~
       ~%~T [q] to quit revising this set, or ~
       ~%~T [i] for instructions ~
     ~%~%~T (please type ~:[a number OR d, ~; ~]a, r, q or i)"  (zerop n))
  (let (ans)
    (loop
      (ct-prompt)
      (setf ans (read sneps:inunit))
      (cond 
         ((numberp ans) 
	  (if (and (<= ans n) (> ans 0))
	      (RETURN (1- ans))
	    (format sneps:outunit 
		    "~T Numbers should be from the list above.~
                      ~%~T Enter a number OR a, r, d, q or i: ")))
	 ((or (eqans ans 'q)
	      (eqans ans 'i)
	      (eqans ans 'r)
	      (eqans ans 'a)
	      (eqans ans 'd))
	  (return ans))
	 (t (format sneps:outunit 
		    "Enter a number from list above OR a, r, d, q or i"))))))


; =============================================================================
;
;
; delete-which 
; ------------
;
;
;       arguments     : n   - <integer>
;
;       returns       : <integer> | 'Q | nil
;
;       description   : Called from browse-through-hyps after the user has 
;                       indicated a desire to discard a hypothesis from a 
;                       list of hypotheses.  User is then asked to enter
;                       the number of the hypothesis to discard or given 
;                       the option to cancel the discard or quit the revision
;                       completely.
;
;
;
;                                  written :  flj  3/22/99 
;                                  modified:  scs 06/14/13
;
;
;

(defun delete-which (n)
  (declare (special sneps:outunit sneps:inunit))
  (let (ans)
    (format sneps:outunit 
	  "~%~T Enter the list number of a hypothesis to discard,~
	   ~%~T [c] to cancel this discard, or [q] to quit revising this set.")
    (loop
      (ct-prompt)
      (setf ans (read sneps:inunit))
      (cond ((numberp ans) 
	     (if (and (<= ans n) (> ans 0)) 
		 (RETURN (1- ans))
	       (format sneps:outunit 
		       "~T Numbers should be from the list above.~
                      ~%~T Enter a number OR c or q")))
	    ((eqans ans 'q) (return ans))
	    ((eqans ans 'c) (return nil))))))



; =============================================================================
;
;
; lookat-hyp
; ----------
;
;
;       arguments     : hyp        - <node>
;                       fullcthyps - <node set>
;
;
;
;       returns       : 'D | 'K | 'U | 'Q 
;
;
;       description   : Shows user a hypothesis (hyp) and the nodes it 
;                       supports. Then asks user what they want to do:   
;                       discard it, keep it, undecided, quit the revision,  
;                       or see instructions (loops to offer user choice, again).
;
;
;
;                                  written :  flj  3/22/99 
;                                  modified:  scs 06/14/13
;
;
;

(defun lookat-hyp (hyp fullcthyps)
  (declare (special sneps:outunit))
  (format sneps:outunit
	  "~%~T ~A ~
           ~% WFFS that depend on ~A: ~%"
	  (snip:describe-or-surface hyp nil)
	  (snip:slight-describe-or-surface hyp nil))
  (print-lst (nodes-supported-by hyp (fullbuildcontext fullcthyps (new.cts))))
  (loop
    (format sneps:outunit
	    "~%~T What do you want to do with hypothesis ~A?~
	     ~%~T [d]iscard from the context, [k]eep in the context,~
	     ~%~T [u]ndecided, [q]uit revising this set, [i]nstructions ~
	     ~%~T (please type d, k, u, q or i)"
	    (snip:slight-describe-or-surface hyp nil))
    (let ((ans (enquire-hyp-fate)))
      (cond ((eqans ans 'i) (inspect-instructions)) 
	    (t  (return ans))))))



;
; =============================================================================
;
; enquire-hyp-fate
; ----------------
;
;
;       arguments     : ---
;
;       returns       : 'D | 'K | 'U | 'Q
;
;       description   : reads user's answers regarding what to do with
;                       a selected hypothesis that the user is examining
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  hc  10/19/88 (remove repeat)
;                                  modified:  flj  3/22/99 
;                                  modified:  scs 06/14/13
;
;
;
(defun enquire-hyp-fate ()
  (declare (special sneps:outunit sneps:inunit))
  (let (ans)
    (loop
      (ct-prompt)
      (setf ans (read sneps:inunit))
      (if (or (eqans ans 'd)
	      (eqans ans 'k)
	      (eqans ans 'u)
	      (eqans ans 'q)
	      (eqans ans 'i))
	  (RETURN ans))
      (format sneps:outunit "Please type d, k, u, q or i"))))

;
; =============================================================================
;
;
; see-removed
; -----------
;
;
;       arguments     : rlst       - <node set>
;                       fullcthyps - <node set>
;                       
;
;       returns       : <node> | 'Q | nil
;
;
;       description   : Called from browse-through-hyps.
;                       Allows user to see list of hypotheses removed from 
;                       context for inspection and possible return to context.
;                       User can also opt to quit the revision process.
;                       
;
;
;                                  written :  flj  3/22/99 
;                                  modified:  scs 06/14/13
;
;
;

(defun see-removed (rlst fullcthyps)
  (declare (special sneps:outunit sneps:inunit))
  (let (ans hyp)
    (if (null rlst)
	(format sneps:outunit
		"~%~T There are no removed hypotheses.~%~%")
      (loop
	(format sneps:outunit
 "~%~T Below is a list of hypotheses you have deleted during this revision.~%")
	(print-num-lst rlst)
	(setf ans (check-removed (cardinality.ns rlst)))
	(cond ((numberp ans) 
	       (if (return-it (setf hyp (nth ans rlst)) fullcthyps)
		   (RETURN hyp)))
	      ((eqans ans 'q) (return ans))
	      ((eqans ans 'r) (return nil)))))))
	    
;
; =============================================================================
;
;
; check-removed
; -------------
;
;
;       arguments     : n   - <integer>
;
;
;       returns       :  <integer> | 'R | 'Q
;
;
;       description   : Called from see-removed.
;                       Prompts user to input the number of a hypothesis
;                       from the removed list to examine and possibly return 
;                       to the set being revised OR to cancel this inspection  
;                       of the removed list and return to revising the set OR  
;                       to quit the revision process for that set entirely.
;
;
;                                  written :  flj  3/22/99 
;                                  modified:  scs 06/14/13
;
;
;

(defun check-removed (n)
  (declare (special sneps:outunit sneps:inunit))
  (format sneps:outunit 
	  "~%~T Enter the list number of a hypothesis to examine and~
           ~%~T     possibly return to the set you are revising, or~
           ~%~T [r] to return to the set you are revising, or ~
           ~%~T [q] to quit that set's revision. ~
         ~%~%~T (please type a number OR r or q)")  
  (let (ans)
    (loop
      (ct-prompt)
      (setf ans (read sneps:inunit))
      (cond ((numberp ans) 
	     (if (and (<= ans n) (> ans 0)) 
		 (RETURN (1- ans))
	       (format sneps:outunit 
		     "~T Numbers should be from the list above.~
                      ~%~T Enter a number OR r or q")))
	   ((or (eqans ans 'r)
		(eqans ans 'q))
	    (return ans))
	   (t (format sneps:outunit 
		      "Enter a number from list above OR r or q"))))))


;
; =============================================================================
;
;
; return-it
; ---------
;
;
;       arguments     : hyp        - <node>
;                       fullcthyps - <node set>
;
;
;       returns       : 'T | nil 
;
;
;       description   : Called by see-removed.
;                       Shows user their chosen "removed" hyp & the  
;                       propositions it supports. Returns T if user wishes  
;                       to return hyp to the context, ow nil.
;                       
;                       
;
;
;                                  written :  flj  3/22/99 
;                                  modified: 
;
;
;

(defun return-it (hyp fullcthyps)
      (declare (special sneps:outunit))
      (format sneps:outunit
	  "~%~T Reviewing the discarded hypothesis:   
	   ~%~T ~A ~
           ~%~T  propositions supported by ~A: ~%"
	  (snip:describe-or-surface hyp nil)
	  (snip:slight-describe-or-surface hyp nil))
  (print-lst (nodes-supported-by hyp (fullbuildcontext fullcthyps (new.cts))))
  (format sneps:outunit
	  "~%~T Do you want hypothesis ~A returned to the context?~
	   ~%~T Enter 'y' or 'n' (if unsure, say 'n')"
	   (snip:slight-describe-or-surface hyp nil))
  (if (user-says-yes)
      'T
    nil))


;
; =============================================================================
;
;
; menu-instructions
; -----------------
;
;
;       arguments     : n   - <integer>
;
;
;       returns       : <> 
;
;
;       description   : Instructions for browse-through-hyps menu options for
;                       examining a list of hypotheses.
;                       Sensitive to the size ('n') of the list - if the list 
;                       currently empty, options examine or discard a 
;                       hypothesis from the list are NOT offered.
;                       
;                       
;
;                                  written :  flj  3/22/99 
;                                  modified: 
;
;
;
	
(defun menu-instructions (n)
  (declare (special sneps:outunit))
  (format sneps:outunit
	  "~%~T You have the following choices from the menu: ~
           ~%~T  [a] to see ALL the hypotheses in the full context,~
           ~%~T  [r] to see a list of any hypotheses you have already removed~
	   ~%~T       (you can return one to the set or continue),")
  (if (> n 0)
      (format sneps:outunit
          "~%~T  [d] to discard some hypothesis from this list~
	   ~%~T       (you'll be prompted for its list number or to cancel or quit),~
           ~%~T  [#] where # is the list number (to the left) that corresponds~
           ~%~T        to the hypothesis you wish to examine.  This examination~
           ~%~T        includes the wffs the hypothesis supports as well as the ~
           ~%~T        options to keep or discard the hypothesis or to quit this revision,"))
  (format sneps:outunit
          "~%~T  [q] to quit revising this set, or ~
           ~%~T  [i] to get these instructions.~%"))


;
; =============================================================================
;
;
; inspect-instructions
; --------------------
;
;
;       arguments     : <>
;
;
;       returns       : <> 
;
;
;       description   : Instructions for lookat-hyp individual hypothesis menu 
;                       options. When examining a hypothesis for possible  
;                       removal from a context, one of the options is to have  
;                       these instructions printed out.
;                       
;
;
;                                  written :  flj  3/22/99 
;                                  modified: 
;
;
;

(defun inspect-instructions ()
  (declare (special sneps:outunit))
  (format sneps:outunit
    "~%~T You have five choices from the menu: ~
     ~%~T  [i] to get these instructions~
     ~%~T  [q] to quit revising this set~
     ~%~T  [d] to discard this hypothesis from the context and return to inspect ~
     ~%~T          the rest if the set ~
     ~%~T  [k] to keep this hypothesis & return to inspect the set as before~
     ~%~T  [u] if uncertain what to do with this hypothesis - returns you to~
     ~%~T          inspect the set as before ~%"))


;
; =============================================================================
;
;
; print-fullct
; ------------
;
;
;       arguments     : hyplst  - <node set>
;                       
;
;       returns       : <> 
;
;
;       description   : Called from browse-through-hyps.
;                       Takes the full context of hypotheses in which a 
;                       contradiction was found (hyplst) and prints it out
;                       in an unnumbered list - 
;                             (with no 'supported proposition' information)
;                       
;
;
;                                  written :  flj  3/22/99 
;                                  modified: 
;
;
;

(defun print-fullct (hyplst)
  (declare (special sneps:outunit))
  (format sneps:outunit 
	  "~%Are you sure - the list might be VERY long.~
           ~T (Enter 'y' or 'n')")
  (cond ((user-says-yes)
	 (format sneps:outunit 
            "~% The context as it stands if you quit revising now:~%")
	 (print-lst hyplst))))
	 
  
  
  
;
; =============================================================================
;
;
; print-num-lst
; -------------
;
;
;       arguments     : lst        - <node set>
;                       fullcthyps - <node set>
;
;
;       returns       :  <>
;
;
;       description   : Prints a numbered list (beginning with 1) of
;                       hypotheses (lst). Beneath each hypothesis, its
;                       supported nodes are also listed (by name)
;                       
;
;
;                                  written :  flj  3/22/99 
;                                  modified: 
;
;
;
(defun print-num-lst (lst &optional fullcthyps)
  "Prints a numbered list of hypotheses."
  (declare (special sneps:outunit))
  (let ((m  (cardinality.ns lst)) hyp) 
    (do ((n 1 (1+ n)))
	((> n m) (format sneps:outunit "~%"))
      (setf hyp (nth (1- n) lst))
      (format sneps:outunit
	      "~% ~A : ~A" 
	      n 
	      (snip:describe-or-surface 
	       hyp
	       nil))
      (if fullcthyps
	  (let* ((supp-lst (nodes-supported-by 
			   hyp 
			   (fullbuildcontext fullcthyps (new.cts))))
		(size-of-lst (cardinality.ns supp-lst)))
	    (format sneps:outunit
		    "          (~A supported proposition~:[s~;~]: ~A )~%"
		    size-of-lst (= 1 size-of-lst)
		    (snip:slight-describe-or-surface.ns 
			   supp-lst
			   nil)))))))


;
; =============================================================================
;
;
; print-lst
; ---------
;
;
;       arguments     : lst - <node set>
;                       
;
;       returns       : <> 
;
;
;       description   : Prints out an unnumbered list of nodes (lst) - with
;                       no supported-node information
;                       
;
;
;                                  written :  flj  3/22/99 
;                                  modified: 
;
;
;
(defun print-lst (lst)
  "Prints an un-numbered list of hypotheses."
  (declare (special sneps:outunit))
  (do ((n 0 (1+ n)))
      ((= n (cardinality.ns lst)) (format sneps:outunit "~%"))
      (format sneps:outunit
	      "~%~T ~A" 
	      (snip:describe-or-surface 
	       (nth n lst)
	       nil))))






; =============================================================================
;
;
; nodes-supported-by 
; ------------------
;
;
;       arguments     : hyp     - <node>
;                       ct  - <context>
;
;       returns       : <node set>
;
;       description   : Returns all the nodes which have a support that:
;                        - contains 'hyp'
;                        - is a subset of 'ct'
;
;       
;
;
;                                  written :  mrc 11/03/88 
;                                  modified:  
;
;
;
(defun  nodes-supported-by (hyp ct)
  (nodes-supported-by-1 (value.sv 'sneps:nodes) hyp ct))

(defun nodes-supported-by-1 (allnodes hyp ct)
  (let ((result (new.ns)))
    (sneps:do.ns (nd allnodes result)
      (if (is-supported-by-and-in-context nd hyp ct)
	  (setq result (insert.ns nd result))))))



; =============================================================================
;
;
; is-supported-by-and-in-context 
; ------------------------------
;
;
;       arguments     : nd  - <node>
;                       hyp - <node>
;                       ct  - <context>
;
;       returns       : T | NIL
;
;       description   : Returns T if 'nd' has a support which:
;                         - contains 'hyp'
;                         - is a subset of 'ct'
;       
;
;
;                                  written :  mrc 11/03/88 
;                                  modified:  
;
;
;
(defun is-supported-by-and-in-context (nd hyp ct)
  (is-supported-by-and-in-context-1 (ctcs-to-cts (node-asupport nd)) 
				    hyp
				    ct))

(defun is-supported-by-and-in-context-1 (supps hyp ct)
  (cond ((isnew.cts supps) nil)
	((and (ismemb.ns hyp (context-hyps (choose.cts supps)))
	      (sneps:issubset.ct (choose.cts supps) ct))
	 t)
	(t (is-supported-by-and-in-context-1 (others.cts supps) hyp ct))))


;
; =============================================================================
;
;
; negate-hyps 
; -----------
;
;
;       arguments     : disb-hyps - <node set>
;                       supp      - <context>
;                       new-supp    - <context>
;                       contr-supp  - <context>
;                       new-ot    - <otag>
;                       contr-ot  - <otag>
;
;       returns       : <>
;
;       description   : Negates the conjunction of the hyps in 'disb-hyps'. 
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc 12/07/88
;                                  modified:  flj 03/22/99
;                                  modified:  scs/flj 06/20/04 --  Uses the new sneps:negate.n
;
;
;

(defun negate-hyps (disb-hyps supp new-supp contr-supp new-ot contr-ot) 
  (sneps:newjust (sneps:negate.n
		  (choose.ns 
		   (if (= (cardinality.ns disb-hyps) 1)
		       disb-hyps
		     (apply #'mybuild 
			    (list 'min (length disb-hyps)
				  'max (length disb-hyps)
				  'arg disb-hyps)))))
		 (make-ot new-ot contr-ot new-supp contr-supp) supp))

(defun mybuild (&rest snd)
  (sneps:find-or-build (sneps:evalsnd snd t)))


(defun make-ot (new-ot contr-ot new-supp contr-supp)
  (cond ((sneps:iseq.ct new-supp contr-supp) 
	 (snip:combine-ots* (list new-ot contr-ot)))
	(t 'sneps:ext)))


;
; =============================================================================
;
;
; user-says-yes 
; -------------
;
;
;       arguments     : ---
;
;       returns       : T | NIL
;
;       description   : This function returns T if the user types "yes", "y",
;                       "ok" or "sure", and returns NIL if the user types "n"
;                       or "no".
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;

(defun user-says-yes ()
  (member (user-says-yes-1) '(snepsul:yes snepsul:y snepsul:ok snepsul:sure)))

(defun user-says-yes-1 () 
  (declare (special sneps:outunit sneps:inunit))
  (let (ans)
    (loop (ct-prompt)
	  (setq ans (read sneps:inunit))
	  (if (or (eq ans 'snepsul:Y)
		  (eq ans 'snepsul:yes)
		  (eq ans 'snepsul:ok)
		  (eq ans 'snepsul:sure)
		  (eq ans 'snepsul:n)
		  (eq ans 'snepsul:no))
	      (RETURN ans))		
	  (format sneps:outunit "Please type n or y"))))

;
; =============================================================================
;
;
; ct-prompt 
; ---------
;
;
;       arguments     : ---
;
;       returns       :  ---
;
;       description   : This function prints a prompt
;
;       
;
;
;                                  written :  jpm 11/30/82 
(defun ct-prompt ()
  (declare (special sneps:outunit))
  (format sneps:outunit "~%=><= "))


(defun min-to-front (nodeset well-order<)
  (let ((running-min (car nodeset)))
    (dolist (node (cdr nodeset))
      (if (funcall well-order< node running-min)
        (setf running-min node)))
    (cons running-min (remove running-min nodeset))))

;
; =============================================================================
;
;
; eqans
; -----
;
;
;       arguments     : ans - <symbol>
;                       option - <symbol>
;
;       returns       : T | NIL
;
;       description   : This function returns T if ans and option are spelled alike,
;                       ignoring package and case,
;                       and returns NIL otherwise.
;
;                                  written :  scs 06/14/13
;

(defun  eqans (ans option)
  (and (symbolp ans) ; option is always supplied by code
       (string-equal (symbol-name ans) (symbol-name option))))
