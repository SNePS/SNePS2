;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEBR; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: sniphandler.lisp,v 1.3 2013/08/28 19:07:24 shapiro Exp $

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




(in-package :snebr)


; =============================================================================
;
; snip-contr-handler 
; -------------------
; 
;
;       arguments     : newnode - <node> 
;                       contrnd - <node>
;                       context - <context>
;
;       returns       : ---
;
;       description   : This function takes as arguments a node `contrnd' which 
;                       contradicts some newly derived node `newnode' (refer 
;                       to the function ck-contradiction), and a context
;                       `context' under which the newly asserted node was derived.
;                       It warns the user of the detected contradiction and
;                       calls the function contr-h to handle the contradiction.
;
;
;                                   
;                                  written :  mrc  11/15/88
;
;
;                                                
;
(defun snip-contr-handler (newnode contrnd context)
  (declare (special sneps:outunit snip:crntctname *br-auto-mode*))
  (progn
    (unless *br-auto-mode*
      (format sneps:outunit
  	    "~%~%~T A contradiction was detected within context ~A.~
                 ~%~T The contradiction involves the newly derived proposition:~
             ~%~T~T~T ~A ~
                 ~%~T and the previously existing proposition:~
             ~%~T~T~T ~A"
  	    snip:crntctname
  	    (snip:describe-or-surface newnode nil)
  	    (snip:describe-or-surface contrnd nil)))
    (contr-h (ctcs-to-cts (sneps:node-asupport newnode))
	     (ctcs-to-cts (sneps:node-asupport contrnd))
	     (sneps:ctcs-to-ots (sneps:node-asupport newnode))
	     (sneps:ctcs-to-ots (sneps:node-asupport contrnd))
	     context)))		
 
;
; =============================================================================
;
;
; contr-h 
; -------
;
;       arguments     : newnd-supps    - <context set>
;                       contrnd-supps  - <context set>
;                       newnd-otlst    - <ot list>
;                       contrnd-otlst  - <ot list>
;                       context        - <context>
;
;       returns       : <node set>
;
;       description   : This function handles contradictions detected during
;                       inference.
;                       In the current implementation the contradiction is
;                       resolved by the user which may choose one of three
;                       options:
;                        1. [C]ontinue anyway, knowing that a contradiction 
;                           is derivable;
;	                 2. [R]e-start the exact same run in a different 
;                           context which is not inconsistent;
;                        3. [D]rop the run altogether.
;
;                       This function asks the user what his choice is and
;                       takes the necessary actions to execute that choice.
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc  11/15/88
;                                             hc   04/26/89
;                                             flj  03/22/99 
;                                         
;
;

(defun contr-h (newnd-supps contrnd-supps newnd-otlst contrnd-otlst context)
  (declare (special sneps:outunit snip:crntctname *br-auto-mode*
                    *most-entrenched-props*))
  (unless *br-auto-mode* (options-in-contr-h))
  (let* ((ans (if *br-auto-mode* 'snepsul:A (read-contr-h-option)))
	 (lastcommand  #-explorer (value.sv 'sneps:command)
		       #+explorer (if (eql (first (value.sv 'sneps:command)) 
					   'sys:displaced)
				      (second (value.sv 'sneps:command))
				      (value.sv 'sneps:command))
		       )
   (mep *most-entrenched-props*))
    (cond ((or (eq ans 'snepsul:C) (eql ans 2)))
    ((or (eq ans 'snepsul:A) (eql ans 1))
      (autobr newnd-supps contrnd-supps context)
      (if (sneps:issubset.ns (context-hyps (car newnd-supps)) (context-hyps sneps:crntct))
        (progn 
          (setf *most-entrenched-props* mep)
          (sneps:topsneval lastcommand))
	      (throw 'snip:Stop-Handled-by-Contradiction-Handler nil)))
	  (t (multi:clear-all-queues)
	     (sneps:clear-infer)
	     (cond ((or (eq ans 'snepsul:R)(eq ans 'snepslog::R)(eql ans 3))
		    (change-context newnd-supps contrnd-supps 
				    newnd-otlst contrnd-otlst 
				    context snip:crntctname)
		    (sneps:topsneval lastcommand))
		   ((is-add lastcommand) (remove-new-hyp lastcommand)))
	     (throw 'snip:Stop-Handled-by-Contradiction-Handler nil)))))

;
; =============================================================================
;
;
; options-in-contr-h 
; ------------------
;
;
;       arguments     : 
;
;       returns       : nil
;
;       description   : This function tells the user the possibilities he
;                       has for handling a contradiction detected during 
;                       inference.
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun options-in-contr-h ()
  (declare (special sneps:outunit))
  (format sneps:outunit
       "~%~T You have the following options:~
      ~%~T~T 1. [A]ttempt to resolve the contradiction automatically;~
      ~%~T~T 2. [C]ontinue anyway, knowing that a contradiction is derivable;~
      ~%~T~T 3. [R]e-start the exact same run in a different context which is~
      ~%~T~T    not inconsistent;~
      ~%~T~T 4. [D]rop the run altogether.
      ~%~T (please type a, c, r or d)"))

;
; =============================================================================
;
;
; read-contr-h-option 
; -------------------
;
;
;       arguments     : 
;
;       returns       : 'C | 'R | 'D
;
;       description   : This function reads an answer of the user.
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;                                             hc  10/19/88 (get rid of repeat)
;
(defun read-contr-h-option ()
  (declare (special sneps:outunit sneps:inunit))
  (let (ans)
    (loop (ct-prompt)
      (case (setf ans (read sneps:inunit))
	((1 2 3 4 snepsul:A snepsul:C snepsul:R snepsul:D)
	 (return ans))
	((snepslog::A snepslog::C snepslog::R snepslog::D)
	 (return ans)))
      (format sneps:outunit "Please type a, c, r or d"))))

;
; =============================================================================
;
; change-context 
; --------------
;
;
;       arguments     : newnd-supps   - <context set>
;                       contrnd-supps - <context set>
;                       newnd-otlst   - <origin tag list>
;                       contrnd-otlst - <origin tag list>
;                       context       - <context>
;
;       returns       : <node set>
;
;       description   : This function takes as arguments:
;                        'newnd-supps'   - set of contexts that support the
;                                          newly derived node
;                        'contrnd-supps' - set of contexts that support the
;                                          node which contradicts the newly 
;                                          derived node.
;                        'newnd-otlst'   - list of origin tags corresponding
;                                          to 'newnd-supps'.
;                        'contrnd-otlst' - list of origin tags corresponding
;                                          to 'contrnd-supps'.
;                        'context'       - context in which the new node was 
;                                          derived.
;                        'context-name'  - the name of the context above
;
; 
;                      This function changes the current context, which is
;                      inconsistent, to a consistent context. The user may
;                      change the current context by
;                        - removing one or more hypotheses from inconsistent
;                          sets of hypotheses that are part of the cuurent
;                          context (see make-consistent) 
;                        - removing hypotheses from a set of hypotheses that
;                          is not known to be inconsistent and that is
;                          also part of the current context (see browse-
;                          through)
;                        - adding new hypotheses to the current context
;                          (see add-new-hyps)
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  mrc 12/12/88
;                                  modified:  flj  3/22/99
;


(defun change-context (newnd-supps contrnd-supps 
		       newnd-otlst contrnd-otlst 
		       context context-name) 
  (inform-user newnd-supps contrnd-supps context)  
  (let ((crntct (context-hyps context))	
	(unseen-hyps (context-hyps context))
	(new-hyps (new.ns))
	(new-ot nil)
	(contr-ot nil)
	(contr-otlst contrnd-otlst))
    (sneps:do.cts 
     (new-supp newnd-supps)
     (setf new-ot (first newnd-otlst))
     (setf newnd-otlst (rest newnd-otlst))
     (setf contr-otlst contrnd-otlst)
     (sneps:do.cts 
      (contr-supp contrnd-supps)
      (let* ((inc-hyps 
	      (sneps::union.ns (context-hyps new-supp) 
			       (context-hyps contr-supp)))
	     (intersec-hyps 
	      (sneps::intersect.ns (context-hyps new-supp) 
				   (context-hyps contr-supp))))	
	(setf contr-ot (first contr-otlst))
	(setf contr-otlst (rest contr-otlst))	
	(when (sneps:issubset.ns inc-hyps crntct) 
	  (if intersec-hyps (warn-intersection intersec-hyps))
	  (setf crntct 
	    (union.ns (compl.ns crntct inc-hyps) 
		      (make-consistent inc-hyps
				       (fullbuildcontext crntct (new.cts))
				       new-supp
				       contr-supp
				       new-ot
				       contr-ot))))	    
	(setf unseen-hyps (compl.ns unseen-hyps inc-hyps)))))   
    (when (not (isnew.ns unseen-hyps))	
      (setf crntct (union.ns (compl.ns crntct unseen-hyps)       
			     (browse-through unseen-hyps crntct))))
    (change-context-name crntct context-name)
    (if (and (not (isnew.ns (setf new-hyps (add-new-hyps))))
	     (choose.ns new-hyps))
	(change-context-name (union.ns crntct new-hyps) context-name)
      crntct)))


;
; =============================================================================
;
;
; change-context-name
; -------------------
;
;
;       arguments     : lhyps    - <node set>
;                       crntname - <context name>
;                       
;
;       returns       : <lhyps>
;
;       description   : This function resets the current context name to refer
;                       to the context formed by lhyps.
;
;
;                                  modified:  flj  3/22/99
;       
;
(defun change-context-name (lhyps crntname)
  (name.ct (fullbuildcontext lhyps (new.cts))  crntname)
  lhyps)

;
; =============================================================================
;
;
; inform-user 
; -----------
;
;
;       arguments     : newnd-supps   - <context set>
;                       contrnd-supps - <context set>
;                       context       - <context>
;
;       returns       : <>
;
;       description   : This function tells the user from which sets he
;                       must remove one or more hypotheses in order to 
;                       make the current consistent. Modification in 99
;                       to advise user of WFFS common to more than one set.
;       
;
;
;                                  written :  mrc 11/16/88 
;                                  modified:  flj  3/22/99
;

(defun inform-user (newnd-supps contrnd-supps context)
  (declare (special sneps:outunit))
  (let ((hyp-sets nil))
    (format sneps:outunit 
       "~%~%~T In order to make the context consistent you must delete at least~
          ~%~T one hypothesis from each of the following sets of hypotheses:")
    (sneps:do.cts 
     (newsupp newnd-supps)
     (if (null contrnd-supps)
	 (let ((union-hyps (context-hyps newsupp)))
	   (if (sneps::issubset.ns union-hyps (context-hyps context))
	       (progn 
		 (setf hyp-sets (cons union-hyps hyp-sets))
		 (format sneps:outunit "~%~T~T~T ~A" 
			 (snip:slight-describe-or-surface.ns 
			  union-hyps
			  nil)))))
       (sneps:do.cts 
	(contrsupp contrnd-supps)
	(let ((union-hyps (union.ns (context-hyps newsupp)
				    (context-hyps contrsupp))))
	  (if (sneps::issubset.ns union-hyps (context-hyps context))
	      (progn 
		(setf hyp-sets (cons union-hyps hyp-sets)) 
		(format sneps:outunit "~%~T~T~T ~A" 
			(snip:slight-describe-or-surface.ns 
			 union-hyps
			 nil)))))))
     )
    (let* ((one-list (apply #'append hyp-sets)) 
	   (doubles (get-doubles one-list)))
      (if doubles 
	  (progn 
	    (if (> (cardinality.ns doubles) 1)	
		(format sneps:outunit 
	      "~%~%~T~T The hypotheses listed below are included in more than~
                ~%~T  one set. Removing one of these will make more than one~
                ~%~T  set consistent.")
	      (format sneps:outunit 
             "~%~%~T The hypothesis listed below is included in more than one~
              ~%~T  set. Removing it will make more than one set consistent."))
	    (format sneps:outunit "~%~T~T~T ~A"
		    (snip:slight-describe-or-surface.ns 
		     doubles
		     nil)))))
    )
  (format sneps:outunit "~%~%~T"))

;
; =============================================================================
;
; get-doubles 
; -----------
;
;
;       arguments     : lst     - <node set>
;                       slst    - <node set>   (optional)
;                       dlst    - <node set>   (optional)
;
;       returns       : <node set>
;
;       description   : helper function called by inform-user to find any  
;                       elements listed more than once in lst
;
;
;
;
;                                  written :   flj  3/22/99
;                                  modified:                                  
;

(defun get-doubles (lst &optional slst dlst)
  "Returns a list of all elements in lst that are listed more than once"
    (cond ((null lst) (reverse dlst))
	  ((member (first lst) dlst) (get-doubles (rest lst) slst dlst))
	  ((member (first lst) slst) (get-doubles (rest lst) slst (cons 
								   (first lst)
								   dlst)))
	  (t (get-doubles (rest lst) (cons (first lst) slst) dlst))))
 	  


; =============================================================================
;
; warn-intersection
; -----------------
;
;
;       arguments     : hyps    - <node set>
;
;       returns       : <>
;
;       description   : Called by change-context.  
;                       Warns user when context to be revised has wff(s) common
;                       to BOTH contradictory WFFS  (number sensitive)
;
;
;
;
;                                  written :   flj  3/22/99
;                                  modified:                                  
;

(defun warn-intersection (hyps)
  (declare (special sneps:outunit))
  (if (> (cardinality.ns hyps) 1)
      (format sneps:outunit 
     "~T WARNING: the following hypotheses support BOTH contradictory WFFS,~
   ~%~T          so removing one of these might be too extreme a revision.~%")
    (format sneps:outunit 
      "~T WARNING: the following hypothesis supports BOTH contradictory WFFS,~
      ~%~T          so removing it might be too extreme a revision.~%"))
  (print-lst hyps))


;					
; =============================================================================
;
; browse-through 
; --------------
;
;
;       arguments     : hyplst     - <node set>
;                       fullcthyps - <node set>
;
;       returns       : <node set>
;
;       description   : This function allows the user to inspect and
;                       eventually discard any hypotheses in 'hyplst'
;                       (see browse-through-hyps). 99 modification
;                       changed list elements appearancefrom "M#!"  
;                       style to "WFF#" style.
;
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  flj  3/22/99
;
;

(defun browse-through (hyplst fullcthyps)
  (declare (special sneps:outunit))
  (format sneps:outunit
	  "~%~T The following (not known to be inconsistent) set of ~
	   ~%~T hypotheses was also part of the context where the ~
	   ~%~T contradiction was derived: ~
	   ~%~T~T~T ~A
	   ~%~T Do you want to inspect or discard some of them?"
	  (snip:slight-describe-or-surface.ns     
			 hyplst
			 nil))
  (cond ((user-says-yes) (browse-through-hyps hyplst fullcthyps))
	(t hyplst)))

;
; =============================================================================
;
; add-new-hyps
; ------------
;
;
;       arguments     : -----
;
;       returns       : <node set>
;
;       description   : This function asks the user if he wants to
;                       add new hypotheses to the current context
;                       and returns the hypotheses added by the user
;                       (eventually none).
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:
;
;
(defun add-new-hyps ()
  (declare (special sneps:outunit snepslog:*SNePSLOGRunning*))
  (let ((option (if snepslog:*SNePSLOGRunning*
		    'snepsul::o
		  'snepsul::u))
	(add-new? (yes-or-no-p "~%~T Do you want to add a new hypothesis?  "))
	(newhyps (new.ns)))
    (loop
      while add-new?
	    do (setf newhyps (insert.ns
			      (request-new-hyp option) newhyps)
		     add-new? (yes-or-no-p
			       "~%~T Do you want to add another hypothesis?  ")))
    newhyps))

;
; =============================================================================
;
;
; request-new-hyp
; ---------------
;
;
;       arguments     : ----
;
;       returns       : <node set>
;
;       description   : This function reads a hypothesis entered by the
;                       user and returns it.
;
;                                  written :  choi ??/??/92

(defun request-new-hyp (option)
  (declare (special sneps:outunit sneps:inunit))
  (if (eq option 'snepsul::u)
      (format sneps:outunit
	      "~%~T Enter a hypothesis using the SNePSUL command `assert': ")
    (format sneps:outunit
	      "~%~T Enter a hypothesis using SNePSLOG all on one line and ending with a period: "))
  (let (newhyp ans)
    (loop (ct-prompt)
	  (if (eq option 'snepsul::u)
	      (setq ans (read sneps:inunit))
	      (setq ans (snepslog::snepslog-read sneps:inunit)))
	  (setq newhyp (sneps:topsneval (insert-context ans)))
	  (cond (newhyp
		 (return (sneps:choose.ns newhyp)))
		(t
		 (format sneps:outunit
			 "~%~T Oops... something went wrong~
                          ~%~T Would you try to enter again?")
		 (unless (user-says-yes)
		   (return (new.ns))))))))


(defun insert-context (comm)
  (declare (special sneps:crntct))
  `(,@comm :context ,(intern (string sneps:crntct) 'snepsul)))

;
; =============================================================================
;
;
; remove-new-hyp 
; --------------
;
;
;       arguments     : <command>
;
;       returns       : <node set>
;
;       description   : This function removes the node built by
;                       the last command (which was an add command)
;                       from the current context. 
;       
;
;
;                                  written :  mrc  12/20/88
;                                  modified:  flj   3/22/99 
;

(defun remove-new-hyp (lastcommand)
  ;; Assume (is-add lastcommand) is True
  (let ((addcommand (if (eql (first lastcommand) 'snip:add)
			(cons 'sneps:assert (rest lastcommand))
			(cons 'sneps:assert (rest (second lastcommand))))))
    (remove-new-hyp-1 (sneps:topsneval addcommand))))

(defun remove-new-hyp-1 (newhyp)
  (declare (special snip:crntctname sneps:outunit))
  (format sneps:outunit "Changing the hypothesis set of ~A.~%" snip:crntctname)
  (change-context-name (compl.ns (context-hyps (value.sv snip:crntctname))
				 newhyp) snip:crntctname))
;
; =============================================================================
;
;
; is-add 
; ------
;
;
;       arguments     : <command>
;
;       returns       : T | NIL
;
;       description   : This function returns T if 'command'
;                       is an add command.
;       
;
;
;                                  written :  mrc  12/20/88 
;                                  modified:  
;
;
(defun is-add (command) 
  (or (eq (first command) 'snip:add)
      (and (consp (second command))
	   (eql (first (second command)) 'snip:add))))



    
    




