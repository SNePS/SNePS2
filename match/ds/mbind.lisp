;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: mbind.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :match)


;;; -----------------------------------------------------------------------------
;;;       Ported from Franz Lisp to Common Lisp:   KEB  Summer 1987
;;; -----------------------------------------------------------------------------
;;;
;;;
;;;
;;; =============================================================================
;;; Data Type:  <mbind> ::= (<mvar> . <mnode>)
;;;
;;;***************************
;;;   Changed to: <mbind> ::= (<key> . <node>)  {as an element of a LISP A-List}
;;;
;;; ============================================================================
;;;
;;; new.mb
;;; ------
;;;
;;;       arguments     : mv - <mvar> 
;;;                       mn - <mnode>
;;;
;;;       returns       : <mbind>
;;;
;;;       description   : Creates a <new mbind> from 'mv' and 'mn'.
;;;
;;;                                  written:  vhs 11/19/84
;;;                                  modified:
;;;
;;; Definition moved to: match/ds/mnoderepset  so that it will be available when
;;;            it is used.
;;;
;;;(defmacro new.mb (mv mn)
;;;  "Creates a <new mbind> from 'mv' and 'mn'."
;;;   `(cons ,mv ,mn))
;;;
;;;
;;; =============================================================================
;;;
;;; is.mb
;;; -----
;;;
;;;       arguments      : u - <universal>
;;;
;;;       returns        : <boolean>
;;;
;;;       description    : returns 'true' if 'u' is a <mbind>, 'false' otherwise.
;;;
;;;
;;;                                  written:  vhs 11/19/84
;;;                                  modified: ssc 11/23/88
;;;
;;;
#|
(defmacro is.mb (u)
  "Returns 'true' if 'u' is a <mbind>, 'false' otherwise."
   `(and (consp ,u)     
         (or (is.n (car ,u))
	     (ispat.n (car ,u)))
         (is.n (cdr ,u))))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; mvar.mb
;;; -------
;;;
;;;        arguments      : mbind -  <mbind>
;;; 
;;;        returns        : <mvar> 
;;; 
;;;        description    : returns the <mvar> of 'mbind'.
;;;
;;;                                  written:  vhs 11/19/84
;;;                                  modified:
;;;
;;;
(defun mvar.mb (mbind)
  "Returns the <mvar> of 'mbind'."
   (car mbind))
;;;
;;;
;;; =============================================================================
;;;
;;; mnode.mb
;;; --------
;;;
;;;        arguments       : mbind - <mbind> 
;;;          
;;;        returns         : <mnode>
;;; 
;;;        description     : returns the <mnode> of 'mbind'.
;;;                             
;;;
;;;                                  written:  vhs 11/19/84
;;;                                  modified:
;;;
(defun mnode.mb (mbind)
  "Returns the <mnode> of 'mbind'."
   (cdr mbind))
;;;
;;;
;;; =============================================================================
;;;
;;; compose.mb
;;; ----------
;;;
;;;       arguments     : mb - <mbind>
;;;                       sub - <substitution>
;;;
;;;       returns       : <mbind> 
;;;
;;;       nonlocal-vars : *INITSUB*
;;;
;;;       description   : It is used to construct the <source substitution>.
;;;                       'mb' is an <mbind> from *RENAMESUB*.  sub is *SUB*.
;;;                       Only <mbind>s whose <mvar> is a <node> are included.
;;;                       It returns an <mbind> with that <mvar> and the 
;;;                       appropriate instance of the <mnode> of 'mb'.
;;;                       In case the <mnode> is changed, *FINALMNRS* is updated.
;;;
;;;       side-effects  : *FINALMNRS*  
;;;
;;;                                        written :  vhs 04/25/85
;;;                                        modified:
;;;
;;;
#|
(defun compose.mb (mb sub)
  "It is used to construct the <source substitution>.
   'mb' is an <mbind> from *RENAMESUB*.  sub is *SUB*.
   Only <mbind>s whose <mvar> is a <node> are included.
   It returns an <mbind> with that <mvar> and the 
   appropriate instance of the <mnode> of 'mb'.
   In case the <mnode> is changed, *FINALMNRS* is updated."
  (let ((*node* (mvar.mb mb))
        (value (mnode.mb mb))
        (all-psym nil)
        (new-msym nil))
    (declare (special *node*))
    (declare (special *RENAMESUB* *FINALMNRS* *INITSUB* *SUB* *CHANGED*))
    (cond ((is.n *node*)
	   (cond ((isbound.sbst value sub)
		  (makeone.set (new.mb *node* (target-instance.n (mnode.sbst value sub)))))
		 ((isbound.sbst value *RENAMESUB*)
		  (makeone.set (new.mb *node* (mnode.sbst value *RENAMESUB*)))) 
		 ((isvar.n *node*)
		  (cond ((is.xsym value) (update.mnrs '*FINALMNRS* value *node* *FINALMNRS*) nil)
			((is.usym value)
			 (cond ((changed.mv value) 
				(setq new-msym (new.usym))
				(rename-mnoderep.mnrs value new-msym *FINALMNRS*)
				(makeone.set (new.mb *node* new-msym)))
			       (t (let ((old-usym (mnode.sbst *node* *INITSUB*)))
				    (put-back.mnrs value old-usym *FINALMNRS*)
				    (update.sbst '*SUB* value old-usym *SUB*)
				    (makeone.set (new.mb *node* old-usym))))))
			((is.isym value)
			 (cond
			  ((changed.mv value) 
			   (setq new-msym (new.isym))
			   (rename-mnoderep.mnrs value new-msym *FINALMNRS*)
			   (makeone.set (new.mb *node* new-msym)))
			  ((is.isym (mnode.sbst *node* *INITSUB*)) 
			   (let ((old-isym (mnode.sbst *node* *INITSUB*)))
			     (update.sbst '*SUB* value old-isym *SUB*)
			     (put-back.mnrs value old-isym *FINALMNRS*)
			     (adjoin.mns '*CHANGED* old-isym *CHANGED*)
			     (makeone.set (new.mb *node* old-isym))))
			  (t (let ((p-bind (mnode.sbst *node* *INITSUB*)))
			       (update.sbst '*SUB* value p-bind *SUB*)
			       (update.mnrs '*FINALMNRS* value p-bind *FINALMNRS*)
			       (makeone.set (new.mb *node* p-bind))))))
			(t (makeone.set mb))))
		 ((ispat.n *node*) 
		  (cond ((is.usym value)
			 (cond ((changed.mv value)
				(setq new-msym (new.usym))
				(rename-mnoderep.mnrs value new-msym *FINALMNRS*)
				(makeone.set (new.mb *node* new-msym)))
			       (t (let ((old-usym (mnode.sbst *node* *INITSUB*)))
				    (update.sbst '*SUB* value *node* *SUB*)
				    (put-back.mnrs value old-usym *FINALMNRS*)
				    (makeone.set (new.mb *node* old-usym))))))
			((is.isym value)
			 (cond
			  ((changed.mv value) 
			   (setq new-msym (new.isym))
			   (rename-mnoderep.mnrs value new-msym *FINALMNRS*)
			   (makeone.set (new.mb *node* new-msym)))
			  (t (update.sbst '*SUB* value *node* *SUB*)
			     (update.mnrs '*FINALMNRS* value *node* *FINALMNRS*) 
			     nil)))
			(t (makeone.set mb))))))
	  (t nil))))
|#
;;;
;;;
;;; =============================================================================
;;;
;;; target.mb
;;; ---------
;;;
;;;       arguments     : mb - <mbind>
;;;
;;;       returns       : (<mb>) | nil 
;;;
;;;       nonlocal-vars : *CHANGED* *INITSUB*
;;;
;;;       description   : It constructs an <mbind> for the <target substitution>.
;;;                       'mb' comes from *SUB* and is considered only if its
;;;                       <mvar> is a <node>.  The function compose.mb.mn insures
;;;                       that the right instance of the <mnode> of the <mbind>
;;;                       is used.
;;;
;;;                                        written :  vhs 04/25/85
;;;                                        modified:
;;;
;;;
#|
(defun target.mb (mb)

  "It constructs an <mbind> for the <target substitution>.
   'mb' comes from *SUB* and is considered only if its
   <mvar> is a <node>.  The function compose.mb.mn insures
   that the right instance of the <mnode> of the <mbind>
   is used."

  (cond ((is.n (mvar.mb mb))
         (makeone.set mb))
        (t nil)))
|#
;;;
;;;     
;;; =============================================================================
;;;
;;; iseq.mb
;;; -------
;;;
;;;       arguments     : mbind1 - <mbind>
;;;                       mbind2 - <mbind>
;;;
;;;       returns       : <boolean>
;;;  
;;;       description   : Compares 'mbind1' and 'mbind2' as <mbind>s : 
;;;                       returns 'true' if 'mbind1' and 'mbind2' have the
;;;                       same <mvar> and the same <mnode>, 'false' otherwise.
;;;
;;;                                  written:  vhs 11/19/84
;;;                                  modified:
;;;
(defmacro iseq.mb (mbind1 mbind2)

   "Compares 'mbind1' and 'mbind2' as <mbind>s : 
    returns 'true' if 'mbind1' and 'mbind2' have the
    same <mvar> and the same <mnode>, 'false' otherwise."

   `(and (eq  (mvar.mb mbind1) (mvar.mb mbind2))
         (iseq.mn (mnode.mb ,mbind1) (mnode.mb ,mbind2))))       
;;;
;;;   
;;; =============================================================================



    
    




