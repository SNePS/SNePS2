;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSUL; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: blocks.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :snepsul)


(defun use-graphics-p ()
  (and (find-package 'opal)
       (fboundp 'bw-initialize)))

(defun initialize-blocksworld ()
  (if (use-graphics-p)
      (bw-initialize)))


;;; Definitions of primitive blocks world actions:
;;; ==============================================

;;; The definitions below assume that object nodes of blocksworld
;;; actions are represented with `lex' case frames.  For example,
;;; the act "Pick up C" needs to be represented by the following node:
;;;
;;;    (build action (build lex "pickup")
;;;           object1 (build lex "C"))
;;;
;;; This representational assumption is also reflected in the call
;;; to `declare-primitive' in the demo files.

(define-primaction pickup (object1)
  (let ((block (first (ns-to-lisp-list #!((find lex- ~object1))))))
    (format t "~&Now doing: Pickup ~A.~%" block)
    (if (use-graphics-p)
        ;; The name of a SNePS node representing a block
        ;; is bound to its associated Garnet object:
        (bw-pickup (eval block)))))

(define-primaction put (object1 object2)
  (let ((block (first (ns-to-lisp-list #!((find lex- ~object1)))))
        (support (or (first (ns-to-lisp-list #!((find lex- ~object2))))
                     'table)))
    (format t "~&Now doing: Put ~A on ~A.~%" block support)
    (if (use-graphics-p)
        (bw-putdown (eval block)
                    (if (string-equal support 'table)
                        table
                      (eval support))))))

(define-primaction lookat (object1)
  (let ((entity (first (ns-to-lisp-list #!((find lex- ~object1)))))
	(color 'red)
        (*with-snepsul-eval-function* #'with-snepsul-toplevel-style-eval))
    (format t "~&Now doing: Look at ~A~%" entity)
    (if (use-graphics-p)
        (setf color (bw-find-color (eval entity))))
    (let ((sneps-current-context sneps:crntct))
      #!((dynamic-add rel (build lex "color")
		      arg1 ~object1
		      arg2 (build lex ~color)
		      :context ~sneps-current-context)))))



;; Simulated actions of an external agent:

(defsnepscom john-pickup ((block) (top))
  (let ((*with-snepsul-eval-function* #'with-snepsul-toplevel-style-eval))
    (if (use-graphics-p)
        (bw-john-pickup (eval block))
      (format t "~&~%*** John now picks up ~A~2%" block))
    #!((add agent (build lex "john")
            act (build action (build lex "pickup")
                       object1 (build lex ~block))))))

(defsnepscom john-putdown ((block support) (top))
  (let ((*with-snepsul-eval-function* #'with-snepsul-toplevel-style-eval))
    (if (use-graphics-p)
        (bw-john-putdown (eval block) (eval support))
      (format t "~&~%*** John now puts ~A on ~A.~2%" block support))
    #!((add agent (build lex "john")
            act (build action (build lex "put")
                       object1 (build lex ~block)
                       object2 (build lex ~support))))))



    
    




