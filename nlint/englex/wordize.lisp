;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ENGLEX; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: wordize.lisp,v 1.2 2013/08/28 19:07:24 shapiro Exp $

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




(in-package :englex)


(defun wordize (numbr lexeme)
  "Returns the singular or plural form of the 2nd arg according to the
   feature given as the 1st arg."
  (let* ((lex (first-atom lexeme))
	 (lexeme (typecase lex
		   (string lex)
		   (symbol (symbol-name lex))
		   (sneps::node (symbol-name (sneps::node-na lex)))))) 
    (cond ((null numbr) (or (lookup-lexical-feature 'root lexeme) lexeme))
	  ((eq numbr 'sing) (or (lookup-lexical-feature 'root lexeme) lexeme))
	  ((lookup-lexical-feature 'plur lexeme))
	  (t (pluralize (or (lookup-lexical-feature 'root lexeme) lexeme))))))

(defun pluralize (wform)
  (let ((wlength (1- (length wform)))
	(vowls '(#\a #\e #\i #\o #\u)))
    (cond ((char= (schar wform wlength) #\f)
	   (concatenate 'string (subseq wform 0 wlength) "ves"))
	  ((char= (schar wform wlength) #\y)
	   (cond ((member (schar wform (1- wlength)) vowls :test #'char=)
		  (concatenate 'string wform "s"))
		 (t
		  (concatenate 'string (subseq wform 0 wlength) "ies"))))
	  ((string= (subseq wform (- wlength 2) (1+ wlength)) "sis")
	   (concatenate 'string (subseq wform 0 (1- wlength)) "es"))
	  ((or (char= (schar wform wlength) #\x)
	       (char= (schar wform wlength) #\s)
	       (char= (schar wform wlength) #\z)
	       (and (char= (schar wform wlength) #\o)
		    (not (member (schar wform (1- wlength)) vowls
				 :test #'char=)))
	       (and (char= (schar wform wlength) #\h)
		    (char= (schar wform (1- wlength)) #\c)))
	   (concatenate 'string wform "es"))
	  (t (concatenate 'string wform "s")))))



    
    




