;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;;; Copyright (C) 1984--2013
;;; Research Foundation of State University of New York

;;; Version: $Id: define.lisp,v 

;;; This file is part of SNePS.

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


(define member
        class 
        before after
        name 
        named
        which
        adj 
        agent
        verb 
        object
        lex 
        stime
        etime)

(build stime now) = now

(^ (defun add-indef (phrase)
     (let ((string-phrase (cond ((listp phrase) (princ-to-string (car phrase)))
				(t (princ-to-string phrase)))))
       (cond ((member (char string-phrase 0) '("a" "e" "i" "o" "u") :test 'string-equal)
	      (append '(|an|) (listify phrase)))
	     (t (append '(\a) (listify phrase))))))
)

(^ (defun mylength (s) 
     (cond ((null s) 0)
           ((atom s) 1)
           (t (1+ (mylength (cdr s)))))))
