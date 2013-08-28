;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: qa-lexicon.lisp,v 

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


("a" ((ctgy . art)(definite . nil)))
("the" ((ctgy . art)(definite . t)))

("Computer Science" ((ctgy . npr)))
("John" ((ctgy . npr)))
("Mary" ((ctgy . npr)))

("computer" ((ctgy . n)))
("Computer" ((ctgy . multi-start) (multi-rest . ("Science")))
	    ((ctgy . n)(root . "computer")))
("dog" ((ctgy . n)))
("man" ((ctgy . n)(plur . "men")))
("men" ((ctgy . n)(root . "man")(num . plur)))
("woman" ((ctgy .  n)(plur . "women")))
("women" ((ctgy . n)(root . "woman")(num . plur)))

("saw" ((ctgy . n))
       ((ctgy . v)(root . "see")(tense . past)))

("believe" ((ctgy . v)(stative . t)))
("bit" ((ctgy . v)(root . "bite")(tense . past)))
("bite" ((ctgy . v)(num . plur)(past . "bit")))
("like" ((ctgy . v)(num . plur)))
("see" ((ctgy . v)(past . "saw")))
("sleep" ((ctgy . v)(past . "slept")))
("slept" ((ctgy . v)(root . "sleep")(tense . past)))	 
("study" ((ctgy . v)))
("use" ((ctgy . v)))

("who" ((ctgy . wh)))
("what" ((ctgy . wh)))

#|
A dog bit John.
The dog slept.
Mary believes that John likes the dog.
Mary studies Computer Science.
Mary used a computer.
John saw a saw.
What bit John?
Who sleeps?
Who studied?
Who uses the computer?
Who likes a dog?
Who sees a saw?
|#
