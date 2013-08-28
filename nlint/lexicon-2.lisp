;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;;; Copyright (C) 1984--2013
;;; Research Foundation of State University of New York

;;; Version: $Id: lexicon-2.lisp,v 

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


("a" ((ctgy . det)))
("an" ((ctgy . det)))
("ant" ((ctgy . n)))
("animal" ((ctgy . n)))
("bat" ((ctgy . n)))
("be" ((ctgy . v)))
("believe" ((ctgy . v)(prop-att . t)))
("Bill" ((ctgy . npr)(gen . m)))
("car" ((ctgy . n)))
("Carol" ((ctgy . npr)(gen . f)))
("cat" ((ctgy . n)))
("dog" ((ctgy . n)))
("elk" ((ctgy . n)))
("fox" ((ctgy . n)))
("girl" ((ctgy . n)))
("gnu" ((ctgy . n)))
("Guatemalan" ((ctgy . adj)))
("he" ((ctgy . pron)))
("Hector" ((ctgy . npr)(gen . m)))
("hen" ((ctgy . n)))
("is" ((ctgy . v)(root . "be")(num . sing)(tense . pres)))
("it" ((ctgy . pron)))
("John" ((ctgy . npr)(gen . m)))
("Lucy" ((ctgy . npr)(gen . f)))
("man" ((ctgy . n)(plur . "men")))
("Mary" ((ctgy .  npr)(gen . f)))
("men" ((ctgy . n)(root . "man")(num . plur)))
("mortal" ((ctgy . adj)))
("old" ((ctgy . adj)))
("owl" ((ctgy . n)))
("pear" ((ctgy . n)))
("pet" ((ctgy . n)))
("people" ((ctgy . n)(root . "person")(num . plur)))
("person" ((ctgy . n)(plur . "people")))
("philosophical" ((ctgy . adj)))
("pick" ((ctgy . v)))
("poor" ((ctgy . adj)))
("rat" ((ctgy . n)))
("rich" ((ctgy . adj)))
("saw" ((ctgy . n)(root . "saw1"))
       ((ctgy . v)(root . "see")(tense . past)))
("saw1" ((ctgy . n)(root . "saw")))
("see" ((ctgy . v)(past . "saw")(pastp . "seen")))
("seen" ((ctgy . v)(root . "see")(tense . pastp)(pprt . t)))
("she" ((ctgy . pron)))
("smart" ((ctgy . adj)))
("Socrates" ((ctgy . npr)(gen . m)))
("sow" ((ctgy . n)))
("Stu" ((ctgy . npr)(gen . m)))
("sweet" ((ctgy . adj)))
("tall" ((ctgy . adj)))
("that" ((ctgy . conj)))
("the" ((ctgy . det)))
("the-editor-of-Byte" ((ctgy . npr)(gen m f)))
("this" ((ctgy . demon)))
("what" ((ctgy . pron)))
("was" ((ctgy . v)(root . "be")(num . sing)(tense . past)))
("who" ((ctgy . pron)))
("wise" ((ctgy . adj)))
("yak" ((ctgy . n)))
("young" ((ctgy . adj)))

