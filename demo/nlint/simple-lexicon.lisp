;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: simple-lexicon.lisp,v 

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



("a" ((ctgy . det)))
("an" ((ctgy . det)))
("animal" ((ctgy . n)))
("are" ((ctgy . v)(root . "be")(num . (sing plur))(tense . pres))
       ((ctgy . aux)(root . "be")(num . (sing plur))(tense . pres)))
("ball" ((ctgy . n)))
("be" ((ctgy . v)(root . "be")(presp . "being"))
      ((ctgy . aux) (root . "be")(presp . "being")))
("big" ((ctgy . adj)))
("boy" ((ctgy . n) (gen . m)(num . sing)))
("circus" ((ctgy . multi-start)(multi-rest . ("elephant"))))
("circus elephant" ((ctgy . n)))
("is" ((ctgy . v)(root . "be")(num . sing)(tense . pres))
      ((ctgy . aux)(root . "be")(num . sing)(tense . pres)))
("block" ((ctgy . n))
         ((ctgy . v)))
("can" ((ctgy . aux)(modal . t)(num . (sing plur)))
       ((ctgy . n)))
("catch" ((ctgy . v)(past . "caught")))
("caught" ((ctgy . v)(root . catch)(tense . past)))
("costume" ((ctgy . n)))
("clothes" ((ctgy . n)))
("dish" ((ctgy . n)))
("do" ((ctgy . v))
      ((ctgy . aux)))
("did" ((ctgy . aux)(root . "do")(tense . past))
       ((ctgy . v)(root . "do")(tense . past)))
("elephant" ((ctgy . n)))
("fish" ((ctgy . n)(num . (sing plur))))
("frog" ((ctgy . n)))
("gentleman" ((ctgy . n)))
("green" ((ctgy . adj)))
("he" ((ctgy . pro)))
("head" ((ctgy . n)))
("help" ((ctgy . n))
        ((ctgy . v)))
("I" ((ctgy . pro)))
("Jack" ((ctgy . npr)))
("love" ((ctgy . v)(stative . t)))
("move" ((ctgy . v)))
("performer" ((ctgy . n)))
("play" ((ctgy . v)(root . "play"))
        ((ctgy . n)(num . sing)))
("she" ((ctgy . pro)))
("story" ((ctgy . n)))
("take" ((ctgy . v)))
("tell" ((ctgy . v)(past . "told")))
("the" ((ctgy . det)))
("told" ((ctgy . v)(root . "tell")(tense . past)))
("will" ((ctgy . aux)(modal . t)(num . (sing plur))))
("you" ((ctgy . pro)))
("." ((ctgy . final-punc)))
