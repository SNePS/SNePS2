;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: elephants-lexicon.lisp,v 

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


("a" ((ctgy . det)(num . sing)))
("an" ((ctgy . det)(num . sing)))
("AI" ((ctgy . n)))
("Alex" ((ctgy . npr)))
("animal" ((ctgy . n)))
("are" ((ctgy . v)(root . "be")(num . (sing plur))(tense . pres))
       ((ctgy . v)(root . "be")(aux . t)(num . (sing plur))(tense . pres)))
("be" ((ctgy . v)(root . "be")(presp . "being"))
      ((ctgy . v)(root . "be")(aux . t)(presp . "being")))
("bird" ((ctgy . n)))
("block" ((ctgy . n))
         ((ctgy . v)))
("boy" ((ctgy . n) (gen . m)(num . sing)))
("circus" ((ctgy . n)))
("color" ((ctgy . n)))
("colorful" ((ctgy . adj)))
("course" ((ctgy . n)))
("can" ((ctgy . v)(modal . t)(aux . t)(num . (sing plur)))
       ((ctgy . n)))
("Clyde" ((ctgy . npr)))
("costume" ((ctgy . n)))
("clothes" ((ctgy . n)(root . "clothes")(num . plur)))
("clown" ((ctgy . n)))
("dance" ((ctgy . v)))
("dish" ((ctgy . n)))
("do" ((ctgy . v)(aux . t)))
("did" ((ctgy . v)(root . "do")(aux . t)(tense . past)))
("Dumbo" ((ctgy . npr)))
("elephant" ((ctgy . n)))
("fish" ((ctgy . n)(num . (sing plur))))
("flew" ((ctgy . v)(root . "fly")(tense . past)))
("fly" ((ctgy . v)))
("frog" ((ctgy . n)))
("genius" ((ctgy . n)))
("Gerry" ((ctgy . npr)))
("gray" ((ctgy . adj)))
("grey" ((ctgy . adj)(root . "gray")))
("had" ((ctgy . v)(root . "have")(tense . past)))
("has" ((ctgy . v)(root . "have")(num . sing)))
("have" ((ctgy . v)(num . (sing plur))))
("head" ((ctgy . n)))
("heart" ((ctgy . n)))
("insane" ((ctgy . adj)))
("instructor" ((ctgy . n)))
("is" ((ctgy . v)(root . "be")(aux . t)(num . sing)(tense . pres)))
("Jack" ((ctgy . npr)))
("leg" ((ctgy . n)))
("love" ((ctgy . v)(stative . t)))
("mouth" ((ctgy . n)))
("move" ((ctgy . v)))
("performer" ((ctgy . n)))
("play" ((ctgy . v)(root . "play"))
        ((ctgy . n)(num . sing)))
("sane" ((ctgy . adj)))
("story" ((ctgy . n)))
("take" ((ctgy . v)))
("trunk" ((ctgy . n)))
("tell" ((ctgy . v)(past . "told")))
("the" ((ctgy . det)(num . (sing plur))))
("told" ((ctgy . v)(root . "tell")(tense . past)))
("Tweety" ((ctgy . npr)))
("what" ((ctgy . wh)))
("who" ((ctgy . wh)))
("will" ((ctgy . v)(modal . t)(aux . t)(num . (sing plur))))
("." ((ctgy . final-punc)))
("?" ((ctgy . final-punc)))
