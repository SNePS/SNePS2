;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PARSER; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: exports.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :parser)


;;;   Export the following symbols from the PARSER package.

(export 's)                   ; "S" is the start state of the parser.

(export '(*config* *trace-level* *parse-trees* *all-parses*
	  *terminating-punctuation-flag*
          *punct-chars* *white-space* *quote-chars* *escape-chars*
	  break-arc unbreak-arc
	  *input-redirect-stream* *lexentry* *atn-arcs-hashtable*
	  setr getr sendr liftr current-configuration packageless-equal
	  buildq lexin parse atnin talk getarcs putarc  geta addr
	  overlap disjoint nullr endofsentence addl flatten parser-print
	  atnreadln convertline list-to-type nl-tell
	  hold to jump cat vir wrd rcall call tst group push pop lex I understand
	  that))

(import '( *trace-level* putarc getarcs overlap disjoint
	  endofsentence buildq pconcat
	  geta addr addl setr sendr liftr getr nullr
	  hold to jump cat vir wrd rcall tst group
	  definenet parse lex I understand that
 	  break-arc unbreak-arc current-configuration
	  *all-parses* *parse-trees* *terminating-punctuation-flag*
          *punct-chars* *white-space* *quote-chars* *escape-chars*
	  packageless-equal)
	(find-package 'snepsul))

;;; 23 Feb 2011 ABCL-specific code to fix bug:
;;; When a SNePS command was later defined for atnin via 
;;; defsnepscom, ABCL 0.24.0 was crashing when defsnepscom called
;;; shadowing-import. Importing atnin initially with shadowing-import 
;;; solves the problem. - JPB
#+abcl (shadowing-import 'atnin (find-package 'snepsul))
#-abcl (import 'atnin (find-package 'snepsul))

(shadowing-import '(call getf push pop) (find-package 'snepsul))



    
    




