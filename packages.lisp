;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: packages.lisp,v 1.1 2011/05/24 17:59:35 mwk3 Exp $

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


(in-package :cl-user)


(defvar *sneps-packages*
  '(:logical-pathname
    :sneps
    :dequeue
    :multi
    :match
    :snip
    :snebr
    :snepslog
    :englex
    :parser
    :snepsul)
  "The list of packages used by the SNePS system")


;; Create all packages:
;;
(dolist (package *sneps-packages*)
  (unless (find-package package)
    (make-package package :use '(:cl))))

;; Shadow various symbols inherited from the LISP package:
;;
(shadow '(find describe + - * ^ = > assert load
          #+clisp !)
	(find-package 'sneps))

(shadow '(print delete)
	(find-package 'dequeue))

(shadow '(conjugate getf)
	(find-package 'englex))

(shadow '(* call getf push pop)
	(find-package 'parser))

;; Export some system utilities:
;;
(export '(sneps-translate sneps-load
          sneps-probe-file simple-system-created-p)
        (find-package 'cl-user))



    
    




