;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: read.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :sneps)


(defvar *original-readtable* (copy-readtable nil)
  "A global copy of the original Common Lisp readtable")

(defvar *sneps-readtable* (copy-readtable nil)
  "The readtable for SNePS.")


(defmacro snepsreadon ()
  "Switch to the sneps readtable"
  ;; written:  SCS 10/27/87
  (declare (special *readtable*))
  `(setq *readtable* *sneps-readtable*))

(defmacro snepsreadoff ()
  "Switch to the Common Lisp readtable"
  ;; written:  SCS 10/27/87
  (declare (special *readtable*))
  `(setq *readtable* *original-readtable*))


; sneps-read 
; ----------
;
;      arguments     : none
;
;      returns       : <snepsul-exp>
;
;      description   : Read in the next snepsul expression from inunit.
;                      In order to read properly this function binds
;                      *readtable* to *sneps-readtable*. The new demo-tool
;                      package made intext-read-fn etc. obsolete.
;
;                                         written:  CCC 08/03/83
;                                         modified: SCS 10/27/87
;                                         modified: SCS 12/23/88
;                                                    hc 12/19/91
;
(defun sneps-read ()
  (declare (special inunit))
  (let* ((*readtable* *sneps-readtable*))
     (read inunit nil :eof)))


; pseudolisp-read
; ---------------
;
;      arguments     : inunit
;      returns       : <s-exp>
;      description   : Read an s-expression from inunit (just like a normal
;                      read). Needed this for proper demo-control of a
;                      pseudolisp loop.
;                                         written: hc 11/20/91
(defun pseudolisp-read (&optional (inunit *standard-input*))
  (read inunit nil :eof))


;  star-reader
;  -----------
;
;      arguments     : <stream>
;                      <char> - macro character "*"
;
;      returns       : <list>
;
;      description   :  This function was written to seperate the call
;                      of the macro character "*" from its use in the
;                      returned list.  When this function was a part
;                      of the following macro-char definitions, it would
;                      try to evaluate the '* as an instance of the macro
;                      character.
;
;                                         written:   ssc 07/29/87
;                                         modified:
;

(defun star-reader (stream char)
  (declare (ignore char))
  `(* ',(read stream t nil t)))

;; Also made the other reader macro functions into named functions so that
;; they can be used by other packages such as the parser (hc 07/23/93):

(defun dollar-reader (stream char)
  (declare (ignore char))
  `($ ',(read stream t nil t)))

(defun percent-reader (stream char)
  (declare (ignore char))
  `(% ',(read stream t nil t)))

(defun question-reader (stream char)
  (declare (ignore char))
  `(? ,(read stream t nil t)))

(defun bang-reader (stream char)
  (declare (ignore stream char))
  '!)

(defun equal-reader (stream char)
  (declare (ignore stream char))
  '=)

;; Now comes the problematic case: For historic reasons the # character is
;; used as a special reader macro to create base nodes. In Common-Lisp
;; this is not a very good idea, because # is the default dispatch macro
;; character which is used for lots of useful things (the with-snepsul #!
;; syntax being just one of them). The function below tries to combine both
;; worlds by trying to figure out whether a standard #-syntax or a base node
;; syntax was intended. Here are some examples that explain the heuristics
;; used:
;;
;; #abc => (|#| 'ABC) ....standard case, first char is alphabetic
;; #a!? => (|#| 'A!?) ....variation on the standard case
;; #@abc@ => (|#| '@ABC@) ....@ does not have a dispatch macro definition
;; #123 => (|#| '123) ....number not followed by dispatch character
;; #123abc => (|#| '123ABC) ....number followed by alphabetic char
;; #123@abc@ => (|#| '123@ABC@) ....number followed by char without dispatch fn
;; #1!((*nodes)) => #! expansion
;; #3(a b c) => #(A B C) ....vector reader syntax
;; #'(lambda () ...) => function object
;; # 1! => (|#| '1!) ....anything goes after whitespace
;;
;; The few # syntaxes that use an alphabetic character are lost because in
;; that case it is assumed that the base node syntax was intended.
;;
;;     hc 07/23/93
;;
(defun hash-reader (stream char)
  (declare (ignore char))
  (let ((white-space '(#\space #\tab #\newline))
	(eof-char (code-char 4))
	peeked-char numeric-argument)
    ;; Some dispatch reader macros can have numeric arguments,
    ;; hence, we first try to collect such an optional argument:
    (loop
      (setq peeked-char (read-char stream nil eof-char t))
      (cond ((digit-char-p peeked-char 10)
	     (setq numeric-argument
	       (cl:+ (cl:* 10 (or numeric-argument 0))
		       (cl:- (char-code peeked-char) (char-code #\0)))))
	    (t (return))))
    ;; Unread the last character read:
    (unread-char peeked-char stream)
    (cond (;; If we have a numeric argument immediately followed by a number
	   ;; terminator then we just return the number (which is not legal
	   ;; syntax for a SNePSUL variable, but that case should be caught
	   ;; by the |#| function and not here):
	   (and numeric-argument
		(or (char-equal peeked-char eof-char)
		    (cl:find peeked-char white-space)
		    ;; Check for a list of special characters that might
		    ;; terminate a number. Note that "(" is missing because
		    ;; #n( ... ) is legal syntax for reading vectors:
		    (cl:find peeked-char "):;'`,")))
	   `(\# ',numeric-argument))
	  ;; In case of EOF signal an error:
	  ((char-equal peeked-char eof-char)
	   (read-char stream t nil t))
	  ;; If the character following the # (and its optional numeric
	  ;; argument) is a letter or whitespace, or neither but also does
	  ;; not have a valid dispatch macro read function (e.g., #@abc),
	  ;; then we assume a SNePSUL variable symbol was intended (we miss
	  ;; a few rare #<alphachar> syntaxes which should never be needed
	  ;; in a SNePS readtable environment):
	  ((or (alphanumericp peeked-char)
	       (member peeked-char white-space)
	       ;; Check for undefined dispatch macro character (in my reading
	       ;; of CLtL-II `get-dispatch-macro-character' should return NIL
	       ;; in that case, however, some Lisps such as CMUCL or TICL
	       ;; return an error function/object, hence, I declare `@' the
	       ;; prototypical undefined character and compare the current
	       ;; character with the entry of `@'):
	       (let (;; Since # is a not a dispatch reader macro char in
		     ;; `*sneps-readtable*' we use the original readtable
		     ;; to get its dispatch definitions:
		     (*readtable* *original-readtable*))
		 (eq (get-dispatch-macro-character #\# peeked-char)
		     (get-dispatch-macro-character #\# #\@))))
	   (let (;; make sure ?! etc. can be part of the symbol name:
		 (*readtable* *original-readtable*))
	     `(\# ',(if numeric-argument
			;; if we had a numeric argument, read the rest,
			;; prepend the number and read the result again:
			(read-from-string
			 (format nil "~a~s"
				 numeric-argument (read stream t nil t)))
		      (read stream t nil t)))))
	  ;; If we are here we know that we have a non-alphabetic character
	  ;; that has a defined dispatch macro read function, hence, use
	  ;; it (e.g., #'(...) ):
	  (t (let ((*readtable* *original-readtable*))
	       (funcall (get-dispatch-macro-character #\# peeked-char)
			stream
			(read-char stream t nil t)
			numeric-argument))))))

; set-sneps-read 
; --------------
;
;      arguments     : none
;
;      returns       : <never-mind>
;
;      nonlocal-vars : readtable
;
;      description   :   This function changes the syntax of several 
;                      characters which have special meanings in sneps.
;                        Those characters include * $ % # as first only
;                      macros; when a user types in <char>atom, the 
;                      read function will change it to (<char> 'atom).
;                        Those characters also include & + - = _ < > as 
;                      single infix macros, so they can be both prefix and 
;                      infix operators. For example, input x <char> y will 
;                      become (<char> x y), but input (<char> x y) will 
;                      remain the same.
;                        Another character with special meaning is !. It is
;                      a single postfix macro, so that it can be both prefix
;                      and postfix. For  example, x! will become (! x),
;                      but (! x) will remain the same.
;                        The character ? is like a first always macro, 
;                      except when the user inputs <char>atom, the read 
;                      function will change it to (<char> atom).  
;                        Except for  the character ?, all characters 
;                      mentioned above are sneps functions.  The ? works 
;                      like an indicator to sneps, so the atom inside will 
;                      not be evaluated and sneps treats the whole thing 
;                      as a variable.
;
;      side-effects  : Changes characters' syntax in readtable.
;
;                                         written:  CCC 08/03/83
;                                         modified: ejm 06/01/84
;
(defun set-sneps-read ()
  (set-macro-character #\* #'star-reader)
  (set-macro-character #\$ #'dollar-reader)
  (set-macro-character #\% #'percent-reader)
  (set-macro-character #\# #'hash-reader)
  (set-macro-character #\? #'question-reader)
  (set-macro-character #\! #'bang-reader)
  (set-macro-character #\= #'equal-reader))

(eval-when (:load-toplevel :execute)
  (let ((*readtable* *sneps-readtable*))
    (set-sneps-read)))


;;; The following used to be macro characters, but it seems that they will always
;;;      be surrounded by lists, so it's not necessary to make them macros.
;
;    (set-macro-character #\+ 'plusop)         ; set union
;    (set-macro-character #\& 'ampersandop)    ; set intersection
;    (set-macro-character #\- 'minusop)        ; set difference
;    (set-macro-character #\_ 'underlineop)    ; relation exception
;    (set-macro-character #\< 'lessthanop)     ; ??
;    (set-macro-character #\> 'greaterthanop)  ; assign relation set to snepsvar
;
;
;==========================================================================
;
; sneps-recover 
; -------------
;
;      arguments     : none
;
;      returns       : <not-important>
;
;      description   : This function is the sneps recover handler.  After 
;                      users exit from the error handler, this function 
;                      will be executed to make sure the user gets back 
;                      to the proper sneps state.
;
;      side-effects  : Resets the sneps input port to the terminal.
;                      Redefines the sneps build and sneps-read-fn 
;                      functions to their original forms.
;
;                                         written:  CCC 08/02/83
;                                         modified: scs 02/26/87
;                                         modified: scs 02/5/88
;                                         modified: scs 03/29/88
;                                                    hc 07/07/93
(defun sneps-recover ()
  (declare (special inunit))
  (unless (eq inunit *standard-input*) (close inunit))
  (setq inunit *standard-input*))

 
; ==========================================================================
;
; rearrange-command
; -----------------
;
;       arguments     : command -  <rearrange-command>
;
;       returns       : <snepsul-form>
;
;       nonlocal-vars : lastvalue
;
;       description   : If the command is "!", then return (! next-read),
;                      otherwise, return (command lastvalue next-read).
;                      Such that next-read is the next value return by
;                      function sneps-read.
;
;       implementation: 
;                                          written  : CCC 11/10/83
;                                          modified :
;
(defun rearrange-command (command)
    (declare (special command))
    (cond ((eq command '!)
           (list '! (value.sv 'lastvalue)))
          (t (list command (value.sv 'lastvalue) (sneps-read)))))



    
    




