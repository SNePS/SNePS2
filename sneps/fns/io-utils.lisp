;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: io-utils.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




;; altered for ACL 6 compatibility (FLJ)

(in-package :sneps)


;;; Contains various utilities to deal with user input
;;; such as various types of read functions and menu support

(defun read-single-character (&optional (stream *standard-input*)
					&key (case nil)
					     (package nil))
  "Reads a single character response from STREAM and returns it as a string
of length one. Depending on whether CASE is :lower, upper or NIL the case
of the result will be converted or left unaffected. If PACKAGE is supplied
the converted string will be interned into that package and the resulting
symbol will be returned. This function should be the point to sort out
differences in character reading across Lisps."
  (let ((input (read-line stream)))
    (setq input (cond ((cl:= (length input) 0)
		       (format nil "~%"))
		      (t (subseq input 0 1))))
    (setq input (build-namestring input))    
    (cond (package
	   (intern input package))	   
	  (t input))))


(defun read-word (&optional (stream *standard-input*))
  "Reads a single word on a line from STREAM and returns it as a string."
  (string-trim '(#\space #\tab) (read-line stream)))

(defun read-word-insist (choices &optional (iostream *terminal-io*))
  "Reads a word from IOSTREAM and compares it to a list of CHOICES. If it
is string-equal to a string representation of any of the choices that
choice will be returned (not the word). Elements of CHOICES are typically
symbols, strings or numbers. If the word does not match any of the choices
the user will be asked to choose again until a valid choice was made."
  (loop
   (let ((input (read-word iostream))
	 choice)
     (setq choice (position-if
		   #'(lambda (choice)
		       (string-equal input
				     (cond ((or (symbolp choice)
						(stringp choice))
					    choice)
					   (t (format nil "~a" choice)))))
		   choices))
     (when choice (return (nth choice choices)))
     (cond ((cl:<= (length choices) 6)
	    (format iostream "~&Please choose one of ~a: " choices))
	   (t (format iostream "~&No such choice, try again: "))))))

(defun read-filename (&optional (stream *standard-input*))
  "Reads a filename from STREAM."
  (string-trim '(#\space #\tab) (read-line stream)))


;;; ------------ Menu support -------------

(defstruct (menu-item (:type list))
  "A menu is a list of menu-items."
  (key nil)           ; the key (a string) to identify an item
  (label nil)         ; the label (a string) that describes the item
  (value nil)         ; a form to be evaluated whose value should be returned
                      ; special values :key and :label refer to those strings
                      ; of the item
  )


(defun menu-selection-insist (menu &key title
				   (menu-io *query-io*)
				   (indentation 0)
				   (quit-key nil))
  "Displays MENU (a list of menu-items) and an optional TITLE on the
stream MENU-IO and asks to choose one of them. Repeats until a valid
item got selected, or, if QUIT-KEY is non-NIL, until the user quits
the selection by entering a string equal to QUIT-KEY. All output will
be indented by INDENTATION spaces."
  (let (choice)
    (menu-display menu :title title :output menu-io :indentation indentation)
    (loop
     (setq choice (menu-choose menu
			       :menu-io menu-io
			       :indentation indentation
			       :quit-key quit-key))
     (when choice (return choice))
     ;; Otherwise let him/her try again:
     (format menu-io "~&~vaNo such item, try again: " indentation ""))))

(defun menu-display (menu &key title 
			  (output *query-io*)
			  (indentation 0))
  "Displays MENU (a list of menu-items) and an optional TITLE on the
OUTPUT stream. All output will be indented by INDENTATION spaces."
  (let* (;; Max width of a numeric key (a numeric key is the
	 ;; default if no key was supplied)
	 (max-numkey-width (1+ (floor (log (length menu) 10))))
	 (max-key-width
	  (apply #'max (mapcar #'(lambda (item)
				   (cond ((menu-item-key item)
					  (length
					   (string (menu-item-key item))))
					 (t max-numkey-width)))
			       menu)))
	 (index 0))
      ;; Display the menu
      (when title (format output "~&~va~a" indentation "" title))
      (dolist (item menu)
	(incf index)
	(format output "~&~va  ~v@a:  "
		indentation ""
		max-key-width (or (menu-item-key item) index))
	(indent-block (menu-item-label item)
		      0 (cl:+ indentation max-key-width 5)
		      output))))


 (defvar *menu-quit-item* "QUIT"
  "Menu selection result in case of a quit action.")

(defun menu-quit-p (selection)
  (eq selection *menu-quit-item*))
  
(defun menu-choose (menu &key (menu-io *query-io*)
			      (indentation 0)
			      (quit-key "q"))
  "Gives the users one chance to choose an item from MENU. The value of
that item will be returned, NIL otherwise. If the QUIT-KEY is non-NIL
 (the default) then the quit option will be announced in the choose
message, and if the user enters a string equal to QUIT-KEY then a
value for which menu-quit-p is true will be returned.  All io will be
done on MENU-IO and will be indented by INDENTATION spaces."
  (format
   menu-io "~&~vaYour choice~@[ (~a to quit)~]: " indentation "" quit-key)
  (let ((choice (read-word menu-io))
	chosen-item)
    (cond ((and quit-key (string-equal quit-key choice))
	   *menu-quit-item*)
	  (t (dotimes (i (length menu))
	       (let ((item (nth i menu)))
		 (when (or (string-equal (menu-item-key item) choice)
			   (string-equal (format nil "~a" (1+ i)) choice))
		   (setq chosen-item item)
		   (return))))
	     (when chosen-item
	       (case (menu-item-value chosen-item)
		 (:key (menu-item-key chosen-item))
		 ((NIL :label) (menu-item-label chosen-item))
		 (t (eval (menu-item-value chosen-item))))
	       )))))
	 
(defun indent-block (string &optional (first-indent 0)
			    (rest-indent 0)
			    (stream *terminal-io*))
  "Takes a STRING and prints it indented onto STREAM. The first line will
be indented with FIRST-INDENT spaces, all other lines will have an indentation
of REST-INDENT spaces."
  (let ((newline-pos -1)
	(indentation first-indent))
    (loop
     (format stream "~va~a"
	     indentation ""
	     ;; print the string from right after the last newline
	     ;; to right before the next newline (this is much faster
	     ;; than using MAP and printing character by character)
	     (subseq string (1+ newline-pos)
		     ;; bummer, clisp can't deal with :end being NIL:
		     (or (setq newline-pos (position #\newline string
						     :start (1+ newline-pos)))
                         (length string))))
     ;; If there was no more newline, return
     (unless newline-pos (return))
     ;; otherwise print a newline
     (format stream "~%")
     (setq indentation rest-indent)
     )))



    
    




