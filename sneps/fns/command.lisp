;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: command.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




(in-package :sneps)


;; DEFSNEPSCOM:
;; ============
;;
;; Author:  Hans Chalupsky
;; Written: 07/17/93
;;
;; A macro for defining SNePSUL commands. It replaces the explicit setting
;; of the various `=XXXcommand' properties, the explicit export/import
;; handling to make the command available in the SNEPSUL package, and the
;; hardwired crossreferencing in the SNePSUL command variables.
;;
;; Some motivation for writing/using `defsnepscom':
;;
;; - aesthetics:  It's a nice and clean command interface.
;; - ease of use: The body can be written just like a normal defun without
;;                having to worry about backquotes etc. The export/import
;;                handling to make the command available in the SNEPSUL
;;                package is done automatically.
;; - consistency: Automatically crossreferences the command in the
;;                various command variables.
;; - efficiency:  The body of a command will get compiled, hence, the
;;                macroexpansion does not cost very much, which saves
;;                a lot in Lisps that have slow/picky evaluators (the only
;;                reason to use macros for SNePSUL commands is to avoid
;;                argument evaluation which is not the best usage for a
;;                macro anyway).

(defmacro defsnepscom (command
		       (&optional arguments
				  (environments '(top))
				  eval-arguments)
		       &body body)
  "Defines a SNePSUL COMMAND that takes ARGUMENTS and is legal in ENVIRONMENTS.
COMMAND will automatically be exported and imported into the SNEPSUL package.
The BODY is written just as in a `defun', it can have a documentation string
and argument access is straightforward. If EVAL-ARGUMENTS is t then the
arguments will be evaluated before they get passed, otherwise they are passed
unevaluated just as in a macro. ENVIRONMENTS can either be `:all' to define
COMMAND as legal in all possible environments, it can be a symbol naming an
already existing command, in which case COMMAND will be legal in all
environments in which the supplied command is legal, or it can be a subset
of `(top rs bns tbns fns ns ons rearrange)' which will make it legal in the
specified environments (`ns' and `ons' are synonymous). Here is an example
of a variation on a find command:

  (defsnepscom my-find ((&rest snd) find)
    \"Does my interesting find stuff.\"
    (print snd)
    <more interesting find code>
    (values result crntct))

Note, that every command that returns a node set should return a context as
a second value which will be used to display the node set. Also, if your
command has the name of a standard Lisp function then you have to shadow that
symbol in the package in which you want to define the command before you
actually define it."
  `(progn
     ;; Define the command functionality:
     ,(if (or eval-arguments (null arguments))
	  ;; If possible define it as a function:
	  `(defun ,command ,arguments
	     ,@body)
	;; Otherwise, we have to make it a macro:
	`(defmacro ,command (&rest arguments)
	   ,@(if (stringp (car body)) (list (car body)))
	   (list 'apply
		 ;; make sure that the command body gets compiled
		 ;; (clisp needs the extra quote to quote the closure):
		 '#'(lambda ,arguments
		     ,@(if (stringp (car body))
			   (cdr body)
			 body))
		 (list 'quote arguments))))

     ;; Now define the command in the requested environments,
     (enable*.com ',command ',environments t)
     ;; and import it into the SNePSUL package:
     (export-import.com ',command)))

(defmacro undefsnepscoms (&rest commands)
  ;; Undefines a list of SNePSUL commands.
  `(dolist (command ',commands t)
     (when (is.com command)
       (enable*.com command :all nil))))

;; Initialize the SNePSUL command variables:
(dolist (command-variable
	    '(commands topcommands rscommands bnscommands
	      tbnscommands fnscommands nscommands rearrangecommands))
  (newsys.sv command-variable)
  ;; This makes sure the variables won't get cleared by `resetnet'. Should
  ;; they also be part of `variables' but permanent so they won't get erased?
  (set.sv 'variables (remove command-variable  (value.sv 'variables))))



    
    




