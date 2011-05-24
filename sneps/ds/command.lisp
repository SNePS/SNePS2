;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: command.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


; =============================================================================
; Data Type:  <command> ::= <top command>   |    ( top command )
;                           <rs function>   |    ( relation set function )
;                           <bns function>  |    ( build node set function )
;                           <tbns function> |    ( tbuild node set function )
;                           <fns function>  |    ( find node set function )
;                           <ons function>       ( other node set function )
; =============================================================================
;
;    A SNePSUL <command> is classified according to its ROLE either as a 
; <procedure> or as a <function>. 
;    A <procedure> is a <command> that performs some action but returns 
; nothing using the Common Lisp (values) function.
;    A <function> is a <command> that returns always something, possibly
; after having performed some action as a side effect. A <function> is
; implemented directly as a <LISP function> since both show the same kind 
; of behavior.
;
;    A <command> is also classified according to the ENVIRONMENT(S) in which 
; it is legal to appear. So, when defining a new <command>, it is necessary 
; to define the environment(s) in which it can appear, in addition to its
; role. This is done by adding some properties with the value <true> 
; to the property list of the <atom> representing the <command>. 
; Each property defines a particular environment where the function can
; appear. 
;
;    In addition, when defining a new <command> ( either a <procedure> 
; or a <function> ) the property "=command" should also be defined.
; Following the adopted criteria for properties representing SNePS-2
; attributes, each property representing a legal environment has a name
; starting with the character "=" followed by a meaningful name.
;
;    A <procedure> can be entered only at the top level of SNePS.
; So, when defining a new <procedure>, the property "=topcommand" should be 
; defined in addition to the property "=command". No other properties should 
; be defined because a <procedure> cannot appear in any other environment.
;
; Unlike <procedure>s, <function>s may appear in many different environments.
; There are <function>s that may appear in all environments but some of them
; may only appear in some specific one(s).
; When defining a new <function> it is necessary to define ALL the 
; environments in which it can appear, i.e., to add all the associated
; properties (indicated inside parhenthesis) besides "=command". 
; There are six environments where a function can be defined: 
;   1. the top level of SNePS  ("=topcommand").
;   2. a <relation set> position embedded in a <command> ("=rsfunction");
;   3. a <node set> position embedded in "build"   ("=bnsfunction");
;   4. a <node set> position embedded in "tbuild"  ("=tbnsfunction");
;   5. a <node set> position embedded in "find" or "findassert"
;      ("=fnsfunction");
;   6. a <node set> position embedded in any of the other <command>s  
;      ("=onsfunction").
;
;
;    Finally, a <command> can be classified according to the relation between
; its position and the position of its <argument>s in the input line. 
;
;    Most <command>s have an arbitrary number of <argument>s. They are called
; <prefix command>s because they can only be entered using the Cambridge 
; prefix notation: 
;
;            (<prefix command> <argument> ... <argument>) .
;
;    Some <command>s which have always two <argument>s can in addition be 
; entered in an infix position and so they are called <infix command>s.
;    When a <infix command> is used in a infix position, SNePS-2 
; rearranges the input line to transform the <form> into a <prefix form>.
; Precedence is always from left to right. An <infix command> can be used as 
;
;            (<infix command> <argument> <argument>)
;
; or as  
;
;            <argument> <infix command> <argument> 
;
; with no parenthesis.
;    Since SNePS-2 remembers always the last result an 
; <infix command> can also be used as
;
;            <infix command> <argument>
;
; in which case SNePS-2 recalls the result of the last result
; and makes it the first argument for the <infix command> before
; rearranging the <form> to the prefix notation.
;
;    Similarly, some <command>s which have always one <argument>, besides 
; being entered in a prefix position, can in addition be entered in a 
; postfix position and so they are called <postfix command>s.
; A <postfix command> can be used as 
;
;            (<postfix command> <argument>) 
;
; or as
;
;            <argument> <postfix command> 
;
; with no parenthesis, or just as
;
;            <postfix command>
;
; in which case the <argument> is the last result.
;
;    There are still other kind of one-argument <command>s: those whose
; <argument> can be nothing but a <svar>. They are called <macro command>s
; since they are read macros. They have one-character names and they are
; used as
;
;             <macro command><svar>
;
; with no parenthesis and preferably with no space in between the <command> 
; and the <svar>. Before passing it to the evaluator, the SNePS-2 reader 
; expands this form to one compatible with the <prefix command>'s <form>s.
;
;                                                        ejm 06/07/84
;                                                        scs 10/15/87
;
; =============================================================================
;
; is.com
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a SNePSUL <command>;
;                       "false" otherwise.
;                       
;
;                                        written :  ejm 11/17/83
;                                        modified:
;
(defmacro is.com (u)
   `(and (symbolp ,u) (get ,u '=command)))
;
;
; =============================================================================
;
; istop.com
; ---------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <top command>; 
;                       "false" otherwise.
;                       A <top command> is a <command> (either a <procedure> or
;                       a <function>) that can be legally entered at the
;                       top level of SNePS.
;
;                                        written :  ejm 11/17/83
;                                        modified:
;
(defmacro istop.com (u) 
   `(and (symbolp ,u) (get ,u '=topcommand)))
;
;
; =============================================================================
;
; isrs.com  
; --------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <rs function>,
;                       i.e., if it can legally appear in a <relation set>
;                       position embedded in a <command>; "false" otherwise.
; 
;                                        written : RMR 10/28/83
;                                        modified: ejm 11/17/83
;
(defmacro isrs.com (u) 
   `(and (symbolp  ,u) (get ,u '=rsfunction)))
;
;
; =============================================================================
;
; isbns.com
; ---------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "u" is a <bns function>,
;                       i.e., if it can legally appear in a <relation set>
;                       position embedded in a <command>; "false" otherwise.
;
;
;                                        written : RMR 11/01/83
;                                        modified: ejm 11/17/83
;
(defmacro isbns.com (u) 
   `(and (symbolp  ,u) (get ,u '=bnsfunction)))
;
;
; =============================================================================
;
; istbns.com 
; ----------
;
;       arguments     : u - <universal> 
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <tbns function>,
;                       i.e., if it can legally appear in a <node set>
;                       position embedded in a <command>; "false" otherwise.
;
;                                        written : RMR 11/01/83
;                                        modified: ejm 11/17/83
;
;
(defmacro istbns.com (u) 
   `(and (symbolp  ,u) (get ,u '=tbnsfunction)))
;
;
; =============================================================================
;
; isfns.com 
; ---------
;
;       arguments     : u - <universal>  
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "u" is a <fns function>,
;                       i.e., if it can legally appear in a <node set>
;                       position embedded in "find" or "findassert";
;                       "false" otherwise.
;
;                                        written : RMR 11/01/83
;                                        modified: ejm 11/17/83
;
(defmacro isfns.com (u) 
   `(and (symbolp  ,u) (get ,u '=fnsfunction)))
;
;
; =============================================================================
;
; isns.com
; --------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <ons function>,
;                       i.e., if it can legally appear in a <node set>
;                       position embedded in a <command> except "build"
;                       "tbuild", "find", and "findassert";
;                       "false" otherwise.
;
;                                        written :  ejm 11/17/83
;                                        modified:
;
(defmacro isns.com (u) 
   `(and (symbolp ,u) (get ,u '=onsfunction)))
;
;
; =============================================================================
;
; isnotprefix.com
; ---------------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is an <infix command> or
;                       a <postfix command>, i.e., a <command> that needs
;                       rearrangement of its parameters to make it a 
;                       <prefix command>; "false" otherwise.
;
;                                        written :  ejm 11/17/83
;                                        modified:
;
(defmacro isrearrange.com (u) 
   `(and (symbolp ,u) (get ,u '=rearrangecommand)))
;
;
; =============================================================================


;; Various command enabling functions necessary
;; for the support of `defsnepscom' (hc 07/17/93):
;; ===============================================

;; An alist with entries of the form
;;    `(<environment> <property name> <command variable>)'
(defvar *command-type-alist*
    '(("COM" =command commands)
      ("TOP" =topcommand topcommands)
      ("RS" =rsfunction rscommands)
      ("BNS" =bnsfunction bnscommands)
      ("TBNS" =tbnsfunction tbnscommands)
      ("FNS" =fnsfunction fnscommands)
      ("NS" =onsfunction nscommands)
      ("ONS" =onsfunction nscommands)
      ("REARRANGE" =rearrangecommand rearrangecommands)))

(defun addsvar.com (command svar)
  ;; Adds COMMAND to the list stored in the SNePSUL variable SVAR.
  (set.sv svar (adjoin command (value.sv svar))))

(defun delsvar.com (command svar)
  ;; Deletes COMMAND from the list stored in the SNePSUL variable SVAR.
  (set.sv svar (remove command (value.sv svar))))

(defun enable.com (command environment &optional (enable? t))
  ;; Enables COMMAND in ENVIRONMENT if ENABLE? is t, disables it otherwise.
  ;; ENVIRONMENT has to be `string-equal' to one of the environments defined
  ;; in `*command-type-alist*'. Enabled commands will be added to their
  ;; respective command variable, disabled commands will be removed from it.
  (let* ((command-type-entry
	  (assoc (string environment) *command-type-alist*
		 :test #'string-equal))
	 (command-property (second command-type-entry))
	 (command-variable (third  command-type-entry)))
    (if command-type-entry
	(cond (enable?
	       (setf (get command command-property) t)
	       (addsvar.com command command-variable))
	      (t
	       (remprop command command-property)
	       (delsvar.com command command-variable)))
      (error "enable.com: Illegal environment: `~a'" environment))))

(defun copyenv.com (new-command old-command)
  ;; Defines NEW-COMMAND to be of the exact same kind as OLD-COMMAND.
  (if (not (is.com old-command))
      (error "copy.com: `~s' is not a SNePS command" old-command))
  (dolist (command-type-entry *command-type-alist*)
    (if (get old-command (second command-type-entry))
	(enable.com new-command (first command-type-entry) t)
      (enable.com new-command (first command-type-entry) nil))))

(defun enable*.com (command &optional (environments '(top)) (enable? t))
  ;; ENABLE?'s COMMAND in the specified ENVIRONMENTS.
  ;; ENVIRONMENTS can either be `:all' which will affect all possible
  ;; environments, it can be a symbol naming an already existing command,
  ;; in which case COMMAND will be legal in exactly the environments in
  ;; which the supplied command is legal, or it can be a subset of
  ;; `(com top rs bns tbns fns ns ons rearrange)' (`ns' and `ons' are
  ;; synonymous, and `com' will always be added automatically).
  (if (eq environments :all)
      (setq environments '(top rs bns tbns fns ns rearrange)))
  (cond ((and environments
	      (symbolp environments))
	 (copyenv.com command environments))
	(t (enable.com command 'com enable?)
	   (dolist (env environments)
	     (enable.com command env enable?)))))

(defun export-import.com (command)
  ;; Exports COMMAND from its home package, imports it into SNEPSUL,
  ;; and also exports it from SNEPSUL.
  (export command (symbol-package command))
  (shadowing-import command (find-package 'snepsul))
  (export command (find-package 'snepsul)))



    
    




