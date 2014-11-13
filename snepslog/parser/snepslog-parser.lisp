
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 2006--2013
;; Research Foundation of State University of New York

;; Version: $Id: snepslog-parser.lisp,v 1.8 2014/11/13 18:59:39 shapiro Exp $

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




;;; SNePSLOG syntax recognized by this parser:
;;;    Note: _(_ and  _)_ are the object language characters between the _ marks.
;;;
;;;   input            ::= wffNameCommand | snepslogCommand | wffCommand
;;;
;;;   wffNameCommand   ::= wffName terminalPunctuation
;;;   wffCommand       ::= wff terminalPunctuation ; wff must not be a single symbol
;;;
;;;   wff              ::= infixedTerm | entailment | prefixedTerm
;;;   infixedTerm      ::= prefixedTerm [( and | or | <=> ) prefixedTerm]+
;;;   entailment       ::= termSet (=> | v=> | &=> | <Lisp integer> =>) termSet
;;;   pTermSet         ::= termSet ; but taken to denote all terms that match
;;;   termSet          ::= prefixedTerm | { termSequence }
;;;   termSequence     ::= prefixedTerm [, prefixedTerm]*
;;;   prefixedTerm     ::= negatedTerm | andorTerm | allTerm | nexistsTerm |
;;;                        threshTerm | atomicTerm |
;;;                        (and | or | nand | nor | xor | iff) termset
;;;   negatedTerm      ::= ~ atomicTerm
;;;   andorTerm        ::= andor (i, j) termSet ; i, j integers, 0 <= i <= j
;;;   threshTerm       ::= thresh (i [, j]) termSet ; i, j integers, 0 <= i <= j
;;;   allTerm          ::= all _(_ symbolSequence _)_ _(_ wff _)_   ; but wff cannot be an atomic symbol
;;;   nexistsTerm      ::= nexists nexistsParameters
;;;                        _(_ symbolSequence _)_ 
;;;                        _(_ termSet : termSet _)_
;;;   nexistsParameters ::= _(_ <Lisp integer>, <Lisp integer>, <Lisp integer> _)_ |
;;;                         _(_ _, <Lisp integer>, _ _)_ |
;;;                         _(_ <Lisp integer>, _, <Lisp integer> _)_
;;;   atomicTerm       ::= wffName | qvar | SNePSLOGsymbol |
;;;                        withsome/allTerm | ; in mode 3 only
;;;                        (qvar | SNePSLOGsymbol) _(_ termSetSequence _)_ |
;;;                        _(_ wff _)_
;;;   withsome/allTerm ::= (withsome | withall) _(_ symbolSequence, termSet, termSet [, termSet]  _)_
;;;   termSetSequence  ::= termSet [, termSet]*
;;;   symbolSequence ::= SNePSLOGsymbol [, SNePSLOGsymbol]*
;;;   wffName          ::= wff <Lisp integer> ; Assuming that mi is a SNePS node
;;;   qvar             ::= ? SNePSLOGsymbol
;;;   SNePSLOGsymbol   ::= (wff <Lisp integer>) | <Lisp atom>
;;;
;;;   terminalPunctuation ::= . | ! | (? [_(_ i [j] _)_]) | ??
;;;
;;;
;;;   snepslogCommand ::= %-command |
;;;                       ^-command |
;;;                       a-command |
;;;                       b-command |
;;;                       c-command |
;;;                       d-command |
;;;                       e-command |
;;;                       l-command |
;;;                       n-command |
;;;                       p-command |
;;;                       r-command |
;;;                       s-command |
;;;                       t-command |
;;;                       u-command
;;;
;;;   %-command ::= % <SNePSUL command>
;;;   ^-command ::= ^^ | ^ <Lisp form>
;;;   a-command ::= activate wff [.] |
;;;                 activate! wff [terminalPunctuation] |
;;;                 add-to-context SNePSLOGsymbol termSet [.] |
;;;                 ask wff [terminalPunctuation] |
;;;                 askifnot wff [terminalPunctuation] |
;;;                 askwh wff [terminalPunctuation] |
;;;                 askwhnot wff [terminalPunctuation]
;;;   b-command ::= beliefs-about pTermSet [.]
;;;                 br-mode [auto|manual] |
;;;                 br-tie-mode [auto|manual]
;;;   c-command ::= clear-infer [.] |
;;;                 clearkb [.] |
;;;                 copyright [.]
;;;   d-command ::= define-frame SNePSLOGsymbol <Lisp list> 
;;;                              [Lisp string] [.]  |
;;;                 define-path SNePSRelation SNePSPath [.] |
;;;                 describe-context [SNePSLOGsymbol] [.] |
;;;                 describe-terms [pTermSet] [.] |
;;;                 demo [<file path>] [t | b | bv | a | av | n] [.]
;;;   e-command ::= expert [.]
;;;   l-command ::= lisp [.] |
;;;                 list-contexts [.] |
;;;                 list-terms [pTermSet] [.] |
;;;                 list-wffs [.] |
;;;                 list-asserted-wffs [SNePSLOGsymbol] [.] |
;;;                 load <file path> [.]
;;;   n-command ::= normal [.]
;;;   p-command ::= perform atomicTerm
;;;   r-command ::= remove-from-context SNePSLOGsymbol pTermSet
;;;   s-command ::= set-context SNePSLOGsymbol [pTermSet]
;;;             ::= set-default-context SNePSLOGsymbol 
;;;             ::= set-mode-1
;;;             ::= set-mode-2
;;;             ::= set-mode-3 [t | nil] |
;;;             ::= set-order <order-function-symbol>
;;;             ::= show [pTermSet] [.]
;;;   t-command ::= trace [SNePSLOGsymbol]* [.]
;;;      Specially recognized symbols: inference, acting, translation, parsing
;;;   u-command ::= undefine-path SNePSRelation [.]
;;;             ::= unlabeled [.]
;;;             ::= untrace [SNePSLOGsymbol]* [.]
;;;      Specially recognized symbols: inference, acting, translation, parsing

(in-package :snepslog)
(in-package :sneps)
(shadow 'common-lisp:load)
(export 'load)
(shadowing-import 'load :snepslog)
(in-package :snepslog)



;;; This eval-when should not have to be included in the final version.
;;; Instead the importation of sneps:demo should be deleted from
;;;    /projects/snwiz/Install/Sneps-2.6.1/snepslog/imports.lisp
;;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (unintern :demo)			; Will define snepslog::demo below
;;;)

(defparameter *WhiteSpace* '(#\Space #\Newline #\Tab #\Page #\Return)
  "Characters to be ignored")

(defparameter *TerminalPunctuation* '(#\) #\})
  "Characters that can terminate sequences.")

(defparameter *SeparatingPunctuation* '(#\( #\{ #\,)
  "Characters that can't be the first character of a symbol.")

(defparameter *Punctuation* (concatenate 'list
				*WhiteSpace* 
				*TerminalPunctuation*
				*SeparatingPunctuation*
				'(#\. #\! #\?)))

(defparameter *SNePSLOGAtomReadTable* (copy-readtable)
  "Readtable for reading SNePSLOG atoms")

;;; Make {, }, !, and ? act like ) and ( for reading SNePSLOG atoms.
(set-syntax-from-char #\{ #\( *SNePSLOGAtomReadTable* *readtable*)
(set-syntax-from-char #\} #\) *SNePSLOGAtomReadTable* *readtable*)
(set-syntax-from-char #\! #\) *SNePSLOGAtomReadTable* *readtable*)
(set-syntax-from-char #\? #\) *SNePSLOGAtomReadTable* *readtable*)


(defconstant *QMark* 'sneps:?
  "The sneps question mark.")

(defconstant *and* "and"
  "The and infix connective.")

(defconstant *or* "or"
  "The or infix connective.")

(defconstant *iff* "<=>"
  "The iff infix connective.")

(defparameter *Input* ""
  "The input string to be parsed.")

(defmacro parseError (&rest msg-args)
  "Throws a continuable error from the SNePSLOG parser.
   msg-args is a format string and its arguments,
      from which the error message is constructed."
  `(progn
       (cerror "Continue with SNePSLOG" ,@msg-args)
       (throw 'SNePSLOGParseError nil)))

(defun snepslogCommand ()
  (case (elt *Input* 0)
    (#\% (%-command))
    (#\^ (^-command))
    (#\a (a-command))
    (#\b (b-command))
    (#\c (c-command))
    (#\d (d-command))
    (#\e (e-command))
    (#\l (l-command))
    (#\n (n-command))
    (#\p (p-command))
    (#\r (r-command))
    (#\s (s-command))
    (#\t (t-command))
    (#\u (u-command))))

(defun a-command ()
  (declare (special *br-auto-mode* *br-tie-mode*))
  "Recognizes the commands beginning with the letter a."
  ;; a-command ::= ask-command
  ;;               activate wff [.] |
  ;;               activate! wff [terminalPunctuation] |
  ;;               add-to-context SNePSLOGsymbol termSet [.]
  (cond ((string-equal (subseq *Input* 0 3) "ask")
	 (ask-command))
	((haveCommand? "activate!" t)
	 `(snip:activate (sneps:assert ,@(rest (requiredWffArg "activate!")))))
	((haveCommand? "activate" t)
	 (let ((trm (wff)))
	   (cond (trm
		  (haveCommand? ".")
		  `(snip:activate ,trm))
		 (t (parseError
		     "The command activate requires a term as an argument at: ~S"
		     *Input*)))))
	((haveCommand? "add-to-context" t)
	 (let ((context-name (SNePSLOGsymbol)))
	       (if (and context-name (checkIsContextName context-name))
		   (let ((hypothesis-set (termSet)))
		     (haveCommand? ".")
		     (when (or (atom hypothesis-set)
			       (eq (first hypothesis-set) 'sneps:build))
		       (setf hypothesis-set (list hypothesis-set)))
		     `(sneps:+ ,@(mapcar
				  #'(lambda (cmd)
				      (if (atom cmd)
					  (sneps:assert.n 
					   (sneps:node cmd) context-name)
				      `(sneps:assert ,@(rest cmd)
					 :context ,context-name)))
				  hypothesis-set)))
		 (parseError
		  "The command add-to-context requires a context name as an argument at: ~S"
		  *Input*))))))

(defun ask-command ()
  "Recognizes one of the ask commands."
  ;; ask-command ::= ask wff [terminalPunctuation] |
  ;;                 askifnot wff [terminalPunctuation] |
  ;;                 askwh wff [terminalPunctuation] |
  ;;                 askwhnot wff [terminalPunctuation]
  (cond ((haveCommand? "askifnot" t)
	 (fixQvarsForDeduce
	  (cons 'snip:deducefalse (rest (requiredWffArg "askifnot")))))
	((haveCommand? "askwhnot" t)
	 (fixQvarsForDeduce
	  (cons 'snip:deducewhnot (rest (requiredWffArg "askwhnot")))))
	((haveCommand? "askwh" t)
	 (fixQvarsForDeduce
	  (cons 'snip:deducewh (rest (requiredWffArg "askwh")))))
	((haveCommand? "ask" t)
	 (fixQvarsForDeduce
	  (cons 'snip:deducetrue (rest (requiredWffArg "ask")))))))

(defun b-command ()
  "Recognizes the commands beginning with the letter b."
  ;; b-command ::= beliefs-about pTermSet [.] |
  ;;               br-mode [auto|manual] |
  ;;               br-preset [fluent|source] |
  ;;               br-tie-mode [auto|manual]
  (cond ((haveCommand? "beliefs-about" t)
    (let ((terms (pTermSet)))
      (if (atom terms)
	      (list 'sneps::beliefs-about terms)
	      (list 'sneps:+ (mapcar
			    #'(lambda (ns) (list 'sneps::beliefs-about ns))
			      terms)))))
  ((haveCommand? "br-mode" t)
    (cond ((emptyInputAfter? 0)
      (if *br-auto-mode*
        (returnForms '(format outunit 
                        "Belief revision mode currently set to 'auto'"))
      (returnForms '(format outunit
                      "Belief revision mode currently set to 'manual'"))))
    ((haveCommand? "auto")
      (returnForms `(br-mode ,t)
                   '(format outunit
                     "Belief revision mode will now be set to 'auto'.")))
    ((haveCommand? "manual")
      (returnForms `(br-mode ,nil)
                   '(format outunit 
                     "Belief revision mode will now be set to 'manual'.")))
    (t (parseError
      "The command br-mode can only accept 'auto' or 'manual' as an argument"))))
 ((haveCommand? "br-tie-mode" t)
    (cond ((emptyInputAfter? 0)
      (if *br-tie-mode*
        (returnForms '(format outunit 
                        "Belief revision tie mode currently set to 'auto'"))
      (returnForms '(format outunit
                      "Belief revision tie mode currently set to 'manual'"))))
    ((haveCommand? "auto")
      (returnForms `(br-tie-mode ,t)
                   '(format outunit
                      "Entrenchment ties will now be broken automatically")))
    ((haveCommand? "manual")
      (returnForms `(br-tie-mode nil)
                   '(format outunit 
                      "The user will be consulted when an entrenchment tie occurs.")))
    (t (parseError
      "The command br-tie-mode can only accept 'auto' or 'manual' as an argument"))))))

(defun d-command ()
  "Recognizes the commands beginning with the letter d."
  ;; d-command ::= define-frame SNePSLOGsymbol <Lisp list>
  ;;                  [descriptionString] [.] |
  ;;               define-path SNePSRelation SNePSPath [.] |
  ;;               describe-context [SNePSLOGsymbol] [.] |
  ;;               demo [<file path>] [t | b | bv | a | av | n] [.] |
  ;;               describe-terms [pTermSet] [.]
  (cond ((haveCommand? "describe-context" t)
	 (let ((context-name (SNePSLOGsymbol)))
	   (or (null context-name) (checkIsContextName context-name))
	   (haveCommand? ".")
	   `(describe-context ,context-name)))
	((haveCommand? "define-frame" t)
	 (define-frame-command))
	((haveCommand? "define-path" t)
	 (define-path-command))
	((haveCommand? "demo" t)
	 (let ((filename (path))
	       (pausecontrol (SNePSLOGsymbol)))
	   (when (and pausecontrol
		      (not (member pausecontrol '(t b bv a av n)
				   :test #'string-equal)))
	     (parseError "Unrecognized pause control, ~A, at: ~S"
			 pausecontrol *Input*))
	   (haveCommand? ".")
	   (returnForms
	    (cond (pausecontrol `(demo ,filename ',pausecontrol))
		  (filename `(demo ,filename))
		  (t `(demo))))))
	((haveCommand? "describe-terms" t) 
	 (returnForms `(describe-terms 
			,(or 
			  (let ((result (pTermSet)))
			    (if (listp result)
				result
			      (list 'sneps:+ result)))
			  `(^ (snepslog::closedTerms))))))))

(defun define-frame-command ()
  "Parses the arguments for the define-frame command."
  ;; define-frame-command ::= define-frame SNePSLOGsymbol <Lisp list>
  ;;                             [Lisp string] [.]
  (let ((pred (SNePSLOGsymbol)))
    (if pred
	(if (firstCharp #\()
	    (multiple-value-bind (caseframe n)
		(read-from-string *Input*)
	      (popInput n)
	      (if (firstCharp #\")
		  (multiple-value-bind (desc-str n1)
		      (read-from-string *Input*)
		    (popInput n1)
		    (haveCommand? ".")
		    (returnForms
			`(define-frame ',pred ',caseframe ',desc-str)))
		(progn (haveCommand? ".")
		       (returnForms
			`(define-frame ',pred ',caseframe)))))
	  (parseError
	   "The command define-frame requires a predicate and a list of relations at: ~S"
	   *Input*))
      (parseError
       "The command define-frame requires a predicate and a list of relations at: ~S"
       *Input*))))

(defun define-path-command ()
  "Parses the arguments for the define-path command."
  ;; define-path-command ::= define-path SNePSRelation SNePSPath [.]
  (when (emptyInput?)
    (parseError
     "The command define-path needs a relation and a path, but has neither."))
  (let ((rel (getForm)))
    (unless (sneps:is.r rel)
      (parseError
       "The command define-path was called on ~S, which is not a relation."
       rel))
    (when (emptyInput?)
      (parseError
       "The command define-path on relation ~S is missing a path." rel))
    (let ((path (getForm)))
      (unless path
	(parseError
	 "The command define-path on the relation ~S is missing a path at: ~S"
	 rel *Input*))
      (haveCommand? ".")
      `(define-path ,rel ,path))))

(defun path ()
  "Parses a file name."
  (setf *Input*
    (string-right-trim *WhiteSpace* *Input*))
  (let ((end (length *Input*)))
    (when (and (> end 0)
	       (char= (elt *Input* (1- end)) #\.))
      (decf end))
    (multiple-value-bind (filename n)
	(read-from-string *Input* nil nil :end end)
      (popInput n)
      (cond ((null filename) nil)
	    ((stringp filename) filename)
	    (t (format nil "~S" filename))))))

(defun l-command ()
  "Recognizes the commands beginning with the letter l."
  ;; l-command ::= lisp [.] |
  ;;               list-contexts [.] |
  ;;               list-terms [pTermSet] [.] |
  ;;               list-wffs [.] |
  ;;               list-asserted-wffs [SNePSLOGsymbol] [.] |
  ;;               load <file path> [.]
  (cond
   ((haveCommand? "lisp")
    (throw :snepslog-end nil))
   ((haveCommand? "list-contexts")
    (haveCommand? ".")
    (list-contexts)
    (returnForms nil))
   ((haveCommand? "list-wffs")
    (returnForms `(mapc ,#'(lambda (node)
			     (when (node-asupport node)
			       (snepslog-print node)))
			(value.sv 'sneps:nodes))))
   ((haveCommand? "list-terms" t)
    (or (pTermSet) `(^ (snepslog::closedTerms))))
   ((haveCommand? "list-asserted-wffs" t)
    (let ((context-name (or (SNePSLOGsymbol) (value.sv 'defaultct))))
      (checkIsContextName context-name)
      (haveCommand? ".")
      (returnForms
       `(mapc ,#'(lambda (node)
		   (let ((sneps:crntct context-name))
		     (declare (special sneps:crntct))
		     (when (and (node-asupport node)
				(sneps:isassert.n node))
		       (snepslog-print node))))
	      (value.sv 'sneps:nodes)))))
   ((haveCommand? "load" t)
    (let ((filename (path)))
      (unless filename
	(parseError
	 "The command load must have a file as an argument at: ~S"
	 *Input*))
      (haveCommand? ".")
      (returnForms
       `(sneps:load ,filename))))))

(defun pTermSet ()
  "Parses a set of terms meant to be used as patterns.
     I.e., the command with a pTermSet as an argument 
         will be performed on all terms that match the patterns."
  ;; pTermSet ::= termSet			; but taken to denote all terms that match
  (let ((haveSet (firstCharp #\{))
	(patterns (and (not (emptyInput?))(termSet))))
    (haveCommand? ".")
    (when patterns
      (cond (haveSet
	     (cons 'sneps:+
		   (mapcar #'(lambda (form)
			       (if (consp form)
				   (cons 'sneps:find (rest form))
				 form))
			   patterns)))
	    ((consp patterns)
	     (cons 'sneps:find (rest patterns)))
	    (t patterns)))))

(defun p-command ()
  "Recognizes the commands beginning with the letter p."
  ;; p-command ::= perform atomicTerm
  (when (haveCommand? "perform" t)
    (let ((act (atomicTerm)))
      (cond (act
	     (haveCommand? ".")
	     `(snip:perform ,act))
	    (t (parseError "The command perform needs an argument at: ~S"
			   *Input*))))))


(defun r-command ()
  "Recognizes the commands beginning with the letter r."
  ;; r-command ::= remove-from-context SNePSLOGsymbol pTermSet
  (when (haveCommand? "remove-from-context" t)
    (let ((context-name (SNePSLOGsymbol)))
      (if (and context-name (checkIsContextName context-name))
	  (let ((hypothesis-set (pTermSet)))
	    `(sneps:remove-from-context ,hypothesis-set ,context-name))
	(parseError
	 "The command remove-from-context requires a context name as an argument at: ~S"
	 *Input*)))))

(defun s-command ()
  "Recognizes the commands beginning with the letter s."
  ;; s-command ::= set-context SNePSLOGsymbol [pTermSet]
  ;;           ::= set-default-context SNePSLOGsymbol 
  ;;           ::= set-mode-1
  ;;           ::= set-mode-2
  ;;           ::= set-mode-3 [t | nil]
  ;;           ::= show [pTermSet] [.]
  (if (string-equal (subseq *Input* 0 4) "set-")
      (if (string-equal (subseq *Input* 4 9) "mode-")
	  (cond 
	   ((haveCommand? "set-mode-1")
	    (returnForms '(resetnet t)
			 '(make.snepslog1)))
	   ((haveCommand? "set-mode-2")
	    (returnForms '(resetnet t)
			 '(make.snepslog2)))
	   ((haveCommand? "set-mode-3" t)
	    (let ((arg (SNePSLOGsymbol)))
	      (haveCommand? ".")
	      (returnForms `(set-mode-3 ,arg)))))
	(cond ((haveCommand? "set-context" t)
	       (let ((context-name (SNePSLOGsymbol)))
		 (if context-name
		     (let ((hypothesis-set (pTermSet)))
		       (haveCommand? ".")
		       `(set-context ,hypothesis-set
				     ,context-name))
		   (parseError
		    "The command set-context requires a context name as an argument at: ~S"
		    *Input*))))
	      ((haveCommand? "set-default-context" t)
	       (let ((context-name (SNePSLOGsymbol)))
		 (cond (context-name
			(checkIsContextName context-name)
			(haveCommand? ".")
			`(set-default-context ,context-name))
		       (t (parseError
			   "The command set-default-context ~
                               requires a context name as an argument at: ~S"
			   *Input*
			   )))))
        ((haveCommand? "set-order" t)
          (let ((order-function-symbol (SNePSLOGsymbol)))
            (case order-function-symbol
              (null-order
               (format outunit
                       "All propositions will be equally epistemically entrenched."))
              (fluent
               (format outunit
                       "Fluents will be ~
                           less epistemically entrenched than non-fluents."))
              (source
               (format outunit
                       "IsBetterSource(s1,s2) will mean ~
                           that s1 is more credible than s2."))
              (explicit
               (format outunit
                       "IsLessEntrenched(p1,p2) will mean ~
                           that p1 is strictly less entrenched than p2."))
              (t (format outunit
                         "(~S p q) will mean that p is less or equally ~
                              epistemically entrenched as q."
                         order-function-symbol)))
            (returnForms `(set-order ,order-function-symbol))))))
    (when (haveCommand? "show" t)
      `(sneps:show ,(or (snepslog::pTermSet) (sneps:* 'sneps:nodes))))))

(defun t-command ()
  "Recognizes the commands beginning with the letter t."
  ;; t-command ::= trace [SNePSLOGsymbol]* [.]
  ;;   Specially recognized symbols: inference, acting, translation, parsing
  (when (haveCommand? "trace" t)
    (cond ((or (string= *Input* "")
	       (haveCommand? "."))
	   (returnForms '(trace topsneval)))
	  (t
	   (apply #'returnForms
		  (loop for f = (SNePSLOGsymbol)
		      while f
		      if (eq f  'inference)
		      collect '(setf snip:*infertrace* :surface)
		      and collect '(format outunit "Tracing inference.")
		      else if (eq f 'acting)
		      collect '(setf snip:*plantrace* :surface)
		      and collect '(format outunit "Tracing acting.")
		      else if (eq f 'translation)
		      collect '(trace topsneval)
		      and collect
			  '(format outunit "Tracing translation to SNePSUL.")
		      else if (eq f 'parsing)
		      collect '(TraceParsing)
		      and collect '(format outunit "Tracing SNePSLOG parsing.")
		      else if (fboundp f)
		      collect `(trace ,f)
		      and collect `(format outunit "Tracing ~S." ',f)
		      else 
		      collect `(format outunit "~S is not fbound." ',f)
		      finally (haveCommand? ".")))))))

(defun u-command ()
  "Recognizes the commands beginning with the letter u."
  ;;   u-command ::= undefine-path SNePSRelation [.]
  ;;             ::= unlabeled [.]
  ;;             ::= untrace [SNePSLOGsymbol]* [.]
  ;;   Specially recognized symbols: inference, acting, translation, parsing
  (cond ((haveCommand? "undefine-path" t)
	 (undefine-path-command))
	((haveCommand? "unlabeled") 
	 (returnForms '(setf complete-description 'unlabeled)))
	((haveCommand? "untrace" t)
	 (cond ((or (string= *Input* "")
		    (haveCommand? "."))
		(returnForms '(untrace topsneval)))
	       (t
		(apply #'returnForms
		       (loop for f = (SNePSLOGsymbol)
			   while f
			   if (eq f  'inference)
			   collect '(setf snip:*infertrace* nil)
			   and collect '(format outunit "Untracing inference.")
			   else if (eq f 'acting)
			   collect '(setf snip:*plantrace* nil)
			   and collect '(format outunit "Untracing acting.") 
			   else if (eq f 'translation)
			   collect '(untrace topsneval)
			   and collect
			       '(format outunit "Untracing translation to SNePSUL.")
			   else if (eq f 'parsing)
			   collect '(UntraceParsing)
			   and collect '(format outunit "Untracing SNePSLOG parsing.")
			   else if (fboundp f)
			   collect `(untrace ,f)
			   and collect `(format outunit "Untracing ~S." ',f)
			   else 
			   collect `(format outunit "~S is not fbound." ',f)
			   finally (haveCommand? "."))))))))

(defun undefine-path-command ()
  "Parses the arguments for the define-path command."
  ;; undefine-path-command ::= undefine-path SNePSRelation [.]
  (when (emptyInput?)
    (parseError
     "The command undefine-path needs a relation."))
  (let ((rel (getForm)))
    (unless (sneps:is.r rel)
      (parseError
       "The command undefine-path was called on ~S, which is not a relation."
       rel))
    (haveCommand? ".")
    `(undefine-path ,rel)))

(defun e-command ()
  "Recognizes the commands beginning with the letter e."
  ;; e-command ::= expert [.]X
  (when (haveCommand? "expert")
    (returnForms '(setf complete-description 'expert))))

(defun n-command ()
  "Recognizes the commands beginning with the letter n."
  ;; n-command ::= normal [.]
  (when (haveCommand? "normal")
    (returnForms '(setf complete-description 'normal))))

(defun c-command ()
  "Recognizes the commands beginning with the letter c."
  ;; c-command ::= clear-infer [.] |
  ;;               clearkb [.] |
  ;;               copyright [.]
  (cond ((haveCommand? "clear-infer")
	 '(sneps:clear-infer))
	((haveCommand? "clearkb")
	 (returnForms '(with-output-to-string (outunit)
			(resetnet))
		      '(format outunit "Knowledge Base Cleared")))
	((haveCommand? "copyright")
	 '(sneps:copyright))))

(defun ^-command ()
  "Parses the commands ^ and ^^."
  ;; Differs from previous SNePSLOG in that argument to ^ is not optional. 
  ;; ^-command ::= ^^ | ^ <Lisp form>
  (cond
   ((char= (elt *Input* 1) #\^)
    (popInput 2)
    (returnForms
     '(let ((*package* (find-package :snepslog)))
       (sneps:pseudolisp))))
   (t
    ;; Have ^ followed by one Sexpression
    (popInput 1)
    (returnForms
     `(prin1 ,(let ((*readtable* sneps::*original-readtable*)
		    (*package* (find-package :snepslog)))
		(getForm)))))))

(defun %-command ()
  "Returns the SNePSUL command following an initial %,
        which was already seen."
  ;; %-command ::= % <SNePSUL command>
  (popInput 1) ; Pop the #\%
  (let* ((*readtable* sneps::*sneps-readtable*)
	 (*package* (find-package :snepsul))
	 (sneps-command (getForm))
	 (*readtable* sneps::*original-readtable*))
    sneps-command))
    
(defun wffNameCommand ()
  "Parse and return a wffName followed by terminal punctuation."
  ;; wffNameCommand ::= wffName terminalPunctuation
  (let ((node (wffName)))
    (when node
      (cond ((and (>= (length *Input*) 2)
		  (string= (subseq *Input* 0 2) "??"))
	     (popInput 2)
	     (if (sneps:isassert.n (sneps:node node))
		     node
		   (returnForms nil)))
	    ((< (length *Input*) 1)
	     (parseError "Invalid punction after wffName at: ~S" *Input*))
	    ((char= (elt *Input* 0) #\.)
	     (popInput 1)
	     `(sneps:! ,node))
	    ((char= (elt *Input* 0) #\!)
	     (popInput 1)
	     `(^ (snip::add* (sneps:! ,node))))
	    ((char= (elt *Input* 0) #\?)
	     (popInput 1)
	     (let ((limits (second (checkDeduceLimits nil))))
	       `(^ (snip::deduce*
		    ',limits 
		    ',(sneps::makeone.ns (sneps:node node))))))))))
  

(defun wffCommand ()
  "Parse and return a wff followed by terminal punctuation.
     The wff must not be a single symbol."
  ;; wffCommand ::= wff terminalPunctuation ; wff must not be a single symbol
  ;; Note:  SNePS does not allow a non-molecular node to be asserted
  (let ((wff (wff)))
    (when wff
      (let ((punct (terminalPunctuation)))
	(cond (punct
	       (if (or (atom wff)
		       (eq (first wff) *QMark*))
		   (parseError
		    "Atomic, top-level wff, ~A, is not allowed" wff)
		 (case punct
		   (\. (if (and (inMode3p)
				(member 'cl:do (rest wff))
				(or (member 'when (rest wff))
				    (member 'whenever (rest wff))
				    (member 'cl:if (rest wff))))
			   (cons 'sneps:adopt (rest wff))
			 (cons 'sneps:assert (rest wff))))
		   (! (cons 'snip:add (rest wff)))
		   (sneps:?
		    (checkDeduceLimits
		     (fixQvarsForDeduce
		      (cons 'snip:deduce (rest wff)))))
		   (?? (cons 'sneps:findassert
				     (subst 'sneps:find 'sneps:build
					    (rest wff))))
		   (t (parseError
		       "Unrecognized punctuation ~S after wff ~S" punct wff)))))
	      (t (parseError
		  "Wff ~S not followed by terminal punctuation at ~S"
		  wff *Input*)))))))

(defun checkDeduceLimits (deduceForm)
  "If *Input* is now a list of one or two integers,
     add them to the deduce form."
  (cond ((firstCharp #\()
	 (let ((limits (getForm)))
	   (cond ((and (cl:= (length limits) 1)
		       (integerp (first limits)))
		  `(,(first deduceForm) ,(first limits)
					,@(rest deduceForm)))
		   ((and (cl:= (length limits) 2)
			 (integerp (first limits))
			 (integerp (second limits)))
		    `(,(first deduceForm) ,limits ,@(rest deduceForm)))
		 (t (parseError "Invalid list following `?': ~S" limits)))))
	;; No limits
	(t deduceForm)))

(defun fixQvarsForDeduce (deduceForm)
  "Returns the snepsul form
     with instances of (? x) replaced by
     first ($ 'x), then (* 'x)."
  (let (vars)
    (labels ((fixns (ns)
	       (cond ((atom ns) ns)
		     ((and (cl:= (length ns) 2)
			   (eq (first ns) *QMark*))
		      (cond ((member (second ns) vars)
			     `(sneps:* ',(second ns)))
			    (t (push (second ns) vars)
			       `(sneps:$ ',(second ns)))))
		     ((eq (first ns) 'sneps:build)
		      (cons 'sneps:build (fixbuildargs (rest ns))))
		     (t (mapcar #'fixns ns))))
	     (fixbuildargs (form)
	       (loop for (r ns) on form by #'cddr
		       collect r
		       collect (fixns ns))))
      (cons (first deduceForm) (fixbuildargs (rest deduceForm))))))

(defun terminalPunctuation ()
  "Parse and return terminal punctuation."
  (unless (emptyInput?)
    (case (elt *Input* 0)
      (#\. (popInput 1) '\.)
      (#\! (popInput 1) '!)
      (#\? (cond ((and (> (length *Input*) 1)
		       (char= (elt *Input* 1) #\?))
		  (popInput 2)
		  '??)
		 (t (popInput 1) *QMark*))))))

(defun wff ()
  "Parse and return a wff."
  ;; wff ::= infixedTerm | entailment | prefixedTerm
  (if (firstCharp #\{)
      (entailment)
    (let* ((firstWff (prefixedTerm)))
      (when firstWff
	(let ((connective (infixOp)))
	  (if connective
	      (infixedTerm firstWff connective)
	    (if (setf connective (haveEntailmentOp?))
		(entailmentCq firstWff connective)
	      firstWff)))))))

(defun infixOp ()
  "Parse and return one of the infix operators:
    *and*, *or*, or *iff*."
  (let ((len (length *Input*)))
    (cond ((and (> len 3)
		(string-equal (subseq *Input* 0 3) *and*))
	   (popInput 3)
	   *and*)
	  ((and (> len 3)
		(string-equal (subseq *Input* 0 3) *iff*))
	   (popInput 3)
	   *iff*)
	  ((and (> len 2)
		(string-equal (subseq *Input* 0 2) *or*))
	   (popInput 2)
	   *or*))))

(defun infixedTerm (firstTerm connective)
  "Parse and return an infixedTerm with the given firstTerm,
        and the given connective."
  ;; infixedTerm ::=  prefixedTerm [( and | or | <=> ) prefixedTerm]*
  (let* ((wffs (cons firstTerm
		     (loop with n = (length connective)
			 collect (prefixedTerm)
			 while (> (length *Input*) n)
			 until (member (elt *Input* 0) *TerminalPunctuation*
				       :test #'char=)
			 if (string-equal (subseq *Input* 0 n)
					  connective)
			 do (popInput n)
			 else 
			 do (parseError
			     "Bad connective in ~Aed sequence at: ~S"
			     connective *Input*))))
	 (n (length wffs)))
    (if (#+abcl string= #-abcl eq connective *iff*)
	`(sneps:build sneps:thresh 1 sneps:arg ,wffs)
      `(sneps:build
	min ,(if (#+abcl string= #-abcl eq connective *or*) 1 n) max ,n
	sneps:arg ,wffs))))

(defun entailment ()
  "Parse and return an entailment term."
  ;; entailment ::= termSet (=> | v=> | &=> | <Lisp integer> =>) termSet
  (let ((ant (termSet)))
    (when ant
      (let ((op (haveEntailmentOp?)))
	(if op
	    (entailmentCq ant op)
	  (parseError
	   "Entailment ~A missing entailment operator at: ~S"
	   ant *Input*))))))

(defun entailmentCq (ant op)
  "Parses the consequent of the entailment
       beginning with ant op,
       and returns the entire entailment."
  (let ((cq (termSet)))
    (if cq
	(cond ((eq op 'v=>)
	       `(sneps:build sneps:ant ,ant sneps:cq ,cq))
	      ((eq op '&=>)
	       `(sneps:build sneps:&ant ,ant sneps:cq ,cq))
	      ((integerp op)
	       `(sneps:build sneps:thresh ,op
			     sneps:&ant ,ant sneps:cq ,cq))
	      (t (parseError "Unknown entailment operator: ~S" op)))
      (parseError "Entailment ~A ~S missing consequent at: ~S"
		  ant op *Input*))))

(defun haveEntailmentOp? ()
  "Parse and return an entailment operator.
      Returns one of 'v=>, '&=>, or i."
  ;; One of: => | v=> | &=> | i=>
  (let ((len (length *Input*)))
    (cond ((and (>= len 2)
		(string= (subseq *Input* 0 2) "=>"))
	   (popInput 2)
	   'v=>)
	  ((and (>= len 3)
		(string= (subseq *Input* 0 3) "v=>"))
	   (popInput 3)
	   'v=>)
	  ((and (>= len 3)
		(string= (subseq *Input* 0 3) "&=>"))
	   (popInput 3)
	   '&=>)
	  ((and (>= len 3)
		(digit-char-p (elt *Input* 0)))
	   (let ((n (position #\= *Input*)))
	     (when (and n
			(every #'digit-char-p (subseq *Input* 0 n))
			(string= (subseq *Input* n (+ n 2)) "=>"))
	       (let ((i (read-from-string (subseq *Input* 0 n))))
		 (popInput (+ n 2))
		 i)))))))

(defun prefixedTerm ()
  "Parse and return a term that starts with a prefixed operator."
  ;; prefixedTerm ::= negatedTerm | andorTerm | allTerm | nexistsTerm |
  ;;                  threshTerm | atomicTerm 
  ;;                  | andTerm |orTerm | nandTerm | norTerm | xorTerm | iffTerm
  (unless (emptyInput?)
    (case (elt *Input* 0)
      (#\~ (negatedTerm))
      (#\a (or (andorTerm) (allTerm) (andTerm) (atomicTerm)))
      (#\i (or (iffTerm) (atomicTerm)))
      (#\n (or (norTerm) (nandTerm) (nexistsTerm) (atomicTerm)))
      (#\o (or (orTerm) (atomicTerm)))
      (#\t (or (threshTerm) (atomicTerm)))
      (#\x (or (xorTerm) (atomicTerm)))
      (t (atomicTerm)))))

(defun nexistsTerm ()
  "Parse and return a term that starts with an nexists quantifier."
  ;; nexistsTerm ::= nexists nexistsParameters
  ;;                 _(_ symbolSequence _)_ 
  ;;                 _(_ termSet : termSet _)_
  (when (and (> (length *Input*) 7)
	     (string-equal (subseq *Input* 0 7) "nexists")
	     (char= (elt *input* 7) #\())
    (popInput 7)
    (cond ((firstCharp #\()
	   (let ((params (nexistsParameters)))
	     (unless params
	       (parseError
		"nexists must be followed by nexistsParameters at: ~S"
		*Input*))
	     (cond ((firstCharp #\()
		    (popInput 1)
		    (let ((vars (symbolSequence)))
		      (unless vars
			(parseError "nexists requires a list of variables at: ~S" *Input*))
		      (popInput 1)	; pop the terminating `)'
		      (unless (firstCharp #\()
			(parseError
			 "nexists~A~A must be followed by (wff(s) : wff(s)) at: ~S"
			 params vars *Input*))
		      (popInput 1)
		      (let ((ant (termSet)))
			(unless ant
			  (parseError "Missing termSet at: ~S"
				      *Input*))
			(unless (firstCharp #\:)
			(parseError
			 "Missing nexists : separator at: ~S"
			 *Input*))
			(popInput 1)
			(let ((cq (termSet)))
			(unless cq
			  (parseError
			   "Missing second sequence of nexists wffs at: ~S"
			   *Input*))
			(unless (firstCharp #\))
			  (parseError
			   "Missing close parenthesis at: ~S"
			   *Input*))
			(popInput 1)
			`(sneps:build
			  emin ,(first params)
			  emax ,(second params)
			  etot ,(third params)
			  pevb ,(mapcar #'(lambda (v) `(sneps:$ ',v))
				      vars)
			  sneps:&ant ,(fixNSVarsForAll ant vars)
			  sneps:cq ,(fixNSVarsForAll cq vars)))))))))
	  (t (replaceToken "nexists")))))

(defun nexistsParameters ()
  "Parse and return a list of nexists parameters."
  ;; nexistsParameters ::= _(_ <Lisp integer>, <Lisp integer>, <Lisp integer> _)_ |
  ;;                       _(_ _, <Lisp integer>, _ _)_ |
  ;;                       _(_ <Lisp integer>, _, <Lisp integer> _)_
  (let (params)
    (when (firstCharp #\()
      (popInput 1)
      (push (nexistsParameter) params)
      (if (firstCharp #\,)
	  (popInput 1)
	(parseError
	 "Incorrect nexists parameter punctuation at: ~S" *Input*))
      (push (nexistsParameter) params)
      (if (firstCharp #\,)
	  (popInput 1)
	(parseError
	 "Incorrect nexists parameter punctuation at: ~S" *Input*))
      (push (nexistsParameter) params)
      (if (firstCharp #\))
	  (popInput 1)
	(parseError
	 "Missing close parenthesis after nexists parameters at: ~S"
	 *Input*))
      (if (or (every #'identity params)
	      (null (second params))
	      (and (null (first params))
		   (null (third params))))
	  (nreverse params)
	(parseError
	 "Illegal nexists parametes: ~S" (nreverse params))))))

(defun nexistsParameter ()
  "Parse and return a single nexists parameter."
  (cond ((firstCharp #\_)
	 (popInput 1)
	 nil)
	(t (let ((p (getForm)))
	     (if (integerp p)
		 p
	       (parseError
		"nexists requires parameters (i, j, k) at: ~S"
		*Input*))))))

(defun allTerm ()
  "Parse and return a term
        that starts with an all quantifier." 
  ;; allTerm ::= all _(_ symbolSequence _)_ _(_ wff _)_  ; but wff cannot be an atomic symbol
  ;;  Note: "wff" used to be "entailment".
  ;;        That was too restrictive,
  ;;             since all(x)(p(x) <=> q(x)) should be OK.
  ;;        However, "wff" is too lax,
  ;;             since it allows all(x)(p(x)).
  (when (and (>= (length *Input*) 3)
	     (string-equal (subseq *Input* 0 3) "all"))
    (popInput 3)
    (cond ((firstCharp #\()
	   (popInput 1)
	   (let ((vars (symbolSequence)))
	     (unless vars
	       (parseError "all requires a list of variables at: ~S" *Input*))
	     (popInput 1)		; pop the terminating `)'
	     (unless (firstCharp #\()
	       (parseError
		"Universally quantified wff all~A is missing ~
                          a parenthesized wff at: ~S."
		vars *Input*))
	     (popInput 1)
	     (let ((wff (wff)))
	       (unless wff
		 (parseError
		  "Universally quantified wff all~A_____ is missing its scope at: ~S."
		  vars *Input*))
	       (unless (firstCharp #\))
		 (parseError
		  "Universally quantified wff all~A(~A is missing ~
                          a close parenthesis at: ~S."
		  vars wff *Input*))
	       (unless (consp wff)
		 (parseError
		  "Scope of all cannot be an atomic symbol ~S before: ~S."
		  wff *Input*))
	       (popInput 1)
	       `(sneps:build sneps:forall
			     ,(mapcar #'(lambda (v) `(sneps:$ ',v))
				      vars)
			     ,@(fixVarsForAll (rest wff) vars)))))
	  (t (replaceToken 'all)))))

(defun fixNSVarsForAll (ns vars)
  "Returns the nodeset-valued form, ns,
       which is either:
          a symbol naming a sneps node;
          a list of the form (* n), (? n) or ($ n);
          a SNePSUL command form such as (build ...), (assert ...), etc;
          or a list of any of them,
     with instances of every x in the list vars replaced by (* 'x)."
  (cond ((atom ns)
	 (if (member ns vars :test #'eq)
	     `(sneps:* ',ns)
	   ns))
	((and (qlistp ns)
	      (member (second ns) vars :test #'eq))
	      `(sneps:* ',(second ns)))
	((member (first ns) '(sneps:* sneps:$) :test #'eq)
	 ns)
	((sneps::is.com (first ns))
	 (cons (first ns) (fixVarsForAll (rest ns) vars)))
	(t (mapcar #'(lambda (f) (fixNSVarsForAll f vars))
		   ns))))

(defun fixVarsForAll (form vars)
  "Returns the snepsul form, which is a sequence of ...relation nodeset...,
     with instances of every x in the list vars replaced by (* 'x)."
  (loop for (r ns) on form by #'cddr
      collect r
      if (eq r 'sneps:forall) collect ns
      else collect (fixNSVarsForAll ns vars)))

(defun andTerm ()
  "Parse and return a functional term
        that starts with an and operator." 
  ;; andTerm ::= and termSet
  (when (and (> (length *Input*) 3)
	     (string-equal (subseq *Input* 0 3) "and")
	     (char= (elt *input* 3) #\{))
    (popInput 3)
    (if (firstcharp #\{)
	(let ((args (termSet)))
	     (cond (args
		    `(sneps:build min ,(length args)
				  max ,(length args)
				  sneps:arg ,args))
		   (t (parseError
		       "and must take a set of at least 1 argument at: ~S" *Input*))))
      (parseError
       "and must be followed by \"{\" at: ~S" *Input*))))

(defun iffTerm ()
  "Parse and return a functional term
        that starts with an iff operator." 
  ;; iffTerm ::= iff termSet
  (when (and (> (length *Input*) 3)
	     (string-equal (subseq *Input* 0 3) "iff")
	     (char= (elt *input* 3) #\{))
    (popInput 3)
    (let ((args (termSet)))
      (cond (args
	     `(sneps:build thresh 1
			   sneps:arg ,args))
	    (t (parseError
		"iff must take a set of at least 1 argument at: ~S" *Input*))))))

(defun norTerm ()
  "Parse and return a functional term
        that starts with a nor operator." 
  ;; norTerm ::= nor termSet
  (when (and (> (length *Input*) 3)
	     (string-equal (subseq *Input* 0 3) "nor")
	     (char= (elt *input* 3) #\{))
    (popInput 3)
    (let ((args (termSet)))
	 (cond (args
		`(sneps:build min 0
			      max 0
			      sneps:arg ,args))
	       (t (parseError
		   "nor must take a set of at least 1 argument at: ~S" *Input*))))))

(defun nandTerm ()
  "Parse and return a functional term
        that starts with a nand operator." 
  ;; nandTerm ::= nand termSet
  (when (and (> (length *Input*) 4)
	     (string-equal (subseq *Input* 0 4) "nand")
	     (char= (elt *input* 4) #\{))
    (popInput 4)
    (if (firstcharp #\{)
	(let ((args (termSet)))
	  (cond (args
		 `(sneps:build min 0
			       max ,(1- (length args))
			       sneps:arg ,args))
		(t (parseError
		    "nand must take a set of at least 1 argument at: ~S" *Input*))))
      (parseError
       "nand must be followed by \"{\" at: ~S" *Input*))))

(defun orTerm ()
  "Parse and return a functional term
        that starts with an or operator." 
  ;; orTerm ::= or termSet
  (when (and (> (length *Input*) 2)
	     (string-equal (subseq *Input* 0 2) "or")
	     (char= (elt *input* 2) #\{))
    (popInput 2)
    (if (firstcharp #\{)
	(let ((args (termSet)))
	  (cond (args
		 `(sneps:build min 1
			       max ,(length args)
			       sneps:arg ,args))
		(t (parseError
		    "or must take a set of at least 1 argument at: ~S" *Input*))))
      (parseError
       "or must be followed by \"{\" at: ~S" *Input*))))

(defun xorTerm ()
  "Parse and return a functional term
        that starts with an xor operator." 
  ;; xorTerm ::= xor termSet
  (when (and (> (length *Input*) 3)
	     (string-equal (subseq *Input* 0 3) "xor")
	     (char= (elt *input* 3) #\{))
    (popInput 3)
    (let ((args (termSet)))
	 (cond (args
		`(sneps:build min 1
			      max 1
			      sneps:arg ,args))
	       (t (parseError
		   "xor must take a set of at least 1 argument at: ~S" *Input*))))))

(defun andorTerm ()
  "Parse and return a functional term
        that starts with an andor operator." 
  ;; andorTerm ::= andor (i, j) termSet ; i, j integers, 0 <= i <= j
  (when (and (> (length *Input*) 5)
	     (string-equal (subseq *Input* 0 5) "andor")
	     (char= (elt *input* 5) #\())
    (popInput 5)
    (if (firstCharp #\()
	(let ((params (nparams 'andor)))
	  (if params
	      (let ((args (termSet)))
		(cond (args
		       `(sneps:build min ,(first params)
				     max ,(second params)
				     sneps:arg ,args))
		      (t (parseError
			  "andor~A must take a set of at least ~D arguments at: ~S"
			  params (max (second params) 1) *Input*))))
	    (parseError
	     "andor must be followed by a list of 2 parameters at: ~S" *Input*)))
      (replaceToken 'andor))))

(defun threshTerm ()
  "Parse and return a term
        that starts with a thresh operator." 
  ;; threshTerm ::= thresh (i [, j]) termSet ; i, j integers, 0 <= i <= j
  (when (and (> (length *Input*) 6)
	     (string-equal (subseq *Input* 0 6) "thresh")
	     (char= (elt *input* 6) #\())
    (popInput 6)
    (if (firstCharp #\()
	(let ((params (nparams 'thresh)))
	  (if params
	      (let ((args (termSet)))
		(cond (args
		       `(sneps:build sneps:thresh ,(first params)
				     sneps:threshmax ,(second params)
				     sneps:arg ,args))
		      (t (parseError
			  "thresh~A must take a set of arguments at: ~S"
			  params *Input*))))
	    (parseError
	     "thresh must be followed by a list of 1 or 2 parameters at: ~S"
	     *Input*)))
      (replaceToken 'thresh))))

(defun negatedTerm ()
  "Parse and return a term
        that starts with a negation operator." 
  ;; negatedTerm ::= ~ atomicTerm
  (when (firstCharp #\~)
    (popInput 1)
    (let ((term (atomicTerm)))
      (if term
	  `(sneps:build min 0 max 0 sneps:arg ,term)
	  (parseError
	   "Negation operator not followed by a term at: ~S"
	   *Input*)))))
  
(defun atomicTerm ()
  "Parse and return an atomic term."
  ;; atomicTerm ::=  wffName | qvar | SNePSLOGsymbol |
  ;;                 withsome/allTerm | ; in mode 3 only
  ;;                 (qvar | SNePSLOGsymbol) _(_ termSequence _)_ |
  ;;                _(_ wff _)_
  ;; Can be a proposition-valued functional term.
  (cond ((firstCharp #\()
	 (popInput 1)
	 (let ((wff (wff)))
	   (if wff
	       (cond ((firstCharp #\))
		      (popInput 1)
		      wff)
		     (t (parseError
			 "Wff (~A missing close parenthesis at: ~S"
			 wff *Input*)))
	     (parseError "Unrecognized wff at: ~S" *Input*))))
	((wffName))
	(t (let ((functionSymbol (or (qvar) (SNePSLOGsymbol))))
	     (when functionSymbol
	       (cond ((firstCharp #\()
		      (popInput 1)
		      (let ((trmsqce (termSetSequence)))
			(cond ((firstCharp #\))
			       (popInput 1) 
			       (let ((snepsulExpr
				      (make-relation 'sneps:build
						     functionSymbol trmsqce)))
				 (if (rest snepsulExpr)
				     (cond ((and (inMode3p)
						 (or (eq functionSymbol
							 'snip:withsome) 
						     (eq functionSymbol
							 'snip:withall)))
					    (withsome/allTerm functionSymbol
							      snepsulExpr))
					   (t snepsulExpr))
				   (parseError "No caseframe has been defined for ~S."
					       functionSymbol))))
			      (t (parseError "Bad punctuation at ~S" *Input*)))))
		     (t functionSymbol)))))))

(defun withsome/allTerm (functionSymbol snepsulExpr)
  "Returns the withsome or withall, given by functionSymbol,
       whose form is snepsulExpr."
  ;; withsome/allTerm ::= (withsome | withall) _(_ symbolSequence, termSet, termSet [, termSet]  _)_
  (if (< (length snepsulExpr) 9)
      (parseError "Insufficient number of arguments for ~A: ~S just before: ~S"
		  functionSymbol snepsulExpr *Input*)
    (let ((vars (varlist (fifth snepsulExpr))))
      (setf snepsulExpr
	`(sneps:build
	  sneps:action ,functionSymbol
	  sneps:vars
	  ,(mapcar #'(lambda (v) `(sneps:$ ',v)) vars)
	  ,@(fixVarsForAll
	     (nthcdr 5 snepsulExpr)
	     vars)))
      snepsulExpr)))

(defun varlist (varsspec)
  "Takes the specifier of bound variables, varsspec,
       and retruns a list of the specified variables."
  ;; varsspec could be something like x, (? x), or a list of such
  (cond ((atom varsspec) (list varsspec))
	((qlistp varsspec) (rest varsspec))
	(t (mapcan #'varlist varsspec))))

(defun qlistp (form)
  "If form is a list of the form (? x),
       returns t; else returns nil."
  (and (consp form)
       (cl:= (length form) 2)
       (eq (first form) *QMark*)
       (atom (second form))))
	       
(defun termSetSequence ()
  "Parse and return a sequence of sets of terms separated by commas."
  ;; termSetSequence ::= termSet [, termSet]*
  (let ((firstTerm (termSet)))
    (when firstTerm
      (cons firstTerm
	    (loop until (member (elt *Input* 0) *TerminalPunctuation*
				:test #'char=)
		if (firstCharp #\,)
		do (popInput 1)
		else
		do (parseError "Bad punctuation in termSequence at ~S"
			       *Input*)
		collect (termSet))))))

(defun replaceToken (token)
  "Replaces token at the front of *Input*,
        and returns nil."
  (setf *Input* 
    (concatenate 'string
      (cond ((stringp token)
	     (format nil "~S" token))
	    ((atom token)
	     (format nil "~A" token))
	    ;; In case token is (? p), for some p 
	    (t (format nil "~a~a"
		       (first token)
		       (second token))))
      *Input*))
  nil)

(defun termSet ()
  "Parse and return a set of terms."
  ;; termSet ::= {  termSequence }
  (cond ((firstCharp #\{)
	 (popInput 1)
	 (let ((trmsqce (termSequence)))
	   (if trmsqce
	       (cond ((firstCharp #\})
		      (popInput 1)
		      trmsqce)
		     (t (parseError "Bad punctuation at ~S" *Input*)))
	     (parseError "Empty termSequence just before ~S" *Input*))))
	(t (prefixedTerm))))

(defun nparams (operator)
  "Parse and return a list of integers separated by commas
        for the given operator,
           which must be either andor, thresh, or nexists."
  (when (firstCharp #\()
    (let ((n (position #\) *Input* :test #'char=)))
      (if n
	  (let* ((stringlist (subseq *Input* 0 (1+ n)))
		 (list (read-from-string
			(substitute #\Space #\, stringlist))))
	    (cond ((or (and (eq operator 'andor)
			    (cl:= (length list) 2)
			    (every #'integerp list)
			    (apply #'<= 0 list))
		       (and (eq operator 'thresh)
			    (<= 1 (length list) 2)
			    (every #'integerp list)
			    (apply #'<= 0 list)))
		   (popInput (1+ n))
		   list)
		  (t (parseError "Improper list of numerical parameters: ~S" stringlist))))
	(parseError "List begins but doesn't end at : ~S" *Input*)))))

(defun termSequence ()
  "Parse and return a sequence of terms separated by commas."
  ;; termSequence ::= prefixedTerm [, prefixedTerm]*
  (let ((firstTerm (prefixedTerm)))
    (when firstTerm
      (cons firstTerm
	    (loop until (member (elt *Input* 0) *TerminalPunctuation*
				:test #'char=)
		if (firstCharp #\,)
		do (popInput 1)
		else
		do (parseError "Bad punctuation in termSequence at ~S"
			       *Input*)
		collect (prefixedTerm))))))

(defun symbolSequence ()
  "Parse and return a sequence of symbols separated by commas."
  ;; symbolSequence ::= SNePSLOGsymbol [, SNePSLOGsymbol]*
  (let ((firstVar (SNePSLOGsymbol)))
    (when firstVar
      (cons firstVar
	    (loop until (member (elt *Input* 0) *TerminalPunctuation*
				:test #'char=)
		if (firstCharp #\,)
		do (popInput 1)
		else
		do (parseError "Bad punctuation in symbolSequence at ~S"
			       *Input*)
		collect (SNePSLOGsymbol))))))

(defun SNePSLOGsymbol ()
  "Parse and return a symbol."
  ;; SNePSLOGsymbol ::= (wff <Lisp integer>) | <Lisp atom>
  (when (string/= *Input* "")
    (unless (member (elt *Input* 0) *SeparatingPunctuation* :test #'char=)
      (cond ((member (char *Input* 0) *Punctuation* :test #'char=)
	     ;; First character is punctuation -- no symbol found
	     nil)
	    (t  (multiple-value-bind (token n) (LispAtom)
		  (setf *Input* 
		    (string-left-trim *WhiteSpace* (subseq *Input* n)))
		  (typecase token
		    (string (intern token))
		    (number (intern (build-namestring token)))
		    (t token))))))))

(defun wffName ()
  "Parse a token of the form wffi, for some integer i,
     on the condition that there is a Lisp node named mi,
     and return mi."
  ;; wffName ::= wff <Lisp integer> ; Assuming that mi is a SNePS node
  (when (and (> (length *Input*) 3)
	     (string-equal (subseq *Input* 0 3) "wff"))
    (multiple-value-bind (token n)
	(read-from-string
	 *Input* nil nil
	 :end (position-if
	       #'(lambda (c) 
		   (member c *Punctuation* :test #'char=))
	       *Input*))
      (let ((nametail (subseq (symbol-name token) 3))
	    mName)
	(when (and (every #'digit-char-p nametail)
		   (sneps:node 
		    (setf mName
		      (intern (build-namestring :m nametail) :snepsul))))
	  (popInput n)
	  ;; Remove #\! at end of wffname,
	  ;;    but not as terminal punctuation.
	  (when (and (firstcharp #\!) 
		     (> (length
			 (string-right-trim '(#\Space #\Tab #\Newline) *input*))
			1))
	    (popinput 1))
	  mName)))))

(defun qmark ()
  "Parses a single question mark."
  (when (firstCharp #\?)
    (popInput 1)
    t))

(defun qvar ()
  "Parse a question-mark variable."
  ;; qvar ::= ? SNePSLOGsymbol
  (when (qmark)
    (let ((var (SNePSLOGsymbol)))
      (if var
	  (list *QMark* var)
	nil))))

(defun requiredWffArg (command)
  "Parses a wff required by the command.
     If it is not found, raises a parseError."
  (let ((wff (wff)))
    (cond (wff
	   (terminalPunctuation)
	   wff)
	  (t (parseError
	      "The command ~A requires a wff as an argument at ~S"
	      command *Input*)))))

 ;;; Parser Utility Functions

(defun checkIsContextName (cname)
  (if (sneps:is.ct (value.sv cname))
      t
    (parseError "The symbol ~A is not the name of a context at ~S"
		cname *Input*)))
    
(defun popInput (n)
  "Pops off the first n characters (and subsequent whitespace)
        from *Input*.
     Returns the substring of the first n characters."
  (prog1
      (subseq *Input* 0 n)
    (setf *Input*
      (string-left-trim *WhiteSpace* (subseq *Input* n)))
    (loop while (firstCharp #\;)
	do (setf *Input*
	     (string-left-trim
	      *WhiteSpace* 
	      (subseq *Input*
		      (1+
		       (position #\Newline *Input* :test #'char=))))))))

(defun getForm ()
  "Deletes and returns the first Lisp form from *Input*."
  (multiple-value-bind (form n)
      (read-from-string *Input*)
    (popInput n)
    form))

(defun haveCommand? (command &optional (hasArg nil))
  "If *Input* begins with the command,
       Returns t;
     Else returns nil.
     If hasArg is nil, *Input* must consist entirely of the command,
        but may terminate with a period."
  (let ((n (length command)))
    (when (and (>= (length *Input*) n)
	       (string-equal (subseq *Input* 0 n) command))
      (cond (hasArg
	     (popInput n)
	     t)
	    (t (emptyInputAfter? n))))))

(defun LispAtom ()
  "If *Input* begins with a Lisp atom,
     returns it
     and the position of the first character in *Input* after it;
     Else returns nil.
     Doesn't change *Input* in either case."
  (cond ((char= (char *Input* 0) #\")	; Symbol is a string
	 (read-from-string *Input*))
	((not (member (elt *Input* 0) *Punctuation* :test #'char=))
	 ;; a Lisp symbol or number
	 (let ((*readtable* *SNePSLOGAtomReadTable*))
	   (read-from-string
	    *Input* nil nil)))))
  
(defun emptyInputAfter? (n)
  "If after the first n characters,
       *Input* ends or has only a period,
      makes *Input* empty and returns t;
      Else doesn't modify *Input*, and returns nil."
  (let ((initstr (popInput n)))
    (cond ((or (emptyInput?)
	       (char= (elt *Input* 0) #\.))
	   (setf *Input* "")
	   t)
	  (t (setf *Input* (concatenate 'string initstr *Input*))
	     nil))))

(defun emptyInput? ()
  "If *Input* is empty, returns t, else nil."
  (string= *Input* ""))

(defun firstCharp (c)
  "If the first character of *Input* is c, return t;
     else return nil."
  (and (not (string= *Input* ""))
       (char= (elt *Input* 0) c)))

(defun returnForms (&rest forms)
  "Returns a SNePSUL expression that will result
        in forms being evaluated and timed,
           even if they are not a SNePSUL command."
  `(^ (progn ,@forms (sneps:+))))

(defun closedTerms ()
  "Returns a list of closed molecular nodes."
  (let (found)
    (sneps:do.ns (n (sneps:* 'sneps:nodes) found)
		 (when (and (sneps:ismol.n n)
			    (not (sneps:ispat.n n)))
		   (push n found)))))

(defparameter *ParsingFunctions*
    '( snepslog-read-from-string %-command SNePSLOGsymbol ^-command
      a-command allTerm andorTerm ask-command atomicTerm c-command
      checkDeduceLimits d-command define-frame-command e-command
      entailment entailmentCq
      haveEntailmentOp? infixOp infixedTerm l-command LispAtom
      n-command negatedTerm nexistsParameter nexistsParameters nexistsTerm
      nparams p-command path prefixedTerm qmark qvar r-command
      replaceToken requiredWffArg s-command snepslogCommand t-command
      termSequence termSet termSetSequence terminalPunctuation
      threshTerm u-command symbolSequence wff wffCommand wffName
      wffNameCommand withsome/allTerm)
  "List of all parsing functions,
       so that they can be traced and untraced.") 

(defmacro TraceParsing ()
  `(trace
    ,@(mapcar #'(lambda (f) `(,f :print-before *Input*))
	     *ParsingFunctions*)))

(defmacro UntraceParsing ()
  `(untrace ,@*ParsingFunctions*))

(defun demo (&optional (file 'x file-supplied-p)
		       (pause nil pause-supplied-p))
  ;; This is snepslog::demo as opposed to sneps:demo
  (let ((menu (when (sneps:initialized-p
		     sneps:*available-snepslog-demos-menu*)
		sneps:*available-snepslog-demos-menu*)))
    (cond (file-supplied-p
	   (if pause-supplied-p
	       (sneps:demo-internal file :pause pause :menu menu)
	       (sneps:demo-internal file :menu menu)))
	  (t (sneps:demo-internal file :pause 'av :menu menu)))))

(defun list-contexts ()
  "Prints all names of all existing contexts."
  (declare (special outunit))
  (loop for cntx being each hash-value of (value.sv 'sneps::contexts)
    when (sneps:context-names cntx)
    do (format outunit "~{~&~S~%~}" (sneps:context-names cntx)))
    (values))

(defun make-relation (action relation arguments)
  (funcall (get 'mode 'make-relation) action relation arguments))

(defun make-relation.1 (action relation arguments)
  "Receives an action, a relation and the arguments of the relation. Returns the SNePSUL command that perform the action
on the relation (following the protocol of mode 1, that is, the predicate arc named is r and the argument arcs are a1, a2,
 etc.)."  
  (let ((counter 0))
    (define-if-not-yet-defined 'snepsul:r)
    (nconc (list action 'snepsul:r relation)
	   (mapcan #'(lambda (x)
		       (list 
			(let ((arc (intern 
				    (build-namestring :a (incf counter))
				    'snepsul)))
			  (define-if-not-yet-defined arc)
			  arc)
			x)) 
		   arguments))))

(defun make-relation.2 (action relation arguments)
  "Receives an action, a relation and the arguments of the relation. Returns the SNePSUL command that perform the action
on the relation (following the protocol of mode 2, that is, the predicate arc named is indexed with the predicate name and
 the argument arcs are also indexed with the predicate. The predicate arc name is the result of appending the string ' rel '
 to the predicate name The first white space is to garantee that the predicate has the lowest alfabetic value of all the arcs
leaving any sneps node. This way, it is easy and some how inexpensive to find the the predicate of a sneps node. The arguments arc is the result of appending the string 'rel-arg ' to the predicate name and a counter. The argument arcs are associated
to the predicate)."
  (let ((counter 0)
	(relation-arc-name 
	 (build-namestring " " :rel " " (symbol-name relation)))
	(arguments-prefix  
	 (build-namestring  :rel-arg# relation))
	argument-arcs
	relation-arc)
    (setq argument-arcs 
      (mapcar #'(lambda (node)
		  (declare (ignore node))
		  (intern 
		   (build-namestring arguments-prefix (incf counter))
		   'snepsul))
	      arguments)
      relation-arc (intern relation-arc-name 'snepsul))  
    (mapc #'define-if-not-yet-defined (cons relation-arc argument-arcs))
    (associate-arcs relation-arc argument-arcs)
    (cons action (intercalate (cons relation-arc argument-arcs)
			      (cons relation arguments)))))

(defun associate-arcs (arc arcs)
"Used in version 2. Given the predicate arc, returns the other arcs of the node"
  (setf (get arc 'snepslog-associated-arcs) arcs))

(defun define-if-not-yet-defined (arc)
  "Receives as argument an atom, arc. If already exists an arc with that name, that arc is returned. Otherwise, creates and
 returns a new arc with that name"
  (if (not (sneps:is.r arc))
      (sneps:new.r arc (find-package :snepslog))))

(defun intercalate (l1 l2)
  (if (null l1)
      l2
    (cons (car l1) (intercalate l2 (cdr l1)))))
