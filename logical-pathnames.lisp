;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LOGICAL-PATHNAME; Base: 10 -*-

;; Version: $Id: logical-pathnames.lisp,v 1.3 2014/10/20 20:34:28 shapiro Exp $

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
;; The Original Code is SNePS 2.8.
;; 
;; The Initial Developer of the Original Code is Research Foun
;; dation of State University of New York, on behalf of Univer
;; sity at Buffalo.
;; 
;; Portions created by the Initial Developer are Copyright (C)
;; 2013 Research Foundation of State University of New York, on
;; behalf of University at Buffalo. All Rights Reserved.
;; 
;;  
;; 
;; 


;; $END LICENSE$



(in-package :LOGICAL-PATHNAME)


;;; Tue Apr  9 19:17:01 1991 by Mark Kantrowitz <mkant@LION.OZ.CS.CMU.EDU>
;;; logical-pathnames.lisp

;;; IMPORTANT: This is a MODIFIED version of Mark Kantrowitz's original 
;;; package. The modifications are intended to 1) allow users to define new
;;; host types without having to modify this package, and 2) to provide for
;;; peaceful coexistance in case a certain LISP implementation provides a
;;; native implementation of logical pathnames. 
;;; Modification comments are prefixed with `hc:'.
;;; Author of modifications: Hans Chalupsky (Aug-22-94).

;;; ****************************************************************
;;; Logical Pathnames System ***************************************
;;; ****************************************************************
;;;
;;; Logical Pathnames provide a facility for referring to pathnames
;;; in a portable manner. Logical pathnames are mapped to physical
;;; pathnames by a set of implementation dependent and site-dependent
;;; rules. 
;;; 
;;; This system is a Common Lisp portable implementation of logical
;;; pathnames. It fulfills most of the X3J13 June 1989 specification
;;; for logical pathnames, as documented in Guy Steele's "Common Lisp:
;;; The Language" (2nd Edition), section 23.1.5 "Logical Pathnames".
;;;
;;; Written by Mark Kantrowitz, July 1990.
;;;
;;; Address: Carnegie Mellon University
;;;          School of Computer Science
;;;          Pittsburgh, PA 15213
;;;
;;; This code is in the public domain and is distributed without warranty
;;; of any kind. 
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 
;;;
;;; Please send bug reports, comments and suggestions to mkant@cs.cmu.edu. 
;;;
;;;
;;; Logical Pathnames are especially useful when coupled with a portable
;;; system construction tool, such as the Defsystem facility written
;;; by Mark Kantrowitz.
;;;

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; djc  = Daniel J. Clancy <clancy@cs.utexas.edu>
;;;
;;; 30-JUL-90  mk    Fixed logical pathnames for VAX-LISP (thanks to
;;;                  Paul Werkowski). In VAX-LISP simple strings are not
;;;                  sub-types of simple-vectors, so svref doesn't work
;;;                  on strings. These calls have been fixed to read
;;;                  #+:cmu svref #-:cmu aref.
;;; 15-NOV-90  mk    Changed convert-file-function to better handle optional 
;;;                  args. This should fix the problem of (ed) and (dribble)
;;;                  returning errors like "argument NIL must be a number"
;;;                  in parse-namestring. Note that some lisps seem to make
;;;                  a distinction between (funcall #'foo) and (foo) with
;;;                  respect to this error.
;;; 29-JAN-91  mk    Defined LISP:NTH-VALUE if not already present (it is
;;;                  a CLtL2 addition) and used it in LOAD-PHYSICAL-HOSTAB
;;;                  to avoid needing a GARBAGE variable in 
;;;                  (multiple-value-setq (garbage pos) ...) which we can
;;;                  not declare ignore and yet causes a compiler warning
;;;                  since we don't use it.
;;; 29-JAN-91  mk    lisp::file-name is particular to CMU Common Lisp
;;;                  and the #+:cmu's were accidentally left off.
;;; 29-JAN-91  mk    Added :explorer physical namestring output to 
;;;                  PHYSICAL-NAMESTRING.                
;;; 29-JAN-91  mk    Warns about name collisions between physical and logical
;;;                  host names.
;;; 30-JAN-91  mk    Added :logical-pathnames-mk to the *features* list.
;;; 25-FEB-91  mk    Added definition of LOAD-LOGICAL-PATHNAME-TRANSLATIONS.
;;; 09-APR-91  mk    Export pathname-host-type, append-logical-directories.
;;; 09-APR-91  mk    Translation rules now support :case :unchanged.
;;; 09-APR-91  djc   Fixed so that (logical-pathname "") returns a 
;;;                  logical-pathname structure.
;;; 12-JUL-91 mk    Changed wildcarding to bring it into closer conformance
;;;                 with reversible wildcard translation, and to make it
;;;                 easier for the user to customize the behavior.
;;;  8-JAN-92 mk    Upgraded packages to work in CLtL2 compatible lisps too.
;;; 20-MAY-92 mk    No longer define NTH-VALUE in the Lisp package to avoid
;;;                 name conflicts in Lucid, because the symbol is defined in
;;;                 the CLIM-LISP using package.
;;; 20-MAY-92 mk    Logical pathnames now have a default host specified by the
;;;                 variable *default-logical-pathname-host*.
;;; 1-JUN-2002 flj  Altered for ACL 6 compatibility (FLJ)
;;; 19-AUG-04 flj   changed lisp: to cl: 

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; should really define *default-logical-pathname-defaults* to avoid
;;; problems with *default-pathname-defaults*.
;;;
;;; Support for Macintosh pathnames. Little tricky, since MACL uses a
;;; colon (:) as the delimiter.
;;;    
;;; support for tops-20/tenex, multics, its, ms-dos
;;; add host-type to pathnames
;;; merge-pathnames, with-open-file 
;;;
;;; Define generic pathname parsing/printing definition interface.
;;;
;;; Redefine with-open-file?
;;;
;;; Port to emacs-lisp for gnu-emacs?
;;;
;;; Logical pathnames needs to case both on the physical host type and on
;;; lisp type (e.g., for canonicalization). Fix this, and define lots of
;;; canonical types. Dependency on lisp type can probably be handled using
;;; #+ and #-. What about conflicts between canonicalization and the
;;; translations (e.g., "L" vs :lisp)?
;;;

;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    LOGICAL-PATHNAMES has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;       Symbolics Common Lisp (8.0)
;;;       Lucid Common Lisp (3.0, 4.0)
;;;       VAXLisp (2.0, 3.1)
;;;
;;;    LOGICAL-PATHNAMES needs to be tested in the following lisps:
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp


;;; ********************************
;;; Documentation ******************
;;; ********************************
;;; 
;;; Logical pathnames allow large programs to be moved between sites
;;; by separating pathname reference from actual file location. The
;;; program will refer to files using logical pathnames. At each site,
;;; a user will specify a set of "translations" which map from the logical
;;; pathnames to the physical pathnames used on the device.
;;;
;;; Logical pathnames provide a uniform convention for filesystem access,
;;; with the following properties:
;;;  1.  Pathname Portability: The program specifies a pathname in
;;;      a conventional format (logical pathnames), which may be
;;;      mapped reasonably literally (via the translations) to
;;;      a variety of filesystems.
;;;  2.  Pathname Aliasing: The files may exist in different locations
;;;      in the various filesystems. For example, the root directory
;;;      might change. The translations allow such a change easily.
;;;  3.  Cross-host Access: The files need not all exist on the same
;;;      physical host. 
;;;
;;; This definition of logical pathnames provides support for physical
;;; pathnames for Unix, VMS/VAX, Symbolics, and TI Explorers, and is
;;; easily extended to handle additional platforms. Code which may need
;;; customization for particular Lisps and platforms has been commented
;;; with three ampersands (&&&). In addition, the user probably should
;;; define their own canonical types, translation rules, and
;;; logical-pathname-translations. Examples are provided.
;;;
;;; Logical pathnames employ the following syntax:
;;;     [host:] [;] {directory ;}* [name] [. type [. version]]
;;; host      ::= word
;;; directory ::= word | wildcard-word | wildcard-inferiors
;;; name      ::= word | wildcard-word
;;; type      ::= word | wildcard-word
;;; version   ::= word | wildcard-word
;;; word      ::= {letter | digit | -}*
;;; wildcard-word ::= [word] * {word *}* [word]
;;; wildcard-inferiors ::= **
;;;
;;; A wildcard-word of * parses as :wild; all others as strings. These
;;; definitions may be extended (e.g., "newest" parsing as :newest) by
;;; defining new canonical types.
;;;
;;; Incompatibilities with the X3J13 specification:
;;;    -  LOGICAL-PATHNAME is not defined as a subclass of PATHNAME
;;;       since we have no guarrantee about the format of PATHNAME
;;;       (i.e., is it a defstruct or a class definition, what are
;;;       its slots, etc.). Many Lisps will be able to replace the
;;;       definition of PHYSICAL-PATHNAME with their definition of
;;;       PATHNAME by doing a string-replace of "physical-pathname"
;;;       with "pathname" and deleting some definitions from this file.
;;;    -  CLtL does not specify the manner in which wildcards are
;;;       translated. We use reversible wildcard pathname translation,
;;;       similar to that used in the Symbolics logical pathnames.
;;;    -  COMPILE-FILE-PATHNAME has not been defined, since it is
;;;       highly implementation dependent.

;;; ********************************
;;; Examples ***********************
;;; ********************************
;;;
;;; The following examples of the use of logical pathnames are taken
;;; from Section 23.1.5.4 of Guy Steele CLtL 2nd Ed.

#|
(setf (lp:physical-host-type "MY-LISPM") :symbolics)
(setf (lp:logical-pathname-translations "foo")
      '(("**;*.*.*" "MY-LISPM:>library>foo>**>")))

<cl> (lp:translate-logical-pathname "foo:bar;baz;mum.quux.3" :namestring)
"MY-LISPM:>library>foo>bar>baz>mum.quux.3"

(setf (lp:physical-host-type "U") :unix)
(setf (lp:physical-host-type "V") :vms)
(setf (lp:logical-pathname-translations "prog")
      '(("RELEASED;*.*.*"    "U:/sys/bin/my-prog/")
	("RELEASED;*;*.*.*"  "U:/sys/bin/my-prog/*/")
	("EXPERIMENTAL;*.*.*" "U:/usr/Joe/development/prog/")
	("EXPERIMENTAL;DOCUMENTATION;*.*.*" "V:SYS$DISK:[JOE.DOC]")
	("EXPERIMENTAL;*;*.*.*" "U:/usr/Joe/development/prog/*/")
	("MAIL;**;*.MAIL"       "V:SYS$DISK:[JOE.MAIL.PROG...]*.MBX")))

<cl> (lp:translate-logical-pathname "prog:mail;save;ideas.mail.3" :namestring)
"V:SYS$DISK:[JOE.MAIL.PROG.SAVE]IDEAS.MBX.3"
<cl> (lp:translate-logical-pathname "prog:experimental;spreadsheet.c" :namestring)
"U:/usr/Joe/development/prog/spreadsheet.c"

(setf (lp:logical-pathname-translations "prog")
      '(("CODE;*.*.*"    "/lib/prog/")))
<cl> (lp:translate-logical-pathname "prog:code;documentation.lisp" :namestring)
"/lib/prog/documentation.lisp"

(setf (lp:logical-pathname-translations "prog")
      '(("CODE;DOCUMENTATION.*.*"    "/lib/prog/docum.*")
	("CODE;*.*.*"    "/lib/prog/")))
<cl> (lp:translate-logical-pathname "prog:code;documentation.lisp" :namestring)
"/lib/prog/docum.lisp"


(setf (lp:logical-pathname-translations "prog")
      `(("**;*.LISP.*"  ,(lp:logical-pathname "PROG:**;*.L.*"))
	("**;*.FASL.*"  ,(lp:logical-pathname "PROG:**;*.B.*"))
	("CODE;DOCUMENTATION.*.*" "/lib/prog/documentatio.*")
	("CODE;*.*.*"             "/lib/prog/")))
<cl> (lp:translate-logical-pathname "prog:code;documentation.lisp" :namestring)
"/lib/prog/documentatio.l"

|#

;;; ****************************************************************
;;; Logical Pathnames **********************************************
;;; ****************************************************************

;;; ********************************
;;; Massage CLtL2 onto *features* **
;;; ********************************
;;; Let's be smart about CLtL2 compatible Lisps:
;;; hc: Let's not be.
;(eval-when (compile load eval)
;  #+(or (and :excl :allegro-v4.0) :mcl)
;  (pushnew :cltl2 *features*))

;;; ********************************
;;; Packages ***********************
;;; ********************************

;;; Putting this in a separate package doesn't prevent collisions
;;; with the LISP (:cl) package, since this package :uses the LISP
;;; package. 
;#-:cltl2
;(in-package "LOGICAL-PATHNAME" :nicknames '("LP"))
;;;; For CLtL2 compatible lisps...
;#+:cltl2
;(defpackage "LOGICAL-PATHNAME" (:nicknames "LP") (:use "COMMON-LISP"))
;#+:cltl2
;(in-package "LOGICAL-PATHNAME")

;; hc: For that reason we shadow the various colliding symbols and only
;; redefine them in the LISP package if we are explicitly asked to do so
;; (this can be done with a call to `redefine-standard-functions'):

(defvar *standard-pathname-functions* nil)
(defvar *standard-file-functions* nil)

;; Important: Use `cl:load', since `load' gets shadowed!
(eval-when (:load-toplevel :load-compile)

  (defun setup-logical-pathname-package ()
    ;; Does all the necessary package magic for the logical-pathname package.
    ;; Putting this into a function instead of doing it directly in the body
    ;; of the `eval-when' makes GCL happy and avoids warnings about
    ;; top-level package operations.
    (let (;; Standard pathname functions re/implemented by this package:
          (standard-pathname-functions
           '(#:logical-pathname-translations
             #:load-logical-pathname-translations
             #:logical-pathname
             #:translate-logical-pathname
             #:parse-namestring))

          ;; Standard file functions re/implemented by this package:
          (standard-file-functions
           '(#:load
             #:open
             #:probe-file
             #:delete-file
             #:truename
             #:directory
             #:dribble
             #:ed
             #:file-author
             #:file-write-date
             #:rename-file
             #:compile-file))

          ;; Bind default package for package operations below: 
          (*package* (find-package 'logical-pathname)))

      ;; Shadow them so they won't interfere with symbols that
      ;; are already available in the Lisp package:
      (shadow standard-pathname-functions)
      (shadow standard-file-functions)

      (dolist (function (append standard-pathname-functions
                                standard-file-functions))
	;; Make the symbol available and export it:
        (export (intern (sneps:build-namestring function)))

        ;; Save real definitions of various functions
        ;; in case we do redefine them:
        (let ((lisp-name (find-symbol (string function) :cl))
              (real-name (intern (sneps:build-namestring :real "-" function))))
          (and lisp-name
               (fboundp lisp-name)
               (not (fboundp real-name))
               (setf (symbol-function real-name)
                 (symbol-function lisp-name)))))

      ;; Export other useful functions:
      (export '(pathname-host-type
                append-logical-directories
                make-logical-pathname
                physical-host-type
                load-physical-hostab
                define-host-type
                redefine-standard-functions))

      ;; Set the constants used by `redefine-standard-functions':
      (setq *standard-pathname-functions* standard-pathname-functions)
      (setq *standard-file-functions* standard-file-functions)))

  ;; Now do it all:
  (setup-logical-pathname-package))

(pushnew :logical-pathnames-mk *features*)


;;; ********************************
;;; Global Variables ***************
;;; ********************************
(defvar *null-vector* (coerce nil 'simple-vector))

(defvar *warn-about-host-type-collisions* t
  "Warn user when a logical host type definition collides with a physical 
   host type definition.")

;;; ********************************
;;; Primitives *********************
;;; ********************************
(defun parse-with-string-delimiter (delim string &key (start 0) end)
  "Returns up to three values: the string up to the delimiter DELIM
   in STRING (or NIL if the field is empty), the position of the beginning
   of the rest of the string after the delimiter, and a value which, if
   non-NIL (:delim-not-found), specifies that the delimiter was not found."
  (declare (simple-string string))
  ;; Conceivably, if DELIM is a string consisting of a single character,
  ;; we could do this more efficiently using POSITION instead of SEARCH.
  ;; However, any good implementation of SEARCH should optimize for that
  ;; case, so nothing to worry about.
  (setq end (or end (length string)))
  (let ((delim-pos (search delim string :start2 start :end2 end))
	(dlength (length delim)))
    (cond ((null delim-pos)		
	   ;; No delimiter was found. Return the rest of the string,
	   ;; the end of the string, and :delim-not-found.
	   (values (subseq string start end) end :delim-not-found))
	  ((= delim-pos start)		
	   ;; The field was empty, so return nil and skip over the delimiter.
	   (values nil (+ start dlength)))
	  ;; The following clause is subsumed by the last cond clause,
	  ;; and hence should probably be eliminated.
;	  ((= delim-pos (- end dlength))
;	   ;; The delimiter is at the end of the string, so return the
;	   ;; field and skip to the end.
;	   (values (subseq string start delim-pos)
;		   end))
	  (t				
	   ;; The delimiter is in the middle of the string. Return the
	   ;; field and skip over the delimiter. 
	   (values (subseq string start delim-pos)
		   (+ delim-pos dlength))))))

(defun parse-with-string-delimiter* (delim string &key (start 0) end
					   include-last)
  "Breaks STRING into a list of strings, each of which was separated
   from the previous by DELIM. If INCLUDE-LAST is nil (the default),
   will not include the last string if it wasn't followed by DELIM
   (i.e., \"foo,bar,\" vs \"foo,bar\"). Otherwise includes it even if
   not terminated by DELIM. Also returns the final position in the string."
  (declare (simple-string string))
  (setq end (or end (length string)))
  (let (result)
    (loop
     (if (< start end)
	 (multiple-value-bind (component new-start delim-not-found)
	     (parse-with-string-delimiter delim string :start start :end end)
	   (when delim-not-found 
	     (when include-last
	       (setq start new-start)
	       (push component result))
	     (return))
	   (setq start new-start)
	   (push component result))
	 (return)))
    (values (nreverse result) 
	    start)))

(defun get-host-string (string &optional (host-delimiter ":") (start 0) end)
  "Strips the host name off the front of the string."
  (setq end (or end (length string)))
  (multiple-value-bind (host pos delim-not-found)
      (parse-with-string-delimiter host-delimiter string :start start :end end)
    (if delim-not-found
	(values nil start)
	(values host pos))))

(defun parallel-substitute (alist string)
  "Makes substitutions for characters in STRING according to the ALIST. 
   In effect, PARALLEL-SUBSTITUTE can perform several SUBSTITUTE
   operations simultaneously."
  (declare (simple-string string))
  ;; This function should be generalized to arbitrary sequences and
  ;; have an arglist (alist sequence &key from-end (test #'eql) test-not
  ;; (start 0) (count most-positive-fixnum) end key).
  (if alist
      (let* ((length (length string))
	     (result (make-string length)))
	(declare (simple-string result))
	(dotimes (i length)
	  (let ((old-char (schar string i)))
	    (setf (schar result i)
		  (or (second (assoc old-char alist :test #'char=))
		      old-char))))
	result)
      string))

(defun name-substitution (alist string)
  "Replaces STRING by it's replacement in ALIST, if present."
  (let ((new-string (second (assoc string alist :test #'string-equal))))
    (or new-string string)))

;; hc: The two calls to `nth-value' don't justify messing around with
;; defining a function that's available in some Lisps but not in others:
;(eval-when (compile)
;(unless (fboundp 'nth-value)
;  ;; NTH-VALUE is a CLtL2 addition, so not every lisp has it yet.
;  ;; This definition conses a lot, so we shouldn't use it in time-critical
;  ;; situations. It is fine for load-physical-hostab which is the only
;  ;; place we use it.
;  ;; We don't define it in the LISP package to avoid problems in Lucid
;  ;; with name conflicts.
;  (defmacro nth-value (n form)
;    "Returns the nth value of the values returned by form."
;    `(nth ,n (multiple-value-list ,form)))
;  ; (export 'cl::nth-value "LISP")   
;  )
;)

;;; ********************************
;;; Logical Host Tables ************
;;; ********************************
(defvar *logical-pathname-translations-table* (make-hash-table :test #'equal))
(defun canonicalize-logical-hostname (host)
  (string-upcase host))
(defun LOGICAL-PATHNAME-TRANSLATIONS (host)
  "If HOST is the host component of a logical pathname and has been defined
   as a logical pathname host name by SETF of LOGICAL-PATHNAME-TRANSLATIONS,
   this function returns the list of translations for the specified HOST.
   Each translation is a list of at least two elements, a from-wildname
   and a to-wildname. The former is a logical pathname whose host is the
   specified HOST. (I.e., the host of the from-pathname need not be 
   explicitly specified.) The latter is any pathname. If to-wildname coerces to
   a logical pathname, TRANSLATE-LOGICAL-PATHNAME will retranslate the
   result, repeatedly if necessary. Translations are listed in
   the order listed, so more specific from-wildnames must precede more
   general ones."
  ;; would be nice to have host:: specify logical host if physical host
  ;; already exists, to distinguish from host:
  (gethash (canonicalize-logical-hostname host)
	   *logical-pathname-translations-table*))
(defsetf logical-pathname-translations (host) (translations)
  "(setf (logical-pathname-translations host) translations) sets the list
   of translations for the logical pathname host to translations. If host
   is a string that has not previously been used as a logical pathname
   host, a new logical pathname host is defined; otherwise an existing
   host's translations are replaced. Logical pathname host names are
   compared with string-equal."
  `(progn
     (when (and *warn-about-host-type-collisions*
		(physical-host-type ,host))
       (format t "~&Warning in (SETF LOGICAL-PATHNAME-TRANSLATIONS):~
                  ~&   ~S is defined as both a physical host and a logical host."
	     ,host))
     (setf (gethash (canonicalize-logical-hostname ,host)
		    *logical-pathname-translations-table*)
	   ,translations)))

;;; ********************************
;;; Load Logical Translations ******
;;; ********************************
(defvar *logical-translations-directory* nil ; &&&
  "Directory where logical pathname translations are stored.")
;;; (setq *logical-translations-directory* "/usr/local/lisp/Registry/")

(defun LOAD-LOGICAL-PATHNAME-TRANSLATIONS (host)
  "Loads the logical pathname translations for host named HOST if the logical 
   pathname translations are not already defined. First checks for a file
   with the same name as the host (lowercase) and type \"translations\" in
   the current directory, then the translations directory. If it finds such
   a file it loads it and returns T, otherwise it signals an error."
  (unless (logical-pathname-translations host)
    (let* ((trans-fname (concatenate 'string (string-downcase host)
				     ".translations"))
	   (pathname (when *logical-translations-directory*
		       (merge-pathnames *logical-translations-directory*
					trans-fname))))
      (cond ((cl:probe-file trans-fname)
	     (cl:load trans-fname)
	     t)
	    ((and *logical-translations-directory*
		  (cl:probe-file pathname))
	     (cl:load pathname)
	     t)
	    (t
	     (error "Logical pathname translations for host ~A not found."
		    host))))))

;;; ********************************
;;; Physical Host Tables ***********
;;; ********************************
(defvar *physical-host-table* (make-hash-table :test #'equal)
  "Table of physical hosts and system types for those hosts. 
   Valid (implemented) types include :vms, :explorer, :symbolics, :unix.")
(defun physical-host-type (host)
  (gethash host *physical-host-table*))
(defsetf physical-host-type (host) (type)
  `(progn
     (when (and *warn-about-host-type-collisions*
		(logical-pathname-translations ,host))
       (format t "~&Warning in (SETF PHYSICAL-HOST-TYPE):~
                  ~&   ~S is defined as both a physical host and a logical host."
	       ,host))
     (setf (gethash ,host *physical-host-table*)
	   ,type)))

(defconstant local-host-table		; &&&
  #+:vms "chaos$root:[host.tables]nethosts.txt"
  #-:vms "nethosts.txt")

(defun load-physical-hostab (&optional (local-hostab local-host-table))
  "Loads the physical host namespace table. This is compatible with
   vms and symbolics host tables. Hostab line format should look
   something like:
      HOST NAME,CHAOS-#,STATUS,SYSTEM-TYPE,MACHINE-TYPE,NICKNAMES
   NAME and SYSTEM-TYPE are required; all others are optional (but delimiting
   commas are still required). SYSTEM-TYPE specifies the operating system
   run on the host. This information is used to figure out how to parse
   pathnames for the host. Common values are: LISP, LISPM, UNIX, MACH,
   VMS, and EXPLORER."
  ;; What about SITE, SHORT-NAME, USER-PROPERTY, ADDRESS, PRETTY-NAME, 
  ;; and other Symbolics host attributes?
  (when local-hostab
    (with-open-file (hostab local-hostab :direction :input)
      (do* ((host (read hostab nil :eof)(read hostab nil :eof))
	    ;; host should be NET or HOST.
	    (line (read-line hostab nil :eof)(read-line hostab nil :eof)))
	  ;; Exit on end of file.
	  ((or (eq host :eof)(eq line :eof)))
	;; For each line in the host table, do
	(cond ((null line)
	       (warn "Unexpected EOF in hostab ~S, exiting." local-hostab)
	       (return))
	      ((string-equal (symbol-name host) "HOST")
	       ;; Delete spaces and tabs.
	       (setq line (delete #\tab (delete #\space line)))
	       (let ((pos 0) name system machine nicknames delim-not-found
		     ignore)
		 ;; Snarf the machine NAME.
		 (multiple-value-setq (name pos)
		     (parse-with-string-delimiter "," line :start pos))
		 ;; Throw away chaos host numbers.
		 (multiple-value-setq (ignore pos)
		   (parse-with-string-delimiter 
		    (if (char-equal #\( (char line pos))
			")," ",")
		    line :start pos))
		 ;; Throw away status.
		 (multiple-value-setq (ignore pos)
		   (parse-with-string-delimiter "," line :start pos))
		 ;; Snarf the system and machine types.
		 (multiple-value-setq (system pos)
		     (parse-with-string-delimiter "," line :start pos))
		 (multiple-value-setq (machine pos delim-not-found)
		     (parse-with-string-delimiter "," line :start pos))
		 (when (and (not delim-not-found)
			    (> (length line) pos))
		   ;; Snarf the nicknames.
		   (setq nicknames
			 (parse-with-string-delimiter* 
			  ","
			  (parse-with-string-delimiter "]" line 
						       :start (1+ pos)))))
		 (unless (or (equal "" system) (null system))
		   (when (equal "LISP" system) (setq system machine))
		   (setq system (intern (sneps:build-namestring system) :keyword))
		   (case system
		     ;; :vms, :ms-dos, etc are left alone.
		     ((:mach :unix :unix42) (setq system :unix))
		     ((:lisp :lispm) (setq system :symbolics))
		     ((:appaloosa :explorer) (setq system :explorer)))
		   (setf (physical-host-type name) system)
		   (dolist (name nicknames)
		     (setf (physical-host-type name) system))))))))))

(defun host-type (host)
  "Returns the type of the host. If HOST is a defined logical pathname
   host (i.e., it has translations), returns :logical. Otherwise checks
   the physical type of the host. If HOST is NIL, uses the type of the
   default physical host (the one lisp is running in)."
  ;; Note that logical hosts have priority over physical hosts...
  ;; This is a bad situation, since we don't have any way of 
  ;; distinguishing between host names that are both logical and physical.
  ;; CLtL2 relies on the convention of naming them differently, but
  ;; collisions are going to occur. It would be better to have some
  ;; way of distinguishing the two in a pathname's printed representation.
  (cond ((multiple-value-bind (ignore present)
	     (logical-pathname-translations host)
	   ;; Yet another use for nth-value.
	   (declare (ignore ignore))
	   present)
	 :logical)
	((physical-host-type host))))

(defun pathname-host-type (pathname)
  (cond ((typep pathname 'logical-pathname) :logical)
	((typep pathname 'physical-pathname)
	 (host-type (physical-pathname-host pathname)))
	((stringp pathname) (host-type (get-host-string pathname ":")))))

;;; Setup Default Physical Host
(eval-when (:load-toplevel :execute)  ; &&&
(setf (physical-host-type nil)		; nil is default host
      (prog1
          #+:vms        :vms
	  #+:explorer   :explorer
	  #+:symbolics  :symbolics
	  #+:unix       :unix
	  #+:hp         :unix
	  #+:cmu        :unix
	  :unix				; default. change if necessary
	  ))
(setf (physical-host-type "Default")
      (physical-host-type nil))
)

;;; ********************************
;;; Host type information **********
;;; ********************************

;; hc: New access functions to store host type information in nested alists.
;; A nested alist is an alist of (SLOT VALUE) entries where each VALUE is
;; either an atom or a nested alist itself (see host type definitions below
;; for examples).

;; Single slot accessors:

(defun slot-equal (a b) (eq a b))       ;; slots are keywords
(defun value-equal (a b) (string= a b)) ;; values are strings

(defmacro get-slot-values (alist slot)
  ;; Get the values of SLOT in ALIST.
  ;; Clisp has a problem with writing #'slot-equal closures,
  ;; hence, we forget about the #:
  `(rest (assoc ,slot ,alist :test 'slot-equal)))

(defmacro get-slot-from-value (alist value)
  ;; Get name of first slot that has VALUE as one of its values in ALIST.
  `(first (find-if #'(lambda (entry)
		       (member ,value (rest entry) :test 'value-equal))
		   ,alist)))

(defun set-slot-values (alist slot values)
  ;; Define SLOT to have VALUES in ALIST.
  ;; This operation is destructive, it either modifies the values
  ;; of an already existing slot, or it adds a new entry to ALIST.
  (let* ((alist (or alist (list (cons slot values))))
	 (old-entry (assoc slot alist :test 'slot-equal)))
    (if old-entry
	(setf (rest old-entry) values)
      (setf (cdr (last alist)) (list (cons slot values))))
    alist))

(defun set-slot-values-from-alist (alist new-alist
				   &optional (significant-path-length 1))
  ;; Copies values from NEW-ALIST into ALIST.
  ;; If values in NEW-ALIST are (nested) alists themselves they are
  ;; recursed on instead of blindly replacing existing ALIST values.
  (dolist (entry new-alist alist)
    (setq alist
      (set-slot-values
       alist (first entry)
       (if (= significant-path-length 1)
	   (rest entry)
	 (set-slot-values-from-alist
	  (get-slot-values alist (first entry))
	  (rest entry)
	  (1- significant-path-length)))))))

;; Slot path accessors:

(defmacro get-slot-values* (alist &rest slot-path)
  ;; Gets the value in ALIST at the end of SLOT-PATH.
  (if (<= (length slot-path) 1)
      `(get-slot-values ,alist ,(car slot-path))
    `(get-slot-values
      (get-slot-values* ,alist ,@(butlast slot-path))
      ,(car (last slot-path)))))

(defmacro get-slot-from-value* (alist value &rest slot-path)
  (if (<= (length slot-path) 0)
      `(get-slot-from-value ,alist ,value)
    `(get-slot-from-value
      (get-slot-values* ,alist ,@slot-path)
      ,value)))

(defun set-slot-values* (alist values &rest slot-path)
  (set-slot-values
   alist (car slot-path)
   (if (<= (length slot-path) 1)
       values
     (apply #'set-slot-values*
	    (get-slot-values alist (car slot-path))
	    values
	    (cdr slot-path)))))

(defun set-slot-values-from-alist* (alist new-alist
				    &optional (significant-path-length 1)
				    &rest slot-path)
  (if (= (length slot-path) 0)
      (set-slot-values-from-alist alist new-alist significant-path-length)
    (apply #'set-slot-values*
	   alist
	   (set-slot-values-from-alist
	    (dolist (slot slot-path alist)
	      (setq alist (get-slot-values alist slot)))
	    new-alist
	    significant-path-length)
	   slot-path)))


;; Host types are defined with the following slots:
;;
;;    :translation-rule
;;    :canonicals
;;    :parser
;;    :generator
;;

;; A nested alist holding host-type specific information:
(defvar *host-types* nil)

(defmacro host-type-values (host-type-name &rest slot-path)
  `(get-slot-values* *host-types* ,host-type-name ,@slot-path))

(defmacro host-type-slot-from-value (host-type-name value &rest slot-path)
  `(get-slot-from-value*
    *host-types* ,value ,host-type-name ,@slot-path))

(defun set-host-type-values (host-type-name values &rest slot-path)
  (setq *host-types*
    (apply #'set-slot-values* *host-types* values host-type-name slot-path)))

(defun set-host-type-values-from-alist
    (host-type-name values-alist &optional (depth 1) &rest slot-path)
  (setq *host-types*
    (apply #'set-slot-values-from-alist*
	   *host-types* values-alist depth host-type-name slot-path)))

;; It is assumed that a host-type named `:default' is always defined
;; (if necessary one could have a more complicated mechanism where
;; host-type definitions specify from what host-types they are derived):
(defmacro find-host-type-values (host-type-name &rest slot-path)
  `(or (host-type-values ,host-type-name ,@slot-path)
       (host-type-values :default ,@slot-path)))

(defmacro find-host-type-slot-from-value (host-type-name value &rest slot-path)
  `(or (host-type-slot-from-value ,host-type-name ,value ,@slot-path)
       ;; otherwise, try the :default host...
       (let ((default-slot (host-type-slot-from-value
			    :default ,value ,@slot-path)))
	 ;; ...but only use it if the found slot does not have an explicit
	 ;; value in the definition of HOST-TYPE-NAME. This is how the
	 ;; original `define-canonical' says it is using the default,
	 ;; but it seems it isn't, i.e., it always uses the default
	 ;; if no match could be found in the definition of HOST-TYPE-NAME:
	 (and default-slot
	      (not (host-type-values ,host-type-name ,@slot-path default-slot))
	      default-slot))))

;;; ********************************
;;; Translation Rules **************
;;; ********************************

;; Define translations for a certain host-type. Since they will be updated
;; from definitions given in a disembodied property list format, it will be
;; easier to maintain them as such property lists rather than structures.

;; The following slot names are defined:
;;  :case				; Default case of pathname
;;  :char-mappings			; Character substitutions 
;;  :component-mappings			; String substitutions
;;  :version-case			; Case for version component
;;  :type-case				; Case for type component
;;  :name-case				; Case for name
;;  :component-case			; Case for directory names

;; :case may be :unchanged, :upper, :lower, or :capitalize. This provides a
;; default case translation; :version-case, :type-case, :name-case, and
;; :component-case will shadow this value if non-nil.
;; :char-mappings is a list of character substitutions which occur in parallel.
;; :component-mappings is a list of string substitutions.

(defmacro translation-rule-value (rule slot)
  `(first (get-slot-values ,rule ,slot)))


;;; ********************************
;;; Canonicals *********************
;;; ********************************

(defmacro canonical-to-surface-form-translations
    (host-type-name level canonical)
  `(find-host-type-values ,host-type-name :canonicals ,level ,canonical))

(defmacro surface-form-to-canonical-translation
    (host-type-name level surface-form)
  `(find-host-type-slot-from-value
    ,host-type-name ,surface-form :canonicals ,level))

	  
(defmacro define-host-type (host-type-name &rest entries)
  ;; Main vehicle for defining a host of type HOST-TYPE-NAME.
  ;; ENTRIES is a nested alist describing the particular features of this
  ;; host type (see definitions below for examples). Definitions are
  ;; cumulative, i.e., subsequent calls to `define-host-type' do not wipe
  ;; out previous definitions, rather they change the values supplied at
  ;; the leafs of the alist. For example, to change the canonical type of
  ;; binary files in a :unix host one can do
  ;;   (define-host-type :unix (:canonicals (:type (:fasl "MYFASL"))))
  ;; which will leave everything else intact.
  `(dolist (entry ',entries ',host-type-name)
     (case (car entry)
       (:translation-rule
	(set-host-type-values-from-alist
	 ',host-type-name (rest entry) 1 :translation-rule))
       (:canonicals
	(set-host-type-values-from-alist
	 ',host-type-name (rest entry) 2 :canonicals))
       (t (set-host-type-values ',host-type-name (rest entry) (car entry))))))

;; Conventions:
;; - all slots/canonicals/etc. are keywords
;; - all values/surface forms are strings

;; The host definitions below are a compilation of the various canonical
;; and translation rule definitions of the original. They could probably
;; need some updating. In particular, mapping a set of surface forms onto
;; a single canonical seems to be problematic, because mapping from surface
;; to canonical and back to surface is not necessarily an identity mapping.

(define-host-type :default
    (:canonicals (:host (:default "")
			(:default2 ""))
		 (:device (:unspecific ""))
		 (:component (:absolute "")
			     (:relative "")
			     (:wild "*")
			     (:wild-inferiors "**"))
		 (:name (:wild "*"))
		 (:type (:unspecific "")
			(:wild "*")
			(:lisp "LISP")
			(:text "TEXT")
			(:fasl "FASL"))
		 (:version (:wild "*")
			   (:newest "newest")))
    (:parser parse-unix-pathname)
    (:generator unix-namestring))

(define-host-type :logical
  (:translation-rule (:case :upper)
		     (:name-case :unchanged))
  (:canonicals (:component (:relative ";")))
  (:parser parse-logical-pathname)
  (:generator logical-namestring))

(define-host-type :unix
  (:translation-rule (:case :unchanged) ; :lower
		     (:type-case :lower))
  (:canonicals
   (:host (:default #+:CMU "Mach" "" "Default")
	  (:default2 nil "" "Default"))
   (:component (:absolute "/"))
   (:type (:lisp #+(and :sun (or :kcl :ibcl) :unix) "lsp"
		 "lisp"
		 "cl"   ;;;added by FLJ 6/26/04
		 ;; uncommenting the "L" causes the last
		 ;; Steele example to break, of course.
		 ;; "L"
		 #+:excl "cl")
	  (:text "text" "txt" "tx")
	  (:fasl #+:hp "b"
		 #+(and :sun (or :kcl :ibcl) :unix) "o"
		 #+:cmu "fasl"
		 "fasl" "bin" "BN")))
  (:parser parse-unix-pathname)
  (:generator unix-namestring))

(define-host-type :explorer
  (:canonicals (:type (:fasl "XLD")))
  (:parser parse-explorer-pathname)
  (:generator explorer-namestring))

(define-host-type :symbolics
  (:canonicals (:component (:absolute ">"))
	       (:type (:fasl "BIN")))
  (:parser parse-symbolics-pathname)
  (:generator symbolics-namestring))

(define-host-type :vms
  (:translation-rule (:case :upper)
		     (:char-mappings ((#\- #\_))))
  (:canonicals (:component (:relative ".")
			   (:wild-inferiors ".."))
	       (:type (:lisp "LSP" "LISP" "CL") ;;; added "CL" (FLJ 6/26/06)
		      (:text "TXT")
		      (:fasl "FAS" "BIN")))
  (:parser parse-vms-pathname)
  (:generator vms-namestring))

(define-host-type :tops-20
    (:canonicals (:type (:text "TXT")
			(:fasl "BIN"))))

(define-host-type :tenex
    (:canonicals (:type (:text "TXT")
			(:fasl "BIN"))))

;; still should have versions of `define-translation-rule' and
;; `define-canonical' for backward compatibility:


(defun choose-case (rule level)
  (or (case level
	(:version (translation-rule-value rule :version-case))
	(:type (translation-rule-value rule :type-case))
	(:name (translation-rule-value rule :name-case))
	(:component (translation-rule-value rule :component-case)))
      (translation-rule-value rule :case)))

(defun casify (thing case)
  (if (stringp thing)
    (case case
      (:upper (string-upcase thing))
      (:lower (string-downcase thing))
      (:capitalize (string-capitalize thing))
      (:unchanged thing)
      (otherwise thing))
    thing))

(defun surface-form (canonical host-type &optional (level :type))
  "Given the canonical form of some canonical type, replaces it with
   the appropriate surface form."
  (casify (or (first (canonical-to-surface-form-translations
		      host-type level canonical))
	      canonical)
	  (choose-case
	   (find-host-type-values host-type :translation-rule) level)))

(defun canonicalize (surface-form host-type &optional (level :type))
  "Given the surface form of some canonical type, replaces it with
   the appropriate canonical type."
  (cond ((stringp surface-form)
	 (or (surface-form-to-canonical-translation
	      host-type level surface-form)
	     (coerce surface-form 'simple-string)))
	(t surface-form)))


#|
;;; Examples:
<cl> (lp::canonicalize "*" :unix)
:WILD
<cl> (lp::surface-form :fasl :unix)
"fasl"
<cl> (lp::surface-form :fasl :vms)
"FAS"
|#


;;; ********************************
;;; Pathname Defstruct *************
;;; ********************************
;;;
;;; We define a generic physical pathname (physical-pathname defstruct) because
;;; we have absolutely NO guarrantees about the structure of pathnames.
;;; Pathnames may be defstructs or classes, and the slots may have arbitrary
;;; types, especially with respect to the directory slot. Depending on the
;;; lisp, the directory slot may be a list, vector, simple-vector,
;;; string, keyword, or nil. If a list or vector, the items in the list
;;; may be strings, keywords (for canonical types), or nil. The first item
;;; in the list may or may not be a special keyword (e.g., :relative and
;;; :absolute). 
;;;
;;; The lack of a common interface to pathnames means that any implementation
;;; of logical pathnames must parse and generate the pathname (namestring)
;;; formats for a variety of file-servers. We can't simply rely on the
;;; lisp's implementation of the PATHNAME defstruct, because that does not
;;; necessarily handle the formats of file-servers of a different type 
;;; (translations may be in the format of the target file server). Also,
;;; inconsistency in the implementation of the PATHNAME type means that we
;;; would have to special case most of the code for each and every lisp.
;;;
;;; Instead, we parse the pathnames into a common format (the physical-pathname
;;; defstruct), from which we generate a namestring in a format acceptable
;;; to the underlying lisp. The namestring (which is a string in *all* the
;;; lisps) serves as the interface to the lisp's implementation of pathnames.
;;;
;;; As it currently stands, X3J13's spec for logical pathnames tries to 
;;; accomplish two distinct goals:
;;;    (1) isolate pathname reference from actual file location (logical
;;;        as opposed to physical pathnames) 
;;;    (2) provide a common format for namestring syntax and 
;;;        pathname structure
;;; This is trying to accomplish too much within a single framework. Instead,
;;; the second goal should be decoupled from logical pathnames and made a 
;;; requirement for pathnames in general. 
;;;
;;; In other words, let there be a standard namestring syntax and a fully
;;; specified structure for physical pathnames (not just logical pathnames).
;;; This standard should subsume the requirements of all current lisps, and
;;; the individual lisp implementation should worry about interfacing with
;;; the file system. There is no good reason why a programmer should have
;;; to know the peculiarities of a filesystem when writing software. The X3J13
;;; spec just shoves it under the rug, forcing the programmer to deal with
;;; it when writing the translations file. 
;;;
;;; Because there is no standard for pathnames, we're forced into a situation
;;; where different lisps running on the same physical host may have
;;; different namestring syntaxes, so knowing the physical host type is not
;;; a guarrantee of the pathname syntax. 
;;;
(defstruct (physical-pathname
	    (:conc-name %physical-pathname-)
	    (:print-function %print-physical-pathname)
	    (:constructor %make-physical-pathname 
	     (host device directory name type version))
	    (:predicate physical-pathnamep))	     
  "Physical-Pathname is the underlying structure for a pathname."
  (host nil :type (or null keyword simple-string))
  (device nil :type (or null keyword simple-string))
  (directory nil :type (or null simple-vector))
  (name nil :type (or null keyword simple-string))
  (type nil :type (or null keyword simple-string))
  version)

(defun %print-physical-pathname (pname stream depth)
  (declare (ignore depth))
  (format stream "#.(physical-pathname ~S)" (physical-namestring pname)))

(defun make-physical-pathname (&key host device directory name type version)
  (let ((host-type (host-type host)))
    (when (stringp directory)
      (setq directory 
	    (%physical-pathname-directory (parse-generic-namestring directory
								    host))))
    (%make-physical-pathname 
     (canonicalize host host-type :host)
     (canonicalize device host-type :device)
     directory
     (canonicalize name host-type :name)
     (canonicalize type host-type :type)
     (canonicalize version host-type :version)
     )))

(defun ensure-physical-pathname (thing)
  (if (physical-pathnamep thing) thing (physical-pathname thing)))

;;; The following cannot be done by the accessors because the pathname
;;; arg may be a string.

(defun physical-pathname-host (pathname)
  "Returns the host of PATHNAME, which may be a string or pathname."
  (%physical-pathname-host (ensure-physical-pathname pathname)))

(defun physical-pathname-device (pathname)
  "Returns the device of PATHNAME, which may be a string or pathname."
  (%physical-pathname-device (ensure-physical-pathname pathname)))

(defun physical-pathname-directory (pathname)
  "Returns the directory of PATHNAME, which may be a string or pathname."
  (%physical-pathname-directory (ensure-physical-pathname pathname)))

(defun physical-pathname-name (pathname)
  "Returns the name of PATHNAME, which may be a string or pathname."
  (%physical-pathname-name (ensure-physical-pathname pathname)))

(defun physical-pathname-type (pathname)
  "Returns the type of PATHNAME, which may be a string or pathname."
  (%physical-pathname-type (ensure-physical-pathname pathname)))

(defun physical-pathname-version (pathname)
  "Returns the version of PATHNAME, which may be a string or pathname."
  (%physical-pathname-version (ensure-physical-pathname pathname)))

;;; ********************************
;;; Logical Pathname Defstruct *****
;;; ********************************
(defstruct (logical-pathname
	    (:include physical-pathname)
	    (:conc-name %logical-pathname-)
	    (:print-function %print-logical-pathname)
	    (:constructor %make-logical-pathname 
			  (host device directory name type version))
	    (:predicate logical-pathnamep))	     
  "Logical-pathname is the underlying structure for a logical pathname.")

(defun %print-logical-pathname (pname stream depth)
  (declare (ignore depth))
  (format stream "#.(logical-pathname ~S)" (logical-namestring pname)))

(defun make-logical-pathname (&key host directory name type version)
  (let ((host-type (host-type host)))
    (when (stringp directory)
      (setq directory 
	    (%logical-pathname-directory (parse-generic-namestring directory
								   host))))
    (%make-logical-pathname 
     (canonicalize host host-type :host)
     :unspecific
     directory
     (canonicalize name host-type :name)
     (canonicalize type host-type :type)
     (canonicalize version host-type :version)
     )))

(defun ensure-logical-pathname (thing)
  (if (logical-pathnamep thing) thing (logical-pathname thing)))

;;; The following cannot be done by the accessors because the pathname
;;; arg may be a string.

(defun logical-pathname-host (logical-pathname)
  "Returns the logical-pathname-host of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-host (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-directory (logical-pathname)
  "Returns the logical-pathname-directory of LOGICAL-PATHNAME.
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-directory (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-name (logical-pathname)
  "Returns the logical-pathname-name of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-name (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-type (logical-pathname)
  "Returns the logical-pathname-type of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-type (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-version (logical-pathname)
  "Returns the logical-pathname-type of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-version (ensure-logical-pathname logical-pathname)))


;;; ********************************
;;; Pathname Namestring Functions **
;;; ********************************
(defun logical-namestring (logical-pathname)
  "Returns the full form of LOGICAL-PATHNAME as a string."
  (setq logical-pathname (logical-pathname logical-pathname))
  (let ((host      (%logical-pathname-host logical-pathname))
	(directory (%logical-pathname-directory logical-pathname))
	(name      (%logical-pathname-name logical-pathname))
	(type      (%logical-pathname-type logical-pathname))
	(version   (%logical-pathname-version logical-pathname))
	(result ""))
    (declare (simple-string result))
    ;; FORMAT would have been easier, but this is faster.
    (when host
      (setq result 
	    (concatenate 'simple-string 
			 (surface-form host :logical :host) ":")))
    (when directory
      (setq result
	    (concatenate 'simple-string 
			 result
			 (the simple-string (%directory-string directory)))))
    (when name
      (setq result
	    (concatenate 'simple-string
			 result
			 (the simple-string (surface-form name :logical :name)))))
    (when type
      (setq result
	    (concatenate 'simple-string
			 result "."
			 (the simple-string (surface-form type :logical :type)))))
    (when version
      (setq result
	    (concatenate 'simple-string
			 result "."
			 (the simple-string
			      (%version-to-string version)))))
    result))

(defun %directory-string (dirlist &optional (host-type :logical)
				  (dir-delim #\;))
  "Converts a vector of the form #(\"foo\" \"bar\" ... \"baz\") into
   a string of the form \"foo;bar;...;baz;\""
  (declare (simple-vector dirlist))
  (let* ((numdirs (length dirlist))
	 (length numdirs))
    (declare (fixnum numdirs length))
    (dotimes (i numdirs)
      (let ((component (#+:cmu svref #-:cmu aref dirlist i)))
	(case component
	  ;; Do we have to worry about Lucid's :root here???
	  ((:relative :absolute) 
	   (incf length
		 (the fixnum
		      (1- (length (surface-form component
						host-type :component))))))
	  (otherwise (incf length
			   (the fixnum
				(length (surface-form component host-type
						      :component))))))))
    (do ((result (make-string length))
	 (index 0 (1+ index))
	 (position 0))
	((= index numdirs) result)
      (declare (simple-string result))
      (let* ((component (#+:cmu svref #-:cmu aref dirlist index))
	     (string (surface-form component host-type :component))
	     (len (length string))
	     (end (+ position len)))
	(declare (simple-string string)
		 (fixnum len end))
	(replace result string :start1 position :end1 end :end2 len)
	(unless (or (eq component :absolute)(eq component :relative))
	  (setf (schar result end) dir-delim)
	  (setq position (+ end 1)))))))

(defun %version-to-string (version &optional (host-type :logical))
  (cond ((surface-form version host-type :version))
	((zerop version) "0")
	((eql version 1) "1")
	(t
	 ;; Using FORMAT would have been easier, but this is faster.
	 (do* ((len (1+ (truncate (log version 10)))) ; base 10 num digits
	       (res (make-string len))
	       (i (1- len) (1- i))
	       (q version) ; quotient
	       (r 0)) ; residue
	     ((zerop q) ; nothing left
	      res)
	   (declare (simple-string res)
		    (fixnum len i r))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))

(defun surfaced-pathname (pathname)
  ;; needs to get appropriate surface forms
  (setq pathname (physical-pathname pathname))
  (let* ((host-type (host-type (%physical-pathname-host pathname))))
    (values
     (surface-form (%physical-pathname-host pathname) host-type :host)
     (surface-form (%physical-pathname-device pathname) host-type :device)
     ;; Does directory need to be mapcar'ed into surface-form?
     ;; Yes, but we can probably ignore it for now, since the only
     ;; canonical types defined so far are :wild and :wild-inferiors,
     ;; which we don't have to support. Probably wouldn't hurt to
     ;; uncomment this code.
     ;; (cons (car directory)
     ;;       (mapcar #'(lambda (comp)
     ;;		          (surface-form comp host-type :component))
     ;;			(cdr directory)))
     (coerce (%physical-pathname-directory pathname) 'list)
     (surface-form (%physical-pathname-name pathname) host-type :name)
     (surface-form (%physical-pathname-type pathname) host-type :type)
     (surface-form (%physical-pathname-version pathname) host-type :version))))

(defun unix-namestring (pathname)
  (multiple-value-bind (host device directory name type version)
      (surfaced-pathname pathname)
    (declare (ignore device))
    (format nil "~@[~A:~]~A~{~A/~}~@[~A~@[.~A~@[.~A~]~]~]"
	    host (case (car directory)
		   (:absolute "/")
		   (otherwise ""))
	    (cdr directory)
	    name type version)))

(defun vms-namestring (pathname)
  (multiple-value-bind (host device directory name type version)
      (surfaced-pathname pathname)
    ;; was "~@[~A:~]~@[~A:~][~A~{~A.~}]~@[~A~@[.~A~@[.~A~]~]~]"
    ;; which was adding an extra "." to path
    ;; such as [a.b] => [a.b.]
    (format nil 
	    "~@[~A:~]~@[~A:~][~A~{~A~^.~}]~@[~A~@[.~A~@[.~A~]~]~]"
	    host device (case (car directory)
			  (:relative ".")
			  (otherwise ""))
	    (cdr directory)
	    name type version)))

(defun explorer-namestring (pathname)
  (multiple-value-bind (host device directory name type version)
      (surfaced-pathname pathname)
    (declare (ignore device))
    (format nil "~@[~A:~]~A~{~A~^.~};~@[~A~@[.~A~@[#~A~]~]~]"
	    host (case (car directory)
		   (:relative ".")
		   (otherwise ""))
	    (cdr directory)
	    name type version)))

(defun symbolics-namestring (pathname)
  (multiple-value-bind (host device directory name type version)
      (surfaced-pathname pathname)
    (declare (ignore device))
    (format nil "~@[~A:~]~A~{~A>~}~@[~A~@[.~A~@[.~A~]~]~]"
	    host (case (car directory)
		   (:absolute ">")
		   (otherwise ""))
	    (cdr directory)
	    name type version)))

(defun physical-namestring (pathname)
  (funcall (first (find-host-type-values
		   (pathname-host-type pathname) :generator))
	   pathname))

;;; ********************************
;;; Pathname Parsing Functions *****
;;; ********************************
(defvar *default-logical-pathname-host* nil ;;"SYS"
  "Default logical pathname host, so that the logical pathname
   foo;bar means SYS:foo;bar.")

(defun logical-pathname (thing 
			 &optional (host *default-logical-pathname-host*))
  "Converts THING to a logical pathname and returns it. THING may be
   a logical pathname, a logical pathname namestring containing a 
   host component, or a stream for which the pathname function returns
   a logical pathname."
  (etypecase thing
    (string
     (values (parse-generic-namestring thing host
				       *default-pathname-defaults* 
				       :force-logical t)))
    (physical-pathname thing)
    (logical-pathname thing)
    #+:CMU(stream           (logical-pathname (cl::file-name thing) host))))

(defun physical-pathname (thing &optional host)
  "Converts THING to a physical-pathname and returns it. THING may be
   a pathname, a pathname namestring containing a 
   host component, or a stream for which the file-name function returns
   a pathname."
  (typecase thing
    (string           (values (parse-generic-namestring thing host)))
    (logical-pathname thing)
    (physical-pathname thing)
    #+:CMU(stream           (physical-pathname (cl::file-name thing) host))))

(defun parse-generic-namestring (thing &optional host
				       (defaults *default-pathname-defaults*)
				       &key (start 0) end junk-allowed
				       force-logical)
  "Convert namestring into a pathname."
  (declare (ignore junk-allowed))
  (unless end (setf end (length thing)))
  (let ((host-string (get-host-string thing ":"))
	host-type)
    (unless host-string (setq host-string host))
    (when (and host host-string (not (string-equal host host-string)))
      (cerror "Ignore it."
	      "Host mismatch in ~S: ~S isn't ~S"
	      'parse-generic-namestring
	      host-string
	      host))
    (if force-logical
	(setq host-type :logical)
      (setq host-type (host-type host-string)))
    (if host-type
	(multiple-value-bind (parsed-host device directory name type version)
	    (funcall (first (find-host-type-values host-type :parser))
		     thing start end)
	  (let ((defaults-p (and (typep defaults 'physical-pathname)
				 (equal host-type
					(pathname-host-type defaults)))))
	    (values
	     (case host-type
	       (:logical
		(make-logical-pathname 
		 :host	(or parsed-host host
			    (and defaults-p (logical-pathname-host defaults))
;			    (when directory "Default")
			    )
		 :directory (or directory
				(and defaults-p
				     (logical-pathname-directory defaults)))
		 :name      (or name
				(and defaults-p
				     (logical-pathname-name defaults)))
		 :type      (or type 
				(and defaults-p
				     (logical-pathname-type defaults)))
		 :version   (or version 
				(and defaults-p
				     (logical-pathname-version defaults)))))
	       (otherwise 
		(make-physical-pathname 
		 :host	(or parsed-host host
			    (and defaults-p (physical-pathname-host defaults))
;			    (when directory "Default")
			    )
		 :device    (or device
				(and defaults-p
				     (physical-pathname-device defaults)))
		 :directory (or directory
				(and defaults-p
				     (physical-pathname-directory defaults)))
		 :name      (or name
				(and defaults-p
				     (physical-pathname-name defaults)))
		 :type      (or type
				(and defaults-p
				     (physical-pathname-type defaults)))
		 :version   (or version
				(and defaults-p
				     (physical-pathname-version defaults))))))
	     end)))
	;; Unknown host type, wing it with parse-namestring.
      (when thing
	(real-parse-namestring thing host defaults 
			       :start start :end end)))))

;;; ********************************
;;; Parse Physical Pathnames *******
;;; ********************************

;; These functions split a string into a logical host, a vector of directories,
;; a file name, a file type, and a file version."

(defun parse-generic-pathname (string &optional (start 0) end
				      (host-delim ":")(lead-is-abs t)
				      (dir-delim "/")
				      (name-delim ".")(type-delim ".")
				      (version-delim "."))
  "Splits string into a host, vector of directories, a file name, type,
   and version. Parses generic pathnames."
  (declare (simple-string string))
  (setq end (or end (length string)))
  (let (host a-vs-r directories name type version host-type)
    (multiple-value-setq (host start)
	(get-host-string string host-delim start end))
    (setq host-type (host-type host))
    ;; Absolute vs. Relative
    (cond ((and (not (string-equal string "" :start1 start))
		(char= (char dir-delim 0) (char string start))) 
	   (setq a-vs-r (if lead-is-abs :absolute :relative))
	   (incf start))
	  (t (setq a-vs-r (if lead-is-abs :relative :absolute))))
    ;; Split off the components
    (multiple-value-bind (dirs new-start)
	(parse-with-string-delimiter* dir-delim string :start start :end end)
      (setq directories
	    (cons a-vs-r 
		  (mapcar #'(lambda (dir)
			      (canonicalize dir host-type :component))
			  dirs))
	    start new-start))
    ;; Split off the name, type, and version
    (when (< start end)
      (multiple-value-setq (name start)
	  (parse-with-string-delimiter name-delim string
				       :start start :end end))
      (when (< start end)
	(multiple-value-setq (type start)
	    (parse-with-string-delimiter type-delim string
					 :start start :end end))
	(when (< start end)
	  (multiple-value-setq (version start)
	      (parse-with-string-delimiter version-delim string
					   :start start :end end)))))
    ;; Return the values
    (values host
	    :unspecific
	    (when (or host directories)
	      (coerce directories 'vector))
	    name
	    type
	    version
	    ;; This last is the remaining cruft. Should be nil.
	    (when (< start end) (subseq string start end)))))

(defun parse-logical-pathname (string &optional (start 0) end)
  (declare (simple-string string))
  ;; Parses Logical Pathnames of the following format:
  ;;     host:dir1;dir2;name.type.version
  (parse-generic-pathname string start end ":" nil ";" "." "." "."))

(defun parse-unix-pathname (string &optional (start 0) end)
  (declare (simple-string string))
  ;; Parses Unix pathnames of the following format:
  ;;     host:/dir1/dir2/*/name.type.version 
  (parse-generic-pathname string start end ":" t "/" "." "." "."))

(defun parse-symbolics-pathname (string &optional (start 0) end)
  (declare (simple-string string))
  ;; Parses Symbolics Pathnames of the following format:
  ;;     host:>dir1>dir2>**>name.type.version
  (parse-generic-pathname string start end ":" t ">" "." "." "."))

(defun parse-vms-pathname (string &optional (start 0) end)
  "Splits string into a host, vector of directories, a file name, type,
   and version. Parses VMS pathnames of the following formats:
       host::device:[dir1.dir2...]name.type;version
       host::device:<dir1.dir2...>name.type.version
       host:device:<dir1.dir2...>name.type.version &c
   .. = :wild-inferiors"
  (declare (simple-string string))
  (setq end (or end (length string)))
  (let (host device a-vs-r (directories "") name type version)
    (multiple-value-bind (new-host new-start)
	(get-host-string string "::" start end)
      (if new-host
	  (setq host new-host start new-start)
	  (multiple-value-setq (host start) (get-host-string string ":" start end))))
    (multiple-value-setq (device start)	(get-host-string string ":" start end))
    (when (plusp (length string))
      (case (char string start)
	(#\[  (multiple-value-setq (directories start)
		  (parse-with-string-delimiter "]" string 
					       :start (1+ start) :end end)))
	(#\<  (multiple-value-setq (directories start)
		  (parse-with-string-delimiter ">" string 
					       :start (1+ start) :end end)))))
    ;; Absolute vs. Relative
    (cond ((and (not (zerop (length directories)))
		(char= #\. (char directories 0)))
	   (setq a-vs-r :relative))
	  (t (setq a-vs-r :absolute)))
    ;; Split off the components
    (multiple-value-bind (dirs)
	(parse-with-string-delimiter* "." directories 
				      :start (if (eq a-vs-r :relative) 1 0)
				      :include-last t) ; <<< fix
      (let ((last2 (when (> (length dirs) 1)
		     (nthcdr (- (length dirs) 2) dirs))))
	(when (equal last2 '(nil nil))
	  (rplaca last2 "..")
	  (rplacd last2 nil)))
      (setq directories
	    (cons a-vs-r 
		  (mapcar #'(lambda (dir) (canonicalize dir :vms :component))
			  dirs))))
    ;; Split off the name, type, and version
    (when (< start end)
      (multiple-value-setq (name start)
	  (parse-with-string-delimiter "." string :start start :end end))
      (when (< start end)
	(multiple-value-bind (new-type new-start delim-not-found)
	    (parse-with-string-delimiter ";" string :start start :end end)
	  (cond (delim-not-found
		 (multiple-value-setq (type start)
		     (parse-with-string-delimiter "." string
						  :start start :end end)))
		(t
		 (setq type new-type start new-start))))
	(when (< start end)
	  (multiple-value-setq (version start)
	      (parse-with-string-delimiter "." string :start start :end end)))))
    ;; Return the values
    (values host
	    device
	    (when (or host directories)
	      (coerce directories 'vector))
	    name
	    type
	    version
	    ;; This last is the remaining cruft. Should be nil.
	    (when (< start end) (subseq string start end)))))

(defun parse-explorer-pathname (string &optional (start 0) end)
  "Splits string into a host, vector of directories, a file name, type,
   and version. Parses TI Explorer pathnames of the following format:
       host:dir1.dir2...;name.type#version"
  (declare (simple-string string))
  (setq end (or end (length string)))
  (let (host a-vs-r (directories "") name type version)
    (multiple-value-setq (host start)
	(get-host-string string ":" start end))
    (multiple-value-setq (directories start)
	(parse-with-string-delimiter ";" string 
				     :start start :end end))
    ;; Absolute vs. Relative
    (cond ((and (not (zerop (length directories)))
		(char= #\. (char directories 0)))
	   (setq a-vs-r :relative))
	  (t (setq a-vs-r :absolute)))
    ;; Split off the components
    (multiple-value-bind (dirs)
	(parse-with-string-delimiter* "." directories 
				      :start (if (eq a-vs-r :relative) 1 0)
				      :end nil :include-last t)

      (setq directories
	    (cons a-vs-r 
		  (mapcar #'(lambda (dir)
			      (canonicalize dir :explorer :component))
			  dirs))))
    ;; Split off the name, type, and version
    (when (< start end)
      (multiple-value-bind (new-name new-start delim-not-found)
	  (parse-with-string-delimiter "." string :start start :end end)
	(when (not delim-not-found)
	  (setq name new-name start new-start)))
      (when (< start end)
	(multiple-value-setq (type start)
	    (parse-with-string-delimiter "#" string :start start :end end))
	(when (< start end)
	  (multiple-value-setq (version start)
	      (parse-with-string-delimiter "." string :start start :end end)))))
    ;; Return the values
    (values host
	    :unspecific
	    (when (or host directories)
	      (coerce directories 'vector))
	    name
	    type
	    version
	    ;; This last is the remaining cruft. Should be nil.
	    (when (< start end) (subseq string start end)))))

(defun parse-unimplemented (string &rest ignore)
  (declare (ignore ignore))
  (warn "Cannot yet parse `%a'." string))

;;; ********************************
;;; Convert Generic Pathnames ******
;;; ********************************
;;; Converts a generic pathname to a format for standard lisp functions.

(defvar *translation-output* :namestring
  "Specifies whether the output of translate-logical-pathname
   should be a :namestring or a :pathname made with cl:make-pathname,
   or :as-is.")

(defconstant directory-structure-type ; &&&
  #+:CMU 'simple-vector
  #+:lispm 'list
  #+:kcl   'list
  #+:hp    'list
  #-(or :cmu :lispm :kcl :hp)
  (cond ((string-equal (lisp-implementation-type) "VAX LISP") 'list)
	(t 'list)))

(defun convert-generic-pathname (pathname 
				 &optional (output-type *translation-output*))
  (when pathname
    (case output-type 
      (:namestring        (physical-namestring pathname))
      (:pathname
       (let ((host       (%physical-pathname-host pathname))
	     (device     (%physical-pathname-device pathname))
	     (directory  (coerce (%physical-pathname-directory pathname)
				 'list))
	     (name       (%physical-pathname-name pathname))
	     (type       (%physical-pathname-type pathname))
	     (version    (%physical-pathname-version pathname))
	     (target-host-type  (host-type nil))
	     a-vs-r)
	 ;; Handle :absolute/:relative crap.
	 (setq a-vs-r (pop directory))
	 (case a-vs-r
	   (:absolute
	    #+:cmu (setf device :absolute)
	    #+(and :sun :kcl :unix) (setq a-vs-r :root))
	   (:relative
	    #+:cmu (setf device "Default")))
	 ;; Reverse canonicalizations
	 (setq host (surface-form host target-host-type :host)
	       directory (mapcar #'(lambda (dir)
				     (surface-form dir target-host-type
						   :component))
				 directory)
	       name (surface-form name target-host-type :name)
	       type (surface-form type target-host-type :type)
	       version (surface-form version target-host-type :version))
	 ;; Fixup Host
	 #+:cmu (setf host "Mach")
	 ;; Fixup Directory
	 #-:cmu (push a-vs-r directory)
	 (setq directory (coerce directory directory-structure-type))
       
	 (when (string-equal (lisp-implementation-type) "VAX LISP")
	   (setq directory
		 (cond ((stringp directory) directory)
		       ((eq (car directory) :absolute)
			(format nil "[~{~A~^.~}]" (cdr directory)))
		       ((eq (car directory) :relative)
			(format nil "[.~{~A~^.~}]" (cdr directory)))
		       (t (format nil "[~{~A~^.~}]" directory)))))

	 ;; Return the new pathname
	 (make-pathname :host host :device device :directory directory
			:name name :type type :version version)
	 ))
      (otherwise pathname)))) 


;;; ********************************
;;; Translate Logical Pathnames ****
;;; ********************************
(defvar *circularity-check-table* (make-hash-table :test #'equal)
  "This table is used to prevent infinite circular loops in the logical
   pathname resolution. If a pathname's entry in this table is set
   to T, it has already been \"seen\". Seeing such a pathname twice
   is an error.")

(defun translate-logical-pathname (logical-pathname
				   &optional
				   (output-format *translation-output*))
  "Translates a logical pathname to the corresponding physical pathname.
   The pathname argument is first coerced to a logical pathname [this 
   should really be pathname, but for that we'd have to redefine
   make-pathname and friends to check whether the host is a logical host].
   If the coerced argument is a logical pathname, the first matching
   translation (according to LOGICAL-PATHNAME-MATCH-P) of the logical pathname
   host is applied, as if by calling TRANSLATE-LOGICAL-PATHNAME-AUX.
   If the result is a logical pathname, this process is repeated. 
   When the result is finally a physical pathname, it is returned. If no 
   translation matches a logical pathname, or the resolution process loops,
   an error is signaled.

   TRANSLATE-LOGICAL-PATHNAME may perform additional translations,  
   to provide translation of file types to local naming conventions, to
   accommodate physical file systems with names of limited length, or to
   deal with special character requirements such as translating hyphens
   to underscores or uppercase letters to lowercase."

  ;; Ensure that it is a logical pathname
  (setq logical-pathname (logical-pathname logical-pathname))
  (when (typep logical-pathname 'logical-pathname)
    ;; To prevent circular loops...
    (let ((namestring (logical-namestring logical-pathname)))
      (setf (gethash namestring *circularity-check-table*) T))
    (unwind-protect
	(resolve-logical-pathname logical-pathname output-format)
      (clrhash *circularity-check-table*))))

(defun resolve-logical-pathname (logical-pathname 
				 &optional
				 (output-format *translation-output*))
  "Resolve the logical pathname into a physical pathname using the
   translations table."
  (let ((logical-host (logical-pathname-host logical-pathname)))
    (if logical-host
	(let ((translated-pathname 
	       (map-logical-pathname logical-pathname logical-host
				     output-format)))
	  (if translated-pathname
	      (or (when (eq (pathname-host-type translated-pathname) :logical)
		    ;; If the translation is itself a logical pathname,
		    ;; repeat the process until a physical pathname is reached.
		    (check-logical-pathname translated-pathname)
		    (resolve-logical-pathname translated-pathname 
					      output-format))
		  translated-pathname)
	      (error "No translation mapping for ~S." logical-pathname)))
	(error "No such logical host in ~S." logical-pathname))))

(defun check-logical-pathname (pathname)
  "Ensure that there are no cycles in the translations."
  (let ((namestring (logical-namestring pathname)))
    (if (gethash namestring *circularity-check-table*) 
	(error "Circularity in translations for ~S." namestring)
	(setf (gethash namestring *circularity-check-table*) T))))

(defun map-logical-pathname (logical-pathname 
			     host
			     &optional (output-format *translation-output*))
  "Find and execute the first matching translation."
  (dolist (translation (logical-pathname-translations host))
    (let ((from-pathname (logical-pathname (car translation) host))
	  (to-pathname   (cadr translation)))
      (when (logical-pathname-match-p logical-pathname from-pathname)
	(return (translate-logical-pathname-aux logical-pathname
						from-pathname
						to-pathname
						output-format))))))

(defun logical-pathname-match-p (logical-pathname from-pathname)
  "Return T if the logical pathname matches the test pathname."
  (setq logical-pathname (logical-pathname logical-pathname)
	from-pathname (logical-pathname from-pathname))
  ;; ignore host. Match directories. Match name. Match type. Match version.
  (and (match-directories (logical-pathname-directory from-pathname)
			  (logical-pathname-directory logical-pathname))
       (match-wildcard-word (logical-pathname-name from-pathname)
			    (logical-pathname-name logical-pathname))
       (match-wildcard-word (logical-pathname-type from-pathname)
			    (logical-pathname-type logical-pathname))
       (match-wildcard-word (logical-pathname-version from-pathname)
			    (logical-pathname-version logical-pathname))))  

(defun translate-logical-pathname-aux (logical-pathname 
				       from-pathname to-pathname
				       &optional
				       (output-format *translation-output*))
  "Translates the logical pathname using the substitution specified by
   a particular translation."
  (let* ((host (physical-pathname-host to-pathname))
	 (host-type (host-type host))
	 (translation-rule (find-host-type-values host-type :translation-rule))
	 (char-map (translation-rule-value translation-rule :char-mappings))
	 (string-map
	  (translation-rule-value translation-rule :component-mappings)))
    (let ((device (physical-pathname-device to-pathname))
	  (directories (map-directories
			(physical-pathname-directory logical-pathname)
			(physical-pathname-directory from-pathname)
			(physical-pathname-directory to-pathname)
			*null-vector* 0 0 0
			(choose-case translation-rule :component)
			char-map string-map))
	  (name (map-wildcard-word (physical-pathname-name logical-pathname)
				   (physical-pathname-name from-pathname)
				   (physical-pathname-name to-pathname)
				   (choose-case translation-rule :name)
				   char-map string-map))
	  (type (map-wildcard-word (physical-pathname-type logical-pathname)
				   (physical-pathname-type from-pathname)
				   (physical-pathname-type to-pathname)
				   (choose-case translation-rule :type)
				   char-map string-map))
	  (version (map-wildcard-word (physical-pathname-version logical-pathname)
				      (physical-pathname-version from-pathname)
				      (physical-pathname-version to-pathname)
				      (choose-case translation-rule :version)
				      char-map string-map)))
      (cond ((eq (pathname-host-type to-pathname) :logical)
	     (make-logical-pathname :host host 
				    :directory directories
				    :name name
				    :type type
				    :version version))
	    (t 
	     (convert-generic-pathname 
	      (make-physical-pathname :host host 
				      :device device
				      :directory directories
				      :name name
				      :type type
				      :version version)
	      output-format))))))

;;; ********************************
;;; Match and Map Wildcards ********
;;; ********************************
(defun wildcard-wordp (string)
  (find #\* string))

(defun must-match (thing)
  (or (eq thing :wild)
      (and (stringp thing)
	   (wildcard-wordp thing))))

(defun match-wildcard-word (template string)
  ;; "*" standalone (:wild) is treated differently from "*" within
  ;; a word.
  (or (eq template :wild)
      (null template)
      (and (stringp string) (stringp template)
	   (match-strings template string))
      ;; e.g., :absolute :absolute
      (eq template string)))

(defun match-strings (template string &optional (t-start 0) (s-start 0))
  (let* ((t-length (length template))
	 (s-length (length string))
	 (t-at-end (= t-length t-start))
	 (s-at-end (= s-length s-start)))
    (cond ((or t-at-end s-at-end) 	; if at end of template or string
	   (and t-at-end s-at-end))     ; both must be at the end.
	  ((char= #\* (char template t-start))
	   (or (match-strings template string (1+ t-start) s-start)
	       (match-strings template string t-start (1+ s-start))
	       (match-strings template string (1+ t-start) (1+ s-start))))
	  ((char-equal (char template t-start)
		  (char string s-start)) ; includes * against *
	   (match-strings template string (1+ t-start) (1+ s-start))))))

(defun match-directories (template dirs &optional (t-start 0) (d-start 0))
  (let* ((t-length (length template))
	 (d-length (length dirs))
	 (t-at-end (= t-length t-start))
	 (d-at-end (= d-length d-start)))
    (cond ((or t-at-end d-at-end)
	   (and t-at-end d-at-end))
	  ((eq (#+:cmu svref #-:cmu aref template t-start) :wild-inferiors)
	   ;; :wild-inferiors matches any number of components, including
	   ;; zero. First try skipping over the :wild-inferiors. If that fails,
	   ;; try matching against one component without skipping over the
	   ;; :wild-inferiors. Finally, try matching against one component
	   ;; while skipping over the :wild-inferiors (the latter really
	   ;; isn't necessary, since the first 2 cases include it).
	   (or (match-directories template dirs (1+ t-start) d-start)
	       (match-directories template dirs t-start (1+ d-start))
	       (match-directories template dirs (1+ t-start) (1+ d-start))))
	  ((match-wildcard-word (#+:cmu svref #-:cmu aref template t-start)
				(#+:cmu svref #-:cmu aref dirs d-start))
	   (match-directories template dirs (1+ t-start) (1+ d-start))))))

(defun map-wildcard-word (string source target 
				 &optional case char-mappings string-mappings)
  (let ((result
	 (cond ((and (stringp target)
		     (not (wildcard-wordp target)))
		;; If the target pattern does not contain *, copy the target
		;; pattern component literally to the target instance.
		target)
	       ((or (eq target :wild) (null target))
		;; If the target pattern is :wild, copy the source string
		;; component to the target string literally with no further
		;; analysis. This holds even for the type, which is 
		;; represented internally in terms of canonical types,
		;; and is "translated" when realized for the new host.
		string)
	       ((not (stringp target))
		target)
	       ((eq source :wild)
		(map-strings string string target))
	       (t (map-strings string source target)))))
    (when (stringp result)
      (setq result 
	    (casify (parallel-substitute char-mappings
					 (name-substitution string-mappings 
							    result))
		    case)))
    result))

(defun map-strings (string source target
			   &optional (result "")
			   (s-start 0) (st-start 0) (tt-start 0))
  (let* ((s-length (length string))
	 (st-length (length source))
	 (tt-length (length target))
	 (s-at-end (= s-length s-start))
	 (st-at-end (= st-length st-start))
	 (tt-at-end (= tt-length tt-start)))
    (cond ((and s-at-end (not (or st-at-end tt-at-end))
		(char= #\* (char target tt-start))
		(char= #\* (char source st-start)))
	   (map-strings string source target result 
			s-start (1+ st-start) (1+ tt-start)))
	  ((or s-at-end st-at-end)
	   ;; When not enough matching values are available due to too few
	   ;; * in the source pattern, use the null string as the matching
	   ;; value for any * remaining in the target.
	   (when (and s-at-end st-at-end)
	     (concatenate 'simple-string 
			  result
			  (delete #\* (subseq target tt-start)))))
	  (tt-at-end
	   ;; When the source pattern has too many *, ignore the first
	   ;; extra * and everything following it.
	   result)
	  ((char= #\* (char target tt-start))
	   ;; Replace * in target pattern with the contents of the source
	   ;; string specified by the next * in the source pattern.
	   (cond ((char= #\* (char source st-start))
		  (or (map-strings string source target result 
				   s-start (1+ st-start) (1+ tt-start))
		      (map-strings string source target 
				   (concatenate 'simple-string result
						(subseq string s-start 
							(1+ s-start)))
				   (1+ s-start) st-start tt-start)))
		 ((char-equal (char source st-start) ; was char=
			      (char string s-start))
		  (map-strings string source target result 
			       (1+ s-start) (1+ st-start) tt-start))))
	  (t;; copy literal strings as is from the target
	   (let ((next-* (position #\* target :start tt-start)))
	     (if next-*
		 (map-strings string source target
			      (concatenate 'simple-string result
					   (subseq target tt-start next-*))
			      s-start st-start next-*)
		 (when (match-strings source string st-start s-start)
		   (concatenate 'simple-string
				result (subseq target tt-start)))))))))

(defun map-directories (dirs source target 
			     &optional (result *null-vector*)
			     (d-start 0) (s-start 0) (t-start 0)
			     case char-map string-map)
  (let* ((d-length (length dirs))
	 (s-length (length source))
	 (t-length (length target))
	 (d-at-end (= d-length d-start))
	 (s-at-end (= s-length s-start))
	 (t-at-end (= t-length t-start)))
    (cond ((and d-at-end (not (or s-at-end t-at-end)) ; added 7/12/91 --mk
		(must-match (#+:cmu svref #-:cmu aref target t-start))
		(must-match (#+:cmu svref #-:cmu aref source s-start)))
	   (map-directories dirs source target result
			    d-start (1+ s-start) t-start
			    case char-map string-map))
	  ((or d-at-end s-at-end)
	   (when (and d-at-end s-at-end)
	     (concatenate 'simple-vector result
			  (map 'simple-vector 
			       #'(lambda (x) 
				   (map-wildcard-word 
				    "" "" x
				    case char-map string-map))
			       (delete :wild-inferiors 
				       (subseq target t-start))))))
	  (t-at-end
	   (when (match-directories source dirs s-start d-start)
	     result))
	  ((eq :wild-inferiors (#+:cmu svref #-:cmu aref target t-start))
	   (cond ((eq :wild-inferiors (#+:cmu svref
				       #-:cmu aref source s-start))
		  (or (map-directories dirs source target result
				       d-start (1+ s-start) (1+ t-start)
				       case char-map string-map)
		      (map-directories dirs source target 
				       (concatenate 'simple-vector result
						    (list (map-wildcard-word
							   (#+:cmu svref 
							    #-:cmu aref
							    dirs d-start)
							   :wild :wild
							   case char-map
							   string-map)))
				       (1+ d-start) s-start t-start
				       case char-map string-map)
		      (map-directories dirs source target 
				       (concatenate 'simple-vector result
						    (list (map-wildcard-word
							   (#+:cmu svref 
							    #-:cmu aref
							    dirs d-start)
							   :wild :wild
							   case char-map
							   string-map)))
				       (1+ d-start) (1+ s-start) (1+ t-start)
				       case char-map string-map)))
		 ((string-equal (#+:cmu svref #-:cmu aref dirs d-start)
				(#+:cmu svref #-:cmu aref source s-start))
		  (map-directories dirs source target result
				   (1+ d-start) (1+ s-start) t-start
				   case char-map string-map))))
	  ((must-match (#+:cmu svref #-:cmu aref target t-start))
	   (cond ((must-match (#+:cmu svref #-:cmu aref source s-start))
		  (map-directories dirs source target
				   (concatenate 'simple-vector result
						(list (map-wildcard-word 
						 (#+:cmu svref 
						  #-:cmu aref dirs d-start)
						 (#+:cmu svref
						  #-:cmu aref source s-start)
						 (#+:cmu svref
						  #-:cmu aref target t-start)
						 case char-map string-map)))
				   (1+ d-start) (1+ s-start) (1+ t-start)
				   case char-map string-map))
		 ((string-equal (#+:cmu svref #-:cmu aref dirs d-start)
				(#+:cmu svref #-:cmu aref source s-start))
		  (map-directories dirs source target result
				   (1+ d-start) (1+ s-start) t-start
				   case char-map string-map))))
	  (t
	   (map-directories dirs source target
			    (concatenate 'simple-vector result
					 (list
					  (map-wildcard-word
					   (#+:cmu svref 
						   #-:cmu aref target t-start)
					   :wild :wild
					   case char-map
					   string-map)))
			    d-start s-start (1+ t-start)
			    case char-map string-map)))))


;;; ********************************
;;; Common Lisp Redefinitions ******
;;; ********************************
;;; Not doing merge-pathnames or with-open-file. Parse-namestring not
;;; really done well.

;;; append-directories
(defun append-logical-directories (absolute-dir relative-dir)
  (when (or absolute-dir relative-dir)
    (setq absolute-dir (logical-pathname (or absolute-dir ""))
	  relative-dir (logical-pathname (or relative-dir "")))
    (logical-namestring 
     (make-logical-pathname
      :host (or (logical-pathname-host absolute-dir)
		(logical-pathname-host relative-dir))
      :directory (concatenate 'simple-vector
			      (logical-pathname-directory absolute-dir)
			      (cdr (coerce (logical-pathname-directory
					    relative-dir)
					   'list)))
      :name (or (logical-pathname-name absolute-dir)
		(logical-pathname-name relative-dir))
      :type (or (logical-pathname-type absolute-dir)
		(logical-pathname-type relative-dir))
      :version (or (logical-pathname-version absolute-dir)
		   (logical-pathname-version relative-dir))))))
	  
(defun real-filename (filename)
  (if (and filename
	   (eq (pathname-host-type filename) :logical))
      (translate-logical-pathname filename :namestring)
      filename))

;; The definitions below rely on the following:
;; - Proper shadowing of symbols that have definitions in the LISP package.
;; - Saving of original definitions on `real-XXX' functions, for example,
;;   the original definition of `cl:load' is assumed to beavailable under
;;   the name `lp::real-load'.
;; These requirements are achieved in the initial section of this
;; file that deals with package stuff.

(defmacro define-file-function (name &optional optionalp)
  (let ((real-name 
	 (intern (sneps:build-namestring :real "-" name))))
    (when (fboundp real-name)
      ;; Yes, some lisps will give compiler warnings about REAL-name
      ;; not being declared or defined as a function. But what can
      ;; we do, with most lisps not yet recognizing CLtL2's ftype
      ;; declaration?
      `(defun ,name ,(if optionalp 
			 '(&optional filename &rest args)
		       '(filename &rest args))
	 ,(if optionalp
	      `(if filename
		   (apply #',real-name (real-filename filename) args)
		 (,real-name))	; instead of (funcall #',real-name)
	    `(apply #',real-name (real-filename filename) args))))))

(defmacro define-file-function-2-args (name)
  (let ((real-name 
	 (intern (sneps:build-namestring :real "-" name))))
    (when (fboundp real-name)
      `(defun ,name (filename1 filename2 &rest args)
	 (apply #',real-name
		(real-filename filename1)(real-filename filename2)
		args)))))

;; Define LP versions of various standard functions:
(define-file-function load)
(define-file-function open)
(define-file-function probe-file)
(define-file-function delete-file)
(define-file-function truename)
(define-file-function directory)
(define-file-function dribble t)
(define-file-function ed t)
(define-file-function file-author)
(define-file-function file-write-date)
(define-file-function-2-args rename-file)
;; should take care of :output-file as well
(define-file-function compile-file)

(defun parse-namestring (thing &optional host
					 (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  "Convert THING (string, symbol, pathname, or stream) into a pathname."
  (declare (ignore junk-allowed))
  (cond ((null thing) nil)	; try to fix bug with (ed). probably not here.
	((or (eq (pathname-host-type thing) :logical)
	     (eq (pathname-host-type defaults) :logical)
	     (eq (host-type host) :logical))
	 ;; Tis a logical pathname
	 (parse-generic-namestring thing host defaults 
				   :start start :end end))
	(t (if end
	       (real-parse-namestring thing host defaults 
				      :start start :end end)
	     (real-parse-namestring thing host defaults 
				    :start start)))))

(defun redefine-standard-functions ()
  ;; Re/define various standard functions in the LISP package:
  (dolist (function (append *standard-pathname-functions*
			    *standard-file-functions*))
    (let ((lisp-name (find-symbol (string function) :cl))
	  (real-name (intern 
		      (sneps:build-namestring :real "-" function)
		      'logical-pathname))
	  (lp-name (intern 
		    (sneps:build-namestring function)
		    'logical-pathname)))
      (and lisp-name
	   (fboundp lisp-name)
	   (fboundp real-name)
	   (fboundp lp-name)
	   (setf (symbol-function lisp-name)
	     (symbol-function lp-name))))))

;;; *EOF*
