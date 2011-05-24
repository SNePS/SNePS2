;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ENGLEX; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: englex.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


;; altered for ACL 6 compatibility (FLJ)
        

(in-package :englex)


;;MORPHOLOGICAL functions

;;ISSUES

;;ISnaturalword
;;Should be allow spaces,hypens,aposts etc to be valid chars?

;;lookup
;;make changes to tracer information

;;lexin
;;Expects that the file is in the NEW-format.
;;That is it contains quoted forms of symbols corresponding to actual words 
;;such as the entry word and any features whos values are words versus 
;;predefined value tokens.
;;Now must be called with the file-name as a string -- else everything gets
;;converted to upper case.

;;puth:
;;Substitute for the original call (put entry '=dict val)
;;for (puth entry val)
;;associates with a word(the entry) a list of feature-lists

;;geth:
;;Substitute for the original call (get entry '=dict)
;;for (geth entry)
;;Gets the feature lists associated with entry

;;lookup-lexical-feature:
;;lexentry needs to be declared a special in calling parser functions
;;Needs definition to be shadowed in parser

;;evalt:
;;Use parser definition
;;*register-* is a global





(defvar *lexicon* :unbound "Hashtable for lexicon.")
  

;;***************************************************************************;;
;; morphological functions : lookup, lexic, root, prefix, suffix             ;;
;;***************************************************************************;;

(defun lexic (word)
  "lexic - args: word returns: all the feature lists of word"
    (if word
        (mapcar #'(lambda (feature-list)
                         (cond ((assoc 'ENGLEX::ROOT feature-list) feature-list)
                               (t (cons (cons 'ENGLEX::ROOT word) feature-list))))
                (cond ((atom word) (geth word))
                      (t (geth word))))))

(defun root (word)
  "root - args: word returns: all the feature lists of word"
    (and (ISnaturalword word)
         (add-default-feats
           (or (lexic word)
               (suffix word)
               (prefix word)))))

;;conv? should be allow spaces,hypens,aposts etc to be valid chars
(defun  ISnaturalword (word)
  "ISNaturalword - args: word returns: t if word is composed of legal chars"
              (not(find-if-not #'alpha-char-p word)))

(defun prefix (word)
  "prefix - args: word
   checks to see if word begins with a sublist corresponding to 
   valid prefix"
    (cond ((string= word "") nil)
	  ((is-sub-str "anti" word) (root (subseq word 4)))
          ((is-sub-str "dis" word) (root (subseq word 3)))
          ((is-sub-str "im" word) (root (subseq word 2)))
          ((is-sub-str "in" word) (root (subseq word 2)))
          ((is-sub-str "ill" word) (root (subseq word 2)))
          ((is-sub-str "irr" word) (root (subseq word 2)))
          ((is-sub-str "mis" word) (root (subseq word 3)))
          ((is-sub-str "un" word) (root (subseq word 2)))
          ((is-sub-str "re" word) (root (subseq word 2)))
          ((is-sub-str "semi" word) (root (subseq word 4)))
          ((is-sub-str "pre" word) (root (subseq word 3)))
          ((is-sub-str "ex-" word) (root (subseq word 3)))
	  ((is-sub-str "self-" word) (root (subseq word 5)))
          ((char-equal (char word 0) '#\-) (root (subseq word 1)))
          (t nil)))

;;Sy you will have to modify the line in this function which handles
;;the tracing --- im not sure of the way you handled it so ill let you change
;;it.
(defun lookup (word)
  "lookup - args:  word"
  (cond ((not (stringp word)) nil)
	((null (symbol-function 'root))(lexic word))
   ;    ((root word))
	(t (root word))
	;;the following is what is in the original code:
	;;(or (null (valuep 'tlvlxx)) (greaterp tlvlxx 0))
	;;i changed it to this temporarily:
        ;;((or (null *trace-level*)(> *trace-level* 0))
        ;;(format t  "% ~S not in dictionary" word)
        ;;nil)
	))

;; New version that respects that the test function used in SEARCH
;; should not have any side effects because the implementation is
;; free to map over sequence2 in any way it wants. (hc, Feb-27-90)
(defun is-sub-str (sub-str word &optional (from-end-p nil)) 
    "Checks if SUB-STR is either an initial or final (if FROM-END-P
is non-NIL) sub-string of WORD. @ counts as wildcard that matches
any character in WORD."
    (let ((sub-str-length (length sub-str))
	  (word-length (length word)))
      (and (<= sub-str-length word-length)
	   (search sub-str word
		   :test #'(lambda (x y)
			     ;; Hard to believe but Allegro
			     ;; uses y for sequence-1
			     (or (char-equal x #\@)
				 (char-equal y #\@)
				 (char-equal x y)))
		   :start2 (cond (from-end-p
				  (- word-length sub-str-length))
				 (t 0))
		   :end2 (cond (from-end-p word-length)
			       (t sub-str-length))))))


(defun suffix (l)
  "suffix - args: l
  Checks is word l contains a recognized suffix"
    (cond ((string= l "") nil)
	  ((is-sub-str "@@s" l t) (s (subseq l 0 (1- (length l)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "@@ing" l t))
           (append (up* '((pprt . t))
                        '(v)
                        (setq l (ing (subseq l 0 (- (length l) 3)))))
                   (append (up* '((ctgy . adj) (pprt . t))
                                '(v)
                                l)
                           (up* '((ctgy . n)) '(v) l))))
          ((and (cl:> (length l) 1)
		(is-sub-str "@@ed" l t))
           (append (up* '((pprt . t) (tense . past))
                        '(v)
                        (setq l (>ed (subseq l 0 (- (length l) 2)))))
                   (up* '((ctgy . adj) (pprt . t) (tense . past))
                        '(v)
                        l)))
          ((is-sub-str "@@y" l t) (y (subseq l 0 (1- (length l)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "@@ful" l t))
           (up* '((ctgys . adj)) '(n) (ful (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 1)
		(is-sub-str "@@al" l t))
           (append (up* '((ctgy . n))
                        '(adj)
                        (setq l (al (subseq l 0 (- (length l) 2)))))
                   (up* '((ctgy . adj)) '(n v) l)))
	  ;; "ABLE" is not currently implemented.
          ;((and (cl:> (length l) 3)
	  ;      (is-sub-str "@@able" l t))
          ; (up* '((ctgy . adj)) '(v) 
	  ;	(able (subseq l 0 (- (length l) 4)))))
          ((and (cl:> (length l) 3)
		(is-sub-str "@@ible" l t))
           (up* '((ctgy . adj)) '(n) 
		(ible (subseq l 0 (- (length l) 4)))))
          ((and (cl:> (length l) 1)
		(is-sub-str "@en" l t))
	   (en (subseq l 0 (- (length l) 2))))
          ((and (cl:> (length l) 1)
		(is-sub-str "@ept" l t))
           (up* '((tense . past))
                '(v)
                (lexic (concatenate 'string (subseq l 0 (- (length l) 2)) "ep"))))
          ((is-sub-str "@@er" l t)
           (append (lim '(adj) (setq l (er (subseq l 0 (1- (length l))))))
                   (up* '((ctgy . n)) '(v) l)))
          ((and (cl:> (length l) 1)
		(is-sub-str "or" l t))
           (up* '((n)) '(v) 
		(er (concatenate 'string (subseq l 0 (- (length l) 2)) "e"))))
          ((and (cl:> (length l) 1)
		(is-sub-str "ee" l t))
           (up* '((ctgy . n)) '(v) (lexic (subseq l 0 (- (length l) 2)))))
          ((and (cl:> (length l) 1)
		(is-sub-str "@@est" l))
           (up* '((ctgy . adj)) '(v adj) (er (subseq l 0 (- (length l) 2)))))
          ((and (cl:> (length l) 3)
		(is-sub-str "@@ment" l))
           (up* '((ctgy . n)) '(v) (ful (subseq l 0 (- (length l) 4)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "ant" l))
           (up* '((ctgy . n)) '(v) (ful (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "ent" l))
           (up* '((ctgy . n)) '(v) (ful (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "@@ate" l))
           (up* '((ctgy . adj)) '(n) (ate (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "@@iou" l))
           (update '((ctgy . n)) (ion (subseq l 0 (- (length l) 3)))))
	  ;; "NCE" is not currently implemented.
          ;((and (cl:> (length l) 2)(is-sub-str "@@nce" l))
	  ;  (nce (subseq l 0 (- (length l) 3))))
          ((is-sub-str "@@is@" l) (is* l))
          ((and (cl:> (length l) 2)
		(is-sub-str "ive" l))
           (up* '((ctgy . adj)) '(v) (lexic (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "ize" l))
           (up* '((ctgy . v)) '(adj) (lexic (subseq l 0 (- (length l) 3)))))
	  ;; "LIKE" is not currently implemented.
          ;((and (cl:> (length l) 4)
	  ;   (is-sub-str "@@like" l))
          ; (up* '((ctgy . adj)) '(n) (like (subseq l 0 (- (length l) 5)))))
          ((char-equal (char l (1- (length l))) '#\')
           (up* '((pos . t) (ctgy . adj))
                '(n npr)
                (possessive (subseq l 0 (1- (length l))))))
          (t nil)))

(defun s (l)
  "s - args: l"
    (prog (d)
          (return
           (cond ((append (lim '(v) (setq d (lexic l)))
                          (up* '((num . plur)) '(n) d)))
                 ((char-equal (char l (1- (length l))) '#\e)
                  (append (up* '((num . plur))
                               '(n)
                               (setq d (es (subseq l 0 (1- (length l))))))
                          (lim '(v) d)))
                 ((and (cl:> (length l) 1)
		       (is-sub-str "@@es" l))
		  (ess (subseq l 0 (- (length l) 2))))
                 ((char-equal (char l (1- (length l))) '#\')
                  (up* '((pos . t) (ctgy . adj))
                       '(n npr)
                       (possessive (subseq l 0 (1- (length l))))))
                 ((and (cl:> (length l) 1)
		       (is-sub-str "#ou" l))
                  (up* '((ctgy . adj)) '(n) 
		       (ous (subseq l 0 (- (length l) 2)))))
                 (t
                  (append (lim '(v) (setq d (suffix l)))
                          (up* '((num . plur)) '(n) d)))))))

(defun es (l)
  "es - args: l"
    (cond ((lexic l))
          ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1- (length l)))))
          ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2)))) 
	   (lexic (subseq l 0 (1- (length l)))))
          (t nil)))

(defun ess (l)
  "ess - args: l"
    (cond ((char-equal (char l (1- (length l))) '#\n)
           (up* '((ctgy . n)) '(adj) (less (subseq l 0 (1- (length l))))))
          ((char-equal (char l (1- (length l))) '#\l)
           (up* '((ctgy . adj)) '(n) (less (subseq l 0 (1- (length l))))))
          (t
           (up* '((fem . t))
                '(n)
                (cond ((char-equal (char l (1- (length l))) '#\r) 
		       (ress (subseq l 0 (1- (length l)))))
                      ((and (cl:> (length l) 1)
			    (char-equal (char l (1- (length l))) (char l (- (length l) 2))))
		       (lexic (reverse (subseq l 0 (1- (length l))))))
                      (t (lexic (reverse l))))))))

(defun addy (l)
  "addy - args: l"
    (cond ((lexic (reverse (concatenate 'string "y" l))))
          ((y l))
          (t (lexic (reverse (concatenate 'string "ye" l))))))

(defun less (l)
  "less - args: l"
    (cond ((lexic l)) 
	  ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1- (length l))))) (t (suffix l))))

(defun ress (l)
  "ress - args l"
    (or (lexic (concatenate 'string l "er"))
        (lexic (concatenate 'string l "or"))
	(lexic l)))

(defun possessive (l)
  "possessive - args: l"
    (cond ((lexic l)) 
	  (t (suffix l))))

(defun >ed (l)
  ">ed - args: l"
    (cond ((lexic (concatenate 'string l "e")))
          ((lexic l))
          ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1- (length l)))))
          ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2))) )
	   (lexic (subseq l 0 (1- (length l)))))
          (t nil)))

(defun ing (l)
  "ing - args: l"
    (cond ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2)))) 
	   (or (lexic (subseq l 0 (1- (length l)))) (lexic l)))
          ((lexic l))
          ((lexic (concatenate 'string l "e")))
          (t (suffix (concatenate 'string l "e")))))

(defun ful (l)
  "fun - args: l"
    (cond ((string= l "") nil)
	  ((lexic l))
          ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1-  (length l)))))
          ((lexic (concatenate 'string l "e")))
          (t nil)))

(defun ible (l)
  "ible - args: l"
    (cond ((lexic l))
          ((lexic (concatenate 'string l "e")))
          ((char-equal (char l (1- (length l))) '#\s) 
	   (lexic (concatenate 'string (subseq l 0 (1- (length l))) "d")))
          (t nil)))

(defun en (l)
  "en - args: l"
  (let ((len (length l)))
    (cond ((and (cl:> len 1)
		(char-equal (char l (1- len)) (char l (- len 2))))
	   (lexic (subseq l 0 (1- len))))
          ((up* '((num . plur))
                '(n)
                (lexic (concatenate 'string l "an"))))
          ((and (cl:> len 1)
		(char-equal (char l (1- len)) (char l (- len 2)))) 
	   (lexic (subseq l 0 (1- len))))
          ((up* '((tense . past))
                '(v)
                (lexic (concatenate 'string l "e"))))
          (t
           (append (up* '((num . plur))
                        '(n)
                        (setq l (lexic l)))
                   (up* '((ctgy . adj)) '(n) l)))
	  )))

(defun er (l)
  "er - args: l"
  (let ((len (length l)))
    (cond ((string= l "") nil)
	  ((lexic l))
          ((lexic (subseq l 0 (1- len))))
          ((char-equal (char l (- len 2)) '#\i) 
	   (addy (subseq l 0 (- len 2))))
          ((and (> len 2)
		(char-equal (char l (- len 2)) (char l (- len 3)))) 
	   (lexic (subseq l 0 (- len 2))))
          ((suffix l))
          (t (suffix (subseq l 0 (1- len)))))))

(defun y (l)
  "y - args: l"
    (cond ((up* '((ctgy . adj)) '(n) (lexic l)))
          ((up* '((ctgy . adj)) '(n) 
		(lexic (concatenate 'string l "e" ))))
          ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2))))
           (up* '((ctgy . adj)) '(n) (lexic (subseq l 0 (1- (length l))))))
          ((and (cl:> (length l) 2)
		(is-sub-str "abl" l t))
           (up* '((ctgy . adv)) '(v) (>ed (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 2)
		(is-sub-str "ibl" l t))
           (up* '((ctgy . adv)) '(n) (ible (subseq l 0 (- (length l) 3)))))
          ((char-equal (char l (1- (length l))) '#\c)
           (up* '((ctgy . n)) '(n adj) (cy (subseq l 0 (1- (length l))))))
          ((char-equal (char l (1- (length l))) '#\t)
           (up* '((ctgy . n)) '(adj) (ty (subseq l 0 (1- (length l))))))
	  ((char-equal (char l (1- (length l))) '#\l)
           (setq l (ly (subseq l 0 (1- (length l)))))
           (cond ((up* '((ctgy . adv)) '(adj) l))
                 (t (up* '((ctgy . adj)) '(n) l))))
          ((and (cl:> (length l) 1)
		(is-sub-str "@if" l t))
           (up* '((ctgy . v)) '(adj) (ify (subseq l 0 (- (length l) 2)))))
          (t nil)))

(defun cy (l)
  "cy - args: l"
    (cond ((lexic (concatenate 'string l "t"))) 
	  (t (lexic (concatenate 'string l "et")))))

(defun ty (l)
  "ty - args: l"
    (cond ((lexic l))
          ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2))) )
	   (lexic (subseq l 0 (1- (length l)))))
          ((not (char-equal (char l (1- (length l))) '#\i)) (suffix l))
          ((lexic (concatenate 'string (subseq l 0 (1- (length l))) "e")))
          ((lexic (concatenate 'string (subseq l 0 (1- (length l))) "ary")))
	  (t nil)))

(defun ly (l)
  "ly - args: l"
    (cond ((lexic (concatenate 'string l "le")))
          ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1- (length l)))))
          ((lexic (concatenate 'string l "e")))
          ((lexic l))
          (t (suffix l))))

(defun al (l)
  "al - args: l"
    (cond ((lexic l))
          ((lexic (concatenate 'string l "a")))
          ((lexic (concatenate 'string l "e")))
          ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1- (length l)))))
          (t (suffix l))))

(defun ate (l)
  "ate - args: l"
    (cond ((lexic l)) 
	  ((lexic (concatenate 'string l "e"))) 
	  (t (suffix l))))

(defun ance (l)
  "ance - args: l"
    (cond ((lexic l))
          ((char-equal (char l (1- (length l))) '#\i) 
	   (addy (subseq l 0 (1- (length  l)))))
          ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2))))
	   (lexic (subseq l 0 (1- (length l)))))
          ((lexic (concatenate 'string l "e")))
          ((lexic (concatenate 'string l "ant")))
          ((lexic (concatenate 'string l "ent")))
          (t nil)))

(defun ion (l)
  "ion - args: l"
    (cond ((lexic l))
          ((lexic (concatenate 'string l "e")))
          (t (suffix (concatenate 'string l "e")))))

(defun ous (l)
  "ous - args: l"
    (cond ((lexic l))
          ((and (cl:> (length l) 1)
		(char-equal (char l (1- (length l))) (char l (- (length l) 2))))
	   (lexic (subseq l 0 (1- (length l)))))
          ((char-equal (char l (1- (length l))) '#\u) 
	   (lexic (concatenate 'string (subseq l 0 (1- (length l))) "e")))
          (t (suffix l))))

;;conv? 't was 'mt what is that
;;conv? adj was ad j changed
(defun is* (l)
    (cond ((and (cl:> (length l) 2)
		(char-equal (char l (1- (length l))) '#\h))
           (up* '((ctgy . adj)) '(adj n) (is** (subseq l 0 (- (length l) 3)))))
          ((and (cl:> (length l) 2)
		(char-equal (char l (1- (length l))) '#\t))
           (up* '((ctgy . n)) '(adj n) (is** (subseq l 0 (- (length l) 3)))))
          (t nil)))

(defun is** (l)
  "is** - args: l"
    (cond ((lexic l)) 
	  ((lexic (concatenate 'string l "e"))) 
	  (t (suffix l))))

(defun ify (l)
  "ify - args: l"
    (cond ((lexic l))
          ((lexic (concatenate 'string l "e")))
          ((lexic (concatenate 'string l "ic")))
          (t (addy l))))


(defun lim (limit def)	
  "lim - args: limit def"
  ;; returns all the feature-pair sublists of the feature-pair list of
  ;; a given word (which is gotton by call to (lexic word))  
  ;; -- which have in them the feature-pair element : (ctgy . limit)
  ;; example: limit : (z) def : (((ctgy . z)..)((ctgy . z)..)((ctgy . other)..))
  ;; --> (((ctgy . z)..)((ctgy . z)..))
    (cond ((null def) nil)
          ((member (get-lexical-feature 'ctgy (car def)) limit)
           (cons (car def) (lim limit (cdr def))))
          (t (lim limit (cdr def)))))

(defun up* (cng limit def)
  "up* - cdg limit def"
    (update cng (lim limit def)))

(defun update (cng def)
  "update - args: cng def"
    (cond ((null def) nil)
          ((null cng) def)
          (t
           (update (cdr cng) (list (enterp (car cng) (caar cng) (car def)))))))

(defun enterp (entry type dlist)
  "enterp - args: entry type dlist"
  ;; given the feature pair sublist, dlist, (gotton by call to lexic word)
  ;; the feature pair element element entry (a pair) -- as well the the name 
  ;; of the feature type -- enterp searches for the feature type in dlist and 
  ;; 1- adds it if  its not there
  ;; 2- changes the pair to look like entry if theres a feature value for type
  ;; 3- leaves alone if it contains it
  ;; entry= (num . plural) type=num dlist=((ctgy.n)(num.sing))
  ;; --> ((ctgy.n)(num.plural))
    (cond ((null dlist) (list entry))
          ((eq (caar dlist) type) (cons entry (cdr dlist)))
          (t (cons (car dlist) (enterp entry type (cdr dlist))))))

(defun get-lexical-feature (feature feature-list)
  "get-lexical-feature  - args: feature feature-list"
   ;;returns the value (if any) for given feature in the feature-list
    (cdr (assoc (intern (symbol-name feature) (find-package 'snepsul))
		feature-list)))	


(defun lookup-lexical-feature (feature lexeme)
  "Looks up lexeme in the lexicon,
   and returns the first value of the feature it finds among the lexcical entries."
  (do* ((ifeature (intern (symbol-name feature) (find-package 'snepsul)))
	(lexentries (geth lexeme) (rest lexentries))
	(value (cdr (assoc ifeature (first lexentries)))
	       (cdr (assoc ifeature (first lexentries)))))
       ((or value (null (rest lexentries))) value)))


(defun add-default-feats (lx)
  "add-default-feats - args: lx"
    (mapcar #'(lambda (pairs)
                     (add-feats pairs
                                (case (get-lexical-feature 'ctgy pairs)
                                         (n '((num . sing)))
                                         (v '((tense . pres))))))
            lx))

(defun add-feats (pairs1 pairs2)
    (cond ((null pairs2) pairs1)
          ((assoc (caar pairs2) pairs1) (add-feats pairs1 (cdr pairs2)))
          (t (cons (car pairs2) (add-feats pairs1 (cdr pairs2))))))


;;to substitute for the original call (put entry '=dict val)
;;associates with a word(the entry) a list of feature-lists
(defun puth (entry val)
  "putf - args: word feature-lists"
	(setf ( gethash entry *lexicon*) val))

;; to substitute for the original call (get entry '=dict)
;; gets the feature lists associated with entry
(defun geth (entry)
  "geth - args: entry"
	(gethash entry *lexicon*))

;; Expects that lexicon file contains lexical entries as defined in the
;; Sneps manual. 
;; If you want to preserve case in filename -- call function with
;; a string: (lexin "lexicon")...else all gets converted to upper case.
(defsnepscom lexin ((file &key (merge nil)))
  "lexin (macro) - args: list of file names"
  (let (lwords undefs input *feature-values*
	 (*package* (find-package 'snepsul)))
	 (declare (special undefs *feature-values*))
	 ;; Don't waste hash tables
	 (cond ((and (boundp '*lexicon*)
		     (hash-table-p *lexicon*))
		(when (not merge)
		  (clrhash *lexicon*)))
	       (t (setq *lexicon* (make-hash-table :test 'equal))))
	 (with-open-file (inunit (cl-user:sneps-translate file) :direction :input)
	   (loop
	     (setq input (read inunit nil :eof))
	     (cond ((eq input :eof)
		    (return t))
		   (t
		    (setq lwords
			  (nconc lwords (definewords input))))))
	   )
	 (mapc #'(lambda (word)
		   (setq undefs (find-and-del word undefs)))
	       lwords)
	 (if undefs
	     (format t "undefined- ~S~%" undefs))
	 lwords))

(defmacro lex-in (x)
  "lex-in - args: file-names"
  `(apply 'lexin ,x))

(defun find-and-del (target undefs)
  "Near as I can figure, this steps through an a-list removing references to the target."
  (do
    ((sofar nil (append sofar (list (car undefs)))))
    ((null undefs) sofar)
    (if (equal (cadar undefs) target)
	(return (append sofar (cdr undefs))))
    (setq undefs (cdr undefs))
  )
)
#|
(defun find-and-del (target undefs)
  "find-and-del - args: target undefs"
	(prog (sofar)
	   loop
            (if (null undefs) (return sofar))
            (if (equal (cadar undefs) target)
                (return (append sofar (cdr undefs))))
            (setq sofar (append sofar (list (car undefs))))
            (setq undefs (cdr undefs)) 
	    (go loop)))
|#


(defun mem* (item list)
  "``reverse member function'' -- like Rassoc, but returns T if ITEM is
   second element (not just Cdr) of any item in LIST, and NIL otherwise"
  (if list
      (or (null item)
	  (not (null (cl::member item list :test #'equal :key #'second)))
       )))    ; Changed 07/01/88 SSA, Force use of LISP:MEMBER rather than ZLC:MEMBER.

(defun definewords (words &aux (word (car words)) (lpairs (cdr words)) rt)
  "Defines words in *lexicon*, identifies words with roots, and exports/imports 
   values of features to the PARSER package. - arg: words"
  (declare (special undefs *FEATURE-VALUES*))
  (puth word lpairs)
  (dolist (lpair lpairs (list word))
    (setq rt (get-lexical-feature (intern (build-namestring :root)) lpair))
    (unless (mem* rt undefs)
      (cl::push (list word rt) undefs))
    ;; Export/import feature values into the PARSER package.
    (dolist (pair lpair)			; One feature at a time.
      (let ((feature (cdr pair)))
	(unless (or (stringp feature)
		    (member feature *FEATURE-VALUES*))
	  (cond ((listp feature)

		   (setq feature (parser::flatten feature))          ; Change 07/01/88 SSA: Allow 
		   (if (atom feature) (setq feature (list feature))) ; 1-element feature lists.

	           (dolist (elt feature)
		     (unless (or (characterp elt)
				 (stringp elt))
		       (export elt)
		       (shadowing-import elt (find-package 'parser)))))
		((not (characterp feature))
		   (export feature)
		   (shadowing-import feature (find-package 'parser))))
	  (cl::push feature *FEATURE-VALUES*))
	))))



    
    




