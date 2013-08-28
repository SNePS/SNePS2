;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ENGLEX; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: verbthread.lisp,v 1.2 2013/08/28 19:07:24 shapiro Exp $

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




(in-package :englex)


;+---------------------------------------------------------------------+
;|                                                                     |
;|           SNePS: Semantic Network Processing System                 |
;|           =========================================                 |
;|                                                                     |
;|               by  SNePS Research Group (SNeRG)                      |
;|                Department of Computer Science                       |
;|            State University of New York at Buffalo                  |
;|                                                                     |
;|                                                                     |
;|  All rights to this software package reserved.                      |
;|  Any questions regarding the copyrights should be directed to       |
;|                                                                     |
;|            Dr. Stuart C. Shapiro, Director                          |
;|            SNePS Research Group                                     |
;|            Dept. of Computer Sciencee                               |
;|            State University of New York at Buffalo                  |
;|            
;; 
;; 201 Bell Hall, Amherst, NY 14260                         |
;|            U.S.A.                                                   |
;|            tel: 716-636-3181                                        |
;|                 716-636-3183                                        |
;+---------------------------------------------------------------------+
;|								       |
;|       ==== VERB CONJUGATION PROGRAM ====			       |
;|            1986/11   Hanyong Yuhan				       |
;|								       |
;+---------------------------------------------------------------------+
;; STATUS
;+---------------------------------------------------------------------+
;           1986/11   Hanyong Yuhan
;			FranzLisp version
;	    1987/8    Tranlated to CommonLisp 
;		       Zuzana Dobes
;		       Works on kcl version of CommonLisp
;+---------------------------------------------------------------------+
;; PROBLEMS / NOTICES / BUGS
;+---------------------------------------------------------------------+
;; interrogativize
;; 	uses *subject* -- make it into a string or assume its in one?
;; 	also what is global var name?
;; capital letters
;;	this program compares input strings with 'string-equal' thefore
;;      being insensitive to case 'hello' = 'Hello'
;;
;+---------------------------------------------------------------------+
;; SYNTAX / DESCRIPTION
;+---------------------------------------------------------------------+
;
;(verbize-y  '<SPECSET>  '<VERBCLUST>))   -- lambda form
;(verbize    '<SPEC>* '<MODAL>* '<MVERB>) -- lexpr form
;(verbthread  <SPEC>*  <MODAL>*  <MVERB>) -- nlambda form
;(v           <SPEC>*  <MODAL>*  <MVERB>) -- alias of (verbthread)
;
; Where
;   <SPECSET>  ::= <SPEC>*
;   <VERBCLUST>::= <MVERB> <REVERSED-MODALS>
;   <REVERSED-MODALS> ::= <MODAL>*  -- modals in a reverse order
;   <SPEC>     ::= { <TENSE>   | <NUMB>    | <PERSON> | <MODE> |
;                    <PROGASP> | <PERFASP> | <VOICE>  | <QUALITY> }
;   <MODAL>    ::= { will | shall | can | must | may | ?? }
;   <MVERB>    ::= { go | take | stay | ...               }
;
;=======================================================================
;<TENSE> <NUMB> <PERSON> <MODE> <PROGASP> <PERFASP>  <VOICE>  <QUALITY>
;-----------------------------------------------------------------------
;@pres  @sing    @p3    @decl @non-prog @non-perf  @active  @affirmative
; past   plur     p1     int   progress  perfective passive  negative
; future          p2     imp
;                        inf
;                        gerund
;
;-The entries prefixed with an @ symbol in the <SPEC> value table show
;   default values.
;
;-In case that more than one modal are requested, a modal of higher
;   scope should come first unless being directed otherwise.
;
;-If more than one value of one SPEC are requested, non-default value
;   always wins.  If more than one non-default value are requested,
;   the behavior is unpredictable.
;
;
;List of alternative specifications for an equivalent value.
;==========================================================
;(past p)
;(future futr ftr fut)
;(plur pl plural)
;(p1 firstperson person1)
;(p2 secondperson person2)
;(int ynq interrogative interrog ques question intrg Q)
;(imp imper imperative impr command request req)
;(inf infinitive infin root)
;(gnd gerund ing grnd)
;(pass passive)
;(prog progr prgr prg progress progressive)
;(perf pft prft prfct perfect perfective)
;(neg nega negative not negated)
;(pres present pr prsnt)
;(sing singular)
;* Any value spec not identified in the above list is ignored
;


;;;
;;;
;;;
;===================================================
(defvar SPpres '(pr pres present prsnt))
(defvar SPpast '(past p DEFAULT))
(defvar SPfuture '(future futr ftr fut))
(defvar SPsing '(sing singular))
(defvar SPplur '(plur pl plural))
(defvar SPp1   '(p1 firstperson person1))
(defvar SPp2   '(p2 secondperson person2))
(defvar SPp3   '(p3 thirdperson person3))
(defvar SPint  '(int ynq interrogative interrog ques question intrg Q))
(defvar SPimp  '(imp imper imperative impr command request req))
(defvar SPinf  '(inf infinitive infin root))
(defvar SPgnd  '(gnd gerund ing grnd))
(defvar SPpass '(pass passive))
(defvar SPprog '(prog progr prgr prg progress progressive))
(defvar SPperf '(perf pft prft prfct perfect perfective))
(defvar SPneg  '(neg nega negative not negated))

;(defun verbize-y (spec vg)
;    (apply (function verbthread)
;           (reverse (append (flistify vg) (flistify spec)))))
;(defun verbize-y (spec vg)
;    (eval (cons 'verbthread
;                (reverse (append (flistify vg) (flistify spec))))))
(defun verbize-y (spec vg)
  (verbthread-function
   (reverse (append (flistify vg) (flistify spec)))))

;(defun verbize (&rest argc)
;    (apply (function verbthread) (listify argc)))
;(defun verbize (&rest argc)
;    (eval (cons 'verbthread (flistify argc))))
(defun verbize (&rest argc)
  (verbthread-function (flistify argc)))

;; Don't know whether anybody actually uses this as a macro (hc 07-15-93):
(defmacro verbthread (&rest ndata)
  "verbthread - args: specs(features)* modals* main-verb
   The main calling routine used in generation"
  `(verbthread-function ',ndata))

;; Doing this in a function is much more efficient than evaluating the
;; whole garbage everytime the macro gets expanded (hc 07-15-93):
(defun verbthread-function (ndata)
  "verbthread - args: specs(features)* modals* main-verb
   The main calling routine used in generation"
  (block nil
    (let* (;; the last element in the argument list is the VERB
	   (lex (car (last ndata)))
	   ;; coerce it to a string, if necessary
	   (vb (get-vroot (typecase lex
			    (string lex)
			    (symbol (symbol-name lex))
			    (sneps::node (symbol-name (sneps:node-na lex))))))
	   (vbc (list vb))
	   (auxc (reverse (extract-auxes (cdr (reverse ndata)))))
	   ;; gets the list of modals starting from the end- past the
	   ;; main verb-- stops at the first non-modal
	   (spec (nthcdr (1+ (length auxc)) (reverse ndata)))
	   ;; gets the list of everything past the  modals (looking
	   ;; at the revese of the list) - if there are any
	   (negflg (intersection spec SPneg :test #'string-equal))
	   (intflg (intersection spec SPint :test #'string-equal))
	   ;;(unknown nil)
	   ) 
      (if (intersection spec SPfuture :test #'string-equal)
	  (setq auxc (cons "will" auxc)))
      (if (intersection spec SPpass :test #'string-equal)
	  (setq vbc (passivize vbc)))
      (if (intersection spec SPprog :test #'string-equal)
	  (setq vbc (progressivize vbc)))
      (if (intersection spec SPperf :test #'string-equal)
	  (setq vbc (perfectivize vbc)))
      (if auxc (setq vbc (auxize auxc vbc spec)))
      (cond  ((intersection spec SPimp :test #'string-equal)
	      (exam-wrongspec spec)
	      (return (imperativize vbc negflg))))
      (cond ((intersection spec SPgnd :test #'string-equal)
	     (exam-wrongspec spec)
	     (return (gerundize vbc negflg))))
      (cond ((intersection spec SPinf :test #'string-equal)
	     (exam-wrongspec spec)
	     (return (to-infinitivize vbc negflg))))
      (if (or negflg intflg) (setq vbc (fortify-vbc vbc)))
      (if negflg (setq vbc (negate-cjg vbc)))
      (if intflg (setq vbc (interrogativize vbc)))
      (return (conjugate1 vbc (find-TPN spec))))))

;; Modified not to check if or not the main verb is an transitive.
;;  User must not request a non-sense passivization.
(defun passivize (vbc)
  "passivize- args: verb"
;    (cond ((or unknown (IStransitive (find-mverb vbc)))
;           (cons "be" (cons (get-pptf (car vbc)) (cdr vbc))))
;          (t (format t "** non-sense passivization ignored **%")
;             vbc))
    (cons "be" (cons (get-pptf (car vbc)) (cdr vbc))))

(defun progressivize (vbc)
  "progressivize- args: verb"
  (if (ISstative (find-mverb vbc))
      vbc
      (cons "be" (cons (get-prptf (car vbc)) (cdr vbc)))))

(defun perfectivize (vbc)
    (cons "have" (cons (get-pptf (car vbc)) (cdr vbc))))

(defun negate-cjg (vbc)
    (cons (car vbc) (cons "not" (cdr vbc))))

(defun auxify (aux vbc)
  (declare (special multaux errflag))
    (cond ((not multaux)
           (setq multaux t)
           (cons aux vbc))
          (t
           (cond ((get-srt (car vbc))
                  (cons aux (preppend (get-srt (car vbc)) (cdr vbc))))
                 ((string-equal (car vbc) "may")
                  (cons aux (preppend "possibly" (cdr vbc))))
                 (t (setq errflag t))))))

(defun conjugate (vb tense prsn num)
    (cond ((string-equal vb "be") (conjg-be tense prsn num))
          ((string-equal vb "have") (conjg-have tense prsn num))
          ((string-equal vb "do") (conjg-do tense prsn num))
          ((ISmodal vb)(conjg-modal vb tense))
          ((eq tense 'past) (get-pastf vb))
          ((or (eq num 'plur)(eq prsn 'p1)(eq prsn 'p2)) vb)
          (t (vfix-prsp3 vb))))

(defun conjugate1 (vbc TPN)
    (cons (apply 'conjugate (cons (car vbc) TPN)) (cdr vbc)))

(defun conjg-be (tense prsn num); conjugation of be
    (cond ((eq tense 'past)
           (cond ((or (eq num 'plur) (eq prsn 'p2)) "were") 
		 (t "was")))
          (t
           (cond ((or (eq num 'plur) (eq prsn 'p2)) "are")
                 ((eq prsn 'p1) "am")
                 (t "is")))))

(defun conjg-do (tense prsn num); conjugation of do
    (cond ((eq tense 'past) "did")
          ((or (eq num 'plur) (member prsn '(p1 p2))) "do")
          (t "does")))

(defun conjg-have (tense prsn num); conjugation of have
    (cond ((eq tense 'past) "had")
          ((or (eq num 'plur) (member prsn '(p1 p2))) "have")
          (t "has")))

(defun conjg-modal (modalvb tense)	; conjugate a modal
  (cond ((eq tense 'pres) modalvb)
	(t (cdr (assoc modalvb
		       '(("may" . "might") ("can" . "could") ("will" . "would")
			 ("shall" . "should") ("must" . "must")) :test #'string=)))))

(defun find-mverb (vbc)
  "find-mverb - args: list 
   Returns the last element in the list (the main verb)"
    (car (last vbc)))

(defun find-TPN (spec); determine TPN (TensePersonNumber) spec.
  "find-TPN - args : specs(features)
   Puts together a list of (possibly default) Tense,Person,Number values"
  (declare (special SPpast SPp1 SPp2 SPplur))
    (list
      (cond ((intersection spec SPpast :test #'string-equal) 'past)
	    (t 'pres))
      (cond ((intersection spec SPp1 :test #'string-equal) 'p1)
            ((intersection spec SPp2 :test #'string-equal) 'p2)
            (t 'p3))
      (cond ((intersection spec SPplur :test #'string-equal) 'plur)(t 'sing))))

(defun get-vroot (vb)
  "Returns the root form of the lexeme vb"
  (or (lookup-lexical-feature 'root vb) vb))

(defun get-srt (vb)
  (cdr (assoc vb '(("can" "be" "able" "to")
		   ("must" "have" "to")
		   ("may" "possibly")) :test #'string=)))

(defun get-prptf (vb)
  (cond ((string-equal vb "be") "being")
	((string-equal vb "see") "seeing")
	(t (pres-part-form vb))))

(defun get-pptf (vb)
    (cond ((string-equal vb "have") "had")
          ((string-equal vb "be") "been")
          ((string-equal vb "do") "done")
          (t (first-atom (past-part-form vb)))))

(defun get-pastf (vb)
    (first-atom (past-form vb)))

(defun IStransitive (vb)
  (typetran 'trans (lookup vb)))

(defun ISmodal (vb)
  (or (member vb '("may" "must" "shall" "will" "can" "do")
	      :test 'string-equal)
      (typetran 'modal (lookup vb))))

(defun ISstative (vb)
  (typetran 'stative (lookup vb)))

(defun auxize (auxes vbc spec)
    (prog (xvbc multaux errflag)
	  (declare (special multaux SPinf))
          (setq xvbc vbc)
          (mapc #'(lambda (aux) (setq xvbc (auxify aux xvbc)))
                (reverse auxes))
          (setq xvbc
                (cond (errflag
                        (format t "** unexpressible modal cluster" )
                        vbc)
                      (t xvbc)))
          (if (intersection spec SPinf :test #'string-equal)
              (setq xvbc
                (cond
                 ((get-srt (car xvbc))
                  (preppend (get-srt (car xvbc)) (cdr xvbc)))
                 (t (format t "** syntactically impossible modal sequence" )
                    (cdr xvbc)))))
          (return xvbc)))

(defun vfix-prsp3 (vb)
    (fixsuffix-s vb))

(defun fixsuffix-s (word)
  "fixsuffix-s - args: string action: Pluralizes the string"
  (prog ((vowels '(#\a #\e #\i #\o #\u))
	 (len (length word)))
	(return
	  (cond ((char-equal (char word (1- len)) '#\f)
		 (concatenate 'string (subseq word 0 (1- len)) "ves"))
		((char-equal (char word (1- len)) '#\y)
		 (cond ((member (char word (- len 2)) vowels) ;;cadr
			(concatenate 'string word "s"))
		       (t (concatenate 'string
				       (subseq word 0 (1- len)) "ies"))))
		((or (char-equal (char word (1- len)) '#\x)
		     (char-equal (char word (1- len)) '#\s)
		     (char-equal (char word (1- len)) '#\z)
		     (and (char-equal (char word (1- len)) '#\o)
			  (not (member (char word (- len 2)) vowels)))
		     (and (char-equal (char word (1- len)) '#\h)
			  (char-equal (char word (- len 2)) '#\c)))
		 (concatenate 'string word "es"))
		((and (char-equal (char word (1- len)) '#\s)
		      (char-equal (char word (- len 2)) '#\i)
		      (char-equal (char word (- len 3)) '#\s))
		 (concatenate 'string (subseq word 0 (- len 2)) "es"))
		(t (concatenate 'string word "s"))))))


(defun extract-auxes (ndata)
    (cond ((ISmodal (car ndata))
           ((lambda (aux) 
              (if (not (equal aux (car ndata)))
                  (format t "**modal in non-root form; -- converted" ))
              (cons aux (extract-auxes (cdr ndata))))
            (get-vroot (car ndata))))
          (t nil)))

(defun preppend (pre lst)
    (cond ((atom pre) (cons pre lst)) (t (append pre lst))))

(defun fortify-vbc (vbc)
    (cond ((and (not (string-equal (car vbc) "be"))
                (or (= (length vbc) 1) (string-equal (cadr vbc) "to")))
           (cons "do" vbc))
          (t vbc)))

(defun interrogativize (vbc)
  (declare (special *subj*))
    (append (cons (car vbc) (cons (string (or *subj* '*subj*)) (cdr vbc))) '("?")))

(defun imperativize (vbc neg)
    (let ((xvbc (exam-multiaux vbc nil 'imperativization)))
      (cons "please" (cond (neg (preppend '("do not") xvbc))(t xvbc)))))

(defun gerundize (vbc neg)
    (let ((xvbc (exam-multiaux vbc 'may 'gerundization)))
      ((lambda (xvbc) (cond (neg (cons "not" xvbc)) (t xvbc)))
       (cons (get-prptf (car xvbc)) (cdr xvbc)))))

(defun to-infinitivize (vbc neg)
    (preppend (cond (neg (list "not" "to")) (t "to"))
              (exam-multiaux vbc 'may 'infinitivization)))

(defun exam-multiaux (vbc nogoods mesg)
    (let (subs)
      (cond ((not (ISmodal (car vbc))) vbc)
            ((and (not (intersection (flistify (car vbc))
				     (flistify nogoods)
				     :test #'string-equal))
                  (setq subs (get-srt (car vbc))))
             (preppend subs (cdr vbc)))
            (t (format t "** impossible ~S" mesg ) vbc))))

(defun exam-wrongspec (spec)
  (declare (special SPp1 SPp3 SPsing SPplur SPpres SPpast))
    (if (intersection spec (append SPp1 SPp3) :test #'string-equal)
        (format t "**subject-person spec not proper; -- ignored" ))
    (if (intersection spec (append SPsing SPplur) :test #'string-equal)
        (format t "**subject-number spec not applicable; -- ignored" ))
    (if (intersection spec (append SPpres SPpast) :test #'string-equal)
        (format t "**tense spec not applicable; -- ignored" )))

;;conv? used by whome?
(defun pres-form (vbinf)
  "pres-form - args: string 
	     action: gets the present form of word else pluralizes"
  (declare (special presf vbinf))
  (or (lookup-lexical-feature 'presf vbinf) (fixsuffix-s vbinf)))

(defun past-form (vbinf)
  "past-form - args: string
             action: gets the past form of word or just adds 'ed'."
  (declare (special past vbinf))
  (or (lookup-lexical-feature 'past vbinf) (add-suff vbinf "ed")))

(defun past-part-form (vbinf)
  "past-part-form - args: string 
	        action: gets the pptf/past form of string else adds suffix 'ed'"
  (declare (special pastp pptf vbinf))
  (cond ((lookup-lexical-feature 'pptf vbinf))
	((lookup-lexical-feature 'pastp vbinf))
	((add-suff vbinf "ed"))))

(defun pres-part-form (vbinf)
  "pres-part-form - args: string 
	          action: gets the prptf form of string else adds suffix 'ing'"
  ;; Modified:       21-FEB-1996  Alistair E. Campbell
  (declare (special prptf presp vbinf))
  (cond ((lookup-lexical-feature 'presp vbinf))
	((lookup-lexical-feature 'prptf vbinf))
	((add-suff vbinf "ing"))))

(defun typetran (typ dplist)
    (and dplist
         (or (typescan typ (get-lexical-feature 'type (car dplist)))
             (get-lexical-feature typ (car dplist))
             (typetran typ (cdr dplist)))))

(defun typescan (typ typecodes) 
  (intersection typecodes (compute-feats typ) :test #'string-equal)) 

(defun compute-feats (type)
  "Returns a list of type symbols that indicate a verb of TYPE"
    (case type
      (modal '(modal))
      (trans '(trans O-trans X-trans S-trans G-trans P-trans))
      ((stative state) '(cop state stative))
      (aux '(modal aux))
      (copula '(cop))
      (intrans '(intrans itrans cop))
      (spatial '(A-coerce P-coerce))
      (t (list type))))

; add-suff modified by Dr. Shapiro's move  84/10/08
(defun add-suff (vbinf suff)
  "add-suff - args: string suffix-string"
  (prog ((len (length vbinf))
	 (vowels '( #\a #\e #\i #\o #\u)))
	(return
	  (cond ((and (char-equal '#\y (char vbinf (1- len)))	; is y in last position
		      (not (string-equal suff "ing")))
		 (cond  ((member (char vbinf (- len 2)) vowels)
	         	 (concatenate 'string vbinf "i" suff))
			;; is there a vowel in next to last position
	         	(t (concatenate 'string (subseq vbinf 0 (1- len)) "i" suff))))
		((char-equal (char vbinf (1- len)) '#\e)
		 (cond ((member (char suff 0) vowels) 
			(cond ((and (char-equal (char vbinf (- len 2)) '#\i)
				    (string-equal suff "ing"))
			       (concatenate 'string (subseq vbinf 0 (- len 2)) 
					    "y" suff))
			      ((and (member (char vbinf (- len 2)) vowels)
				    ;; eg hoeing & seeing
				    (string-equal  suff "ing"))
			       (concatenate 'string vbinf suff))
			      (t (concatenate 'string (subseq vbinf 0 (1- len)) suff))))
		       (t (concatenate 'string vbinf suff))))
		((and (not (member (char vbinf (1- len)) vowels))
		      (member (char vbinf (- len 2)) vowels)
		      (not (member (char vbinf (- len 3)) vowels)))
		 (concatenate 'string vbinf (subseq vbinf (1- len)) suff))
		(t (concatenate 'string vbinf suff))))))

(defun flistify (s)
  "Listifies its argument, which may be a list or atom."
  (cond
    ((listp s) s)
    (t (list s))))

(defun first-atom (sexp)
  "Returns sexp if its an atom, otherwise 'car's down sexp til it finds an atom."
  (loop
      (cond
       ((atom sexp) (return sexp))
       (t (setq sexp (car sexp))))))



    
    







    
    




