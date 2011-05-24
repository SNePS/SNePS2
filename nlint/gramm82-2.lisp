;;; -*- Base: 10; Syntax: Common-lisp -*-



;;;  This is the original nlu82 grammar modified with the generation grammar for rules 
;;;
;;;  And has been further *MODIFIED* to run under SNePS-2.   ssc. 
;;;
;;;  The necessary modifications for SNePS-2 are as follows:
;;;
;;;      'findorbuild'     ----->   'build'
;;;      '(forbtop xx yy)  ----->   '(! (build xx yy))'
;;;      '(build xx- yy)'  ----->   '(find xx- (build xx #new_yy)'
;;;      '(^ define ...)'  ----->   '(^^ define ...)
;;;      '*x' not used as  ----->   'sneps:|*| x'
;;;       a register, but 
;;;       rather as a macro
;;;
;;;
;;;DO NOT MODIFY --> For now.


;;;  DEFINE Relations
(^^ define adj after agent before class etime lex member name named
    object stime verb which)

;;; Establish the NOW.
(^^ build stime now) = now

;;; ADD-INDEF
(^^ defun add-indef (phrase)
     (let ((string-phrase (cond ((listp phrase) (princ-to-string (car phrase)))
				(t (princ-to-string phrase)))))
       (cond ((member (char string-phrase 0) '("a" "e" "i" "o" "u") :test 'string-equal)
	      (append '(|an|) (flistify phrase)))
	     (t (append '(\a) (flistify phrase))))))

;;; MY-LENGTH
(^^ defun mylength (s) 
     (cond ((null s) 0)
           ((atom s) 1)
           (t (1+ (mylength (cdr s))))))


;;;
;;;
;;;  STATES
;;;
(s ; parse a sentence and generate a response.
  (push sp t (jump respond)))

(respond ; generate the response represented by the semantic node in *.
         (jump gs (and (getr *) (eq (getr typ) 'd))
                  ; the input was a statement represented by *.
                  (setr string '(i understand that)))
         (jump gs (and (getr *) (eq (getr typ) 'q))))
                  ; the input was a question answered by *.

(sp ; parse a sentence.
    (wrd ("who" "what") ; if it starts with "who" or "what", it's a question.
             t (setr typ 'q) (liftr typ) (setr subj %x) (to v))
    (push npp ; a statement starts with a noun phrase -- its subject.
            t (sendr typ 'd) (setr typ 'd) (liftr typ) (setr subj *)
              (to v)))

(v (cat v t ; the next word must be a verb.
          (setr vb (build lex (^ (getr *)))) (setr tns (getf tense))
          (to compl)))

(compl ; consider the word after the verb.
       (cat v (and (getf pprt) (overlap (getr vb) (geta lex- 'be)))
              ; it must be a passive sentence.
              (setr obj (getr subj)) (setr subj nil) (setr vc 'pass)
              (setr vb (build lex (^ (getr *)))) (to sv))
       (cat adj (overlap (getr vb) (geta lex- 'be))
                ; a predicate adjective.
                (setr adject (build lex (^ (getr *)))) (to svc))
       (jump sv *))

(sv ; start building the temporal structure.
    (jump o
	  (and (getr *) (eq (getr typ) 'q)))
            ; ignore the tense of a question.
    (jump o
	  (and (getr *) (eq (getr tns) 'pres))
            ; present means starting before and ending after now.
            (setr stm (build before *now before
			     (build after *now) = etm)))
    (jump o
	  (and (getr *) (eq (getr tns) 'past))
            ; past means starting and ending before now.
            (setr stm (build before (build before *now) = etm))))

(o ; parse what follows the verb group.
   (wrd "by" (eq (getr vc) 'pass) ; a passive sentence will have "by np".
           (to pag))
   (push npp t ; an active sentence will have an object np.
             (sendr typ) (sendr pats) (setr obj *) (liftr vc) (to svo)))

(pag (push npp t ; parse the subject np of a passive sentence.
               (sendr typ) (sendr pats) (setr subj *) (liftr vc) (to svo)))

(svo ; return a semantic node.
     (pop (build agent (^ (getr subj)) verb (^ (getr vb))
                 object (^ (getr obj))  stime (^ (getr stm)) etime *etm)
          (eq (getr typ) 'd)) ; an agent-verb-object statement.
;     (pop (deduce agent (^(getr subj)) verb (^(getr vb)) object (^(getr obj)))
;          (and (nullr pats) (eq (getr typ) 'q)))
;     (pop (excpt
;             (find arg- (deduce min 2 max 2
;                           arg (tbuild agent (^ (getr subj))
;                                    verb (^(getr vb)) object (^(getr obj)))
;                           arg (tbuild min (^(mylength (getr pats)))
;                                    max (^(mylength (getr pats)))
;                                    arg (^(getr pats)))))      
;             '(arg))
;          (eq (getr typ) 'q))) ; an agent-verb-object question.
      )

(svc (pop (eval (buildq (! (build which + adj +)) subj adject))
          (eq (getr typ) 'd)) ; a noun-be-adj statement.
     (pop  (deduce which (^ (getr subj)) adj (^ (getr adject)))
           (eq (getr typ) 'q))) ; a noun-be-adj question.

(npp ; parse a noun phrase.
     (wrd ("a" "an") t (setr indef t) (to npdet))
     (jump npdet *))

(npdet ; parse a np after the determiner.
       (cat adj t ; hold adjectives for later.
                (hold 'adj (build lex (^ (getr *)))) (to npdet))
       (cat n (and (getr indef) (eq (getr typ) 'd))
              ; "a noun" means a new member of the class noun.
              (setr nh
                 (find member-
                   (build member #new_member class (build lex (^ (getr *))))))
              (to npa))
       (cat n (and (getr indef) (eq (getr typ) 'q))
              ; "a noun" in a question refers to a known noun.
              (setr nh %y)
              (addr pats (tbuild member *y class (tbuild lex (^ (getr *)))))
              (liftr pats) (to npaq))
       (cat npr t ; a proper noun is someone's name.
                (setr nh (find named-
                           (! (build named #new_named name (build lex (^(getr *)))))))
                (to npa)))

(npa ; remove all held adjectives and build which-adj propositions.
     (vir adj t (! (build which (^ (getr nh)) adj (^ (getr *)))) (to npa))
     (pop nh t))

(npaq ; remove held adjectives and add patterns to a query np
      (vir adj t (addr pats (tbuild which (^(getr nh)) adj (^(getr *))))
                  (liftr pats) (to npaq))
      (pop nh t))

(gs ; generate a sentence to express the semantic node in *.
  (group
   (jump gs1 (and (geta object) (overlap (getr vc) 'pass))
            ; a passive sentence is "object verb by agent".
            (setr subj (geta object)) (setr obj (geta agent)) (setr prep 'by))
   (jump gs1 (and (geta agent) (disjoint (getr vc) 'pass))
            ; an active sentence is "agent verb object".
            (setr subj (geta agent)) (setr obj (geta object))
            (setr vc 'act))
   (jump gs1 (and (geta which)) (setr subj (geta which))
            ; a which-adj sentence is "which be adj".
            (setr obj (geta adj)) (setr vc 'act))
   (call np (geta member) (geta member) (addr done *) (sendr done) reg
            ; a member-class sentence is "member be a class"
            (addr string reg) (jump gmemcl)))
  )

(gmemcl (call pred * t ; generate a verb group using "be"
                   (sendr numbr 'sing) (sendr vc 'act) (sendr vb 'be)
                   reg (addr string reg) (jump gmemcl-cl)))

(gmemcl-cl (call np (geta class) t ; generate a np for the class
                 (sendr done) (sendr indef t) (sendr numbr 'sing) *
                 (addr string  *) (to end)))

(gs1 (call np subj t ; generate a np to express the subject.
              (addr done *) (sendr done) (sendr numbr) reg
              (addr string reg) (jump svb)))

(svb (call pred * t ; generate a verb group.  use "be" if no other verb.
                (sendr numbr) (sendr vc)
                (sendr vb (or (geta lex (geta verb)) "be"))
           reg (addr string reg) (jump surobj)))

(surobj (call np obj obj ; generate a np to express the obj if there is one.
                 (sendr done) * (addr string prep *) (to end))
        (to (end) t))

(pred ; figure out the proper tense.
      (call past (geta etime) t tense (to genvb))
                 ; past tense depends on ending time.
      (call futr (geta stime) t tense (to genvb))	
                 ; future tense depends on starting time.
      (to (genvb) t (setr tense 'pres))) ; present tense is the default.

(genvb ; return the verb group.
       (pop (verbize (getr numbr) (getr tense) (getr vc)
		     (princ-to-string (getr vb))) t))

(past ; if we can get to *now by before arcs, it is past tense.
      (to (pastend) (overlap * *now))
      (to (past (geta before)) t))
(pastend (pop 'past t))

(futr ; if we can get to *now by after arcs, it is future tense.
      (to (futrend) (overlap * *now))
      (to (futr (geta after)) t))
(futrend (pop 'futr t))

(np ; the proper number is pl for a class, sing for an individual
    (group
       (jump np1 numbr)
       (jump np1 (or (geta sub-) (geta sup-) (geta class-)) (setr numbr 'pl))
       (jump np1 (not (or (geta sub-)(geta sup-)(geta class-)))
                    (setr numbr 'sing))
    )
)

(np1 
     (call adjs (append (flistify (geta which-)) '(<>)) (geta which-) 
                (sendr done) string (jump npadj))
     (jump npadj *))

(npadj  ; generate a np to express *.
;    (to (nppop) (and (geta :var) (geta :val))
;                 ; use the value of a variable
;                 (addr string (wrdize (getr numbr) (geta :val))))
;    (to (nppop) (geta :var)
;                 ; use "something" for a free variable 
;                 (addr string 'something))
    (to (nppop) (geta lex)
              ; just use the word at the end of the lex arc if present.
	      ; CHANGE: (wrdize (getr numbr)(geta lex)) after wrdize fixed
              (addr string (geta lex)))
    (call names (append (flistify (geta named-)) '(<>)) (geta named-) 
               ; use its name if it has one
               (sendr done) reg
               (addr string reg) (to nppop))
    (call classes (append (flistify (geta member-)) '(<>)) (geta member-)
               ; use its class if it has one
               (sendr done) (sendr numbr) reg (setr indef t)
               (addr string reg) (to nppop))
    (to (nppop) t ; use its identifier if nothing else
                  (addr string *)))

(nppop (pop (add-indef (getr string)) indef)
       (pop string (nullr indef)))

(adjs ; generate a string of adjectives, one for each which-adj node in *.
      (wrd <> t (to end))			; was (to endpop)
      (call np (geta adj) (disjoint * done) (sendr done) *
	       (addr string *) (to adjs))
      (to (adjs) t)
      (pop string t))

(names ; use the first usable name
       (call np (geta name) (and (disjoint * '<>)
                                 (disjoint * done))
                string (to flush))
       (to (names) (and (disjoint * '<>) (overlap * done))))

(flush (wrd <> t (to end))			; was (to endpop)
       (to (flush) (disjoint * '<>)))

(classes ; use the first usable class
         (call np (geta class) (and (disjoint * '<>) (disjoint * done))
                  (sendr numbr) string (to flush))
         (to (classes) (and (disjoint * '<>) (overlap * done))))
;
;=============== rulegrm 2/5/81 ======================
;========== written by stuart c. shapiro ==============
;
;       this is a grammar for generating english from sneps deduction
;  rules.  to use it do the following --
;
;     1.  add a generation grammar for atomic assertions.
;         this grammar must start at the state 'gs', must build a
;         sentence in the register 'string', and must transfer to
;         the state 'end', having consumed its node.
;         the register 'neg' will be set to 't', if the sentence
;         is to be negated.
;
;     2.  only the top level of your network should terminate at
;         the state 'end'.  Levels you call or push to should
;         terminate at some other state, such as 'endpop'.
;
;     3.  start the parser at state 'g', or use the sneps function,
;         'surface'.
;
;     4.  to trace inferences in english, (setq infertrace 'surface).
;

(g (jump grule * (setr conj 'and) (setr tab 0)))

(g1 (group (jump grule (and (getr *) (getr count))
                (addr string (getr count) '\))
                (setr count (add1 (getr count))))
           (jump grule (and (getr *) (nullr count)))))

(grule (group (jump grulec (and (getr *) (not (or (geta avb) (geta evb)))))
              (jump grulec (and (geta avb) (atom (geta avb)))
                (addr string "for every" (pack(snoc(unpack(geta avb))'\,))))
              (jump grulec (and (geta evb) (atom (geta evb)))
                    (addr string "there exists a" (geta evb) "such that"))
              (jump grulec (and (getr *) (eq (mylength (geta avb)) 2))
                    (addr string "for every" (car(geta avb)) "and"
                            (pack(snoc(unpack(cadr (geta avb)))'\,))))
              (jump grulec (and (getr *) (eq (mylength (geta evb)) 2))
                    (addr string "there exists a" (car (geta evb)) "and a"
                                 (cadr (geta evb)) "such that"))
              (jump grulec (and (getr *) (greaterp (mylength (geta avb)) 2))
                (addr string "for every"
                ((lambda (vbls)
                   (append (mapcar
                            (function
                             (lambda(v) (pack (snoc (unpack v) '\,))))
                            (cddr vbls))
                           (list (cadr vbls) 'and
                                 (pack(snoc(unpack(car vbls))'\,)))))
                 (geta avb))))
              (jump grulec (and (getr *) (greaterp (mylength(geta evb)) 2))
                (addr string "there exists"
                 ((lambda (vbls)
                    (append
                     (mapconc
                      (function
                        (lambda(v)(list 'a(pack (append (unpack v) '(\,))))))
                      (cddr vbls))
                     (list 'a (cadr vbls) "and a"(car vbls))))
                 (geta evb)) "such that"))
             ))

(grulec(group (jump gorent (geta ant))
       (jump gnumquant (geta pevb) (setr emax (geta emax))
                                   (setr emin (geta emin)))
       (jump g&ent (or (geta cq) (geta dcq)))
       (jump gthresh (geta thresh) (setr tot (mylength (geta arg)))
                     (setr thresh (geta thresh)))
       (jump gandor (geta max) (setr tot (mylength (geta arg)))
                    (setr min (geta min)) (setr max (geta max)))
       (jump gs *))
      (to (end) t (prin3 <> "======error -- no generation grammar for" <>)
                   (describe (^ (getr \*)))))

(gandor (group (jump gandorn neg)
        (jump gandorp (nullr neg))))

(gandorn(group(jump gandorp-n1n (overlap min tot) (setr min 1))
         (jump gandorp-nnn (and (overlap min 1) (overlap max tot))
                       (setr min tot))
         (call gandorp (append (flistify (geta arg)) '(<>)) t
               (sendr conj 'and)
                (sendr tab (plus (getr tab) 3)) str
                 (addr string "it is not the case that" '<>
                       '% (plus 3 (getr tab)) str) (to end))))

(gandorp(group(call grule (append (flistify (geta arg)) '(<>)) (overlap max 0)
           (sendr conj 'and)(sendr neg (nullr neg))
           (sendr tab) * (addr string *) (to end))
       (jump gandorp-nnn (overlap min tot))
       (jump gandorp-n1n (and (overlap min 1) (overlap max tot)))
         (call grule (append (flistify (geta arg)) '(<>))
                     (and (overlap min 1) (overlap max 1))
                     (sendr conj 'or) (sendr tab (plus 7 (getr tab)))
                     (sendr neg) str
                     (addr string "either" str) (to end))
   (call g1 (append (flistify (geta arg)) '(<>)) (overlap max tot) (sendr neg)
           (sendr count 1) (sendr tab (plus 3 (getr tab))) str
        (addr string "at least" min "of the following:" '<>
         '% (plus 3 (getr tab)) str) (to end))
   (call g1 (append (flistify (geta arg)) '(<>)) (overlap min 0) (sendr count 1)
          (sendr tab (plus 3 (getr tab))) (sendr neg) str
          (addr string "at most" max
"of the following:" '<>
             '% (plus 3 (getr tab)) str) (to end))
   (call g1 (append (flistify (geta arg)) '(<>)) (overlap min max) (sendr count 1)
         (sendr tab (plus 3 (getr tab))) (sendr neg) str
         (addr string "exactly" min "of the following:" '<>
               '% (plus 3 (getr tab)) str) (to end))
   (call g1 (append (flistify (geta arg)) '(<>)) t(sendr count 1)
       (sendr tab (plus 3 (getr tab))) (sendr neg) str
       (addr string 'between min 'and max
                      "of the following:" '<>
                     '% (plus 3 (getr tab)) str) (to end))))

(gandorp-nnn (call grule (append (flistify (geta arg)) '(<>)) t
          (sendr conj 'and) (sendr neg)
          (sendr tab) * (addr string *) (to end)))
(gandorp-n1n (call grule (append (flistify (geta arg)) '(<>))t
                  (sendr conj 'or) (sendr tab (plus 7 (getr tab)))
                  (sendr neg) str
                  (addr string "either" str "(or"
                                         (cond ((overlap max 2) "both)")
                                               (t (list 'all (getr max) '\)))))
                 (to end)))

(gnumquant (call grule (append (flistify (geta &ant))
			       (flistify (geta cq))
			       '(<>))
		 (nullr neg)
                 (sendr conj 'and)
                 (sendr tab (plus 3 (getr tab))) str
                 (addr string "there is at most" (getr emax)
		       (geta pevb) "such that" '<> '% (plus 3(getr tab)) str)
                 (to end)))
(g&ent(group(call grule (append (flistify (geta &ant)) '(<>)) (geta &ant) (sendr conj 'and)
             (sendr tab (plus 3 (getr tab)))(sendr string 'if)str
            (addr string (if (getr neg)  "it is not the case that" '<>) str
                     '<> '% (plus 3 (getr tab))) (jump gcq))
       (call grule (append (flistify (geta cq)) '(<>)) t (sendr tab) (sendr conj 'and)
             * (addr string *) (to end))))
(gorent (call grule (append (flistify (geta ant)) '(<>)) t (sendr conj 'or)
              (sendr tab (plus 3 (getr tab))) (sendr string 'if) str
              (addr string (if (getr neg) "it is not the case that" '<>) str
                   '<> '% (plus 3 (getr tab))) (jump gcq)))

(gcq (call grule (append (flistify (geta cq)) '(<>)) (geta cq)
           (sendr conj 'and)(sendr string 'then)(sendr tab(plus 8(getr tab)))
           * (addr string *) (to end)))

(gthresh
   (group(jump gandorp neg (setr min thresh) (setr max (sub1 (getr tot))))
         (call grule (append (flistify (geta arg)) '(<>))
               (and (overlap tot 2) (overlap thresh 1))
             (sendr tab (plus 3 (getr tab))) (sendr conj "if and only if")
               str (addr string str) (to end))
         (call g1 (append (flistify (geta arg)) '(<>)) (overlap thresh 1)
               (sendr count 1) (sendr tab (plus 3 (getr tab)))
               (sendr conj 'and)
            str(addr string "the following are equivalent:" '<> str) (to end))
         (call g1 (append (flistify (geta arg)) '(<>)) t (sendr count 1)
          (sendr conj 'and)
               (sendr tab (plus 3 (getr tab)))
               str (addr string "if any of the following are true,"
                                "they all are:" '<> str) (to end))))

;(end (group (wrd <> t (to endpop))
;            (jump g1 * (addr string conj '<> '% (getr tab)))
;            (pop string t)))		      

;(endpop (pop string t))

(end (pop string t))
