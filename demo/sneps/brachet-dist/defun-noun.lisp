(in-package :snepsul)

(defvar *dmode* nil 
  "Indictates whether algorithm should operate in definition mode (t) which uses Ehrlich's
    logic to decide which information should be reported and which infomation should be
    ignored, or teaching mode (nil) which reports all information that can be found.")

(defstruct ndefn
  "A structure to store and report definitions of nouns"
  noun
  classInclusions
  probableClassInclusions
  possibleClassInclusions
  structuralElements
  probableStructuralElements
  possibleStructuralElements
  actions
  probableActions
  possibleActions
  properties
  probableProperties
  possibleProperties
  owners
  synonyms
  possibleSynonyms
  agents
  spatial
  namedIndividuals)

;;;-------------------------------------------------------------------------------
;;;
;;;     function: defineNoun
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun defineNoun (noun &optional (lexicographicMode t) (traceLevel -1))
  "Generates a definition for 'noun'. If the optional argument lexicographicMode is t then
    Ehrlich's theory will be used to exclude some information from the definition, else
    all info will be reported.  If the optional argument traceLevel is specified
    tracing/debugging will be enabled.  The values of traceLevel are 0-4 where 0 means no
    tracing and 4 means trace all functions."
  ;; the default for traceLevel is -1 so that any tracing set up manually by the user
  ;;  will not be overridden by the program when the optional argument traceLevel is not
  ;;  specified.
  (setTraceLevel traceLevel)
  (setq *dmode* lexicographicMode)
  ;; get the requested definition and print it in human readable format.
  (prettyPrintDef (if lexicographicMode 
		      (defineNounLexicographic noun)
		    (defineNounTeaching noun))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: defineNounLexicographic
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun defineNounLexicographic (noun)
  "Makes a list of information that is known about the noun and reports only the information
    that is deemed relevant according to Ehrlich's theory."
  (let (definition)
    ;; get all the info
    (setf definition (defineNounTeaching noun))
    ;; now examine all the info and eliminate parts that Ehrlich's theory says are unnecessary
    ;; if there are class inclusions, don't report probable class inclusions
    (if (ndefn-classInclusions definition) 
	(or (setf (ndefn-probableClassInclusions definition) nil)
	    (setf (ndefn-possibleClassInclusions definition) nil)))
    ;; if there are probable class inclusions, don't report possible class inclusions
    (if (ndefn-probableClassInclusions definition)
	(setf (ndefn-possibleClassInclusions definition) nil))
    ;; if there are structural elements don't report probable or possible struct. elems.
    (if (ndefn-structuralElements definition)
	;; using "or" is just a way to make sure that both setf statements get evaluated
	(or (setf (ndefn-probableStructuralElements definition) nil)
	    (setf (ndefn-possibleStructuralElements definition) nil)))
    ;; if there are probable structural elements don't report possible structural elements
    (if (ndefn-probableStructuralElements definition)
	(setf (ndefn-possibleStructuralElements definition) nil))
    ;; if there are actions don't report probable or possible actions
    (if (ndefn-actions definition)
	(or (setf (ndefn-probableActions definition) nil)
	    (setf (ndefn-possibleActions definition) nil)))
    ;; if there are probable actions don't report possible actions
    (if (ndefn-probableActions definition)
	(setf (ndefn-possibleActions definition) nil))
    ;; if there are any type of actions, don't report agents
    (if (or (ndefn-actions definition) 
	    (ndefn-probableActions definition)
	    (ndefn-possibleActions definition))
	(setf (ndefn-agents definition) nil))
    ;; if there are properties, don't report probable or possible properties
    (if (ndefn-properties definition)
	(or (setf (ndefn-probableProperties definition) nil)
	    (setf (ndefn-possibleProperties definition) nil)))
    ;; if there are probable properties, don't report possible properties
    (if (ndefn-probableProperties definition)
	(setf (ndefn-possibleProperties definition) nil))
    ;; if there are class inclusions or probable class inclusions, 
    ;;   don't report named individuals
    (if (or (ndefn-classInclusions definition) (ndefn-probableClassInclusions definition))
	(setf (ndefn-namedIndividuals definition) nil))
    
    ;; now return the revised definition
    definition
    ))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: defineNounTeaching
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun defineNounTeaching (noun)
  "Makes a list of all information that is known about the noun.  This information makes
    up the definition."
  (let (definition)
    ;; get a new instance of the structure ndefn
    (setf definition (make-ndefn))
    ;; populate the fields of the definition structure
    ;; the noun itself
    (setf (ndefn-noun definition) noun)
    ;; class inclusions
    (setf (ndefn-classInclusions definition) (classFilter (findClassInclusions noun) noun))
    ;; probable (with 'mode presumably') class inclusions
    (setf (ndefn-probableClassInclusions definition) 
      (classFilter (findProbableClassInclusions noun) noun))
    ;; possible class inclusions
    (setf (ndefn-possibleClassInclusions definition) 
      (classFilter 
       (findPossibleClassInclusions noun (append (ndefn-classInclusions definition)
						 (ndefn-probableClassInclusions definition)))
       noun))
    ;; structure
    (setf (ndefn-structuralElements definition) (findStructure noun))
    ;; probable structure
    (setf (ndefn-probableStructuralElements definition) (findProbableStructure noun))
    ;; possible structure
    (setf (ndefn-possibleStructuralElements definition) 
      (findPossibleStructure noun (ndefn-classInclusions definition)))
    ;; properties
    (setf (ndefn-properties definition) (findProperties noun))
    ;; probable properties
    (setf (ndefn-probableProperties definition) (findProbableProperties noun))
    ;; possible properties
    (setf (ndefn-possibleProperties definition) (findPossibleProperties noun))
    ;; owners
    (setf (ndefn-owners definition) (findOwners noun))
    ;; spatial information
    (setf (ndefn-spatial definition) (findSpatial noun))
    ;; synonyms
    (setf (ndefn-synonyms definition) (findSynonyms noun))
    ;; possible synonyms
    (setf (ndefn-possibleSynonyms definition)
      (findPossibleSynonyms noun
			    (union (ndefn-structuralElements definition)
				    (ndefn-probableStructuralElements definition))
			    (union (ndefn-classInclusions definition)
				    (ndefn-probableClassInclusions definition))
			    (ndefn-owners definition)
			    (ndefn-synonyms definition))) 
    ;; agents who act on 'noun's
    (setf (ndefn-agents definition) (findAgents noun))
    ;; names of specific 'noun's
    (setf (ndefn-namedIndividuals definition) (findNamedIndividuals noun))
    ;; actions
    (setf (ndefn-actions definition) (act_filter (findActions noun) noun))
    ;; probable actions
    (setf (ndefn-probableActions definition) (act_filter (findProbableActions noun) noun))
    ;; possible actions
    (setf (ndefn-possibleActions definition) (findPossibleActions noun))
    
    ;; return the definition
    definition))

;;;-----------------------------------------------------------------------------
;;;
;;;	function: traceLevel
;;;     input:    An integer 0-4 representing the amount of tracing info that
;;;              should be given:
;;;                0 - no tracing
;;;                1 - trace only definition function (defineNoun)
;;;                2 - trace different definition types (lexicographic, teaching)
;;;                3 - trace top level info finding functions
;;;                4 - trace all info finding functions.
;;;                                                            created: stn 2002
;;;-----------------------------------------------------------------------------
(defun setTraceLevel (level)
  (case level
    (0 (untrace defineNoun defineNounTeaching defineNounLexicographic
		structureOfAll findStructure indiv_struct
		findProbableStructure struct-rule struct-presume findPossibleStructure 
		struct-indiv findClassInclusions findPossibleClassInclusions 
		class-indiv findProbableClassInclusions class-sub-sup 
		class-rule findActions findProbableActions findPossibleActions
		act-object-rule act-object-&-rule act-object-presum-rule
		act-object-presum-&-rule act-object-noun
		obj-act-indiv obj-act-presume-&-rule obj-act-presum-rule
		obj-act-&-rule obj-act-rule 
		findProperties findProbableProperties findPossibleProperties prop-rule 
		prop-presume prop-indiv findOwners owner-rel owner-poss rel-for-owner
		syn-sub-sup syn-syn findSynonyms findPossibleSynonyms 
		eliminateDissimilarClasses similarSuperclassesp noAntonymsp
		antonymp eliminateDissimilarStructure similarStructurep 
		eliminateDissimilarOwners findSpatial
		similarOwnersp removeElement findNamedIndividuals named-indiv 
		findAgents agent-object action-object prop-relation-1 
		prop-& prop-&-relation-1 prop-&-relation-1-presume 
		prop-relation-1-presume prop-&-presume prop-relation-2 
		prop-&-relation-2 prop-relation-2-presume 
		prop-&-relation-2-presume prop-indiv prop-relation-1-indiv
		prop-relation-2-indiv obj-rel-1 obj-&-rel-1 obj-rel-2
		obj-&-rel-2 obj-rel-1-presume obj-&-rel-1-presume
		obj-rel-2-presume obj-&-rel-2-presume obj-rel-1-indiv
		obj-rel-2-indiv loc-prop loc-cls loc-str loc-act-obj 
		loc-rel loc-own loc-prop-cat loc-cls-cat loc-str-cat 
		loc-act-obj-cat loc-rel-cat loc-own-cat 
	      ))
    (1 (trace defineNoun))
    (2 (trace defineNoun defineNounTeaching defineNounLexicographic))
    (3 (trace defineNoun defineNounTeaching defineNounLexicographic
	      indiv_struct structureOfAll findStructure findProbableStructure 
	      findPossibleStructure findProperties findProbableProperties 
	      findClassInclusions findProbableClassInclusions findPossibleClassInclusions
	      findPossibleActions findActions findProbableActions findPossibleProperties 
	      findOwners findSynonyms findPossibleSynonyms findSpatial))
    (4 (trace defineNoun defineNounTeaching defineNounLexicographic
	      structureOfAll findStructure findSpatial
	      findProbableStructure struct-rule struct-presume findPossibleStructure 
	      struct-indiv findClassInclusions findPossibleClassInclusions 
	      class-indiv findProbableClassInclusions class-sub-sup 
	      class-rule findActions findProbableActions findPossibleActions
	      act-object-rule act-object-&-rule act-object-presum-rule
	      act-object-presum-&-rule act-object-noun
	      obj-act-indiv obj-act-presume-&-rule obj-act-presum-rule
	      obj-act-&-rule obj-act-rule indiv_struct
	      findProperties findProbableProperties findPossibleProperties prop-rule 
	      prop-presume prop-indiv findOwners owner-rel owner-poss rel-for-owner
	      syn-sub-sup syn-syn findPossibleSynonyms
	      findSynonyms eliminateDissimilarClasses similarSuperclassesp noAntonymsp
	      antonymp eliminateDissimilarStructure similarStructurep 
	      eliminateDissimilarOwners 
	      similarOwnersp removeElement findNamedIndividuals named-indiv 
	      findAgents agent-object action-object prop-relation-1 
	      prop-& prop-&-relation-1 prop-&-relation-1-presume 
	      prop-relation-1-presume prop-&-presume prop-relation-2 
	      prop-&-relation-2 prop-relation-2-presume 
	      prop-&-relation-2-presume prop-indiv prop-relation-1-indiv
	      prop-relation-2-indiv obj-rel-1 obj-&-rel-1 obj-rel-2
	      obj-&-rel-2 obj-rel-1-presume obj-&-rel-1-presume
	      obj-rel-2-presume obj-&-rel-2-presume obj-rel-1-indiv
	      obj-rel-2-indiv loc-prop loc-cls loc-str loc-act-obj 
	      loc-rel loc-own loc-prop-cat loc-cls-cat loc-str-cat 
	      loc-act-obj-cat loc-rel-cat loc-own-cat 
	      ))
    ))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: prettyPrintDef
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prettyPrintDef (definition)
  "Prints human readable version of the definition generated by the algorithm to
    standard output."
  (format t "~& Definition of ~A: " (ndefn-noun definition))
  
  (if (not (null (ndefn-classInclusions definition)))
      (format t "~& Class Inclusions: ~{~A, ~}" 
	      ;; call lexicalize on each element of the list of class inclusions
	      ;;  and then print each of them, separated by commas
	      (report (ndefn-classInclusions definition))))
  (if (not (null (ndefn-probableClassInclusions definition)))
      (format t "~& Probable Class Inclusions: ~{~A, ~}" 
	      (report (ndefn-probableClassInclusions definition))))
  (if (not (null (ndefn-possibleClassInclusions definition)))
      (format t "~& Possible Class Inclusions: ~{~A, ~}"
	      (report (ndefn-possibleClassInclusions definition))))
  (if (not (null (ndefn-structuralElements definition)))
      (format t "~& Structure: ~{~A, ~}" 
	      (report (ndefn-structuralElements definition))))
  (if (not (null (ndefn-probableStructuralElements definition)))
      (format t "~& Probable Structure: ~{~A, ~}" 
	      (report (ndefn-probableStructuralElements definition))))
  (if (not (null (ndefn-possibleStructuralElements definition)))
      (format t "~& Possible Structure: ~{~A, ~}" 
	      (report (ndefn-possibleStructuralElements definition))))
;;; If functions are put back into the algorithm, they will go here
  (if (not (null (ndefn-actions definition)))
      (format t "~& Actions: ~{~A, ~}"
	      (report (ndefn-actions definition))))
  (if (not (null (ndefn-probableActions definition)))
      (format t "~& Probable Actions: ~{~A, ~}"
	      (report (ndefn-probableActions definition))))
  (if (not (null (ndefn-possibleActions definition)))
      (format t "~& Possible Actions: ~{~A, ~}"
	      (report (ndefn-possibleActions definition))))
  (if (not (null (ndefn-agents definition)))
      (format t "~& Actions performed on a ~A: ~{~A, ~}" 
	      (ndefn-noun definition)
	      (report (ndefn-agents definition))))
  (if (not (null (ndefn-properties definition)))
      (format t "~& Properties: ~{~A, ~}" 
	      (report (ndefn-properties definition))))
  (if (not (null (ndefn-probableProperties definition)))
      (format t "~& Probable Properties: ~{~A, ~}" 
	      (report (ndefn-probableProperties definition))))
  (if (not (null (ndefn-possibleProperties definition)))
      (format t "~& Possible Properties: ~{~A, ~}" 
	      (report (ndefn-possibleProperties definition))))
  (if (not (null (ndefn-spatial definition)))
      (format t "~& ~A is a place where: ~{~A, ~}"
	      (ndefn-noun definition)
	      (report (ndefn-spatial definition))))
  (if (not (null (ndefn-owners definition)))
      (format t "~& Possessive: ~{~A, ~}" 
	      (report (ndefn-owners definition))))
  (if (not (null (ndefn-synonyms definition)))
      (format t "~& Synonyms: ~{~A, ~}" 
	      (report (ndefn-synonyms definition))))
  (if (not (null (ndefn-possibleSynonyms definition)))
      (format t "~& Possibly Similar Items: ~{~A, ~}" 
	      (report (ndefn-possibleSynonyms definition))))
  (if (not (null (ndefn-namedIndividuals definition)))
      (format t "~& Named Individuals: ~{~A, ~}" 
	      (report (ndefn-namedIndividuals definition))))
  )

;;;-------------------------------------------------------------------------------
;;;
;;;     function: report
;;;                                                              created: stn 2003
;;;-------------------------------------------------------------------------------
(defun report (nodes)
  "Returns a list of the human language representations of the input list of nodes
    with any duplicates removed."
  (cond ((null nodes) nil)
	(t (union (lexicalize (first nodes)) (report (rest nodes)) :test #'string=))))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: lexicalize
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun lexicalize (nodes)
  "Finds and returns the human language representation of the sneps nodes listed in
     'nodes' if one exists. If no human language representation can be found, the 
     node itself is returned."
  (let (humanRep)
    (cond 
     ;; if the list is empty return the empty string
     ((null nodes) nil)
     ;; if we have a list of lists process each sublist individually
     ((and (listp nodes) (listp (first nodes))) 
	   (append (lexicalize (first nodes)) (lexicalize (rest nodes))))
     ;; if we have a list consisting of two nodes, process them and
     ;;   concatenate the result
     ((and (listp nodes) (eql (length nodes) 2) 
	   (not (listp (first nodes))) (not (listp (second nodes)))) 
      (list (concatenate 'string (first (lexicalize (first nodes))) " " 
			 (first (lexicalize (second nodes))))))
     
     ;; look for lex arcs coming from the node
     ((setf humanRep #3! ((find lex- ~nodes)))
      (list (nodes2string humanRep)))
     ;; look for mod/head arcs coming from the node
     ((setf humanRep (append #3! ((find (compose mod- lex-) ~nodes))
			     #3! ((find (compose head- lex-) ~nodes))))
      (list (nodes2string humanRep)))
     ;; if the node itself is not named, see if it is a member of a named class
     ((setf humanRep 
	(removeAllSuperclasses #3! ((find (compose lex- class- ! member) ~nodes))))
      (list (nodes2string humanRep)))
     ;; if the node is part of a skolem function, just use "something"
     ;;  Note: the setf here is unnecessary, but leaving it out was confusing
     ((setf humanRep #3! ((find skf- ~nodes)))
	    (list "something"))
     
     ;; other possible representations would go here
     
     ;; if we can't find a human language representation, return the name of the sneps node
     (t (list (nodes2string nodes))))))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: removeAllSuperclasses
;;;       Due to path-based-inference making class-sub-sup transitive, extraneous
;;;       superclasses were being added to the definition -- this function removes
;;;       them.  -- This function may need to be applied to other areas of lexicalize.
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun removeAllSuperclasses (inList &optional outList) 
  "Returns all elements of the input list that do not have any subclass in the list."
  ;; if we are done checking the incoming list, check the outgoing list
  (if (null inList) outList
    (let (subs)
      ;; find all subclasses of the first element of inList
      (setf subs #3! ((find (compose lex- subclass- ! superclass lex) ~(first inList))))
      ;; if there are no subclasses in the rest of the input list
      ;;  or in the current output list
      (if (null (intersection subs (append (rest inList) outList)))
	  ;; add the element to the output list and process the rest of the input list
	  (removeAllSuperclasses (rest inList) (cons (first inList) outList))
	;; otherwise, omit the element from the output list and process the rest
	(removeAllSuperclasses (rest inList) outList)))))
    
;;;-------------------------------------------------------------------------------
;;;
;;;     function: nodes2string
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun nodes2string (nodes)
  "Converts a list of sneps nodes into a string consisting of the names of the nodes,
     separated as spaces."
  (cond ((null nodes) "")
	((not (listp nodes)) (get-node-string nodes))
	;; this is just a hack to remove extra spaces that were showing up
	((and (eql (length nodes) 1) (not (listp (first nodes)))) 
	 (get-node-string (first nodes)))
	(t (concatenate 'string (get-node-string (first nodes)) 
			" " (nodes2string (rest nodes))))))

;;; ------------------------------------------------------------------------------
;;;                         CLASS INCLUSIONS SECTION
;;; ------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: findClassInclusions
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findClassInclusions (noun)
  "Find all superclasses of 'noun'.  Find all things Y such that, if X is a 'noun' then
    X is a Y."
  (let (superclasses)
      ;; see if we can infer any class relationships that we don't explicitly know
      ;;  about yet -- after inference we will find this info in subsequent steps
     #3! ((deduce superclass (build lex $spr) subclass (build lex ~noun)))
     ;; now extract any relevant info
     (cond 
      ;; get superclasses represented using sub-sup case frame, in definition mode return
      ;;  them, in teaching mode continue accumulating information
      ((and (setf superclasses (append superclasses (class-sub-sup noun))) *dmode*) 
       superclasses)
      ;; superclasses represented using a rule
      ((and (setf superclasses (append superclasses (class-rule noun))) *dmode*)
       superclasses)
     
      ;; if we are in teaching mode, return all the accumulated info
      ;; if we are in definition mode, superclasses must be nil here, so return nil
      (t superclasses)))
   )

;;;-------------------------------------------------------------------------------
;;;
;;;	function: findProbableClassInclusions
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findProbableClassInclusions (noun)
  "Find all superclasses of 'noun' that are marked with the 'mode presumably' tag."
   (let (superclasses)
     (cond 
      ;; SN: I don't know why Ehrlich uses deduce here, this is the only place in the
      ;;  algorithm that it is used -- I am leaving it as it is until I understand it
      ((and *dmode* #3! ((deduce mode (build lex "presumably")
				 object (build subclass (build lex ~noun)
					       superclass (build lex $maybesuper)))))
       (setf superclasses (append superclasses (class-sub-sup-presum noun))))
      ;; superclasses represented using a presumable rule
      ((and (setf superclasses (append superclasses (class-rule-presum noun))) 
	    *dmode*)
       superclasses)
     
      ;; if we are in teaching mode, return all the accumulated info
      ;; if we are in definition mode, superclasses must be nil here, so return nil
      (t superclasses)))
   )
   
;;;-------------------------------------------------------------------------------
;;;
;;;	function: findPossibleClassInclusions
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findPossibleClassInclusions (noun classIncls)
  "Find possible superclasses of noun.  If some X is a member of the class 'noun'
     and X is also a member of the class Y, then Y is listed as a possible class
     inclusion for 'noun'."  
  ;; eliminate any items that would be duplicates of class inclusions or probable 
  ;;  class inclusions from the list of possible class inclusions
   (set-difference (class-indiv noun)
				;; eliminate classIncls and the noun itself from the list
				(append classIncls #3! ((find lex ~noun))))
   )

;;;-------------------------------------------------------------------------------
;;;
;;;	function: class-sub-sup
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun class-sub-sup (noun)
  "Finds superclasses of 'noun' represented using the subclass/superclass case frame."
  #3! ((find (compose superclass- ! subclass lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: class-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun class-rule (noun)
  "Finds superclasses of 'noun' represented using a rule"
  #3! ((find (compose class- cq- ! ant class lex) ~noun
	     (compose class- member member- class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: class-sub-sup-presum
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun class-sub-sup-presum (noun)
  "Finds things that are presumably superclasses of 'noun'."
  #3! ((find (compose superclass- subclass lex) ~noun
	     (compose superclass- object- ! mode lex) "presumably")))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: class-rule-presum
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun class-rule-presum (noun)
  "Finds superclasses of 'noun' represented using a rule"
  #3! ((find (compose class- object- cq- ! ant class lex) ~noun
	     (compose class- object- mode lex) "presumably"
	     (compose class- member member- class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: class-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun class-indiv (noun)
  "Finds possible level superclasses of a level noun."
    #3! ((find (compose class- ! member member- ! class lex) ~noun)))

;;;------------------------------------------------------------------------------
;;;
;;;	function: classFilter
;;;	input:  a list of superclasses as output by "classes", an empty list, 
;;;		and the noun to be defined.
;;;	output: a list of classes not redundant with the rest of the definition
;;;	calls:  mostSpecificSuperclassp, classFilter recursively
;;;------------------------------------------------------------------------------
(defun classFilter (superclasses noun &optional filtered)
  "Removes any superclasses of 'noun' which less specific than other superclasses of 'noun'
    from the list of class inclusions and returns the result.  For an example of the 
    heirarchy of class inclusions see the documentation for mostSpecificSuperclassp."
    (cond ((null superclasses) filtered)	
						;;if first element of input is a list
	  ((listp (first superclasses))         ;;add classFilter of car &
           (append filtered 			;;classFilter of cdr to output
		   (list (classFilter (first superclasses) noun filtered))
		   (classFilter (rest superclasses) noun filtered)))
						;;if car input is an ok atom
						;;add it and classFilter of
	                                        ;;cdr to output.
	  ;;; TODO: this should return all the way to the top of the recursion,
	  ;;; not just to the next level -- ie basic level info should be th
	  ;;;  only thing returned (maybe we need a different function to check for that
	  ;;;  first
	  ((basicLevelp (first superclasses))
	   (list (first superclasses)))
          ((mostSpecificSuperclassp (first superclasses) noun) 
	   (append filtered (list (first superclasses))
	   	   (classFilter (rest superclasses) noun filtered)))
						;;otherwise car input not ok.
						;;add classFilter of cdr to output.
	  (t (classFilter (rest superclasses) noun filtered))))	    

;;;-------------------------------------------------------------------------------
;;;
;;;	function: basicLevelp
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun basicLevelp (word)
  "If 'word' is basic level return T else return nil."
  #3! ((deduce member ~word class (build lex "basic ctgy"))))  

;;;------------------------------------------------------------------------------
;;;
;;;	function: mostSpecificSuperclassp  (a predicate)
;;;	input:  a noun to be defined and a superclass attributed to <noun>
;;;	returns nil if the class can be deduced from other elements of the
;;;	  	definition, t otherwise.
;;;------------------------------------------------------------------------------
(defun mostSpecificSuperclassp (class noun)
  "Returns t if there are no classes between 'class' and 'noun' in a superclass-subclass
    relation, nil otherwise.  For example if class = vertebrate, noun = cat and 
    vertebrate is a superclass of mammal which is a superclass of cat, mostSpecificSuperclassp
    would return nil because mammal is a class between cat and vertebrate."
  (not #3! ((find (compose superclass- ! subclass lex) ~noun
		      (compose subclass- ! superclass) ~class))))


;;; ------------------------------------------------------------------------------
;;;                            ACTIONS SECTION
;;; ------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: act-object-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun act-object-rule (noun)
  "Finds actions performed by all 'noun's and the objects that those actions are performed on"
  (let (actions)
    (setf actions #3! ((find (compose action- act- ! cq- ! ant ! class lex) ~noun
			     (compose action- act- ! agent member- ! class lex) ~noun)))
    ;; find objects associated with each of the actions
    (mapcar #'(lambda (act) (obj-act-rule noun act)) actions)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-act-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-act-rule (noun action)
  "Finds the objects that 'noun' performs 'action' on."
  (let (objects)
    (setf objects #3! ((find (compose object- act- ! cq- ! ant ! class lex) ~noun
			     (compose object- action) ~action)))
    (if (null objects) action
      (mapcar #'(lambda (obj) (list action obj)) objects))))
  
;;;-------------------------------------------------------------------------------
;;;
;;;	function: act-object-&-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun act-object-&-rule (noun)
  "Finds actions performed by all 'noun's and the objects that those actions are performed on"
  (let (actions)
    (setf actions #3! ((find (compose action- act- ! cq- ! &ant ! class lex) ~noun
			     (compose action- act- ! agent member- ! class lex) ~noun)))
    (mapcar #'(lambda (act) (obj-act-&-rule noun act)) actions)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-act-&-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-act-&-rule (noun action)
  "Finds the objects that 'noun' performs 'action' on."
  (let (objects)
    (setf objects #3! ((find (compose object- act- ! cq- ! &ant ! class lex) ~noun
			     (compose object- action) ~action)))
    (if (null objects) action
      (mapcar #'(lambda (obj) (list action obj)) objects))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: act-object-presum-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun act-object-presum-rule (noun)
  "Finds actions that are presumed to be performed by all 'noun's and the objects that 
   those actions are presumed to be performed on."
  (let (actions)
    (setf actions #3! ((find (compose action- act- object- mode lex) "presumably"
			     (compose action- act- object- cq- ! ant class lex) ~noun
			     (compose action- act- agent member- class lex) ~noun)))
    (mapcar #'(lambda (act) (obj-act-presum-rule noun act)) actions)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-act-presum-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-act-presum-rule (noun action)
  "Finds the objects that 'noun' presumably performs 'action' on."
  (let (objects)
    (setf objects #3! ((find (compose object- act- object- cq- ! ant class lex) ~noun
			     (compose object- action) ~action)))
    (if (null objects) action
      (mapcar #'(lambda (obj) (list action obj)) objects))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: act-object-presum-&-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun act-object-presum-&-rule (noun)
  "Finds actions that are presumed to be performed by all 'noun's and the objects that 
   those actions are presumed to be performed on."
  (let (actions)
    (setf actions #3! ((find (compose action- act- object- mode lex) "presumably"
			     (compose action- act- object- cq- ! &ant class lex) ~noun
			     (compose action- act- agent member- class lex) ~noun)))
    (mapcar #'(lambda (act) (obj-act-presume-&-rule noun act)) actions)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-act-presum-&-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-act-presume-&-rule (noun action)
  "Finds the objects that 'noun' presumably performs 'action' on."
  (let (objects)
    (setf objects #3! ((find (compose object- act- object- cq- ! &ant class lex) ~noun
			     (compose object- action) ~action)))
    (if (null objects) action
      (mapcar #'(lambda (obj) (list action obj)) objects))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: act-object-noun
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun act-object-noun (noun)
    "Finds actions performed by at least one member of the category 'noun' and
      the objects that those actions are performed on."
  (let (actions)
    (setf actions #3! ((find (compose action- act- ! agent member- ! class lex) ~noun)))
    (mapcar #'(lambda (act) (obj-act-indiv noun act)) actions)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-act-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-act-indiv (noun action)
  "Finds the objects that 'noun' performs 'action' on."
  (let (objects)
    (setf objects #3! ((find (compose object- act- ! agent member- ! class lex) ~noun
			     (compose object- action) ~action)))
    (if (null objects) action
      (mapcar #'(lambda (obj) (list action obj)) objects))))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: findActions
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findActions (noun)
  "Find actions (and the objects that those actions are performed on, if any) that are 
    performed by all 'noun's."
  (let (results indivNoun)
    ;; get an individual noun so that we can use it in the deduce
    (setf indivNoun (first #3! ((find (compose member- ! class lex) ~noun))))
    ;; see if we can infer any actions that we don't explicitly kow about yet
    (if (not (null indivNoun))
	#3! ((deduce agent ~indivNoun act $someAct)))
    ;; now extract any relevant info
    (cond
     ;;definite rule, or-entail
     ((and (setf results (append results (act-object-rule noun))) *dmode*)
      results)
     ;;definite rule, &-entail
     ((and (setf results (append results (act-object-&-rule noun))) *dmode*)
      results)
     
     ;; If we are in teaching mode, return all the info we have accumulated. If we are 
     ;;  in definition mode results = nil so return nil.
     (t results))))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: findProbableActions
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findProbableActions (noun)
  "Find actions (and the objects that those actions are performed on, if any) that can be 
    presumed to be performed by all 'noun's."
  (let (results)
    (cond 
     ;;"presumably" rule, or-entail, transitive
     ((and (setf results (append results (act-object-presum-rule noun))) *dmode*)
      results)
     ;;"presumably" rule, &-entail, transitive
     ((and (setf results (append results (act-object-presum-&-rule noun))) *dmode*)
      results)
     
     ;; If we are in teaching mode, return all the info we have accumulated. If we are 
     ;;  in definition mode results = nil so return nil.
     (t results))))

;;;--------------------------------------------------------------------------
;;;
;;;	function: findPossibleActions
;;;	input:  a noun to be defined
;;;	output: a list of actions attributed to any object of type <noun>
;;;                                                        modified: mkb 2002
;;;                                                        modified: stn 2002
;;;--------------------------------------------------------------------------
(defun findPossibleActions (noun)
  "Find actions (and the objects that those actions are performed on, if any) that are
    performed by at least one 'noun'."
  (act-object-noun noun))

;;; THE FOLLOWING IN UNNECESSARILY COMPLICATED BECAUSE WE ONLY HAVE ONE CHECK TO DO
;;;   IF MORE CHECKS ARE ADDED IN THE FUTURE THIS VERSION OF FINDPOSSIBLEACTIONS SHOULD
;;;   BE UNCOMMENTED AND USED. -- IF WE BECOME REASONABLY SURE THAT NO MORE CHECKS WILL
;;;   BE ADDED THEN WE SHOULD JUST RENAME ACT-OBJECT-NOUN TO FINDPOSSIBLEACTIONS
  
;;;  (let (results)
;;;    (cond 
;;;     ;; find actions performed by at least one member of the class 'noun'
;;;     ((and (setf results (append results (act-object-noun noun))) 
;;;	   *dmode*) 
;;;      results)
;;;     
;;;     ;; if we are in teaching mode, return all the information that we accumulated above,
;;;     ;; otherwise, return results (= nil).
;;;     (t results))))

;;;------------------------------------------------------------------------------
;;;
;;;	function: act_filter
;;;	input:  a list of actions as output by "acts", an empty list, and the
;;;		noun to be defined.
;;;	output: a list of actions not redundant with the rest of the definition
;;;	calls:  non_redundant_act, act_filter recursively
;;;------------------------------------------------------------------------------
(defun act_filter (act-list noun &optional filtered) 
  (cond 
   ;; if we are done filtering, return the filtered list
   ((null act-list)  filtered)
   ;; if the first element is a list, filter recursively
   ((listp (first act-list))
    (append filtered 
	    (list (act_filter (first act-list) noun filtered))
	    (act_filter (rest act-list) noun filtered)))
   ;; if first element is not redundant add it to filtered and filter rest
   ((non_redundant_act (first act-list) noun)
    (append filtered (list (first act-list))
	    (act_filter (rest act-list) noun filtered)))
   ;; if first element is redundant, just filter rest of list
   (t (act_filter (rest act-list) noun filtered))))
;;;------------------------------------------------------------------------------
;;;
;;;	function: non_redundant_act  (a predicate)
;;;	input:  a noun to be defined and an act attributed to <noun>
;;;	returns nil if the act can be deduced from other elements of the
;;;	  	definition, t otherwise.
;;;------------------------------------------------------------------------------
(defun non_redundant_act (act noun)
  (cond 
   ;; definite case
   (#3! ((find (compose superclass- ! subclass lex) ~noun
	       (compose class- ant- ! cq act action) ~act)) nil)
   ;; presumably
   (#3! ((find (compose superclass- ! subclass lex) ~noun
	       (compose class- ant- ! cq object act action) ~act)) nil)
   (t t)))

;;; ------------------------------------------------------------------------------
;;;                            STRUCTURE SECTION
;;; ------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: findStructure
;;;
;;;       Used for finding structure of a noun.
;;;                                                             modified: mkb 2002
;;;                                                             modified: stn 2002
;;;-------------------------------------------------------------------------------
(defun findStructure (noun)
  "Attempts to find structural features common to all members of the class noun."
  (struct-rule noun))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: findProbableStructure
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findProbableStructure (noun)
  "Attempts to find structural features that are presumably part of all 'noun's."
  (struct-presume noun))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: findPossibleStructure
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findPossibleStructure (noun superclasses) 
  "Find things that are part of some (but not necessarily all) members of the class 'noun', 
    and are also not part of all members of a superclass of 'noun'.  For example: if the
    knowledge base says 'Dogs are Mammals. Rover is a Dog. Rover has nose. All Mammals 
    have fur.' Then 'nose' would be returned but 'fur' would not be returned."
    (set-difference (indiv_struct noun) (structureOfAll superclasses)))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: struct
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun struct-rule (noun)
  "Find any things that are a part of all members of the category 'noun'."
  #3! ((find (compose part- whole member- class lex) ~noun
	     (compose part- cq- ! ant class lex) ~noun
	     (compose part- whole forall- ! ant class lex) 
	     ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: struct-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun struct-presume (noun)
  "Find any things that are presumably a part of all members of the category 'noun'."
  #3! ((find (compose part- whole member- class lex) ~noun
	     (compose part- object- mode lex) "presumably"
	     (compose part- whole forall- ! ant class lex) 
	     ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;     function: struct-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun struct-indiv (noun)
  "Find any things that are part of some individual who is a member of the
     category 'noun'."
  #3! ((find (compose part- ! whole member- ! class lex) ~noun)))

;;;------------------------------------------------------------------------------
;;;
;;;	function: indiv_struct
;;;	input:	 a noun to be defined and a list of its superclasses.
;;;	output:	 a list of possessions attributed to individuals 
;;;		 of class <noun>.  (See note on "struct")
;;;                                                            modified: mkb 2002
;;;                                                            modified: stn 2002
;;;------------------------------------------------------------------------------
(defun indiv_struct (noun)
  "Find things that are part of some (but not necessarily all) members of the class 'noun'."
  (let (parts)
    (cond ((and (setf parts (append parts (struct-indiv noun))) *dmode*)
           parts)
	  ;; if we have gotten to this point there are 2 possible scenarios:
	  ;;  1) we have found no parts -- so 'parts' = nil
	  ;;  2) we are in teaching mode (*dmode* = nil) and we want to return all the
	  ;;    info that we accumulated in the steps above.
	  (t parts))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: structureOfAll
;;;                                                             modified: stn 2002
;;;-------------------------------------------------------------------------------
(defun structureOfAll (classes)
  "Find the structure of all the classes listed as parameters."
  (if (not (null classes))
      (append (findStructure (first classes)) (structureOfAll (rest classes)))
  nil))

;;;------------------------------------------------------------------------------
;;;                              PROPERTIES SECTION
;;;------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-rule
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-rule (noun)
  "Finds properties belong to all members of the class 'noun', where 'noun' is a
     category."
  #3! ((find (compose property- ! object member- ! class lex) ~noun
	     (compose property- ! cq- ! ant ! class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-relation-1
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-relation-1 (noun)
  "Finds relationships that all members of the category 'noun' are involved
     in and the other objects that are also in the relation."
  (let (relations)
    (setf relations #3! ((find (compose rel- ! object1 member- ! class lex) ~noun
			      (compose rel- ! cq- ! ant ! class lex) ~noun)))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-rel-1 noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-rel-1
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-rel-1 (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose object2- ! object1 member- ! class lex) ~noun
			     (compose object2- ! cq- ! ant ! class lex) ~noun
			     (compose object2- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list relation obj)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-&
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-& (noun)
  "Finds properties belong to all members of the class 'noun'."
  #3! ((find (compose property- ! object member- ! class lex) ~noun
	     (compose property- ! cq- ! &ant ! class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-&-relation-1
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-&-relation-1 (noun)
  "Finds relationships that all members of the category 'noun' are involved
     in and the other objects that are also in the relation."
  (let (relations)
    (setf relations #3! ((find (compose rel- ! object1 member- ! class lex) ~noun
			      (compose rel- ! cq- ! &ant ! class lex) ~noun)))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-&-rel-1 noun rel)) relations)))
    
;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-&-rel-1
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-&-rel-1 (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose object2- ! object1 member- ! class lex) ~noun
			     (compose object2- ! cq- ! &ant ! class lex) ~noun
			     (compose object2- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list relation obj)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-relation-2
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-relation-2 (noun)
  "Finds relationships that 'noun's are involved in (where the noun is the object2) 
    and the other objects that are also in the relation."
  (let (relations)
    (setf relations 
      #3! ((find (compose rel- ! object2 lex) ~noun
		 (compose rel- ! object1 forall- ! cq ! object2 lex) ~noun)))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-rel-2 noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-rel-2
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-rel-2 (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose class- ! member object1- ! object2 lex) ~noun
			     (compose class- ! ant- ! cq ! object2 lex) ~noun
			     (compose class- ! member object1- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list obj relation)) objects)))
  
;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-&-relation-2
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-&-relation-2 (noun)
  "Finds relationships that 'noun's are involved in (where the noun is the object2) 
    and the other objects that are also in the relation."
  (let (relations)
    (setf relations 
      #3! ((find (compose rel- ! object2 lex) ~noun
		 (compose rel- ! object1 forall- ! cq ! object2 lex) ~noun)))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-&-rel-2 noun rel)) relations)))
    
;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-&-rel-2
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-&-rel-2 (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose class- ! member object1- ! object2 lex) ~noun
			     (compose class- ! &ant- ! cq ! object2 lex) ~noun
			     (compose class- ! member object1- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list obj relation)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-relation-1-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-relation-1-presume (noun)
  "Finds relationships that all members of the category 'noun' are presumably
     involved in and the other objects that are also in the relation."
  (let (relations)
    (setf relations #3! ((find (compose rel- ! object1 member- ! class lex) ~noun
			       (compose rel- ! object- cq- ! ant ! class lex) ~noun
			       (compose rel- ! object- mode lex) "presumably")))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-rel-1-oresume noun rel)) relations)))
    
;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-rel-1-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-rel-1-presume (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose object2- ! object1 member- ! class lex) ~noun
		   (compose object2- ! object- cq- ! ant ! class lex) ~noun
		   (compose object2- ! object- mode lex) "presumably"
		   (compose object2- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list relation obj)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-presume (noun)
  "Finds properties that presumably belong to all members of the class 'noun'."
  #3! ((find (compose property- object member- ! class lex) ~noun
	     (compose property- object- mode lex) "presumably"
	     (compose property- object- cq- ! ant class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-&-relation-1-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-&-relation-1-presume (noun)
  "Finds relationships that all members of the category 'noun' are presumably
     involved in and the other objects that are also in the relation."
  (let (relations)
    (setf relations #3! ((find (compose rel- ! object1 member- ! class lex) ~noun
			       (compose rel- ! object- cq- ! &ant ! class lex) ~noun
			       (compose rel- ! object- mode lex) "presumably")))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-&-rel-1-presume noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-&-rel-1-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-&-rel-1-presume (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose object2- ! object1 member- ! class lex) ~noun
		   (compose object2- ! object- cq- ! &ant ! class lex) ~noun
		   (compose object2- ! object- mode lex) "presumably"
		   (compose object2- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list relation obj)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-&-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-&-presume (noun)
  "Finds properties that presumably belong to all members of the class 'noun'."
  #3! ((find (compose property- ! object member- ! class lex) ~noun
	     (compose property- ! object- mode lex) "presumably"
	     (compose property- ! object- cq- ! &ant ! class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-relation-2-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-relation-2-presume (noun)
  "Finds relationships that 'noun's are involved in (where the noun is the object2) 
    and the other objects that are also in the relation."
  (let (relations)
    (setf relations 
      #3! ((find (compose rel- ! object2 lex) ~noun
		 (compose rel- ! object1 forall- ! cq object ! object2 lex) ~noun
		 (compose rel- ! object1 forall- ! cq mode lex) "presumably")))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-rel-2-presume noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-rel-2-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-rel-2-presume (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose class- ! member object1- ! object2 lex) ~noun
			     (compose class- ! ant- ! cq object ! object2 lex) ~noun
			     (compose class- ! ant- ! cq mode lex) "presumably"
			     (compose class- ! member object1- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list obj relation)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-&-relation-2-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-&-relation-2-presume (noun)
  "Finds relationships that 'noun's are involved in (where the noun is the object2) 
    and the other objects that are also in the relation."
  (let (relations)
    (setf relations 
      #3! ((find (compose rel- ! object2 lex) ~noun
		 (compose rel- ! object1 forall- ! cq object ! object2 lex) ~noun
		 (compose rel- ! object1 forall- ! cq mode) "presumably")))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-&-rel-2-presume noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-&-rel-2-presume
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-&-rel-2-presume (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose class- ! member object1- ! object2 lex) ~noun
			     (compose class- ! &ant- ! cq object ! object2 lex) ~noun
			     (compose class- ! &ant- ! cq mode lex) "presumably"
			     (compose class- ! member object1- ! rel ) ~relation)))
    (mapcar #'(lambda (obj) (list obj relation)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-indiv
;;;                                                              created: stn 2002
;;;------------------------------------------------------------------------------- 
(defun prop-indiv (noun)
  "Finds properties that presumably belong to all members of the class 'noun'."
  #3! ((find (compose property- ! object member- ! class lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-relation-1-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-relation-1-indiv (noun)
  "Finds relationships that one or more  members of the category 'noun' are 
     involved in and the other objects that are also in the relation."
  (let (relations)
    (setf relations #3! ((find (compose rel- ! object1 member- ! class lex) ~noun)))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-rel-1-indiv noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-rel-1-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-rel-1-indiv (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose object2- ! rel lex lex- rel- ! object1 
				      member- ! class lex) ~noun
			     (compose object2- ! rel) ~relation)))
    (mapcar #'(lambda (obj) (list relation obj)) objects)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: prop-relation-2-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun prop-relation-2-indiv (noun)
  "Finds relationships that some 'noun's are involved in (where the noun is the object2) 
    and the other objects that are also in the relation."
  (let (relations)
    (setf relations #3! ((find (compose rel- ! object2 lex) ~noun)))
    ;; find the objects associated with each of the relations
    (mapcar #'(lambda (rel) (obj-rel-2-indiv noun rel)) relations)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: obj-rel-2-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun obj-rel-2-indiv (noun relation)
  "Finds the objects that are involved in the specified relation with 'noun' and returns
    a list consisting of the relation followed by the objects."
  (let (objects)
    (setf objects #3! ((find (compose class- ! member object1- ! rel lex lex- rel- ! 
				      object2 lex) ~noun
		             (compose class- ! member object1- ! rel) ~relation)))
    (mapcar #'(lambda (obj) (list obj relation)) objects)))
    
;;;------------------------------------------------------------------------------
;;;
;;;	function: findProperties
;;;	input:  a noun to be defined
;;;	output:	a list containing any general properties that are known to
;;;		pertain to <noun>s as a class.
;;;                                                            modified: mkb 2002
;;;                                                            modified: stn 2002
;;;------------------------------------------------------------------------------
(defun findProperties (noun)
  "Finds properties that are known to belong to all things which are members of the class
    'noun'."
  (let (properties)
    (cond 
     ;; property of a ctgy.
     ((and (setf properties (append properties (prop-rule noun))) *dmode*)
      properties)
     ;; property of a ctgy, &-rule.
     ((and (setf properties (append properties (prop-& noun))) *dmode*)
      properties)
     ;; relation with 'noun' as object1
     ((and (setf properties (append properties (prop-relation-1 noun))) *dmode*)
      properties)
     ;;  relation with 'noun' as object1, &-rule
     ((and (setf properties (append properties (prop-&-relation-1 noun)))
	   *dmode*)
      properties)
     ;; relation with noun as object2
     ((and (setf properties (append properties (prop-relation-2 noun)))
	   *dmode*)
      properties)
     ;; relation with noun as object2, &-rule
     ((and (setf properties (append properties (prop-&-relation-2 noun)))
	   *dmode*)
      properties)
     
     ;; if we are in teaching mode return the info we have accumulated, if we are in
     ;;  definition mode return nil
     (t properties))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: findProbableProperties
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findProbableProperties (noun)
  "Finds properties that are presumed to belong to all things which are members of the class
    'noun'."
  (let (properties)
    (cond
     ;; presumable property
     ((and (setf properties (append properties (prop-presume noun))) *dmode*)
      properties)	
     ;; presumable property &-rule
     ((and (setf properties (append properties (prop-&-presume noun))) *dmode*)
      properties)
     ;; presumable relation, object2 is not in any category
     ((and (setf properties (append properties (prop-relation-1-presume noun))) 
	   *dmode*)
      properties)
     ;; presumable relation, object2 is not in any category, &-rule
     ((and (setf properties (append properties (prop-&-relation-1-presume noun)))
	   *dmode*)
      properties)
     ;; presumable relation with noun as object2
     ((and (setf properties (append properties (prop-relation-2-presume  noun)))
	   *dmode*)
      properties)
     ;; presumable relation with noun as object2
     ((and (setf properties (append properties (prop-&-relation-2-presume noun)))
	   *dmode*)
      properties)
     
     ;; if we are in teaching mode return the info we have accumulated, if we are in
     ;;  definition mode return nil
     (t properties))))

;;;--------------------------------------------------------------------------
;;;
;;;	function: findPossibleProperties
;;;	input:  a noun to be defined
;;;	output: a list of properties attributed to any object of type <noun>
;;;                                                        modified: mkb 2002
;;;                                                        modified: stn 2002
;;;--------------------------------------------------------------------------
(defun findPossibleProperties (noun)
  "Finds properties that belong to at least one thing which is a member of the class
    'noun'."
  (append
   ;; property belonging to a 'noun'
   (prop-indiv noun)
   ;; relation with 'noun' as object1
   (prop-relation-1-indiv noun)
   ;; relation with noun as object2
   (prop-relation-2-indiv noun)))

;;;-------------------------------------------------------------------------------
;;;                          SPATIAL INFORMATION SECTION
;;;-------------------------------------------------------------------------------
;;;-------------------------------------------------------------------------------
;;;
;;;	function: findSpatial
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findSpatial (noun)
  "If 'noun' is a location, find things that can occur in the location or that are
    true in the location"
  (append (loc-cls noun)
	  (loc-cls-cat noun)
	  (loc-str noun)
	  (loc-str-cat noun)
	  (loc-act-obj noun)
	  (loc-act-obj-cat noun)
	  (loc-prop noun)
	  (loc-prop-cat noun)
	  (loc-rel noun)
	  (loc-rel-cat noun)
	  (loc-own noun)
	  (loc-own-cat noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-cls
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-cls (noun)
  "Find things that are members of a class in the location 'noun'"
  (let (mem cls)
    (setf mem #3! ((find (compose member- ! object- ! location lex) ~noun)))
    (setf cls #3! ((find (compose class- ! object- ! location lex) ~noun)))
    (if (and mem cls)
	(list (append mem cls))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-cls
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-cls-cat (noun)
  "Find things that are members of a class in the category of locations 'noun'"
  (let (mem cls)
    (setf mem #3! ((find (compose member- ! object- ! location member- ! class lex) ~noun)))
    (setf cls #3! ((find (compose class- ! object- ! location member- ! class lex) ~noun)))
    (if (and mem cls)
	(list (append mem cls))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-str
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-str (noun)
  "Find things that are parts of a whole in the location 'noun'"
  (let (prt whl)
    (setf prt #3! ((find (compose part- ! object- ! location lex) ~noun)))
    (setf whl #3! ((find (compose whole- ! object- ! location lex) ~noun)))
    (if (and whl prt)
	(list (append whl prt))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-str-cat
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-str-cat (noun)
  "Find things that are parts of a whole in the category of locations 'noun'"
  (let (prt whl)
    (setf prt #3! ((find (compose part- ! object- ! location member- ! class lex) ~noun)))
    (setf whl #3! ((find (compose whole- ! object- ! location member- ! class lex) ~noun)))
    (if (and whl prt)
	(list (append whl prt))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-act
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
;;;(defun loc-act (noun)
;;;  "Find agents and the actions that they perform in the location 'noun'."
;;;  (let (ag act)
;;;    (setf ag #3! ((find (compose agent- object- ! location lex) ~noun)))
;;;    ;; we use a lex arc here to be sure that we are not picking up ag-act-action-object info
;;;    (setf act #3! ((find (compose lex- act- object- ! location lex) ~noun)))
;;;    (if (and ag act)
;;;	(list (append ag act))
;;;      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-act-cat
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
;;;(defun loc-act-cat (noun)
;;;  "Find agents and the actions that they perform in the category of locations 'noun'."
;;;  (let (ag act)
;;;    (setf ag #3! ((find (compose agent- object- ! location member- ! class lex) ~noun)))
;;;    ;; we use a lex arc here to be sure that we are not picking up ag-act-action-object info
;;;    (setf act #3! ((find (compose lex- act- object- ! location member- ! class lex) ~noun)))
;;;    (if (and ag act)
;;;	(list (append ag act))
;;;      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-act-obj
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-act-obj (noun)
  "Find agents, the actions that they perform in the location 'noun', and the 
    objects of the actions."
  (let (ag act obj)
    (setf ag #3! ((find (compose agent- object- ! location lex) ~noun)))
    (setf act #3! ((find (compose action- act- object- ! location lex) ~noun)))
    (setf obj #3! ((find (compose object- act- object- ! location lex) ~noun)))
    (if (and ag act obj)
	(list (append ag act obj))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-act-obj-cat
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-act-obj-cat (noun)
  "Find agents, the actions that they perform in the category of locations 'noun', and the 
    objects of the actions."
  (let (ag act obj)
    (setf ag #3! ((find (compose agent- object- ! location member- ! class lex) ~noun)))
    (setf act #3! ((find (compose action- act- object- ! location 
				  member- ! class lex) ~noun)))
    (setf obj #3! ((find (compose object- act- object- ! location 
				  member- ! class lex) ~noun)))
    (if (and ag act obj)
	(list (append ag act obj))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-prop
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-prop (noun)
  "Find objects and their properties which occur in the location 'noun'."
  (let (obj prop)
    (setf obj #3! ((find (compose object- ! object- ! location lex) ~noun)))
    (setf prop #3! ((find (compose property- ! object- ! location lex) ~noun)))
    (if (and obj prop)
	(list (append obj prop))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-prop-cat
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-prop-cat (noun)
  "Find objects and their properties which occur in the category of locations 'noun'."
  (let (obj prop)
    (setf obj #3! ((find (compose object- ! object- ! location member- ! class lex) ~noun)))
    (setf prop #3! ((find (compose property- ! object- ! location 
				   member- ! class lex) ~noun)))
    (if (and obj prop)
	(list (append obj prop))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-rel
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-rel (noun)
  "Find objects that have some relationship in the location 'noun'."
  (let (obj1 relation obj2)
    (setf obj1 #3! ((find (compose object1- object- ! location lex) ~noun)))
    (setf obj2 #3! ((find (compose object2- object- ! location lex) ~noun)))
    (setf relation #3! ((find (compose rel- object- ! location lex) ~noun)))
    (if (and obj1 relation obj2)
	(list (append obj1 relation obj2))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-rel-cat
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-rel-cat (noun)
  "Find objects that have some relationship in the category of locations 'noun'."
  (let (obj1 relation obj2)
    (setf obj1 #3! ((find (compose object1- object- ! location member- ! class lex) ~noun)))
    (setf obj2 #3! ((find (compose object2- object- ! location member- ! class lex) ~noun)))
    (setf relation #3! ((find (compose rel- object- ! location member- ! class lex) ~noun)))
    (if (and obj1 relation obj2)
	(list (append obj1 relation obj2))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-own
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-own (noun)
  "Find owners and the things they own in the location 'noun'."
  (let (owner prpty)
    (setf owner #3! ((find (compose possessor- ! object- ! location lex) ~noun)))
    ;; I assume that the nature of the property owned by a possesor is best described
    ;;  by the 'rel' arc in the object-rel-possessor case frame (e.g. given 
    ;;  object-pyewacket-rel-cat-possessor-person it is better to say "a person owns a cat"
    ;;  than "a person owns pyewacket"
    (setf prpty #3! ((find (compose rel- ! object- ! location lex) ~noun)))
    (if (and owner prpty)
	(list (append owner prpty))
      nil)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: loc-own-cat
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun loc-own-cat (noun)
  "Find owners and the things they own in the category of locations 'noun'."
  (let (owner prpty)
    (setf owner #3! ((find (compose possessor- ! object- ! location 
				    member- ! class lex) ~noun)))
    (setf prpty #3! ((find (compose rel- ! object- ! location member- ! class lex) ~noun)))
    (if (and owner prpty)
	(list (append owner prpty))
      nil)))

;;;-------------------------------------------------------------------------------
;;;                              OWNERSHIP SECTION
;;;-------------------------------------------------------------------------------
;;;-------------------------------------------------------------------------------
;;;
;;;	function: owner-rel
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun owner-rel (noun)
  "Finds things which own a 'noun', where the noun is specified by a 'rel' arc."
  #3! ((find (compose class- ! member possessor- ! rel lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: owner-poss
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun owner-poss (noun)
  "Finds things which own a 'noun', where some member of the class 'noun' is the
     object and the relation between the owner and 'noun'."
  (let (owners rel)
    (setf owners 
      #3! ((find (compose class- ! member possessor- ! object member- ! class lex) ~noun)))
    ;; find relations associated with each of the owners
    (mapcar #'(lambda (own) (rel-for-owner noun own)) owners)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: rel-for-owner
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun rel-for-owner (noun owner)
  "Finds relations where 'owner' is a possessor of a 'noun'."
  (let (rel)
    ;; find relations
    (setf rel
      #3! ((find (compose rel- ! object member- ! class lex) ~noun
		 (compose rel- ! possessor member- ! class) ~owner)))
    ;; eliminate the noun itself from the list of relations
    (setf rel (set-difference rel #3! ((find lex ~noun)) ))
    ;; if there were any relations other than the noun itself, join them with the
    ;;  owner and return that list, otherwise return nil
    (if (not (null rel))
	(cons owner rel)
      nil)))

;;;--------------------------------------------------------------------------
;;;
;;;	function: findOwners
;;;	input:  a noun to be defined
;;;	output:	a list of those things which possess any object of type <noun>		
;;;
;;;--------------------------------------------------------------------------
(defun findOwners (noun)
  "Find things that can own a 'noun'."
  ;; find owners & get rid of any stray 'nil's that may have crept into the list
  (set-difference (append (owner-rel noun) (owner-poss noun)) '(nil)))

;;;------------------------------------------------------------------------------
;;;                              INDIVIDUALS SECTION
;;;------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: findNamedIndividuals
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun findNamedIndividuals (noun)
  "Find the proper names of individuals who are members of the class noun."  
  ;; find individuals
  (named-indiv noun))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: named-indiv
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun named-indiv (noun)
  "Finds individuals with proper names who are members of the basic level class noun."
  #3! ((find (compose  proper-name- ! object member- ! class lex) ~noun)))

;;;------------------------------------------------------------------------------
;;;                       AGENTS WHO ACT ON 'NOUN'S SECTION
;;;------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: agent-object
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun agent-object (noun)
  "Find agents who perform actions on 'noun's and the actions that they perform."
  (let (agents)
    (setf agents #3! ((find (compose agent- ! act object member- ! class lex) ~noun)))
    ;; now find the actions that each agent performs on 'noun's.
    (mapcar #'(lambda (ag) (action-object noun ag)) agents)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: action-object
;;;                                                              created: stn 2002
;;;-------------------------------------------------------------------------------
(defun action-object (noun ag)
  "Find actions performed on 'noun's by 'ag'."
  ;; The act- ! act goes up and down the same arc, we need to do this because we 
  ;;   only want to find actions that Cassie believes.
  (let (actions)
    (setf actions #3! ((find (compose action- act- ! act object member- ! class lex) ~noun
			     (compose action- act- ! agent) ~ag)))
    ;; make a list of lists -- the inner lists are pairs of agents and actions 
    ;; that they perform -- the outer list consist off all pairs involving the
    ;; same agent
    (mapcar #'(lambda (act) (list ag act)) actions)))
  
;;;------------------------------------------------------------------------------
;;;
;;;	function: findAgents
;;;	input : a noun to be defined
;;;	returns : a list of the agent(s) and act(s) for which <noun>
;;;		  serves as object in an agent-act-object case frame.
;;;
;;;                                                         modified: mkb 04/2002
;;;                                                         modified: stn 2002
;;;------------------------------------------------------------------------------
(defun findAgents (noun)
  "Find agents who perform actions on 'noun's and the actions that they perform."
  (let (agents)
    (cond 
     ((and (setf agents (append agents (agent-object noun))) *dmode*)
      agents)
    
     ;; if we are in teaching mode return the info we have accumulated, else return nil
     (t agents)
     )))
    
;;;------------------------------------------------------------------------------
;;;                              SYNONYMS SECTION
;;;------------------------------------------------------------------------------

;;;-------------------------------------------------------------------------------
;;;
;;;	function: syn-syn
;;;                                                              created: stn 2002
;;;------------------------------------------------------------------------------- 
(defun syn-syn (noun)
  "Finds things which are explicitly marked as synonyms of 'noun'."
  #3! ((find (compose synonym- ! synonym lex) ~noun)))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: syn-sub-sup
;;;                                                              created: stn 2002
;;;------------------------------------------------------------------------------- 
(defun syn-sub-sup (superclasses)
  "Finds subclasses of the given list of superclasses."
  #3! ((find (compose lex- subclass- ! superclass) ~superclasses)))

;;;-----------------------------------------------------------------------------
;;;
;;;	function: findSynonyms
;;;	input: 	a noun to be defined
;;;	output: a list of synonyms and possible synonyms of <noun>
;;;
;;;							written: kae ??/??/92
;;;							modified: kae 05/12/94
;;;                                                     modified: stn 2002 
;;;-----------------------------------------------------------------------------
(defun findSynonyms (noun)
  "Find words that are specifically marked as synonyms of 'noun'."
    ;; find things that are explicitly labeled as synonyms of 'noun'.
    (removeElement noun (syn-syn noun)))

(defun findPossibleSynonyms (noun structuralElements superclasses owners synonyms)
  "Find words that have definitions which are similar to the definition of 'noun'."
  (let (possibleSynonyms)
    ;; find things that are subclasses of the same superclasses as 'noun', e.g. if we 
    ;;   are trying to define 'cat', and we know that cats and dogs are both subclasses of
    ;;   mammal, then this will find 'dog'.
    (setf possibleSynonyms (syn-sub-sup superclasses))
    ;; since 'noun' is itself a subclass of its superclass, remove 'noun' from the list
    (setf possibleSynonyms (removeElement noun possibleSynonyms))
    ;; superclasses are not synonyms, they are class inclusions, so if any snuck into the
    ;;  list of possible synonyms, get rid of them.
    (setf possibleSynonyms (set-difference possibleSynonyms superclasses))
    ;; explicit synonyms are obviously not possible synonyms -- they are definite synonyms, 
    ;;  so we need to get rid of them
    (setf possibleSynonyms (set-difference possibleSynonyms synonyms))
    ;; get rid of any possible synonyms whose superclasses are not sufficiently similar
    ;;  to the superclasses of 'noun'
    (setf possibleSynonyms (eliminateDissimilarClasses superclasses possibleSynonyms))
    ;; get rid of any possible synonyms whose structural features are not sufficiently 
    ;;  similar to the structural features of 'noun'
    (setf possibleSynonyms (eliminateDissimilarStructure structuralElements possibleSynonyms))
    ;; get rid of any possible synonyms whose ownership relations are not sufficiently
    ;;   similar to the ownership relations of 'noun'
    ;(setf possibleSynonyms (eliminateDissimilarOwners owners possibleSynonyms nil))
    ;; return the remaining synonmys
    possibleSynonyms))

;;;------------------------------------------------------------------------------
;;;	function: eliminateDissimilarClasses
;;;	input: 	  supers, a list of the superclasses of the target noun;
;;;		  possibleSynonyms, a list of possible synonyms for the target noun;
;;;		  verifiedSynonyms, a list of those possible synonmys that survive
;;;			    comparison of superclasses (initially nil)
;;;       returns:  verifiedSynonyms
;;;							written: kae ??/??/92
;;;							modified: kae 05/12/94
;;;                                                     modified: stn 2002
;;;------------------------------------------------------------------------------
(defun eliminateDissimilarClasses (supers possibleSynonyms &optional verifiedSynonyms)
  "Examine each of the elements of 'possibleSynonyms' and eliminate any synonyms whose 
    class inclusions are not sufficiently similar to the class inclusions of 'noun'."
  (cond 
   ;; if there are no more possibe synonyms, return the list of verified synonyms
   ((null possibleSynonyms) verifiedSynonyms)
   ;; if the superclasses of 'noun' are sufficiently similar to the superclasses of the
   ;;  first possible synonym on the list 'possibleSynonyms' then add the possible synonym
   ;;  to the list 'verifiedSynonyms' and check the rest of 'possibleSynonyms'
   ((similarSuperclassesp supers (union
				  (findClassInclusions (first possibleSynonyms)) 
				  (findProbableClassInclusions (first possibleSynonyms))))
    (eliminateDissimilarClasses supers (rest possibleSynonyms) 
			 (cons (first possibleSynonyms) verifiedSynonyms)))
   ;; in this case, the sets of superclasses examined above were not sufficiently similar,
   ;;   so do not add the first element of the list 'possibleSynonyms' to 'verifiedSynoynms'
   ;;   (thereby removing it) and check the rest of 'possibleSynonyms'
   (t (eliminateDissimilarClasses supers (rest possibleSynonyms) verifiedSynonyms))))

;;;------------------------------------------------------------------------------
;;;
;;;	function: similarSuperclassesp  (a predicate)
;;;	input:  two lists of superclasses, superclassesOfNoun is the superclasses of the
;;;		target noun; superclassesOfSynonym is the superclasses of a possible synonym.
;;;	returns t if target and possible synonym belong to similar lists of
;;;	        superclasses, nil otherwise.
;;;                                                     modified: stn 2002
;;;------------------------------------------------------------------------------
(defun similarSuperclassesp (superclassesOfNoun superclassesOfSynonym)
  "Return t if the two lists of superclasses are sufficiently similar."
  ;; return true if:
  (and 
   ;; the two sets of superclass have at least as many elements in common as they have 
   ;;  elements which are different
   (>= (length (intersection superclassesOfNoun superclassesOfSynonym))
       (length (union (set-difference superclassesOfNoun superclassesOfSynonym)
		      (set-difference superclassesOfSynonym superclassesOfNoun))))
   ;; they share at least two superclasses
   (>= (length (intersection superclassesOfNoun superclassesOfSynonym)) 2)
   ;; none of their superclasses are opposites of one another
   (noAntonymsp superclassesOfNoun superclassesOfSynonym)))

;;;------------------------------------------------------------------------------
;;;
;;;	function: noAntonymsp  (a predicate)
;;;	input:  two lists of superclasses, superclassesOfNoun is the superclasses of the
;;;		target noun; superclassesOfSynonym is the superclasses of a possible synonym.
;;;	returns nil if an an element of one list has an antonym in the other,
;;;	        t otherwise.
;;;------------------------------------------------------------------------------
(defun noAntonymsp (superclassesOfNoun superclassesOfSynonym)
  "Return t if there are no members of 'superclassesOfNoun' which are explicitly labeled as
    an antonym of any member of 'superclassesOfSynonym', nil otherwise."
    (cond ((null superclassesOfNoun) t) 
	  (t (if (null (antonymp (first superclassesOfNoun) superclassesOfSynonym))
                 (noAntonymsp (rest superclassesOfNoun) superclassesOfSynonym)))))

;;;------------------------------------------------------------------------------
;;;
;;;    function:  antonymp (a predicate)
;;;
;;;------------------------------------------------------------------------------
(defun antonymp (class superclassesOfSynonym)
  "Return t if there is at least one explicitly labeled antonym of 'class' among the list
    of classes 'superclassesOfSynonym', nil otherwise."
  (intersection #3! ((find (compose antonym- ! antonym lex) ~class))
		(removeElement (get-node-name class) superclassesOfSynonym)))

;;;------------------------------------------------------------------------------
;;;	function: eliminateDissimilarStructure
;;;	input:    structuralElements, a list of structural elements of the target noun;
;;;	       	  possibleSynonyms, a list of possible synonyms for the target noun;
;;;		  verifiedSynonyms, a list of those possible synonmys that survive
;;;			    comparison of structure (initially nil).
;;;       returns:  verifiedSynonyms
;;;							written: kae ??/??/92
;;;							modified: kae 05/12/94
;;;                                                     modified: stn 2002
;;;------------------------------------------------------------------------------
(defun eliminateDissimilarStructure (structuralElements possibleSynonyms 
				     &optional verifiedSynonyms)
  "Examine each element of 'possibleSynonyms' and remove it from the list if its structural
     elements are not sufficiently similar to 'structuralElements'."
  (cond 
   ;; if there are no more possible synonyms to examine, return the list of verified synonyms
   ((null possibleSynonyms) verifiedSynonyms)
   ;; if the first element of 'possibleSynonyms' has a list of structural elements that
   ;;  is sufficiently similar to the structural elements of 'noun' then add it to the list
   ;;  of verified synonyms and check the rest of the list.
   ((similarStructurep structuralElements 
		       (append (findStructure (first possibleSynonyms))
			       (findProbableStructure (first possibleSynonyms))))
    (eliminateDissimilarStructure structuralElements (rest possibleSynonyms) 
				  (cons (first possibleSynonyms) verifiedSynonyms)))
   ;; don't put the first element of 'possibleSynonyms' into the list of verified synonyms
   ;;  and keep checking the rest of the list
   (t (eliminateDissimilarStructure structuralElements (rest possibleSynonyms) 
				    verifiedSynonyms))))

;;;------------------------------------------------------------------------------
;;;
;;;	function: similarStructurep (a predicate)
;;;	input:  two lists of structural elementss, structureOfNoun is the structure 
;;;		of the target noun; structureOfSynonym is the structure of a possible
;;;		synonym.
;;;	returns t if target and possible synonym have similar lists of
;;;	        structural elements, nil otherwise.
;;;------------------------------------------------------------------------------
(defun similarStructurep (structureOfNoun structureOfSynonym)
  "Return t if there are more elements shared by the two sets of input lists than
    elements that separate them."
  (>= (length (intersection structureOfNoun structureOfSynonym))
      (length (union (set-difference structureOfNoun structureOfSynonym)
		     (set-difference structureOfSynonym structureOfNoun)))))

;;;------------------------------------------------------------------------------
;;;	function: eliminateDissimilarOwners
;;;	input:    noun, the target being defined
;;;	       	  possibleSynonyms, a list of possible synonyms for the target noun;;;
;;;		  verifiedSynonyms, a list of those possible synonmys that survive
;;;			    comparison of ownership or part/whole relation 
;;;  			    (initially nil).
;;;       returns:  verifiedSynonyms
;;;							written: kae ??/??/92
;;;							modified: kae 05/12/94
;;;------------------------------------------------------------------------------
(defun eliminateDissimilarOwners (owners possibleSynonyms verifiedSynonyms)
  "Examine each element of 'possibleSynonyms' and remove it from the list if its owners
    are not sufficiently similar to the owners of 'noun'."
  (cond 
   ;; if there are no more possible synonyms to examine, return the list of verified synonyms
   ((null possibleSynonyms) verifiedSynonyms)
   ;; if the first element of 'possibleSynonyms' has a list of owners that is 
   ;;  sufficiently similar to the owners of 'noun' then add it to the list of 
   ;;  verified synonyms and check the rest of the list.
   ((similarOwnersp owners (findOwners (first possibleSynonyms)))
    (eliminateDissimilarOwners owners (rest possibleSynonyms) 
			       (append (list (first possibleSynonyms)) verifiedSynonyms)))
   ;; don't put the first element of 'possibleSynonyms' into the list of verified synonyms
   ;;  and keep checking the rest of the list
   (t (eliminateDissimilarOwners owners (rest possibleSynonyms) verifiedSynonyms))))

;;;------------------------------------------------------------------------------
;;;
;;;	function: similarOwnersp  (a predicate)
;;;	input:  two lists of relations, ownersOfNoun is the relations of the
;;;		target noun; ownersOfSynonym is the relations of a possible synonym.
;;;	returns t if target and possible synonym have similar relations,
;;;	        nil otherwise.
;;;------------------------------------------------------------------------------
(defun similarOwnersp (ownersOfNoun ownersOfSynonym)
  "Return t if the input lists have more elements in common than elements that
     separate them."
    (>= (length (intersection ownersOfNoun ownersOfSynonym))
        (length (union (set-difference ownersOfNoun ownersOfSynonym)
	               (set-difference ownersOfSynonym ownersOfNoun)))))

;;;-------------------------------------------------------------------------------
;;;
;;;	function: removeElement
;;;
;;;-------------------------------------------------------------------------------
(defun removeElement (removeMe nodeList &optional weeded)
  "Remove an element whose name is equal to the name of 'removeMe' from 'nodeList'."
  (cond 
   ;; if all the elements have been checked, return the list of elements that passed the check
   ((null nodeList) weeded)
   ;; if the name of the node 'removeMe' is the same as the name of the first node in
   ;;  'nodeList' then do not add the first element to the list of verified elements
   ;;  (weeded) and check the rest of the nodeList
   ((string-equal (string removeMe) (get-node-string (first nodeList)))
    (removeElement removeMe (rest nodeList) weeded))
   ;; if the name of the node that has a lex pointing to 'removeMe' is the same as the
   ;;  first node in 'nodeList' then do not add the first element to the list of verified
   ;;  elementns and check the rest of nodeList
   ((string-equal (get-node-string (first #3! ((find lex ~removeMe))))
		  (get-node-string (first nodeList)))
    (removeElement removeMe (rest nodeList) weeded))
   ;; the name of the node 'removeMe' is not the same as the name of the first node in
   ;;  'nodeList', so add the first node in 'nodeList' to 'weeded'.
   (t (removeElement removeMe (rest nodeList) (cons (first nodeList) weeded)))))

;;;-------------------------------------------------------------------------------
;;;     function:  get-node-name
;;;-------------------------------------------------------------------------------
(defun get-node-name (node)
    (and (sneps:node-p node)
	 (sneps:node-na node)))

;;;-------------------------------------------------------------------------------
;;;     function:  get-node-string
;;;-------------------------------------------------------------------------------
(defun get-node-string (node)
  (string (get-node-name node)))
