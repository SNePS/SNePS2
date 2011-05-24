;;; This is the set of assertions which build the base network which    
;;; corresponds to Karen Ehrlich's vocabulary acquisition project.      
;;; Commented and made accessible by Alan Hunt and Geoffrey Koplas, '97 
;;; The following is the information that needs to get fed into the network
;;; for the Narrative Acquisition demos; namely the background knowledge
;;; on the words _other_ than the one being acquired.

; Animals are physical objects
(describe
(assert subclass (build lex "animal") superclass (build lex "phys obj")
	kn_cat "life"))

; Quadrupeds are vertebrates
(describe
(assert subclass (build lex "quadruped") superclass (build lex "vertebrate")
	kn_cat "life"))

; Ungulates are herbivores
(describe
(assert subclass (build lex "ungulate") superclass (build lex "herbivore")
	kn_cat "life"))

; Mammals are animals
(describe
(assert subclass (build lex "mammal") superclass (build lex "animal")
	kn_cat "life"))

; Mammals are vertebrates
(describe
(assert subclass (build lex "mammal") superclass (build lex "vertebrate")
	kn_cat "life"))

; Deer are mammals
(describe
(assert subclass (build lex "deer") superclass (build lex "mammal")
	kn_cat "life"))

; Deer are quadrupeds
(describe
(assert subclass (build lex "deer") superclass (build lex "quadruped")
	kn_cat "life"))

; Deer are herbivores
(describe
(assert subclass (build lex "deer") superclass (build lex "herbivore")
	kn_cat "life"))

; Deer are animals
(describe (assert subclass (build lex "deer") superclass (build lex "animal")
		  kn_cat "life"))

; Deer is a basic level category
(describe
(assert member (build lex "deer")  class (build lex "basic ctgy")
	kn_cat "life"))

;"Harts are deer"
(describe 
(assert subclass (build lex "hart") superclass (build lex "deer")
	kn_cat "life"))


; Horses are quadrupeds
(describe
(assert subclass (build lex "horse") superclass (build lex "quadruped")
	kn_cat "life"))

; Horses are herbivores 
(describe
(assert subclass (build lex "horse") superclass (build lex "herbivore")
	kn_cat "life"))

; Horses are animals
(describe
(assert subclass (build lex "horse") superclass (build lex "animal")
	kn_cat "life"))

; Horse is a basic level category
(describe
(assert member (build lex "horse")  class (build lex "basic ctgy")
	kn_cat "life"))

; Ponies are animals
(describe (assert subclass (build lex "pony") superclass (build lex "animal")
		  kn_cat "life"))

;"Dogs are mammals"
(describe
(assert subclass (build lex "dog") superclass (build lex "mammal")
	kn_cat "life"))

;"Dogs are quadrupeds"
(describe
(assert subclass (build lex "dog") superclass (build lex "quadruped")
	kn_cat "life"))

;"Dogs are animals"
(describe
(assert subclass (build lex "dog") superclass (build lex "animal")
	kn_cat "life"))

; Dog is a basic level category
(describe
 (assert member (build lex "dog")  class (build lex "basic ctgy")
	kn_cat "life"))

;"Hounds are dogs"
(describe
(assert subclass (build lex "hound") superclass (build lex "dog")
	kn_cat "life"))

;"Something is a member of the class dog"
(describe
(assert member #rex class (build lex "dog")))

;"The dog is possesed by something"
(describe
(assert object *rex rel (build lex "dog") possessor #rexboss))

;"The thing that owns the dog is a person"
(describe
(assert member *rexboss class (build lex "person")))

; Kings are persons
(describe 
(add subclass (build lex "king") superclass (build lex "person")
	kn_cat "life"))

; Wizards are persons.
(describe
(assert subclass (build lex "wizard") superclass (build lex "person")
	kn_cat "life"))

; Knights are persons.
(describe
(assert subclass (build lex "knight") superclass (build lex "person")
	kn_cat "life"))

; Person is a basic level category
(describe
(assert member (build lex "person")  class (build lex "basic ctgy")
	kn_cat "life"))

; Something is named 'King Arthur'
(describe
 (assert object #KA proper-name (build lex "King Arthur")
	 kn_cat "life"))

; King Arthur is a king
(describe
(assert member *KA class (build lex "king")
	kn_cat "life"))

; The Round Table is a table
(describe
(assert member (build lex "Round Table") class (build lex "table")
	kn_cat "life"))

; King Arthur owns the Round Table
(describe
(assert possessor *KA object (build lex "Round Table") rel (build lex "table")
	kn_cat "life"))

; There is something named 'Excalibur'
(describe
(assert object #Excalibur proper-name (build lex "Excalibur")
	kn_cat "life"))

; Excalibur is a sword
(describe
(assert  member *Excalibur class (build lex "sword")
	kn_cat "life"))

; King Arthur owns Excalibur
(describe
(assert object *Excalibur rel (build lex "sword") possessor *KA
	kn_cat "life"))

; Something is named 'Merlin'
(describe
(assert object #Mer proper-name (build lex "Merlin")
	kn_cat "life"))

; Merlin is a wizard.
(describe
(assert member *Mer  class (build lex "wizard")
	kn_cat "life"))

; Something is named 'Sir Galahad'
(describe
 (assert object #Galahad proper-name (build lex "Sir Galahad")
	 kn_cat "life"))

; Sir Galahad is a knight
(describe
(assert member *Galahad  class (build lex "knight")
	kn_cat "life"))

; Something is named 'Sir Tristram'
(describe
(assert object #Tris proper-name (build lex "Sir Tristram")
	kn_cat "life"))

; Sir Tristram is a knight
(describe
(assert member *Tris  class (build lex "knight")
	kn_cat "life"))

; Something is named 'Sir Gawain'
(describe
(assert object #SG proper-name (build lex "Sir Gawain")
	kn_cat "life"))

; Sir Gawain is a knight
(describe
(assert member *SG  class (build lex "knight")
	kn_cat "life"))

; Something is named 'King Ban'
(describe
(assert object #Ban proper-name (build lex "King Ban")
	kn_cat "life"))

; King Ban is a king
(describe
(assert member *Ban  class (build lex "king")
	kn_cat "life"))

; Something is named 'King Bors'
(describe
(assert object #Bors proper-name (build lex "King Bors")
	kn_cat "life"))

; King Bors is a king
(describe
(assert member *Bors  class (build lex "king")
	kn_cat "life"))

; Something is named 'King Lot'
(describe
(assert object #Lot proper-name (build lex "King Lot")
	kn_cat "life"))

; King Lot is a king
(describe
(assert member *Lot  class (build lex "king")
	kn_cat "life"))

; Sideboards are furniture
(describe
(assert subclass (build lex "sideboard") superclass (build lex "furniture")
	kn_cat "life"))

; Tables are furniture
(describe
(assert subclass (build lex "table") superclass (build lex "furniture")
	kn_cat "life"))

; Chairs are furniture 
(describe
(assert subclass (build lex "chair") superclass (build lex "furniture")
	kn_cat "life"))

; Chair is a basic level category
(describe
(assert member (build lex "chair")  class (build lex "basic ctgy")
	kn_cat "life"))

; Table is a basic level category
(describe
(assert member (build lex "table")  class (build lex "basic ctgy")
	kn_cat "life"))

; White is a color
(describe
(assert member (build lex "white") class (build lex "color")
	kn_cat "life"))

; Black is a color
(describe
(assert member (build lex "black") class (build lex "color")
	kn_cat "life"))

; Small is a size
(describe
(assert member (build lex "small") class (build lex "size")
	kn_cat "life"))

; "Small" and "little" are synonyms
(describe
(assert synonym (build lex "small") synonym (build lex "little")
	kn_cat "life"))

; Large is a size
(describe
(assert member (build lex "large") class (build lex "size")
	kn_cat "life"))

; "Large" and "big" are synonyms
(describe
(assert synonym (build lex "large") synonym (build lex "big")
	kn_cat "life"))

; Spears are weapons
(describe
(add subclass (build lex "spear") superclass (build lex "weapon")
	kn_cat "life"))

; "Kill" and "Slay" are synonyms
(describe
(assert synonym (build lex "kill") synonym (build lex "slay") kn_cat "life"))

;; ######################
;;          RULES
;; ######################

; If something is a hound then that thing hunts.
(describe
(add forall $hound1
        ant (build member *hound1 class (build lex "hound"))
        cq (build agent *hound1 act (build action (build lex "hunt")))
	kn_cat "life-rule.1"))

;; If something bays and it is a member of some class then
;;  that class is a subclass of hound 
(describe
 (add forall ($bayer $categ)
   &ant ((build agent *bayer act (build action (build lex "bay")))
	 (build member *bayer class *categ))
   cq (build subclass *categ superclass (build lex "hound"))))

;; Newly inferred information:
;; 
;; "Deer" is a basic category
;; "Horse" is a basic category
;; "Dog" is a basic category
;; The dog (b1) is a quadruped
;; The dog (b1) is a mammal
;; The dog (b1) is a vertebrate
;; The dog (b1) is a physical object
;; The dog (b1) is a animal
;; "Person" is a basic category
;; King Arthur is a king
;; King Arthur is a person
;; The Round Table is a piece of furniture
;; The Round Table is a table
;; Excalibur is a sword
;; Merlin is a wizard
;; Merlin is a person
;; Sir Galahad is a knight
;; Sir Galahad is a person
;; Sir Tristram is a knight
;; Sir Tristram is a person
;; Sir Gawain is a knight
;; Sir Gawain is a person
;; King Ban is a king
;; King Ban is a person
;; King Bors is a king 
;; King Bors is a person
;; King Lot is a king
;; King Lot is a person
;; "Chair" is a basic category
;; "Table" is a basic category
;; White is a color
;; Black is a color
;; Small is a size
;; Large is a size

;; If one thing bites another and the biter is a member of some 
;;  class then that class is a subclass of animal 
(describe
(add forall ($animal1 $bitten *categ)
        &ant ((build agent *animal1 act (build action (build lex "bite")
		                              object *bitten))
	     (build member *animal1 class *categ))
        cq (build subclass *categ superclass (build lex "animal"))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; None

;; If something is an animal and part of another class then
;;   presumably, that class is a subclass of animal 
(describe
(add forall (*animal1 $class2)
        &ant ((build member *animal1 class (build lex "animal"))
              (build member *animal1 class *class2))
        cq (build mode   (build lex "presumably")
		  object (build subclass *class2 superclass (build lex "animal")))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;;
;; Presumably, quadruped is a subclass of animal
;; Presumably, vertebrate is a subclass of animal
;; Presumably, mammal is a subclass of animal
;; Presumably, deer is a subclass of animal
;; Presumably, hart is a subclass of animal
;; Presumably, horse is a subclass of animal
;; Presumably, dog is a subclass of animal
;; Presumably, hound is a subclass of animal
;; Presumably, physical object is a subclass of animal


;; If something is presumably an animal and is a member of another class then
;;  presumably, that class is a subclass of animal 
(describe
(add forall (*animal1 *class2)
        &ant ((build mode (build lex "presumably")
		     object (build member *animal1 class (build lex "animal")))
              (build member *animal1  class *class2))
        cq (build mode (build lex "presumably")
		  object (build subclass *class2 
			        superclass (build lex "animal")))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; None

;; If something is a mammal and a member of another class then
;; presumably, that class is a subclass of mammal
(describe
(add forall (*animal1 *class2)
        &ant ((build member *animal1  class (build lex "mammal"))
              (build member *animal1  class *class2))
        cq (build mode   (build lex "presumably")
		  object (build subclass *class2 superclass (build lex "mammal")))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; Presumably, mammal is a subclass of mammal
;; Presumably, deer is a subclass of mammal
;; Presumably, hart is a subclass of mammal
;; Presumably, horse is a subclass of mammal
;; Presumably, pony is a subclass of mammal
;; Presumably, hound is a subclass of mammal
;; Presumably, animal is a subclass of mammal
;; Presumably, vertebrate is a subclass of mammal
;; Presumably, quadruped is a subclass of mammal
;; Presumably, dog is a subclass of mammal
;; Presumably, physical object is a subclass of mammal

;; If something is a mammal, then it presumably bears live young
(describe
(add forall *animal1
	ant (build member *animal1  class (build lex "mammal"))
	cq (build mode (build lex "presumably")
		  object (build agent *animal1 act (build action (build lex "bear")
							  object (build lex "live young"))))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; The dog (b1) bears live young

;"If something bears something else, the bearer is an animal"
(describe
(add forall (*animal1 $animal2)
        ant (build agent *animal1 act (build action (build lex "bear") 
					     object *animal2))
	cq (build member *animal1  class (build lex "mammal"))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; None

; If there is a person and that person can carry something, then the
;  thing that can be carried has the property "small".
(describe
(add forall ($thingy $person)
        &ant ((build member *person class (build lex "person"))
              (build agent *person act (build action (build lex "carry") object *thingy)))
        cq (build object *thingy property (build lex "small"))
	kn_cat "life-rule.2"))

;; Newly inferred information:
;; 
;; None

; If something wants something then the thing that is wanted is valuable
(describe
(add forall (*thingy *person)
        ant (build agent *person act (build action (build lex "want") object *thingy))
        cq (build object *thingy property (build lex "valuable"))
	kn_cat "life-rule.2"))

;; Newly inferred information:
;; 
;; None

; If something says that it wants another thing, then it actually
;  does want that thing
(describe
(add forall (*thingy *person)
        ant (build agent *person act (build action (build lex "say that")
                                            object (build agent *person  
                                                          act (build action (build lex "want")
                                                                     object *thingy))))
        cq (build agent *person 
		  act (build action (build lex "want") 
		             object *thingy))
	kn_cat "life-rule.2"))

;; Newly inferred information:
;; 
;; None.

;
(describe
; If a member of some class has a property that is a color, 
;  then the class that it is a member of is a subclass of 'physical object'
(add forall ($thing $prop $foo)
        &ant ((build member *foo class *thing)
              (build object *foo property *prop)
              (build member *prop class (build lex "color")))
        cq (build subclass *thing superclass (build lex "phys obj"))
	kn_cat "intrinsic"))

;; Newly inferred information:
;; 
;; None.

; If a member of some class has a property that is a size,
;  then the class that it is a member of is a subclass of 'physical object'
(describe
(add forall (*thing *prop *foo)
        &ant ((build member *foo class *thing)
              (build object *foo property *prop)
              (build member *prop class (build lex "size")))
        cq (build subclass *thing superclass (build lex "phys obj"))
	kn_cat "intrinsic"))

;; Newly inferred information:
;; 
;; None.

; A weapon damages
(describe
(add forall $weapon1
        ant (build member *weapon1  class (build lex "weapon"))
        cq (build agent *weapon1 act (build action (build lex "damage")))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; None.

; If something is an elder then that thing is old and is presumably a person
(describe
(add forall $eld1
	ant (build member *eld1  class (build lex "elder"))
	cq ((build object *eld1 property (build lex "old"))
	    (build mode (build lex "presumably")
		   object (build member *eld1 class (build lex "person"))))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; None.

; if one thing chases another, the former runs behind the latter
(describe
(add forall ($chaser $chasee)
	ant (build agent *chaser act (build action (build lex "chase") object *chasee))
	cq ((build agent *chaser act (build action (build lex "run")))
	    (build object1 *chaser rel (build lex "behind") object2 *chasee))
	kn_cat "life-rule.1"))

;; Newly inferred information:
;; 
;; None.
