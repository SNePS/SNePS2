("a" ((ctgy . art)(definite . nil)))
("the" ((ctgy . art)(definite . t)))

("Computer Science" ((ctgy . npr)))
("John" ((ctgy . npr)))
("Mary" ((ctgy . npr)))

("computer" ((ctgy . n)))
("Computer" ((ctgy . multi-start) (multi-rest . ("Science")))
	    ((ctgy . n)(root . "computer")))
("dog" ((ctgy . n)))
("man" ((ctgy . n)(plur . "men")))
("men" ((ctgy . n)(root . "man")(num . plur)))
("woman" ((ctgy .  n)(plur . "women")))
("women" ((ctgy . n)(root . "woman")(num . plur)))

("saw" ((ctgy . n))
       ((ctgy . v)(root . "see")(tense . past)))

("believe" ((ctgy . v)(stative . t)))
("bit" ((ctgy . v)(root . "bite")(tense . past)))
("bite" ((ctgy . v)(num . plur)(past . "bit")))
("like" ((ctgy . v)(num . plur)))
("see" ((ctgy . v)(past . "saw")))
("sleep" ((ctgy . v)(past . "slept")))
("slept" ((ctgy . v)(root . "sleep")(tense . past)))	 
("study" ((ctgy . v)))
("use" ((ctgy . v)))

("who" ((ctgy . wh)))
("what" ((ctgy . wh)))

#|
A dog bit John.
The dog slept.
Mary believes that John likes the dog.
Mary studies Computer Science.
Mary used a computer.
John saw a saw.
What bit John?
Who sleeps?
Who studied?
Who uses the computer?
Who likes a dog?
Who sees a saw?
|#
