(define member
        class 
        before after
        name 
        named
        which
        adj 
        agent
        verb 
        object
        lex 
        stime
        etime)

(build stime now) = now

(^ (defun add-indef (phrase)
     (let ((string-phrase (cond ((listp phrase) (princ-to-string (car phrase)))
				(t (princ-to-string phrase)))))
       (cond ((member (char string-phrase 0) '("a" "e" "i" "o" "u") :test 'string-equal)
	      (append '(|an|) (listify phrase)))
	     (t (append '(\a) (listify phrase))))))
)

(^ (defun mylength (s) 
     (cond ((null s) 0)
           ((atom s) 1)
           (t (1+ (mylength (cdr s)))))))
