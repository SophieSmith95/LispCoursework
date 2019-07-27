;------------------Poly+-------------------------
(defun canAdd(a1 a2)
	(if (not(EQ a2 '()))
		(return-from canAdd (append(append (list(car a1)) (list(car (cdr a1)))) 
			(list(+ (car(cdr(cdr a1))) (car(cdr(cdr a2)))))))
		(return-from canAdd a1)
	);end if
);end function

(defun addCoeffs (a1)
	;takes one list of like terms
	(do
		(
			(x a1 (cdr x))
			(y '() (canAdd (car x) y))
		)
		(
			(EQ x '())
			y
		)
	);end do
);end function

(defun comp-expos  (e1 e2)
	(if   (=(car e2)(car e1))
		(if (=(car (cdr e1))  (car (cdr e2)) ) 
			(if (=  (car(cddr e1))  (car(cddr e2)))
				(return-from comp-expos 1)
				(return-from comp-expos 0)
			)
			(return-from comp-expos 0)
		)
		(return-from comp-expos 0)
	)
)

(defun checkS (s1 s2)
	;takes two terms e.g.("x" (1 0 0) 2) and checks if same
	(if (=  (comp-expos(car (cdr s1)) (car (cdr s2))) 1)
		(return-from checkS (list s2))
		(return-from checkS NIL)
	);end if
);end function

(defun check (d1 d2) 
	;takes d1 and looks for similar in d2 then returns that list
	(when d2
		(return-from check(append(checkS d1 (car d2)) (check d1 (cdr d2))))
	);end when
)

(defun inList (i1 i2)
	;takes whole list i1 and ans list i2
	(if(not (EQ i2 '()))	;if ans not empty
		(if (=  (comp-expos(car (cdr (car i1))) (car (cdr (car i2)))) 1)
			(return-from inList '())
			(inList i1 (cdr i2))
		)
	(return-from inList addCoeffs (append (list(car i1)) 
	   (check(car i1)(cdr i1))));if ans empty return addtion of all first terms
	);end if
);end function

(defun poly+ (l1 l2)
	(let ((a (append l1 l2)))
		(do
			(	
				(x a (cdr x))
				(ans '() (remove NIL (append(list(inList x ans)) ans)))
			)
			(	
				(EQ x '())
				ans
			)
		);end do
	);end let
);end function

;-----------------------------Poly- ------------------------------

(defun add(e1 e2)
	(if(EQ e2 '())
		(return-from add e1)
		(append (list(car e1) (car(cdr e1)) (+(car(cdr(cdr e1))) 
			(car(cdr(cdr e2))))))
	);end if
);end function

(defun addCoeffs2 (a1)
;takes one list of like terms
	(do
		(
			(x a1 (cdr x))
			(y '() (add (car x) y))
		)
		(
			(EQ x '())
			y
		)
	);end do
);end function

(defun checkS2 (s1 s2)
;takes two terms and checks if same
	(if(string= (car s1) (car s2))
		(if(= (car(cdr s1)) (car(cdr s2)))
			(return-from checkS2 (list s2))
		);end if
		(return-from checkS2 NIL)
	);end if
	(return-from checkS2 NIL)
);end function

(defun check2 (d1 d2) 
;takes d1 and looks for similar in d2 then returns that list
	(when d2
		(return-from check2(append(checkS2 d1 (car d2)) (check2 d1 (cdr d2))))
	);end when
);end function

(defun inList2 (i1 i2)
;takes whole list i1 and ans list i2
	(if(not (EQ i2 '()))
		(if(string= (car(car i1)) (car(car i2)))
			(if(= (car(cdr(car i1))) (car(cdr(car i2))))
				(return-from inList2 '())
			);end if
			(inList2 i1 (cdr i2))
		);end if
	(return-from inList2 (addCoeffs2(append (list(car i1)) 
		(check2 (car i1) (cdr i1)))))
	);end if
);end function

(defun poly++ (l1 l2)
	(let ((a (append l1 l2)))
		(do
			(	
				(x a (cdr x))
				(ans '() (remove NIL (append(list(inList2 x ans)) ans)))
			)
			(	
				(EQ x '())
				ans
			)
			
		);end do
	);end let
);end function

(defun invert(i1)
	(return-from invert (append(append(list(car i1)) (list(car(cdr i1)))) 
		(list(-(car(cdr(cdr i1)))))))
);end function

(defun poly-(p1 p2)
	(poly++ p1
	(do
		(
			(x p2 (cdr x))
			(l '() (append(list(invert (car x))) l ))
		)
		(
			(EQ x '())
			l
		)
	);end do
	)

);end function

;----------------------Poly*------------------------
(defun checkExpos(e1 e2)
	(if(not (EQ e1 '()))
	(if(EQ (car e1)(car e2))
		(append (list(+(car e1)(car e2))) (checkExpos (cdr e1)(cdr e2))) 
		(append (list(+(car e1)(car e2))) (checkExpos (cdr e1)(cdr e2)))  
	);end if 
	)
);end function

;takes 2 terms
(defun multiCoeffs(c1 c2)
	(if(string= (car c1)(car c2))
		(append (list(car c1) (checkExpos (car(cdr c1)) (car(cdr c2)))
				(*(car(cdr(cdr c1))) (car(cdr(cdr c2))))))
			(if(string= (car c2) "0")
				(append (list(car c1) (checkExpos (car(cdr c1)) (car(cdr c2))) 
					    (*(car(cdr(cdr c1))) (car(cdr(cdr c2))))))
				(append (list(car c1) (checkExpos (car(cdr c1)) (car(cdr c2))) 
					    (*(car(cdr(cdr c1))) (car(cdr(cdr c2))))))
			);end if
	);end if
);end function

(defun multiNumbs(n1 n2)
		(append (list(car n2) (checkExpos (car(cdr n1)) (car(cdr n2))) 
			    (*(car(cdr(cdr n1))) (car(cdr(cdr n2))))))
);end function

;take 2 terms
(defun checkVars(v1 v2)
	(if(string= (car v1) "0")
		(return-from checkVars(multiNumbs v1 v2))
		(return-from checkVars(multiCoeffs v1 v2))
	);end if 
);end function	

;takes a term and a list
(defun checkFunction(m1 m2)
	(when m2
		(if(string= (car m1)(car(car m2)))
			(return-from checkFunction (append (list(multiCoeffs m1 (car m2))) 
				                               (checkFunction m1 (cdr m2))))
			(return-from checkFunction (append (list(checkVars m1 (car m2))) 
				                               (checkFunction m1 (cdr m2))))
		);end if
	);end when
);end function

(defun poly* (l1 l2)
	(poly+ '()
		(do
			(
				(x l1 (cdr x))
				(l '() (append(checkFunction (car x) l2) l))
			)
			(
				(EQ x '())
				l
			)
		);end do
	)

);end function


