(setq p1 '(and x (or x (and y (not z)))))
(setq p2 '(and (and z nil) (or x 1)))
(setq p3 '(or 1 a))
(setq p4 '(and x (and y (not z))))
(setq p5 '(and x (or y (not z))))

(defun andexp (e1 e2) (list 'and e1 e2))
(defun orexp (e1 e2) (list 'or e1 e2))
(defun eqlexp (e1 e2) (list 'eql e1 e2))
(defun notexp (e1) (list 'not e1))

(defun operator (exp) (car exp))
(defun left (exp) (car(cdr exp)))
(defun right (exp) (car(cdr(cdr exp))))


(defun deep-shit (old new lefta)
	;(print "deep-shit")
	;(print old)
	;(print new)
	;(print lefta)
	(cond
		((null lefta)	nil)
		((listp (car lefta)) (cons (deep-shit old new (car lefta)) (deep-shit old new (cdr lefta))))
		((eq old (car lefta)) (cons new (deep-shit old new (cdr lefta))))		
	(T (cons (car lefta) (deep-shit old new (cdr lefta))))
	)
)

(defun bind-shit (exp bindings)
	;(print "bind-shit")
	;(print bindings)
	(cond
		((null bindings) exp)
		((listp (car bindings)) 
			(bind-shit exp (car bindings))
				(bind-shit 
					(deep-shit 
						(car(car bindings)) (car(cdr(car bindings))) exp) (cdr bindings)))
	(T (deep-shit (car bindings) (car(cdr bindings)) exp))
	)
)

(defun simpand (lefta righta)
	;(print (lefta))
	;(print righta)
	(cond
		
		;( (list 'and righta lefta))
		;((and (null righta) (null (not lefta))) (list 'not lefta righta))
		;((and (null lefta) (null righta)) 1)
		;((and (null lefta) (null (not righta))) (list 'not lefta righta))
		((null righta) nil)
		((null lefta) nil)
		((and (not (listp lefta)) (or (equal lefta '1) (equal lefta 't))) righta)
		((and (not (listp righta)) (or (equal righta '1) (equal righta 't))) lefta)
		;((OR (equal lefta '1) lefta) righta)
		((OR (listp lefta) (listp righta)) (list 'and lefta righta))
		((equal lefta righta) lefta)
	(T (list 'and lefta righta))
	)
)

(defun simpor (lefta righta)
	(cond
		
		;((and (null righta) (null lefta)) 1)
		;((and (null righta) (null (not lefta))) (list 'not lefta righta))
		;((and (null lefta) (null righta)) 1)
		;((and (null lefta) (null (not righta))) (list 'not lefta righta))
		((null righta) lefta)
		((null lefta) righta)
		((OR (equal righta '1) (equal righta 'T)) 1)
		((OR (equal lefta '1) (equal lefta 'T)) 1)
		((OR (listp lefta) (listp righta))(list 'or lefta righta))
	(T (list 'or lefta righta))
	)
)

(defun simpnot (lefta)
	;(print lefta)
	;(print (listp lefta))
	;(print righta)
	;(print (null (not lefta)))
	(cond
		((AND (not (null lefta)) (listp lefta)) lefta)
		((null lefta) T)
		((OR (equal lefta '1) (equal lefta 'T)) nil)
		;((not (null lefta)) (not lefta))
	(T (list 'not lefta))
	)
)

(defun simplify (opr lefta righta)
	(cond
		((equal opr 'and) (simpand lefta righta))
		((equal opr 'or) (simpor lefta righta))
		((equal opr 'eql) (equal lefta righta))
		((equal opr 'not) (simpnot lefta))
	;(T '(opr lefta rigta))
	)
)

(defun simplifyexp (exp)
	(cond
		((not exp) nil)
		((listp exp) (simplify (operator exp) (simplifyexp (left exp)) (simplifyexp (right exp))))
	(T exp)
	)
)

(defun evalexp (exp bindings)
	(simplifyexp (bind-shit exp bindings))
)
