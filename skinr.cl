(defparameter *x*  (list 1.0 1.0 1.0))
(defparameter *w1* (list 2.0 2.0 2.0 2.0))
(defparameter *w2* (list -1.0 -1.0 -1.0 -1.0 1.0))


; add the dummy each time.
; gonna add the dummy at the end instead of the start, too retarded to figure out how to prepend in this moron language.


(write *x*)
(write *w1*)
(write *w2*)


(defun hs (x)
	(cond
		((minusp x)		0) 
		(t 				1)
	)
)

; need a function to multiply the two lists together.
(defun multlist (x y)
	(loop for i in x and j in y
	collect (* i j))
)

(terpri)
(write (hs (apply '+ (multlist (append (multlist (append *x* '(1.0)) *w1*) '(1.0)) *w2*))))
(terpri)




; Let's do a single runout example I think.
; See how that goes.
; Then can scale up the the proper results.
; The fitness only comes into the first example.