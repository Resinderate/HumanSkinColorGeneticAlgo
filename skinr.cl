(defparameter *x*  (list 1.0 1.0 1.0))
(defparameter *w1* (list 2.0 2.0 2.0 2.0))
(defparameter *w2* (list 1.0 1.0 1.0 1.0 1.0))


; add the dummy each time.
; gonna add the dummy at the end instead of the start, too retarded to figure out how to prepend in this moron language.
(setf *x* (append *x* (list 2.0)))

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




; Let's do a single runout example I think.
; See how that goes.
; Then can scale up the the proper results.
; The fitness only comes into the first example.