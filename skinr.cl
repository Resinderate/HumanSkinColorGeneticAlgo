(defparameter *x*  (list 1.0 1.0 1.0))
(defparameter *w1* (list 2.0 2.0 2.0 2.0))
(defparameter *w2* (list -1.0 -1.0 -1.0 -1.0 1.0))

(defparameter *listo*  '((1.0 1.0 1.0) (1.0 1.0 1.0)))

(defparameter *results*  (list 0 0 0))
(defparameter *ans*  (list 1 0))

(defun hs (x)
	(cond
		((minusp x)		0) 
		(t 				1)
	)
)

(defun multlist (x y)
	"Elementwise multiplication of two lists."
	(loop for i in x and j in y
	collect (* i j))
)

(defun sumlist (l)
	(apply '+ l)
)

(defun adddummy (l)
	(append l '(1.0))
)

(defun multicheck (lists)
	(loop for l in lists
		collect (skincheck l *w1* *w2*))
)

(defun skincheck (inputs w1 w2)
	"Take the inputs and the weights, and give back a 0 or 1."
	(hs (sumlist (multlist (adddummy (multlist (adddummy inputs) w1)) w2)))
)

(defun getfitness (results answers)
	"Pass in two lists containing 0 / 1 's and compare them for correctness"
	(let ((len (list-length results)) (correct 0)) 
		(loop for r in results and a in answers
			; Check if they are the same, increase correct.
			do (cond ((eq r a) (setf correct (+ 1 correct))))
			)
		; correct / len = percentage.
		(/ (coerce correct 'float) (coerce len 'float))
		)
	)

(defun readfromfile (filename)
	"Reads in data from file. Each element seperated by space. Returns list."
	(with-open-file (in filename)
		(loop for num = (read in nil)
			until (null num)
			collect num))
	)

(defun parseinputdata (l)
	"Splits every 3 values into it's own list."
	(let ((ar (coerce l 'array)))
		(loop for i upto (- (/ (list-length l) 3) 1)
			collect (list (aref ar (* i 3)) (aref ar (+ (* i 3) 1)) (aref ar (+ (* i 3) 2)))
			)))

(defun test ()
	(getfitness 
		(multicheck (parseinputdata (readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-inputs-small.txt")))
		(readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-outputs-small.txt")))

; Can now do a single runout with some blank values.

;C:/HyperProgramming/Lisp/HumanSkinColorGeneticAlgo/skinr.cl

; Need a way of selecting and doing a genetic algo for the weights used for this process.