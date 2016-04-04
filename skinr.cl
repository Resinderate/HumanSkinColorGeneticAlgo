;(defparameter *inputs* '())
;(defparameter *answers*  '())

; Rate of 10%
(defparameter *mutationrate* 15)
(defparameter *gen* 0)
(defparameter *maxgen* 25)

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

(defun multicheck (lists w1 w2)
	(loop for l in lists
		collect (skincheck l w1 w2))
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

(defun genstartingstate (n)
	(loop for i upto (- n 1)
		collect (- (random 2.0) 1)))

(defun test ()
	(getfitness 
		(multicheck (parseinputdata (readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-inputs-small.txt")))
		(readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-outputs-small.txt")))

(defun loaddata ()
	(setf *inputs* (parseinputdata (readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-inputs-small.txt")))
	(setf *answers* (readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-outputs-small.txt"))
	(write "Finished Loading.."))


(defun startingpop ()
	"Generate 5 pairs of 3 and 4 weight values."
	(loop for i upto 4
		collect (list (genstartingstate 3) (genstartingstate 4))))

(defun generation (pop)
	"Do the test multiple times with the different weights. Gives back the inputs with their fitness at the end."
	(loop for p in pop
		collect (list p (getfitness (multicheck *inputs* (nth 0 p) (nth 1 p)) *answers*))))

(defun customsort (a b)
	(> (nth 1 a) (nth 1 b)))

(defun sortfitness (pop)
	(sort pop #'customsort))

(defun choplast(l)
	"Return the list without the last element."
	(butlast l))

(defun roulettewheel (l)
	(let ((tot (loop for i in l sum (nth 1 i))))
		(let ((count (random tot)))
			(loop for i in l 
				do (setf count (- count (nth 1 i)))
				(cond 
					((<= count 0.0)    (return-from roulettewheel (nth 0 i))))))))

(defun mateparents (a b)
	(list (makechild (nth 0 a) (nth 0 b)) (makechild (nth 1 a) (nth 1 b))))

(defun makechild (a b)
	"Take two sets of weights and return a single child weight. Currently gets the average of both."
	(loop for i in a and j in b
		collect (/ (+ i j) 2.0))
	)

(defun mutate (a)
	"Decides if the elements with be mutated."
	(cond
		((<= (random 100) *mutationrate*) 	(performmutation a))
		(t 					   				a)
		)
	)

(defun performmutation (a)
	"Carries out the mutation."
	(let ((weights (random 2)))
		(setf (nth (random (list-length (nth weights a))) (nth weights a)) 
			(+ (nth (random (list-length (nth weights a))) (nth weights a)) (- (random 1.0) 0.5)))
		a
		)
	)

(defun stripfitness (l)
	(car l))

(defun evolve (inputpop)
	"Can now get a random weighted value from the list of 4. Should prolly choplast before then though."
	(let ((pop (sortfitness (generation inputpop))))
		(write "Generation ")
		(write *gen*)
		(terpri)
		(write "Highest Fitness: ")
		(write (nth 1(nth 0 pop)))
		(terpri)
		(write "Values:")
		(terpri)
		(write pop)
		(terpri)

		(setf pop (choplast pop))
		(setf *gen* (+ *gen* 1))

		(cond
			((>= *gen* *maxgen*) (return-from evolve 'Finished)) 
			(t 
				(evolve
					(list 	(stripfitness (nth 0 pop)) 
						(stripfitness (nth 1 pop)) 
						(mutate (mateparents (roulettewheel pop) (roulettewheel pop)))
						(mutate (mateparents (roulettewheel pop) (roulettewheel pop)))
						(mutate (mateparents (roulettewheel pop) (roulettewheel pop))))
					)
				)
			)
		
		))


;
;



; Have some weights.
; Can run this with some data, and input the weights we have here.
; Get's back the fitness.

; When we have the fitness, can order all of the stuff.
	; Have the inputs mapped to the fitness?

; Chop the last one.
; Elite the first two

; Generate 3 more using 6 parents.
; Use roulette to pick the parents, and then mate them

; Pass them to a function for them to reproduce.
; Possibly mutate them.
; Mutate function.

; Then the cycle continues.
; Looking for some acceptable values.




;C:/HyperProgramming/Lisp/HumanSkinColorGeneticAlgo/skinr.cl

