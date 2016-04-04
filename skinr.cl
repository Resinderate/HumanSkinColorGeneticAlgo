; All code written from scratch by team consisting of Ronan Murphy and Kevin Duffy.

; Data input variables.
(defparameter *inputs* '())
(defparameter *answers*  '())

(defparameter *testing-inputs* '())
(defparameter *testing-answers*  '())

; Rate of 80%
(defparameter *mutationrate* 80)
; The current generation.
(defparameter *gen* 0)
; The maximum number of generations.
(defparameter *maxgen* 10)

(defun hs (x)
	"Heavyside function."
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
	"Add all the elements of a list up"
	(apply '+ l)
)

(defun adddummy (l)
	"Add a dummy value to the end of the list to use with the weights."
	(append l '(1.0))
)

(defun multicheck (lists w1 w2)
	"Doing multiple skincheck's for all of the input data."
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
	"Generate a starting state of length N. From -1 to 1 for each value."
	(loop for i upto (- n 1)
		collect (- (random 2.0) 1)))

(defun loaddata ()
	"Load data from file. Needs to contain correct location of files."
	(setf *inputs* (parseinputdata (readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-inputs-small.txt")))
	(setf *answers* (readfromfile "C:/HyperProgramming/Lisp/skinhsl-training-outputs-small.txt"))

	(setf *testing-inputs* (parseinputdata (readfromfile "C:/HyperProgramming/Lisp/skinhsl-testing-inputs-small.txt")))
	(setf *testing-answers* (readfromfile "C:/HyperProgramming/Lisp/skinhsl-testing-outputs-small.txt"))

	(write "Finished Loading..")
	(terpri))


(defun startingpop ()
	"Generate 5 pairs of 4 and 5 weight values."
	(loop for i upto 4
		collect (list (genstartingstate 4) (genstartingstate 5))))

(defun generation (pop)
	"Do the test multiple times with the different weights. Gives back the inputs with their fitness at the end."
	(loop for p in pop
		collect (list p (getfitness (multicheck *inputs* (nth 0 p) (nth 1 p)) *answers*))))

(defun testing-generation (pop)
	"Generation that uses the testing data instead of the training data."
		(list pop (getfitness (multicheck *testing-inputs* (nth 0 pop) (nth 1 pop)) *testing-answers*)))

(defun customsort (a b)
	"Custom sort to allow to sort by fitness."
	(> (nth 1 a) (nth 1 b)))

(defun sortfitness (pop)
	(sort pop #'customsort))

(defun choplast(l)
	"Return the list without the last element."
	(butlast l))

(defun roulettewheel (l)
	"Uses roulette wheel selection to get a weighted random pick from the list."
	(let ((tot (loop for i in l sum (nth 1 i))))
		(let ((count (random tot)))
			(loop for i in l 
				do (setf count (- count (nth 1 i)))
				(cond 
					((<= count 0.0)    (return-from roulettewheel (nth 0 i))))))))

(defun mateparents (a b)
	"Wrapper for makechild to deal with data formatting."
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
	"Carries out the main portion of the program, iterating over generations of new populations.."
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
			((>= *gen* *maxgen*) (return-from evolve (stripfitness (nth 0 pop))))
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

(defun run ()
	"Entry point for the program."
	(setf *gen* 0)
	(let ((pop (evolve (startingpop))))
		(terpri)
		(write "Run against testing data..")
		(terpri)
		(write "Fitness against testing data: ")
		(write (nth 1 (testing-generation pop)))
		)
	'Finished
	)

; Load the data in.
(loaddata)
; Run the program
(run)

(terpri)
(write "Enter (run) to run simulation again....")
(terpri)