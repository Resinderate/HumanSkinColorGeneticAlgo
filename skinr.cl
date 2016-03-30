(defparameter *x* (make-array 0 :fill-pointer 0 :adjustable t))

(let ((in (open "c:/HyperProgramming/Lisp/ron.txt")))
	(loop
		for line = (read-line in nil 'eof)
		until (eq line 'eof)
		(vector-push-extend line *x*)
		))

(write *x*)
