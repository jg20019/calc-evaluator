; Calculator Interpreter written in Common Lisp 
; based on the Lecture by Brian Harvey 

(defun calc ()
   (princ "calc> ") 
   (force-output)
   (format t "~a~%" (calc-eval (read)))
   (calc)) 
   
  
(defun calc-eval (expr)
  (cond ((numberp expr) expr) 
        ((listp expr) (calc-apply (car expr) (mapcar #'calc-eval (cdr expr))))
        (t (error (format t "bad expression: ~a" expr)))))

(defun calc-apply (fn args) 
  (cond ((eq fn '+) (reduce #'+ args :initial-value 0))
        ((eq fn '*) (reduce #'* args :initial-value 1))
        ((eq fn '-) (reduce #'- args :initial-value 0))
        ((eq fn '/) (cond ((null args) (error "/ needs at least one argument.")) 
                          ((= (length args) 1) (/ (car args)))
                          (t (/ (car args) (reduce #'* (cdr args) :initial-value 1)))))))

