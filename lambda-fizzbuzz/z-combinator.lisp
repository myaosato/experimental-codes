(defun z (f)
  ((lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))
   (lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))))

(defun fact (f)
  (lambda (x) 
    (if (= x 1)
        1
        (* x (funcall f (1- x))))))
