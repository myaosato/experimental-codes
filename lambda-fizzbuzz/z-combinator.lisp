(defun z (f)
  ((lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))
   (lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))))

(defun fact (x)
  (if (= x 1)
      1
      (* x (fact (1- x)))))

(defun %fact (f)
  (lambda (x)
    (if (= x 1)
      1
      (* x (funcall f (1- x))))))

(defun fib (x)
  (cond ((= x 0) 0)
        ((= x 1) 1)
        (t (+ (fib (- x 1)) (fib (- x 2))))))

(defun %fib (f)
  (lambda (x)
    (cond ((= x 0) 0)
          ((= x 1) 1)
          (t (+ (funcall f (- x 1)) (funcall f (- x 2)))))))

