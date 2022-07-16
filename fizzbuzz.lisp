(defun main () 
  (print #.(format nil "~{~A ~}"
                   (loop for i from 1 below 100
                         collect (cond
                                   ((= 0 (mod i 15)) "fizzbuzz") 
                                   ((= 0 (mod i 5)) "buzz")
                                   ((= 0 (mod i 3)) "fizz")
                                   (t i))))))

(defun main2 () 
  (print (format nil "~{~A ~}"
                 (loop for i from 1 below 100
                       collect (cond
                                 ((= 0 (mod i 15)) "fizzbuzz") 
                                 ((= 0 (mod i 5)) "buzz")
                                 ((= 0 (mod i 3)) "fizz")
                                 (t i))))))

