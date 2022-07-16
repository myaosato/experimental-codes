(defun equal-list (lst1 lst2 indexes)
  (cond
    ((null indexes)
     t)
    ((equal (nth (car indexes) lst1) (nth (car indexes) lst2))
     (equal-list lst1 lst2 (cdr indexes)))
    (t nil)))
      

(defun group-by-rec (list-of-list indexes queue result) 
  (cond
    ((null list-of-list)
     (return-from group-by-rec result))
    ((equal-list (car list-of-list) queue indexes)
     (push (car list-of-list) (car result)))   
    (t
     (push (list (car list-of-list)) result)))
  (group-by-rec (cdr list-of-list) indexes (car list-of-list) result))

(defun group-by (list-of-list indexes)
  (group-by-rec (cdr list-of-list) indexes (car list-of-list) (list (list (car list-of-list)))))




    
