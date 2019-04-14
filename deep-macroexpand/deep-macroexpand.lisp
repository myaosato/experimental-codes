(defun deep-macroexpand (exp)
  (if (atom exp)
      (let ((eexp (macroexpand exp)))
        (if (equal eexp exp)
            exp
            (expand eexp)))
      (let ((eexp (macroexpand exp)))
        (mapcar #'expand (if (eq (car eexp) 'function) (cadr eexp) eexp)))))