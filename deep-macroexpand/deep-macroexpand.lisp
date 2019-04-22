(uiop/package:define-package :deep-macroexpand/deep-macroexpand
                             (:nicknames :deep-macroexpand) (:use :cl)
                             (:shadow) (:export :deep-macroexpand) (:intern))
(in-package :deep-macroexpand/deep-macroexpand)
;;don't edit above
(defun deep-macroexpand (exp)
  (if (atom exp)
      (let ((eexp (macroexpand exp)))
        (if (equal eexp exp)
            exp
            (deep-macroexpand eexp)))
      (let ((eexp (macroexpand exp)))
        (mapcar #'deep-macroexpand (if (eq (car eexp) 'function) (cadr eexp) eexp)))))
