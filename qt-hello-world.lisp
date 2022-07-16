(defpackage :qt-user
  (:use :qt :cl)
  (:export main))
(in-package :qt-user)
(named-readtables:in-readtable :qt)
(defun main ()
  (qt:make-qapplication)
  (with-objects ((window (#_new QWidget)))
    (#_setGeometry window 100 100 500 355)
    (#_new QPushButton "Quit" window)
    (#_show window)
    (#_exec *qapplication*)))





