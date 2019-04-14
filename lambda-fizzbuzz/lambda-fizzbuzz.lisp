;; lambda
(defmacro -> (param &body body)
  `(lambda (,param) (declare (ignorable ,param)) ,@body))

;; util
(defmacro ! (prev &rest rest)
  (if (= (length rest) 0)
      prev
      `(! (funcall ,prev ,(car rest))
           ,@(cdr rest))))

(defmacro _ (func &rest rest)
  (if (= (length rest) 1)
      (list '! func (car rest))
      `(! ,func (_ ,@rest))))

;; integer
(define-symbol-macro zero (-> p (-> x x)))
(define-symbol-macro one (-> p (-> x (! p x))))
(define-symbol-macro two (-> p (-> x (_ p p x))))
(define-symbol-macro three (-> p (-> x (_ p p p x))))
(define-symbol-macro five (-> p (-> x (_ p p p p p x))))

;; bool
(define-symbol-macro true (-> x (-> y x)))
(define-symbol-macro false (-> x (-> y y)))
(define-symbol-macro _if (-> x x))

;; predicate
(define-symbol-macro zero? (-> p (! p (-> x false) true)))

;; pair
(define-symbol-macro pair (-> x (-> y (-> f (! f x y)))))
(define-symbol-macro left (-> p (! p true)))
(define-symbol-macro right (-> p (! p false)))

;; increment decrement
(define-symbol-macro increment (-> n (-> p (-> x (! p (! n p x))))))
(define-symbol-macro slide 
  (-> p (! pair (! right p) (! increment (! right p)))))
(define-symbol-macro decrement 
  (-> n (! left (! n slide (! pair zero zero)))))

;; arithmetic
(define-symbol-macro add (-> m (-> n (! n increment m))))
(define-symbol-macro sub (-> m (-> n (! n decrement m))))
(define-symbol-macro mul (-> m (-> n (! n (! add m) zero))))
(define-symbol-macro pow (-> m (-> n (! n (! mul m) one))))

;; comparison
(define-symbol-macro less-or-equal?
  (-> m (-> n (! zero? (! sub m n)))))

#| 
;;Y combinator
(define-symbol-macro y-comb
  (-> f 
      (! (-> x (_ f x x)) 
         (-> x (_ f x x)))))
|#

;; Z combinator
(define-symbol-macro z
  (-> f 
      (! (-> x (! f (-> y (! x x y)))) 
         (-> x (! f (-> y (! x x y)))))))

;; modulo
(define-symbol-macro modulo
  (! z (-> f
         (-> m
           (-> n
             (! _if (! less-or-equal? n m)
                (-> x (! f (! sub m n) n x))
                m))))))

;; list
(define-symbol-macro empty (! pair true true))
(define-symbol-macro unshift (-> l (-> x (! pair false (! pair x l)))))
(define-symbol-macro empty? left)
(define-symbol-macro _first (-> l (_ left right l)))
(define-symbol-macro _rest (-> l (_ right right l)))

(define-symbol-macro range 
  (! z
     (-> f 
       (-> m 
         (-> n
           (! _if (! less-or-equal? m n)
              (-> x (! unshift (! f (! increment m) n) m x))
              empty))))))
(define-symbol-macro fold 
  (! z 
     (-> f
       (-> l 
         (-> x 
           (-> g
             (! _if (! empty? l)
                x
                (-> y 
                  (! g (! f (! _rest l) x g) (! _first l) y)))))))))
(define-symbol-macro _map 
  (-> k 
    (-> f
      (! fold k empty
         (-> l (-> x (! unshift l (! f x))))))))

;; character for fizzbuzz and bigger integer
(define-symbol-macro ten (! mul two five))
(define-symbol-macro fifteen (! mul three five))
(define-symbol-macro hundred (! mul ten ten))

(define-symbol-macro bee ten)
(define-symbol-macro ef (! increment bee))
(define-symbol-macro i (! increment ef))
(define-symbol-macro u (! increment i))
(define-symbol-macro zed (! increment u))

(define-symbol-macro fizz
  (! unshift
     (! unshift
        (! unshift
           (! unshift empty zed)
           zed)
        i)
     ef))
(define-symbol-macro buzz
  (! unshift
     (! unshift
        (! unshift
           (! unshift empty zed)
           zed)
        u)
     bee))
(define-symbol-macro fizzbuzz
  (! unshift
     (! unshift
        (! unshift
           (! unshift buzz zed)
           zed)
        i)
     ef))

;; to_digits
(define-symbol-macro div 
  (! z
     (-> f
       (-> m 
         (-> n 
           (! _if (! less-or-equal? n m)
              (-> x (! increment (! f (! sub m n) n) x))
              zero))))))
(define-symbol-macro _push
  (-> l
    (-> x
      (! fold l (! unshift empty x) unshift))))
(define-symbol-macro to-digits
  (! z 
     (-> f
       (-> n
         (! _push 
            (! _if (! less-or-equal? n (! decrement ten))
               empty
               (-> x (! f (! div n ten) x)))
            (! modulo n ten))))))

;; FizzBuzz
(define-symbol-macro run-fizzbuzz
  (! _map (! range one hundred) 
     (-> n 
       (! _if (! zero? (! modulo n fifteen))
          fizzbuzz
          (! _if (! zero? (! modulo n five))
             buzz
             (! _if (! zero? (! modulo n three))
                fizz
                (! to-digits n)))))))

;; meta language
(defun to-bool (p) (! p t nil))
(defun to-integer (p) (! p #'1+ 0))
(defun to-list (l)
  (if (to-bool (! empty? l))
      nil
      (cons (! _first l) (to-list (! _rest l)))))
(defun to-char (c) (elt "0123456789BFiuz" (to-integer c)))
(defun to-string (s) (coerce (mapcar #'to-char (to-list s)) 'string))

;; test

(eq (to-bool (! _if (! zero? zero) true false)) t)
(= (to-integer zero) 0)
(= (to-integer one) 1)
(= (to-integer two) 2)
(= (to-integer three) 3)
(= (to-integer five) 5)
(= (to-integer fifteen) 15)
(= (to-integer (! left (! pair three five))) 3)
(= (to-integer (! right (! pair three five))) 5)
(= (to-integer (! increment zero)) 1)
(= (to-integer (! decrement fifteen)) 14)
(= (to-integer (! add five fifteen)) 20)
(= (to-integer (! sub fifteen five)) 10)
(= (to-integer (! mul three five)) 15)
(= (to-integer (! pow two three)) 8)
(= (to-integer (! modulo three two)) 1)
(= (to-integer (! modulo five three)) 2)
(equal (mapcar #'to-integer (to-list (! unshift (! unshift (! unshift empty three) two) one)))
       (list 1 2 3))
(equal (mapcar #'to-integer (to-list (! range one five)))
       (list 1 2 3 4 5))
(= (to-integer (! fold (! range one five) zero add)) 15)
(= (to-integer (! fold (! range one five) one mul)) 120)
(equal (mapcar #'to-integer (to-list (! _map (! range one five) increment)))
       (list 2 3 4 5 6))
(to-char zed)
(string= (format nil "~A~A" (to-string fizz) (to-string buzz))
         (to-string fizzbuzz))
(to-string (! to-digits five))
(to-string (! to-digits (! pow five three)))
(equal (mapcar #'to-string (to-list (! run-fizzbuzz)))
       (list "1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz"
             "11" "Fizz" "13" "14" "FizzBuzz" "16" "17" "Fizz" "19" "Buzz"
             "Fizz" "22" "23" "Fizz" "Buzz" "26" "Fizz" "28" "29" "FizzBuzz"
             "31" "32" "Fizz" "34" "Buzz" "Fizz" "37" "38" "Fizz" "Buzz"
             "41" "Fizz" "43" "44" "FizzBuzz" "46" "47" "Fizz" "49" "Buzz"
             "Fizz" "52" "53" "Fizz" "Buzz" "56" "Fizz" "58" "59" "FizzBuzz"
             "61" "62" "Fizz" "64" "Buzz" "Fizz" "67" "68" "Fizz" "Buzz"
             "71" "Fizz" "73" "74" "FizzBuzz" "76" "77" "Fizz" "79" "Buzz"
             "Fizz" "82" "83" "Fizz" "Buzz" "86" "Fizz" "88" "89" "FizzBuzz"
             "91" "92" "Fizz" "94" "Buzz" "Fizz" "97" "98" "Fizz" "Buzz"))


;; expand
(defun expand (exp)
  (if (atom exp)
      (let ((eexp (macroexpand-1 exp)))
        (if (equal eexp exp)
            exp
            (expand eexp)))
      (mapcar #'expand exp)))
;;(defun show (exp) (cl-ppcre:regex-replace-all "\\s+" (format nil "~A" (expand exp)) " "))

