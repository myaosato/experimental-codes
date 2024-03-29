; Proof

;; ****************************************************************
;; meta data type
#|
const := const-val | func const*
term := const | var
atomic := prop | predicate term*
formula := formula ∧ formula | formula ∨ formula | ¬ formula | atomic
|#
;; ****************************************************************
(defun ∧ (f1 f2)
  (list :and f1 f2))
(defun is-∧ (formula)
  (eq (car formula) :and))
(defun ∧-1 (and-formula)
  (nth 1 and-formula))
(defun ∧-2 (and-formula)
  (nth 2 and-formula))

(defun ∨ (f1 f2)
  (list :or f1 f2))
(defun is-∨ (formula)
  (eq (car formula) :or))
(defun ∨-1 (and-formula)
  (nth 1 and-formula))
(defun ∨-2 (and-formula)
  (nth 2 and-formula))

(defun ¬ (f)
  (list :not f))
(defun is-¬ (formula)
  (eq (car formula) :not))
(defun ¬-1 (not-formula)
  (nth 1 not-formula))

(defun → (f1 f2)
  (list :to f1 f2))
(defun is-→ (formula)
  (eq (car formula) :to))
(defun →-1 (to-formula)
  (nth 1 to-formula))
(defun →-2 (to-formula)
  (nth 2 to-formula))

(defun prop (name)
  (list :prop name))
(defun is-prop (formula)
  (eq (car formula) :prop))
(defun prop-name (formula)
  (nth 1 formula))

(defun is-atomic (formula)
  (or (is-prop formula)))

(defun format-formula (formula)
  (cond ((equal formula 0)
         0)
        ((atom formula)
         (format nil "~A" formula))
        ((is-prop formula)
         (format nil "~A" (prop-name formula)))
        ((is-¬ formula)
         (format nil "¬~A" (format-formula (¬-1 formula))))
        ((is-∧ formula)
         (format nil "(~A ∧ ~A)" (format-formula (∧-1 formula)) (format-formula (∧-2 formula))))
        ((is-∨ formula)
         (format nil "(~A ∨ ~A)" (format-formula (∨-1 formula)) (format-formula (∨-2 formula))))
        ((is-→ formula)
         (format nil "(~A ∧ ~A)" (format-formula (→-1 formula)) (format-formula (→-2 formula))))
        ((eq (car formula) :nat)
         (format nil "~A is Nat" (format-formula (nth 1 formula))))
        ((eq (car formula) :succ)
         (1+ (cadr formula)))
        (t
         (error "invalid formula"))))


;; ****************************************************************
;; sequent
;; ****************************************************************
(defun make-sequent (antecedent succedent)
  (cons antecedent succedent))

(defun l (seq)
  (car seq))

(defun r (seq)
  (cdr seq))

(defun length-l (seq)
  (length (l seq)))

(defun length-r (seq)
  (length (r seq)))

(defun nth-l (n seq)
  (nth n (l seq)))

(defun nth-r (n seq)
  (nth n (r seq)))

(defun empty-l (seq)
  (null (l seq)))

(defun empty-r (seq)
  (null (r seq)))


;; ****************************************************************
;; goal
;; ****************************************************************
(defun make-goal (sequent &rest sequents)
  (list* sequent sequents))

(defun do-step (current-goal n rule args)
  (let ((result (apply rule (nth n current-goal) args)))
    `(,@(subseq  current-goal 0 n)
      ,@(cond ((equal result (list t))
               nil)
              ((null result)
               (error ""))
              (t result))
      ,@(subseq  current-goal (1+ n)))))


;; ****************************************************************
;; macros
;; ****************************************************************
;;;; Axiom
;;;; -----
;;;; Γ ⊢ Δ
(defmacro def-axiom (name lambda-list &body condition)
  `(defun ,name ,lambda-list
     (if (progn ,@condition)
         (list t)
         (error ""))))

;;;; ll0, ll1,... lln, lr0, lr1,... |- rl0, rl1,... rlm, rr0, rr1,...
(defmacro with-splited-seqent (sequent (n m ll lr rl rr) &body body)
  (let ((seq (gensym "SEQUENT-")))
    `(cond ((< (length-l seq) ,n) (error ""))
           ((< (length-r seq) ,m) (error ""))
           (t
            (let* ((,seq ,sequent)
                   (,ll (subseq (l ,seq) 0 ,n))
                   (,lr (subseq (l ,seq) ,n))
                   (,rl (subseq (r ,seq) 0 ,m))
                   (,rr (subseq (r ,seq) ,m)))
              ,@body)))))


;; ****************************************************************
;; LK
;;
;; ref. https://ja.wikipedia.org/wiki/シークエント計算
;;
;; WIP: implemented propositional logic part only.
;; TODO
;; - ∀
;; - ∃
;; ****************************************************************
;;;; axiom of LK
;;;; ------
;;;; A  ⊢ A
(def-axiom id (seq)
  (and (= (length-l seq) 1)
       (= (length-r seq) 1)
       (equal (nth-l 0 seq) (nth-r 0 seq))))

;;;; Cut
;;;; Γ ⊢ A,Δ  A,Σ ⊢ Π
;;;; -----------------
;;;; Γ,Σ ⊢ Δ,Π
(defun cut (seq a n m)
  (with-splited-seqent seq (n m g s d p)
    (make-goal (make-sequent g (cons a d))
               (make-sequent (cons a s) p))))

;;;; And
;;;; A,Γ ⊢ Δ          B,Γ ⊢ Δ          Γ ⊢ A,Δ  Σ ⊢ B,Π
;;;; ---------(∧L1)   ---------(∧L2)   ----------------(∧R)
;;;; A∧B,Γ ⊢ Δ        A∧B,Γ ⊢ Δ        Γ,Σ ⊢ A∧B,Δ,Π 
(defun and-l (seq op)
  (let ((focus (nth-l 0 seq)))
    (if (is-∧ focus)
        (make-goal (make-sequent (cons (funcall op focus) (cdr (l seq)))
                                 (r seq)))
        (error ""))))

(defun and-l1 (seq)
  (and-l seq #'∧-1))

(defun and-l2 (seq)
  (and-l seq #'∧-2))

(defun and-r (seq n m)
  (with-splited-seqent seq (n m g s f-d p)
    (destructuring-bind (f d) f-d
      (if (is-∧ f)
          (make-goal (make-sequent g (cons (∧-1 f) d))
                     (make-sequent s (cons (∧-2 f) p)))
          (error "")))))

;;;; Or
;;;; Γ ⊢ A,Δ          Γ ⊢ B,Δ          A,Γ ⊢ Δ  B,Σ ⊢ Π
;;;; ---------(∨R1)   ---------(∨R2)   ----------------(∨L)
;;;; Γ ⊢ A∨B,Δ        Γ ⊢ A∨B,Δ        A∨B,Γ,Σ ⊢ Δ,Π 
(defun or-r (seq op)
  (let ((focus (nth-r 0 seq)))
    (if (is-∨ focus)
        (make-goal (make-sequent (l seq)
                                 (cons (funcall op focus) (cdr (r seq)))))
        (error ""))))

(defun or-r1 (seq)
  (or-r seq #'∨-1))

(defun or-r2 (seq)
  (or-r seq #'∨-2))

(defun or-l (seq n m)
  (with-splited-seqent seq (n m f-g s d p)
    (destructuring-bind (f g) f-g
      (if (is-∨ f)
          (make-goal (make-sequent (cons (∨-1 f) g) d)
                     (make-sequent (cons (∨-2 f) s) p))
          (error "")))))

;;;; Not
;;;; Γ ⊢ A,Δ       A,Γ ⊢ Δ
;;;; --------(¬L)  ---------(¬R)
;;;; ¬A,Γ ⊢ Δ      Γ ⊢ ¬A,Δ
(defun not-l (seq)
  (let ((focus (nth-l 0 seq)))
    (if (is-¬ focus)
        (make-goal (make-sequent (cdr (l seq))
                                 (cons (¬-1 focus) (r seq))))
        (error ""))))

(defun not-r (seq)
  (let ((focus (nth-r 0 seq)))
    (if (is-¬ focus)
        (make-goal (make-sequent (cons (¬-1 focus) (l seq))
                                 (cdr (r seq))))
        (error ""))))


;;;; To
;;;; A,Γ ⊢ B,Δ      Γ ⊢ A,Δ  B,Σ ⊢ Π
;;;; ---------(¬R)  ----------------(¬L)  
;;;; Γ ⊢ A→B,Δ      A→B,Γ,Σ ⊢ Δ,Π
(defun to-r (seq)
  (let ((focus (nth-r 0 seq)))
    (if (is-→ focus)
        (make-goal (make-sequent (cons (→-1 focus) (l seq))
                                 (cons (→-2 focus) (cdr (r seq)))))
      (error ""))))

(defun to-l (seq n m)
  (with-splited-seqent seq (n m f-g s d p)
    (destructuring-bind (f g) f-g
      (if (is-→ f)
          (make-goal (make-sequent g (cons (→-1 f) d))
                     (make-sequent (cons (→-2 f) s) p))
          (error "")))))

;;;; Weakening
;;;; Γ ⊢ Δ         Γ ⊢ Δ
;;;; -------(WL)  ---------(WR)
;;;; A,Γ ⊢ Δ       Γ ⊢ A,Δ
(defun wl (seq)
  (if (>= (length-l seq) 1)
      (make-goal (make-sequent (cdr (l seq)) (r seq)))
      (error "")))

(defun wr (seq)
  (if (>= (length-r seq) 1)
      (make-goal (make-sequent (l seq) (cdr (r seq))))
      (error "")))

;;;; Contraction
;;;; A,A,Γ ⊢ Δ      Γ ⊢ A,A,Δ
;;;; ---------(CL)  ---------(CR)
;;;; A,Γ ⊢ Δ        Γ ⊢ A,Δ
(defun cl (seq)
  (if (>= (length-l seq) 1)
      (make-goal (make-sequent (cons (nth-l 0 seq) (l seq)) (r seq)))
      (error "")))

(defun cr (seq)
  (if (>= (length-r seq) 1)
      (make-goal (make-sequent (l seq) (cons (nth-r 0 seq) (r seq))))
      (error "")))


;;;; Permutation
;;;; Γ0,..Γm,..Γn,.. ⊢ Δ      Γ ⊢ Δ0,..Δm,..Δn,..
;;;; -------------------(PL)  -------------------(CR)
;;;; Γ0,..Γn,..Γm,.. ⊢ Δ      Γ ⊢ Δ0,..Δn,..Δm,..
(defun pl (seq n m)
  (if (> (length-l seq) (max m n))
      (let ((l (subseq (l seq) 0)))
        (rotatef (nth n l) (nth m l))
        (make-goal (make-sequent l (r seq))))
      (error "")))

(defun pr (seq n m)
  (if (> (length-r seq) (max m n))
      (let ((r (subseq (r seq) 0)))
        (rotatef (nth n r) (nth m r))
        (make-goal (make-sequent (l seq) r)))
      (error "")))


;; ****************************************************************
;; formater
;;
;; TODO q
;;   - what is formula ?
;;   - how to define formula ?
;; ****************************************************************
(defun format-seq (seq n)
  (format nil "H~A: ~{~A~^, ~}~%C~A: ~{~A~^, ~}~%"
          n (mapcar #'format-formula (l seq))
          n (mapcar #'format-formula (r seq))))

(defun print-seq (seq n)
  (format t "~A" (format-seq seq n))
  seq)

(defun print-goal (goal)
  (if (null goal)
      (format t "Complete !!~%")
      (loop :for seq :in goal
            :for n :from 0
            :do (print-seq seq n))))


;; ****************************************************************
;; theorem (printer)
;;
;; TODO
;;   - structure of theorem
;;   - separate definition(structure) and printer
;; ****************************************************************
(defmacro def-theorem (name antecedent succedent &body proof)
  (let ((current-goal (gensym "GOAL-"))
        (step (gensym "STEP-"))
        (n (gensym "N-"))
        (rule (gensym "RULE-"))
        (args (gensym "ARGS-")))
    `(defun ,name ()
       (let ((,current-goal (make-goal (make-sequent ,antecedent ,succedent))))
         (format t "↓↓↓ GOAL ↓↓↓ ~%")
         (print-goal ,current-goal)
         (loop :for ,step :in ',proof
               :for ,n := (car ,step)
               :for ,rule := (cadr ,step)
               :for ,args := (cddr ,step)
               :do (setf ,current-goal (do-step ,current-goal ,n ,rule ,args))
               :do (format t "↓↓↓ ~A ↓↓↓ ~%" ,rule)
               :do (print-goal ,current-goal))
         ,current-goal))))


;; ****************************************************************
;; ex. law-of-exclded-middle
;; ⊢ A∨¬A
;; ****************************************************************
(let ((a (prop "A")))
  (def-theorem law-of-exclded-middle nil (list (∨ a (¬ a)))
    (0 cr)
    (0 or-r1)
    (0 pr 0 1)
    (0 or-r2)
    (0 not-r)
    (0 id)))


;; ****************************************************************
;;;; equivalence
;;;; -------(eq-ref)  -------------(eq-sym)
;;;; ⊢ a = a          a = b ⊢ b = a
;;;; --------------------(eq-assoc)
;;;; a = b, b = c ⊢ a = b
;; ****************************************************************

;; TODO


;; ****************************************************************
;; ex. natural number
;;
;;;; Axiom
;;;; ---------(nat-zero)  -----------------------(nat-succ)
;;;; ⊢ 0 ∈ Nat            a ∈ Nat ⊢ succ(a) ∈ Nat
;;;; 
;;;; -------------------------(nat-eq-succ)
;;;; succ(a) = succ(b) ⊢ a = b
;;
;; ****************************************************************
(def-axiom nat-zero (seq)
  (and (empty-l seq)
       (= (length-r seq) 1)
       (eq (nth 0 (nth-r 0 seq)) :nat)
       (= (nth 1 (nth-r 0 seq)) 0)))

(def-axiom nat-succ (seq)
  (and (= (length-l seq) 1)
       (= (length-r seq) 1)
       (eq (car (nth-l 0 seq)) :nat)
       (eq (car (nth-r 0 seq)) :nat)
       (eq (car (nth 1 (nth-r 0 seq))) :succ)
       (equal (nth 1 (nth-l 0 seq)) (nth 1 (nth 1 (nth-r 0 seq))))))

(def-theorem one-is-nat nil '((:nat (:succ 0)))
  (0 cut (:nat 0) 0 0)
  (0 nat-zero)
  (0 nat-succ))
