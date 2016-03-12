(format t "hello, world\n")

(defun my-length (x)
  (if (atom x)
      0
      (1+ (my-length (cdr)))))

(defun my-append (x y)
  (if (atom x)
      y
      (cons (car x) (my-append (cdr x) y))))
(defun my-subst (old new tree)
  (cond
    ((equal old tree) new)
    ((atom tree) tree)
    (t (cons (my-subst old new (car tree))
             (my-subst old new (cdr tree))))))

(defun hanoi (n from to via)
  (cond ((= n 1)
         (format t "from ~A to ~A\n" from to))
        (t
         (hanoi (1- n) from via to)
         (format t "from ~A to ~A\n" from to)
         (hanoi (1- n) via to from))))

(defun fact (x)
  (let ((result 1))
    (dotimes (n x result)
      (setq result (* result (1+ m))))))

(defun my-length2 (x)
  (let ((result 0))
    (dolist (y x result)
      (setq result (1+ result)))))

(defun fact-while (x)
  (let ((result 1) (i 2))
    (while (<= i x)
      (setq result (* result i)
            i (1+ i)))
    result))

(defun fact-loop (x)
  (let ((result 1) (i 2))
    (loop
         (if (> i x) (return result))
       (setq result (* result i)
             i (1+ i)))))

(defun fact-do (x)
  (do ((n 2 (1+ n)) (result 1))
      ((< x n) result)
    (setq result (* result n))))

(defun prime-p (n k prime-list)
  (dolist (m prime-list)
    (cond ((zerop (mod n m)) (return))
          ((< k m) (result t)))))

(defun prime (n)
  (do ((prime-list '(2))
       (m 3 (+ m 2)))
      ((< n m) prime-list)
    (if (prime-p m (sqrt m) prime-list)
        (setq prime-list (append prime-list (list m))))))

(defun push-stack (data)
  (setq *stack* (cons data *stack*)))

(defun pop-stack ()
  (prog1 (car *stack*)
         (setq *stack* (cdr *stack*))))

(defun ncr (n r)
  (if (or (= n r) (zerop r))
      1
      (/ (* (ncr n (1- r)) (1+ (- n r))) r)))

(defun pascal (n)
  (dotimes (i (1+ n))
    (dotimes (j (1+ i))
      (format t "  ~3D" (ncr i j)))
    (terpri)))

(defun insert-element (f n l)
  (cond ((atom l) (cons n nil))
        ((funcall f n (car l)) (cons n l))
        (t (cons (car l) (insert-element f n (cdr l))))))

(defun insert-sort (f l)
  (if (atom l)
      nil
      (insert-element f (car l) (insert-sort f (cdr l)))))

(defun quick-sort (f l)
  (unless (atom l)
    (let ((p (car l)) l1 l2)
      (dolist (n (cdr l))
        (if (funcall f n p) (push n l1) (push n l2)))
      (append (quick-sort f l1) (cons p (quick-sort f l2))))))

