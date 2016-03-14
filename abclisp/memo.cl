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

(defun fibo-org (x)
  (if (or (= 0 x) (= 1 x))
      1
      (+ (fibo (- x 1)) (fibo (- x 2)))))

(setq *fibo-table-limit* 100
      *fibo-table* (make-array (1+ *fibo-table-limit*)))

(defun fibo1 (n)
  (if (<= 0 n 1)
      1
      (if (aref *fibo-table* n)
          (aref *fibo-table* n)
          (setf (aref *fibo-table* n)
                (+ (fibo1 (1- n)) (fibo1 (- n 2)))))))

(defun fobo (n)
  (if (<= 0 n *fibo-table-limit*)
      (fibo1 n)))

(defun init-stack (n)
  (setq *stack-size* n
        *top* 0
        *stack* (make-array n)))

(defun push-down (data)
  (when (< *top* *stack-size*)
    (setf (aref *stack* *top*) data)
    (incf *top*)))

(defun pop-up ()
  (when (plusp *top*)
    (decf *top*)
    (aref *stack* *top*)))

(defun prime-p (n k prime-list count)
  (dotimes (m count)
    (cond ((zerop (mod n (aref prime-list m))) (return))
          ((<= k (aref prime-list m)) (return t)))))

(defun prime-vector (n)
  (let ((prime-list (make-array n))
        (count 0))
    (setf (aref prime-list count) 2)
    (incf count)
    (do ((m 3 (+ m 2)))
        ((<= n count) prime-list)
      (when (prime-p m (sqrt m) prime-list count)
        (setf (aref prime-list count) m)
        (incf count)))))

(defun pascal-sub (num-list)
  (if (second num-list)
      (cons (+ (first num-list) (second num-list))
            (pascal-sub (rest num-list)))
      '(1)))

(defun pascal-list (n)
  (let (buf)
    (dotimes (i n)
      (setq buf (pascal-sub (cons 0 buf)))
      (print buf))))

(defun pascal-vector (n)
  (let ((buff (make-array (1+ n) :initial-element 0)))
    (setf (aref buff 1) 1)
    (dotimes (i n)
      (do ((j (1+ i) (1- j)))
          ((zerop j))
        (format t " ~3D" (setf (aref buff j)
                               (+ (aref buff j)(aref buff (1- j))))))
      (terpri))))

(defun my-rev-sub (l z)
  (if (atom l)
      z
      (my-rev-sub (cdr l) (cons (car l) z))))

(defun my-reverse (l)
  (my-rev-sub l nil))

(defun my-reverse (l &optional z)
  (if (atom l)
      z
      (my-reverse (cdr l) (cons (car l) z))))

(defun merge-list (f l1 l2)
  (cond ((atom l1) l2)
        ((atom l2) l1)
        ((funcall f (car l1) (car l2))
         (cons (car l1) (merge-list f (cdr l1) l2)))
        (t (cons (car l2) (merge-list f l1 (cdr l2))))))

(defun merge-sort (f l n)
  (cond ((= n 1) (list (car l)))
        ((= n 2)
         (let ((x (first l)) (y (second l)))
           (if (funcall f x y) (list x y) (list y x))))
        (t (let ((m (truncate n 2)))
             (merge-list f
                         (merge-sort f l m)
                         (merge-sort f (nthcdr m l) (- n m)))))))


