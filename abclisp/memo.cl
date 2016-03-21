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

(defun fibo (n &optional (a1 1)(a2 0))
  (if (< n 1)
      a1
      (fibo (1- n)(+ a1 a2) a1)))

(defmacro sum-macro (x) (cons '+ x))

(defmacro push-macro (item place)
  (list 'setq
        place
        (list 'cons item place)))

(defmacro pop-macro (place)
  (list 'prog1
        (list 'car place)
        (list 'setq
              place
              (list 'cdr place))))

(defmacro push-macro2 (item place)
  `(setq ,place (cons ,item ,place)))

(defmacro pop-macro2 (place)
  `(prog1 (car ,place)
     (setq ,place (cdr ,place))))

(defmacro define (args-list &rest body)
  `(defun ,(car args-list) ,(cdr args-list) ,@body))

(defun my-union (x y)
  (cond ((atom x) y)
        ((member (car x) y) (my-union (cdr x) y))
        (t (cons (car x) (my-union (cdr x) y)))))

(defun my-intersection (x y)
  (cond ((atom x) nil)
        ((member (car x) y) (cons (car x) (my-intersection (cdr x) y)))
        (t (my-intersection (cdr x) y))))

(defstruct (WordCount (:conc-name get-))
  (word nil)
  (count 1))

(defun word-count (filename)
  (with-open-file (in filename :direction :input)
    (let (buffer word-list wc)
      (while (setq buffer (read-line in nil))
        (dolist (word (split-string buffer " \t"))
          (if (setq wc (find word word-list :key #'get-word :test #'equal))
              (incf (get-count wc))
              (push (make-WordCount :word word) word-list))))
      (print-word-count word-list))))

(defun print-word-count (word-list)
  (dolist (wc (sort word-list #'string< :key #'get-word))
    (format t "~A ~D~%" (get-word wc) (get-count wc))))

(defun binary-search (item table)
  (let ((low 0) (high (1- (length table))) middle)
    (while (<= low high)
      (setq middle (truncate (+ low high) 2))
      (cond ((= (aref table middle) item)
             (return item))
            ((< (aref table middle) item)
             (setq low (1+ middle)))
            (t (setq high (1- middle)))))))

(defun make-point ()
  (let (point)
    (dotimes (x 3 point)
      (push (random 100) point))))

(defun make-data (n)
  (let ((count 0) point buffer)
    (while (< count n)
      (setq point (make-point))
      (unless (find point buffer :test #'equal)
        (push point buffer)
        (incf count)))
    buffer))

(defconstant *hash-size* 4001)

(defun hash-func (point)
  (let ((value 0))
    (dolist (x point (mod value *hash-size*))
      (setq value (+ (* value 100) x)))))

(defun insert-hash (point hash-table)
  (let ((value (hash-func point)))
    (unless (find point (aref hash-table value) :test #'equal)
      (push point (aref hash-table value)))))

(defun make-data-fast (n)
  (let ((hash-table (make-array *hash-size*))
        (count 0) buffer point)
    (while (< count n)
      (setq point (make-point))
      (when (insert-hash point hash-table)
        (push point buffer)
        (incf count)))
    buffer))

(defstruct Queue (front nil)(rear nil))

(defun enqueue (queue item)
  (let ((new-cell (list item)))
    (if (Queue-front queue)
        (setf (cdr (Queue-rear queue)) new-cell)
        (setf (Queue-front queue) new-cell))
    (setf (Queue-rear queue) new-cell)))

(defun dequeue (queue)
  (if (Queue-front queue)
      (prog1
          (pop (Queue-front queue))
        (unless (Queue-front queue)
          (setf (Queue-rear queue) nil)))))

(defun circular-list-p (l)
  (let ((fast l)(slow l))
    (loop
         (setq fast (cddr fast)
               slow (cdr slow))
       (cond ((endp fast) (return))
             ((eq fast slow) (return t))))))

(defun make-gen-fibo ()
  (let ((a0 1) (a1 0) (a2 0))
    #'(lambda () (prog1 a0
                   (setq a2 a1 a1 a0)
                   (setq a0 (+ a1 a2))))))

(defstruct Queue
  (front  0)
  (rear   0)
  (count  0)
  (buffer (make-array 16)))

(setf (get 'A 'adjacent) '(B C)
      (get 'B 'adjacent) '(A C D)
      (get 'C 'adjacent) '(A B E)
      (get 'D 'adjacent) '(B E F)
      (get 'E 'adjacent) '(C D G)
      (get 'F 'adjacent) '(D)
      (get 'G 'adjacent) '(E))

(defun search (goal path)
  (dolist (node (get (car path) 'adjacent))
    (if (eq goal node)
        (print (reverse (cons node path)))
        (unless (member node path)
          (search goal (cons node path))))))

(defstruct Queue (front nil) (rear nil))

(defun enqueue (queue item)
  (let ((new-cell (list item)))
    (if (Queue-front queue)
        (setf (cdr (Queue-rear queue)) new-cell)
        (setf (Queue-front queue) new-cell))
    (setf (Queue-rear queue) new-cell)))

(defun dequeue (queue)
  (if (Queue-front queue)
      (prog1
          (pop (Queue-front queue))
        (unless (Queue-front queue)
          (setf (Queue-rear queue) nil)))))

(defun breadth-search (goal start)
  (let ((queue (make-Queue)) path)
    (enqueue queue (list start))
    (while (setq path (dequeue queue))
      (dolist (node (get (car path) 'adjacent))
        (if (eq goal node)
            (print (reverse (cons node path)))
            (unless (member node path)
              (enqueue queue (cons node path))))))))

(defun search-id (limit goal path)
  (if (= limit (length path))
      (if (eq goal (car path))
          (print (reverse path)))
      (dolist (node (get (car path) 'adjacent))
        (unless (member node path)
          (search-id limit goal (cons node path))))))

(defun solve (&optional (numbers '(1 2 3 4 5 6 7 8 9)) perm)
  (if numbers
      (dolist (x numbers)
        (solve (remove x numbers) (cons x perm)))
      (check perm)))

(defun make-number (perm)
  (let ((value 0))
    (dotimes (x 3 value)
      (setq value (+ (* value 10) (pop perm))))))

(defun check (perm)
  (let ((abc (make-number perm))
        (def (make-number (nthcdr 3 perm)))
        (ghi (make-number (nthcdr 6 perm))))
    (if (and (oddp abc)
             (oddp def)
             (oddp ghi)
             (< abc def ghi)
             (= (+ abc def ghi) 999))
        (format t "~D + ~D + ~D = 999~%" abc def ghi))))

(defun solve-fast (&optional (n 0) (numbers '(1 2 3 4 5 6 7 8 9)) (value 0) num-list)
  (if numbers
      (if (= n 3)
          (if (and (oddp value)
                   (apply #'> value num-list))
              (solve-fast 0 numbers 0 (cons value num-list)))
          (dolist (x numbers)
            (solve-fast (1+ n) (remove x numbers) (+ (* value 10) x) num-list)))
      (if (and (oddp valur)
               (apply #'> value num-list)
               (= (apply #'+ value num-list) 999))
          (format t "~D + ~D + ~D = 999~%" (second num-list) (first num-list) value))))

(defvar *pattern* #(#x0000023 #x0000047 #x000008e #x000011c #x0000218
                    #x0000461 #x00008e2 #x00011c4 #x0002388 #x0004310
                    #x0008c20 #x0011c40 #x0023880 #x0047100 #x0086200
                    #x0118400 #x0238800 #x0471000 #x08e2000 #x10c4000
                    #x0308000 #x0710000 #x0e20000 #x1c40000 #x1880000))

(defun solve (board)
  (dotimes (i 32)
    (let ((new-board board) pushed-button)
      (dotimes (j 5)
        (when (logbitp j i)
          (push j pushed-button)
          (setq new-board (logxor new-board (aref *pattern* j)))))
      (dotimes (j 20)
        (when (logbitp j new-board)
          (push (+ j 5) pushed-button)
          (setq new-board (logxor new-board (aref *pattern* (+ j 5))))))
      (if (zerop new-board)
          (print-answer (reverse pushed-button))))))

(defun print-answer (pushed-button)
  (dotimes (x 25 (terpri))
    (if (zerop (mod x 5)) (terpri))
    (cond ((and pushed-button
                (= x (car pushed-button)))
           (princ "@")
           (pop pushed-button))
          (t (princ "x")))))

(defun max-position (nim)
  (let ((num -1) pos)
    (dotimes (x 3 (values pos 1))
      (if (< num (nth x nim))
          (setq pos x
                num (nth x nim))))))

(defun safe-position (nim)
  (let ((nim-sum (apply #'logxor nim)) num)
    (dotimes (x 3)
      (setq num (- (nth x nim) (logxor (nth x nim) nim-sum)))
      (if (plusp num)
          (return (values x num))))))

(defun move-com (nim)
  (format t "~%~S my turn~%" nim)
  (multiple-value-bind
        (pos num)
      (if (zerop (apply #'logxor nim))
          (max-position nim)
          (safe-position nim))
    (remove-stone nim pos num)))

(defun make-expr (n expr)
  (if (= n 10)
      (check (reverse expr))
      (dolist (op '(+ - @))
        (make-expr (1+ n) (cons n (cons op expr))))))

(defun check (expr)
  (setq *expr* expr)
  (let ((num (get-first-number)))
    (while *expr*
      (case (pop *expr*)
        (+ (incf num (get-number)))
        (- (decf num (get-number)))))
    (if (= num 100)
        (print expr))))

(defun get-first-number ()
  (cond ((integerp (car *expr*))
         (get-number))
        ((eq '- (car *expr*))
         (pop *expr*)
         (- (get-number)))))

(defun get-number ()
  (let ((num (pop *expr*)))
    (while (eq '@ (car *expr*))
      (pop *expr*)
      (setq num (+ (* num 10) (pop *expr*))))
    num))

(defvar *adjacent* #((1 2 3)       ; 0
                     (0 3 4)       ; 1
                     (0 3 5)       ; 2
                     (0 1 2 4 5 6) ; 3
                     (1 3 6)       ; 4
                     (2 3 6)       ; 5
                     (3 4 5)))     ; 6

(defvar *state* (make-array 5040))
(defvar *prev* (make-array 5040))
(defvar *space* (make-array 5040))

(defun move-piece (piece board)
  (cond ((null board) nil)
        ((= piece (car board))
         (cons 0 (move-piece piece (cdr board))))
        ((= 0 (car board))
         (cons piece (move-piece piece (cdr board))))
        (t (cons (car board) (move-piece piece (cdr board))))))

(defun solve-b (start goal)
  (let ((rear 1) (front 0))
    (setf (aref *state* 0) start
          (aref *prev* 0) -1
          (aref *space* 0) (position 0 start))
    (while (< front rear)
      (let ((space (aref *space* front))
            (board (aref *state* front)) new-board)
        (dolist (pos (aref *adjacent* space))
          (setq new-board (move-piece (nth pos board) board))
          (unless (find new-board *state* :end rear :test #'equal)
            (setf (aref *state* rear) new-board
                  (aref *space* rear) pos
                  (aref *prev* rear) front)
            (incf rear)
            (when (equal new-board goal)
              (print-answer (1- rear))
              (return-from solve-b)))))
      (incf front))))

(defun print-answer (pos)
  (if (/= pos 0)
      (print-answer (aref *prev* pos)))
  (print (aref *state* pos)))

(defvar *direction* (make-array 5040))  ; 探索の方向

(defun solve-b1 (start goal)
  (let ((rear 2) (front 0))
    (setf (aref *state* 0) start
          (aref *prev* 0) -1
          (aref *space* 0) (position 0 start)
          (aref *direction* 0) 'forward
          (aref *state* 1) goal
          (aref *prev* 1) -1
          (aref *space* 1) (position 0 goal)
          (aref *direction* 1) 'backward)
    (while (< front rear)
      (let ((space (aref *space* front))
            (board (aref *state* front)) new-board x)
        (dolist (pos (aref *adjacent* space))
          (setq new-board (move-piece (nth pos board) board))
          (setq x (position new-board *state* :end rear :test #'equal))
          (cond ((null x)
                 (setf (aref *state* rear) new-board
                       (aref *space* rear) pos
                       (aref *prev* rear) front
                       (aref *direction* rear) (aref *direction* front))
                 (incf rear))
                ((not (eq (aref *direction* x) (aref *direction* front)))
                 (print-answer1 front x)
                 (return-from solve-b1)))))
      (incf front))))

(defun print-answer-forward (pos)
  (if (/= pos 0)
      (print-answer-forward (aref *prev* pos)))
  (print (aref *state* pos)))

(defun print-answer-backward (pos)
  (while (/= pos -1)
    (print (aref *state* pos))
    (setq pos (aref *prev* pos))))

(defun print-answer1 (p1 p2)
  (cond ((eq (aref *direction* p1) 'forward)
         (print-answer-forward p1)
         (print-answer-backward p2))
        (t (print-answer-forward p2)
           (print-answer-backward p1))))

(defvar *state* (make-array 140))
(defvar *prev-state* (make-array 140))
(defvar *space-postion* (make-array 140))

(defun move-position (state)
  (let (result)
    (dotimes (i 7 result)
      (if (and (aref state i) (aref state (1+ i)))
          (push i result)))))

(defun move-stone (front rear n)
  (let ((new-state (copy-seq (aref *state* front)))
        (space (aref *space-postion* front)))
    (setf (aref new-state space)      (aref new-state n)
          (aref new-state (1+ space)) (aref new-state (1+ n))
          (aref new-state n)          nil
          (aref new-state (1+ n))     nil
          (aref *space-postion* rear) n
          (aref *prev-state* rear)    front
          (aref *state* rear)         new-state)))

(defun solve-max ()
  (let ((rear 1) (front 0))
    (setf (aref *state* 0) '(1 2 3 4 5 6 0)
          (aref *move* 0) 0
          (aref *space* 0) 6
          (aref *piece* 0) 0)
    (while (< front rear)
      (let ((space (aref *space* front))
            (board (aref *state* front))
            (prev (aref *piece* front)) piece new-board)
        (dolist (pos (aref *adjacent* space))
          (setq piece (nth pos board))
          (when (/= piece prev)
            (setq new-board (move-piece piece board))
            (unless (find new-board *state* :end rear :test #'equal)
              (setf (aref *state* rear) new-board
                    (aref *space* rear) pos
                    (aref *move* rear) (1+ (aref *move* front))
                    (aref *piece* rear) piece)
              (incf rear)))))
      (incf front))
    (print-answer-max (1- rear))))

(defun count-word (word filename)
  (with-open-file (in filename :direction :input)
    (let ((word-table (make-hash-table :test #'equal))
          buffer find-index start-index)
      (while (setq buffer (read-line in nil))
             (setq start-index 0)
             (while (setq find-index (string-match word buffer start-index))
                    (setq start-index (match-end 0))
                    (incf (gethash (subseq buffer find-index start-index) word-table 0))))
      (print-word-table word-table))))

(defun print-word-table (word-table)
  (let (temp)
    (maphash #'(lambda (key value) (push (cons key value) temp))
             word-table)
    (map nil
         #'(lambda (x) (format t "~A ~A~%" (car x) (cdr x)))
         (sort temp #'> :key #'cdr))))

(defun solve1 ()
  (let ((table (make-array 254016) j k l)
        (dotimes (n 10)
          (if (= n (aref *fact-table* n))
              (print n))
          (setf (aref table n) (aref *fact-table* n)))
        (do ((n 1 (1+ n)))
            ((>= n 254016))
          (setq l (* n 10))
          (dotimes (m 10)
            (setq k (+ (aref table n) (aref table m))
                  j (+ l m))
            (if (< j 254016)
                (setf (aref table j) k))
            (if (= k j)
                (print k)))))))


