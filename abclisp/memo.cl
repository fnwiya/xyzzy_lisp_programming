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
