(setq n (read)) ;no.of lists
(defparameter a (make-array (+ 1 n) :fill-pointer 1)) ;list of lists as input
(let((i 0))(loop (when (eq i n)(return))(vector-push  (read) a)(incf i)))
(setq q (read)) ;no.of quries
(defun unionn (s1 s2) ;union function
    (if (eq s1 ())
        s2
        (unionn (cdr s1) (if(check (car s1) s2) s2 (cons (car s1) s2)))
    )
)

(defun intersect  (s1 s2) ;intersection fucntion
   (if (eq s1 ())
       s1
       (if (check (car s1) s2)
           (cons (car s1) (intersect (cdr s1) s2))
           (intersect (cdr s1) s2)
       )
   )
)

(defun difference (s1 s2) ;difference function
   (cond
     ((eq s1 ()) s1)
     ((check (first s1) s2) (difference (cdr s1) s2))
     (t (cons (car s1) (difference (cdr s1) s2))))
)


(defun cartesian-product2 (l)
  (if (eq l ())
      (list nil)
      (loop for x in (car l)
            nconc (loop for y in (cartesian-product2 (cdr l))  
                        collect (cons x y))))
)

(defun cartesian-product1 (res l)
    (if (eq l ())
        res
        (cartesian-product1 ( reverse ( cons (aref a  (car l)) (reverse res) ) ) (cdr l) ) 
    )
)

(defun cartesian-product (l)
    (cartesian-product2 (cartesian-product1  nil l))
)


(defun check(a s)      ; function to know if the given set contains a given element
    (if (= (list-length s) 0) NIL (or (= a (car s)) (check a (cdr s))))
)


(defun powerset (s)     ;power-set fuction
    (if (null s)
          '(())
        (adding (car s) (powerset (cdr s)))
    )
)

(defun adding (a s)     ;adding to add to the set
    (if (null s)
          nil
        (cons (car s)
                (cons (cons a (car s))
                      (adding a (cdr s))
                )
        )
    )
)

(defun cmp (s1 s2)  ;functor for sorting in lexico order
  (cond ((null s1) (not (null s2)))
        ((null s1) nil)
      ((< (list-length s1)(list-length s2))t)
        ((= (list-length s1)(list-length s2))(if(= (car s1)(car s2))(cmp (cdr s1)(cdr s2))(if(< (first s1)(first s2))1)))))

(defun print1(p)  ;fucntion to print a list
    (princ "(") 
    (terpri)
    (dolist (n p)
      (princ n)(terpri))
    (princ ")")
    (terpri)
)
(defun print2(p)   ;function to print list of lists
    (princ "(")
    (terpri)
    (dolist (n p)
        (print1 n))
    (princ ")")
    (terpri)
)   


(setq i 0)  ;taking quiries as input using loop
(loop (when(= i q)(return))
      (setq j (read))
      (if(= j 1)( progn
        (setq l (read-from-string (concatenate 'string "(" (read-line) ")"))) ;converting quiries into a list
        (setq s (aref a (car l)))
        (setq l (cdr l))
        (loop (when (eq l ())(return))(setq s (unionn s (aref a (car l))))(setq l (cdr l) ))
        (setq s1 (sort (copy-seq s) #'<))
        (print1 s1)))
      (if(= j 2)( progn
        (setq l (read-from-string (concatenate 'string "(" (read-line) ")")))
        (setq s (aref a (car l)))
        (setq l (cdr l))
        (loop (when (eq l ())(return))(setq s (intersection s (aref a (car l))))(setq l (cdr l) ))
        (setq s1 (sort (copy-seq s) #'<))
        (print1 s1)))
      (if(= j 3)( progn
        (setq l (read-from-string (concatenate 'string "(" (read-line) ")")))
        (setq s (aref a (car l)))
        (setq l (cdr l))
        (loop (when (eq l ())(return))(setq s (difference s (aref a (car l))))(setq l (cdr l) ))
        (setq s1 (sort (copy-seq s) #'<))
        (print1 s1)))
      (if(= j 4)( progn
        (setq l (read-from-string (concatenate 'string "(" (read-line) ")")))
        (setq s1 (cartesian-product l))
        (setq s1 (sort (copy-seq s1) #'cmp))
        (print2 s1)))
      (if(= j 5)( progn
        (setq l (read-from-string (concatenate 'string "(" (read-line) ")")))
        (setq s (check (car(cdr l)) (aref a (car l))))
        (if(null s)(setf s1 0)(setf s1 1))
        (prin1 s1)
        (terpri)
        ))
      (if(= j 6)( progn
        (setq l (read-from-string (concatenate 'string "(" (read-line) ")")))
        (setq s (aref a (car l)))
        (setq s (powerset s))
        (setf (car s) '())
        (setq s1 (sort (copy-seq s) #'cmp))
        (print2 s1)
        ))
      (incf i))



