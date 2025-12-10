(defun remove-each-rnth-reducer (n &key (key #'identity))

  (unless (and (integerp n) (plusp n))
    (error "n must be a positive integer, got ~S" n))
  (let ((count 0))                 
    (lambda (element acc)
      (if (funcall key element)
          (progn
            (incf count)           
            (if (zerop (mod count n))
                acc                
                (cons element acc))) 
          (cons element acc))))))    




(defun check-remove (name n input expected &key (key #'identity))
  (let* ((result (reduce (remove-each-rnth-reducer n :key key)
                         input
                         :from-end t
                         :initial-value '())))
    (format t "~:[FAILED~;passed~] ~a~%"
            (equal result expected)
            name)))


(defun test-remove ()

  (check-remove "test 1" 2 '(1 2 3 4 5) '(1 3 5))
  (check-remove "test 2" 3 '(1 2 3 4 5 6 7) '(1 3 4 6 7))
  (check-remove "test 3" 2 '() '())

  (check-remove "test 4"
                2
                '(1 2 2 2 3 4 4 4 5)
                '(1 2 3 4 4 5)
                :key #'evenp)

  (check-remove "test 5"
                3
                '("hi" "hello" "abc" "world" "yo" "test" "ok")
                '("hi"  "abc" "world" "yo" "test" "ok")
                :key (lambda (x) (> (length x) 3)))

  )



(defun swap-pass (pairs test)
  (cond
    ((null (rest pairs))
     (values pairs nil))

    (t
     (let* ((a (first pairs))
            (b (second pairs))
            (ka (cdr a))
            (kb (cdr b)))
       (if (funcall test kb ka)
           (multiple-value-bind (rest-of-pass swapped?)
               (swap-pass (cons a (rest (rest pairs))) test)
             (values (cons b rest-of-pass) t))
           (multiple-value-bind (rest-of-pass swapped?)
               (swap-pass (rest pairs) test)
             (values (cons a rest-of-pass) swapped?)))))))

(defun bubble-hof (lst &key (key #'identity) (test #'>))
  (let ((pairs (mapcar (lambda (x) (cons x (funcall key x))) lst)))

    (labels ((loop-sort (pairs)
               (multiple-value-bind (new-pairs flag)
                   (swap-pass pairs test)
                 (if flag
                     (loop-sort new-pairs)
                     new-pairs))))
      (mapcar #'car (loop-sort pairs)))))

(defun check-sort (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (bubble-hof input :key key :test test)
                 expected)
          name))

(defun test-sort ()
  ;; без key/test
  (check-sort "test 1" '(5 3 4 1 2) '(1 2 3 4 5))
  (check-sort "test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-sort "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-sort "test 4" '(2 2 3 3 1) '(1 2 2 3 3))
  (check-sort "test 5" nil nil)

  (check-sort "test 6" '(-5 3 -1 2) '( -1 2 3 -5 )
              :key #'abs)

  (check-sort "test 7" '(1 2 3 4) '(4 3 2 1)
              :test #'>)

  (check-sort "test 8" '("aaa" "bb" "c")
              '("c" "bb" "aaa")
              :key #'length :test #'<))

