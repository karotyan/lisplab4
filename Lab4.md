<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Кукса Кирило Віталійович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:

- використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. No3);

 - додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.
12). При цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.
## Варіант першої частини 3
Алгоритм сортування обміном No2 (із використанням прапорця) за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
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
```
### Тестові набори та утиліти першої частини
```lisp
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

```
### Тестування першої частини
```lisp
CL-USER> (test-sort)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
passed test 6
passed test 7
passed test 8
NIL
```
## Варіант другої частини 8
Написати функцію remove-each-rnth-reducer , яка має один основний параметр n та
один ключовий параметр — функцію key . remove-each-rnth-reducer має повернути
функцію, яка при застосуванні в якості першого аргумента reduce робить наступне: при
обході списку з кінця, кожен n -ний елемент списку-аргумента reduce , для якого
функція key повертає значення t (або не nil ), видаляється зі списку. Якщо
користувач не передав функцію key у remove-each-rnth-reducer , тоді зі списку
видаляється просто кожен n -ний елемент. Обмеження, які накладаються на
використання функції-результату remove-each-rnth-reducer при передачі у reduce
визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають
бути значення ключових параметрів функції reduce from-end та initial-value ).
## Лістинг реалізації другої частини завдання
```lisp
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

```
### Тестові набори та утиліти другої частини
```lisp
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
```
### Тестування другої частини
```lisp
CL-USER> (test-remove)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
NIL
```