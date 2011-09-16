
(define (string->bytes str)
  (map char->integer (string->list str)))

(define (floor->exact n)
  (inexact->exact (flfloor (exact->inexact n))))
(define (list-head l k)
  (if (zero? k)
    '()
    (cons (car l)
          (list-head (cdr l) (- k 1)))))


(define (fold-left f b . a*)
  (define (fold a* r)
    (if (null? (car a*))
      r
      (fold (map cdr a*)
            (f r (car (car a*))))))
  (fold a* b))

(define (index object l)
  (define (index-loop l i)
    (if (null? l)
      #f
      (if (eq? (car l) object)
        i
        (index-loop (cdr l) (+ i 1)))))
  (index-loop l 0))

(define (generate-list generator n)
  (define (loop n)
    (if (zero? n)
      '()
      (append (generator)
              (loop (- n 1)))))
  (loop n))

(define (cons-generated generator n)
  (define (loop n)
    (if (zero? n)
      '()
      (cons (generator)
            (loop (- n 1)))))
  (loop n))

(define (make-loop combiner item the-list)
  (define (loop l)
    (if (null? l)
      '()
      (combiner (item (car l))
                (loop (cdr l)))))
  (loop the-list))

(define (flatten l)
  (if (null? l)
    '()
    (append (car l)
            (flatten (cdr l)))))

(define (vector-find v key)
  (define (loop index)
    (if (negative? index)
      #f
      (if (eq? key (vector-ref v index))
        index
        (loop (- index 1)))))
  (loop (- (vector-length v) 1)))

(define (multiply object n)
  (generate-list (lambda() object)
                 n))

(define (identity o)
  o)
(define (find-max l)
  (if (null? l)
    0
    (max (car l)
         (find-max (cdr l)))))

(define << fxarithmetic-shift)

(define (generate-sequence n)
  (if (zero? n)
    '()
    (cons n
          (generate-sequence (- n 1)))))
;;; shuffling the list of four-digit numbers
(define (shuffle ls0)
  (let ((vec (list->vector ls0)))
    (let loop ((n (vector-length vec)) (ls1 '()))
      (if (= n 0)
        ls1
        (let* ((r (random-integer n))
               (v (vector-ref vec r)))
          (vector-set! vec r (vector-ref vec (- n 1)))
          (loop (- n 1) (cons v ls1)))))))
(define (random-sequence n)
  (shuffle (generate-sequence n)))
(define (select-random l)
  (list-ref l (random-integer (length l))))
