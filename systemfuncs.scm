
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

