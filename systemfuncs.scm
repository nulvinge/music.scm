
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
