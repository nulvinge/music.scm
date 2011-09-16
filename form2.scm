(load "systemfuncs.scm")
(load "math.scm")

;target format, which the aform->nform func converts to
;(define pop-form
;    '((2 1) ;variations: three for 0 and two for 1
;      ((0 . 0)
;       (1 . 0)
;       (0 . 1)
;       (1 . 0)
;       (0 . 2)
;       (1 . 1)
;       )))

(define pop-form
    '(A B 'A B ''A 'B))

(define (aform->nform form)
    (define alfa-num '())
    (define variations (vector))

    (define (deepest i)
      (define (loop n i)
        (if (list? i)
          (if (eq? (car i) 'quote)
            (loop n (cdr i))
            (loop (++ n) (car i)))
          (cons i n)))
      (loop 0 i))
    (define (loop i)
      (if (null? i)
        '()
        (let* ((d (deepest (car i)))
               (n (index (car d)
                         alfa-num)))
          (if (eq? n #f)
            (begin
              (set! alfa-num
                    (append alfa-num
                            (list (car d))))
              (set! variations
                    (vector-append variations
                            (vector (cdr d))))
              (set! n
                    (-- (length alfa-num)))))
          (if (< (vector-ref variations n)
                 (cdr d))
            (vector-set! variations n
                        (cdr d)))

          (cons (cons n (cdr d))
                (loop (cdr i))))))
    (let ((r (loop form)))
      (cons (vector->list variations)
            (list r))))

(write pop-form)(newline)
(write (aform->nform pop-form)) (newline)

