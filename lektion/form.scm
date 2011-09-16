
(define (make-form height bg-limit)
  (define (change-items cl l)
    (map (lambda (i c)
           (case c
             ((-1) (cons 0       i))
             (( 0) (cons (car i) i))
             (( 1) (cons 1       i))))
         l
         cl))
  (define make-change-return '())
  (define (make-change to cl)
    (define (loop n)
      (if (zero? n)
        '()
        (cons (if (eq? #f (memv n cl))
                (if (eq? #f (memv n make-change-return))
                  0
                  1)
                to)
              (loop (- n 1)))))
    (let ((ret (reverse (loop height))))
      (set! make-change-return '())
      ret))
  (define (change to cl l)
    (change-items (make-change to cl) l))
  (define (change-1 cl l)
    (change -1 cl l))
  (define (change0 cl l)
    (change 0 cl l))
  (define (change1 cl l)
    (change 1 cl l))
  (define (make-loop2 combiner part l start)
    (define (loop l)
      (if (null? l)
        start
        (combiner (part l)
                  (loop (cdr l)))))
    (loop l))
  (define (intro count start)
    (make-loop2 change1
                (lambda(i) (list (car i)))
                (random-sequence count)
                start))
  (define (outro count start)
    (make-loop2 change-1
                (lambda(i) (list (car i)))
                (random-sequence count)
                start))
  (define (play-pause line break start)
    (let ((ret (change-1 (append break (list line))
                         (change1 (list line)
                                  start))))
      (set! make-change-return break)
      ret))
  (define (buildup lines start)
    (make-loop2 (lambda(l s) (play-pause l '() s))
                car
                lines
                start))
  (define (release lines start)
    (make-loop2 (lambda(l s) (change -1 (list l) s))
                car
                lines
                (change 0
                        '()
                        (make-loop2 (lambda(l s) (change 1 (list l) s))
                                    car
                                    lines
                                    start))))
  (define (buildup-break lines start)
    (define break (list-tail (random-sequence bg-limit)
                             (+ 1 (random-integer (- bg-limit 1)))))
    (let ((ret (play-pause (car lines)
                           break
                           (buildup (cdr lines) start))))
      (set! make-change-return break)
      ret))
  (define (perform action-list)
    (define (loop action-list)
      (if (null? action-list)
        (multiply (list (list 0)) height)
        ((car action-list) (loop (cdr action-list)))))
    (map (lambda(l) (cdr (reverse (cdr l))))
         (loop (reverse action-list))))

  (define buildup-list
    (map (lambda(n) (+ bg-limit n))
         (random-sequence (- height bg-limit))))

  (perform (list (lambda(l) (intro bg-limit l))
                 (lambda(l) (buildup-break buildup-list l))
                 (lambda(l) (buildup buildup-list l))
                 (lambda(l) (release buildup-list l))
                 (lambda(l) (outro bg-limit l))
           )))