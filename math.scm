(random-source-randomize! default-random-source)

(define (-- x)
        (- x
           1))
(define (++ x)
        (+ x
           1))

;makes an exponential number between 0 and (l^2)
;with the highest probability around 0
(define (random2 l)
  (let ((r (random-integer l)))
    (* r r)))
;makes an exponential number between -(l^2) and (l^2)
;with the highest probability around 0
(define (randomn l)
  (let ((l2 (* l 2)))
    (let ((r (random-integer l2)))
      (* (- r l)
         (/ r 2)))))
;Uses the masraglia algorithm to make a random number
;with normal distribution of my=0 and sigma^2=1
;This gives a spread of about from -4 to 4
;and with 0.4 probability of becoming close to 0
(define (marsaglia)
  (let ((x (random-real))
        (y (random-real)))
    (let ((s (+ (* x x) (* y y))))
      (if (> 1 s)
        (let ((s2 (sqrt (/ (* -2 (log s)) s))))
          (* x s2)) ;(* y s2) is unused
        (marsaglia)))))
(define (chop x a)
  (max a
       (min x
            (- a))))
(define (mars midpoint scale)
  (+ midpoint
     (* (/ scale 4)
        (marsaglia))))
(define (from0mars->int midpoint)
  (floor->exact (mars midpoint midpoint)))
(define (from0mars->absint maxi)
  (floor->exact (abs (mars 0 maxi))))
(define (mars->int midpoint scale)
  (floor->exact (mars midpoint scale)))

(define (randomk)
    (- 1 (sqrt (random-real))))

(define hexchars (string->list "0123456789ABCDEF"))
(define (byte->hex num)
  (list->string (list (list-ref hexchars (fxarithmetic-shift (fxand num #xf0) -4))
                      (list-ref hexchars (fxand num #xf)))))

(define (num->intel16 num)
  (list (fxand num #xff)
        (fxarithmetic-shift (fxand num #xff00) -8)))
(define (num->intel32 num)
  (append (num->intel16 (bitwise-and num #xffff))
          (num->intel16 (arithmetic-shift num -16))))

(define (num->motor16 num)
  (list (fxarithmetic-shift (fxand num #xff00) -8)
        (fxand num #xff)))
(define (num->motor32 num)
  (append (num->motor16 (arithmetic-shift num -16))
          (num->motor16 (bitwise-and num #xffff))))
