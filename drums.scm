(define ninstruments 4)
(define (geninstruments count)
  (if (zero? count)
    '() ;29 instead of 46 to get rid of the annoying sounds
    (append (list (+ 34 (random-integer 29)))
            (geninstruments (- count 1)))))
(define instruments (geninstruments ninstruments))
(write "drum instruments")(write instruments) (newline)
(define (drumline)
  (define beats 4)
  (define (beat)
    (ni (/ pphn beats)
        (list-ref instruments (from0mars->absint 4))))
  (cons-generated beat beats))

(define (edist middle)
  (if (zero? (random-integer 1))
    middle
    (if (zero? (random-integer 2))
      (+ middle 1)
      (- middle 1))))

(define (rlen len note middle)
  (define (loop lleft)
    (let ((l (/ ppf (arithmetic-shift 1 (edist middle)))))
      (if (> l lleft)
        (note lleft)
        (append (note l)
                (loop (- lleft l))))))
  (loop len))

(define (bassline)
  (define beats 4)
  (define notes 2)
  (define (rbeat l)
    (ni l
        (+ 36 (vector-ref scale (random-integer (vector-length scale))))))
  (define (nbeat l)
    (multiply (list (rbeat (/ l (* beats notes))))
              notes))
  (define (abeat)
    (nbeat pphn))
  ;(rlen pphn
  ;      nbeat
  ;      5))
  (generate-list abeat
                 beats))


(define (pattern generator pattern)
  (let ((l (cons-generated generator
                          (find-max pattern))))
    (make-loop append
               (lambda(p) (list-ref l (- p 1)))
               pattern)))

(pattern bassline
         '(1 2 1 3))
