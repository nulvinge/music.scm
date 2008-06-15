(define ninstruments 4)
(define (geninstruments count)
  (if (zero? count)
    '() ;29 instead of 46 to get rid of the annoying sounds
    (append (list (+ 34 (random-integer 29)))
            (geninstruments (- count 1)))))
(define instruments (geninstruments ninstruments))
(write instruments) (newline)
(define (drumline)
  (define beats 4)
  (define (beat)
    (ni (/ ppf beats)
        9
        (list-ref instruments (from0mars->absint 4))))
  (cons-generated beat beats))

(define (bassline)
  (define beats 4)
  (define notes 2)
  (define (beat n)
    (ni (/ ppf (* notes beats))
        2
        n))
  (define (nbeat n)
    (cons-generated (lambda() (beat n))
                    notes))
  (define (rbeat)
    (nbeat (+ 36
              (vector-ref scale
                          (random-integer (vector-length scale))))))
  (generate-list rbeat
                 beats))
