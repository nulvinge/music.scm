(define notes (list->vector
  '(C Cis D Dis E F Fis G Gis A Ais H)))
(define intervals '(uni min2 maj2 min3 maj3 per4 dim5 per5 min6 maj6 min7 maj7 octave))

(define key (vector-ref notes (random-integer 12)))

(define (make-scale start-index type)
  (define types '((major .      (2 2 1 2 2 2 1))
                  (minor .      (2 1 2 2 1 2 2))
                  (whole-tone . (2 2 2 2 2 2))
                  (octatonic .  (2 1 2 1 2 1 2 1))
                  (arabic .     (1 3 1 2 1 3 1))
                  (phrygian .   (1 3 1 2 1 2 2))
                  (hungarian .  (2 1 3 1 1 3 1))
                  (chromatic .  (1 1 1 1 1 1 1 1 1 1 1 1))))
  (define (loop intervals index)
    (if (null? intervals)
      '() ;skip the last index, becouse it's the same as the first
      (cons index
            (loop (cdr intervals)
                  (+ index (car intervals))))))

  ;another way, more unsafe
  (define (loop2 index count)
    (if (= index start-index)
      index
      (cons index
            (loop (modulus (+ index (list-ref intervals count)) 12)
                  (+ count 1)))))
  ;(loop2 start-index 0))
  (let ((intervals (table-ref (list->table types) type)))
    (loop intervals (vector-find notes start-index))))
(define scale (list->vector (make-scale key 'major)))

(define (scale-ref index)
  (+ (vector-ref scale (modulo index (vector-length scale)))
     (* 12           (quotient index (vector-length scale)))))
(define (make-chords scale)
  (define (make-chord root-index)
    (list (scale-ref root-index)
          (scale-ref (+ root-index 2))
          (scale-ref (+ root-index 4))))
  (define (loop index)
    (if (= (vector-length scale) index)
      '()
      (cons (make-chord index)
            (loop (+ 1 index)))))
  (loop 0))
(define chord-list (make-chords scale))

