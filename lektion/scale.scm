(define notes (list->vector
  '(C Cis D Dis E F Fis G Gis A Ais H)))

(define key (random-integer 12))

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

  (let ((intervals (table-ref (list->table types) type)))
    (loop intervals start-index)))
(define scale (list->vector (make-scale key 'major)))

(define (scale-ref index)
  (+ (vector-ref scale (modulo index (vector-length scale)))
     (* 12           (quotient index (vector-length scale)))))

(define intervals '(uni min2 maj2 min3 maj3 per4 dim5 per5 min6 maj6 min7 maj7 octave))

(define (chord->notes chord)
  ;A little dictionary:
  ;     min     minor
  ;     maj     major
  ;     s       slash, lower the selected note
  ;     dim     diminished
  ;     aug     augmented
  ;     x7      7x
  ;     b       minor or diminished
  ;     sus     suspended
  (define chordlist '((min      (uni min3 per5))
                      (maj      (uni maj3 per5))
                      (majs1    ((slash uni)  uni maj3 per5))
                      (majs2    ((slash maj2) uni maj3 per5))
                      (majs3    ((slash maj3) uni maj3 per5))
                      (majs5    ((slash per5) uni maj3 per5))
                      (maj6     (uni maj3 per5 maj6))
                      (min6     (uni min3 per5 maj6))
                      (maj7     (uni maj3 per5 maj7))
                      (dom7     (uni maj3 per5 min7))
                      (min7     (uni min3 per5 min7))
                      (min7b5   (uni min3 dim5 min7)) ;I think...
                      (min7b5sb3((slash min3) uni min3 dim5 min7)) ;I think...
                      (dim07    (uni min3 dim5 maj6))
                      (dim7     (uni min3 dim5 maj6)) ;I think, and wierd
                      (maj9     (uni maj3 per5 maj7 maj2)) ;wraparound
                      (dom9     (uni maj3 per5 min7 maj2))
                      (dim      (uni min3 dim5 maj6))
                      (dimsb3   ((slash min3) uni min3 dim5 maj6))
                      (aug      (uni min3 min6))
                      (maj2     (uni maj2 maj3 per5)) ;I think
                      (sus2     (uni maj2 per5))
                      (sus27    (uni maj2 per5 min7))
                      (sus4     (uni per4 per5))
                      (sus47    (uni maj4 per5 min7))
                      (dom5     (uni per5))
                      (dim5     (uni maj3 dim5))
                      (dim57    (uni maj3 dim5 min7))
                      (mindim57 (uni min3 dim5 min7))
                      (aug57    (uni maj3 min6 min7))
                      (min97    (uni maj3 per5 min7 min2))
                      (aug97    (uni maj3 per5 min7 min3))
                      (min9aug57(uni maj3 min6 min7 min3))
                      (dom7s6   (uni maj3 per5 maj6 min7))
                      (dom9dim5 (uni maj3 dim5 min7 maj2))
                      (dom9aug5 (uni maj3 min6 min7 maj2))
                      (maj9     (uni maj3 per5 maj7 maj2))
                      (min9     (uni min3 per5 min7 maj2))
                      (maj9s6   (uni maj3 per5 maj6 maj2))
                      (min9s6   (uni min3 per5 maj6 maj2))
                      (domb9    (uni maj3 per5 min7 min2)) ;I think
                      (add9     (uni maj3 per5 maj2))
                      (dom11    (uni maj3 per5 min7 maj2 per4))
                      (min11    (uni min3 per5 min7 per4))
                      (aug11    (uni maj3 per5 min7 maj2 dim5))
                      (dom13    (uni maj3 per5 min7 maj2 maj6))
                      (dom13min9(uni maj3 per5 min7 min2 maj6))
                      (dom13b9b5(uni maj3 dim5 min7 min2 maj6))
                      ))

  (define (findcaar a l)
    (if (null? l)
      (begin (display (list "Couldn't find chord: " a))
             #f)
      (if (eq? (caar l) a)
        (cadar l)
        (findcaar a (cdr l)))))

  (define (interval->note i)
    (if (not (list? i))
      (index i intervals)
      (if (eq? (car i) 'slash)
        (- (index (cadr i) intervals) 12)
        'undef)))

  (define (note->chord interval-list note)
    (map (lambda(i) (+ note (interval->note i)))
         interval-list))

  (note->chord (findcaar (cdr chord) chordlist)
               (+ (interval->note (car chord)) key)))

(define (random-chord-inversion chord)
  (map (lambda(c)
         (if (zero? (random-integer 2))
           c
           (+ 12 c)))
       (chord)))

(define (chord-progression len)
  (define chord-base-map
    ;chord in key
       ;interval  chord types                        progression possibilities
    '(((uni     maj maj2 maj6 maj7 maj9 sus4)        all)
      ((per5    maj dom7 dom9 dom11 dom13 sus4)      (maj3 . min) (maj6 . min)   (uni  . majs3))
      ((maj2    min min7 min9)          (per5 . maj) (maj3 . min) (uni  . majs3) (uni  . majs5)
                                        (per4 . min7)(min2 . dom7))
      ((per4    maj maj6 maj7 min min6) (maj2 . min) (per5 . maj) (uni  . majs3) (uni  . majs5))
      ((maj6    min min7 min9)          (per4 . maj) (maj2 . min))
      ((maj3    min min7)               (maj6 . min) (per4 . maj))
      ((uni     majs3)                  (per4 . maj) (maj2 . min))
      ((uni     majs5)                  (per5 . maj))
      ((per4    majs1)                  (uni  . maj))
      ((per5    majs1)                  (uni  . maj))))

  (define chord-extension-map
    ;chords outside key
    '(((per4    min7)                   (uni  . maj))
      ((min2    dom7)                   (uni  . maj))

      ((min7    maj dom9)               (uni  . maj))
      ((min6    maj)                    (min7 . maj))

      ((min6    dom7)                   (uni  . majs5))
      ((min7    dom9)                   (uni  . majs5))

      ((maj2    maj dom7 dom9 domb9)    (per5 . maj))
      ((uni     min6)                   (maj2 . maj) (per5 . majs1))
      ((per5    majs2)                  (maj2 . maj))

      ((min2    dim7)                   (maj2 . min))
      ((uni     dimsb3)                 (maj2 . min))
      ((maj6    maj dom7 dom9 domb9)    (maj2 . min))
      ((maj3    min7b5)                 (maj6 . maj) (per4 . maj))

      ((uni     dom7 dom9 domb9)        (per4 . maj))
      ((per5    min)                    (uni  . dom7))

      ((min6    dim7)                   (maj6 . min))
      ((maj3    maj dom7 dom9 domb9)    (maj6 . min))
      ((maj7    min7b5)                 (maj3 . maj))

      ((maj3    dim7)                   (maj3 . min))
      ((maj7    maj dom7 dom9 domb9)    (maj3 . min))
      ((dim5    min7b5)                 (maj7 . maj) (uni  . majs5) (per5 . maj))

      ((maj6    min7b5sb3)              (maj2 . maj))))

  (define (find pred l)
    (if (null? l)
      #f
      (if (pred (car l))
        (car l)
        (find pred (cdr l)))))
  (define (find-chord object)
    (define (pred i)
      (and (eq? (caar i)  (car object))
           (eq? (cadar i) (cdr object))))
    (let ((i (find pred chord-base-map)))
      (if i
        i
        (let ((i (find pred chord-extension-map)))
          (if i
            i
            (write (list 'Couldnt 'find: object)))))))

  (define (next-chord chord)
    (if (not (eq? (car chord) 'all))
      (select-random chord)
      (let ((chord (select-random (if (zero? (random-integer 2))
                                    chord-extension-map
                                    chord-base-map))))
        (cons (caar chord)
              (cadar chord)))))

  (define (walk to len)
    (if (zero? len)
      '()
      (let ((chord (find-chord to)))
        (cons (cons (caar chord)
                    (select-random (cdar chord)))
              (walk (next-chord (cdr chord))
                    (- len 1))))))
  (walk '(uni . maj) len))
(write (map chord->notes
            (chord-progression 4)))
(newline)
