(load "systemfuncs.scm")
(load "math.scm")
(load "midi.scm")

(define notes
  '(C Cis D Dis E F Fis G Gis A Ais H))

(define (consRnd rnd)
  (define (lrnd count)
    (if (or (> 1 count) (< (random 10) 8))
      #f
      #t))
  (define (consRndLoop count)
    (if (lrnd count) ;25% chance
      '()
      (cons (rnd) (consRndLoop (+ 1 count)))))
  (consRndLoop 0))


(define ppqn 120)
(define ppf (* ppqn 4))

(define (rpitch)
  (rpitch3))
(define (rpitch1)
  (mars->int 60 30))
(define (rpitch2)
  (define sscale '(C D E G A))

  (define (conv-scale sscale)
    (if (null? sscale)
      '()
      (cons (index (car sscale) notes)
            (conv-scale (cdr sscale)))))
  (define scale (conv-scale sscale))
  (+ (list-ref scale (random (length scale)))
     (* 12 (+ (mars->int 4 1) 1))))
(define (rpitch-chord chord)
  (+ chord (mars->int 6 6)))

(define chord-pitch 0)
(define chord-chord 'maj)
(define (rpitch-meta)
  (if (zero? chord-pitch)
    (rpitch2)
    (rpitch-chord chord-pitch)))

(define rpitch-pitch 60)
(define (rpitch3)
  (set! rpitch-pitch
    (+ rpitch-pitch
       (rdelta-rising)))
  rpitch-pitch)


(define (rdelta-rising)
  (rdelta 85))
(define (rdelta-faling)
  (rdelta 15))
(define (rdelta percent)
  (if (> percent (random-integer 100)) ;50%
    (rposdelta)
    (- (rposdelta))))
(define (rposdelta)
  (if (>= 8 (random-integer 10)) ;20%
    (random-integer 3) ;0-2
    (floor->exact (* 12 (randomk)))))

(define (rlength)
  (rlength2))
; 1/(2^(r 0-8)) * ppqn*4 ;whole note devided by random note
; ppqn*4/(2^(r 0-8))
; ppqn*4 >> (r 0-8)
(define (rlength1)
  (fxarithmetic-shift ppf (- (mars->int 3 2))))
(define (note->time n)
  (floor->exact (/ ppf n)))
(define rlength-count 0)
(define (rlength2)
  (define rythm '(4 4   2.66 8
                  4 8 8 2.66 8
                  8 8 4 2.66 8))
  (set! rlength-count (+ 1 rlength-count))
  (note->time (list-ref rythm
                        (modulo rlength-count
                                (length rythm)))))

(define (randnote)
  (note (from0mars->absint 100) ;delay    0-100
        (rlength)               ;length
        (if (zero? (random-integer  1)) 2 9)
        (mars->int 60 30)       ;note     30-90
        (mars->int 100 30)))    ;velocity 0-256


(define (rnote l)
  (rnote3 l))
(define (rnote1 l)
  (note 0
        l
        1
        (rpitch)
        (mars->int 100 30)))
(define (rnote2 l)
  (if (zero? (random-integer 1))
    (note 0
          l
          2
          (mars->int 60 30)
          (mars->int 100 30))
    (let ((l2 (fxarithmetic-shift l -1)))
      (append (rnote2 l2)
              (rnote2 l2)))))
(define (rnote3 l)
  (note 0
        l
        1
        (rpitch)
        (mars->int 100 30)))


(define (rwholenote)
  (rwholenote2))
(define (rwholenote1)
  (rnote ppf))
(define (rwholenote2)
  (define (loop absl)
    (let ((l (rlength)))
      (let ((d (- (+ absl l) ppf)))
        (cond ((zero? d) 
               (rnote l))
              ((< d 0)
               (append (rnote l)
                       (loop (+ absl l))))
              ((> d 0)
               (loop absl))))))
  (loop 0))

(define (frame)
  (rwholenote))
(define (sixframes)
  (let ((f1 (frame))
        (f2 (frame))
        (f3 (frame))
        (f5 (frame)))
    (append f1
            f2
            f3
            f1
            f5
            f3)))
(define (nframe n)
  (nframe2 n))
(define (nframe1 n)
  (define (loop one three count)
    (if (zero? count)
      '()
      (append one
              (frame)
              three
              (loop one three (- count 1)))))
  (loop (frame) (frame) n))

(define frame-number 0)
(define (nframe2 n)
  (set! frame-number n)
  (if (zero? n)
    '()
    (append (frame)
            (nframe2 (- n 1)))))

(define (drumline n)
  (define ninstruments 4)
  (define beats 4)
  (define (geninstruments count)
    (if (zero? count)
      '()
      (append (list (+ 35 (random-integer 46)))
              (geninstruments (- count 1)))))
  (define instruments (geninstruments ninstruments))
  (define (beat)
    (note 0
          (/ ppf beats)
          9
          (list-ref instruments (from0mars->absint 4))
          (mars->int 100 30)))

  (define (frame count)
    (if (zero? count)
      '()
      (append (beat)
              (frame (- count 1)))))

  (define (loop count frame)
    (if (zero? count)
      '()
      (append frame
              (loop (- count 1) frame))))
  (loop (* n 1) (frame beats)))


(define (chord pitch)
  (define intervals '(uni min2 maj2 min3 maj3 per4 dim5 per5 min6 maj6 min7 maj7 octave))
  (define chordlist '((minor (uni min3 per5))
                      (major (uni maj3 per5))
                      (maj7  (uni maj3 per5 maj7))
                      (dim07 (uni min3 dim5 maj6))
                      (sus4  (uni maj4 per5))))

  (define (rnotewpitch i)
    (note 0
          ppf
          2
          (+ pitch i)
          (mars->int 100 30)))
  (define (chorddata->notes chord)
    (if (null? chord)
      '()
      (combine (rnotewpitch (index (car chord) intervals))
               (chorddata->notes (cdr chord)))))

  (define (findcaar a l)
    (if (null? l)
      #f
      (if (eq? (caar l) a)
        (cadar l)
        (findcaar a (cdr l)))))
  (define (chord->notes chord)
    (chorddata->notes (findcaar chord chordlist)))

  (set! chord-pitch pitch)
  (set! chord-chord 'major)
  (chord->notes 'major))
  ;(chorddata->notes '(uni per5)))
(define (chordframes n)
  (generate-list (lambda () (chord (rpitch)))
                 n))

(define (chordedmelodyframes n)
  (generate-list (lambda () 
                   (set! chord-pitch 0)
                   (combine (chord (rpitch))
                            (frame)))
                 n))

;(define n (+ 2 (random 4)))
(define n 3)
(define (music) (combine (nframe n)
                         ;(chordedmelodyframes n)
                         ;(drumline n)
                         ))

(define (refrain)
  (if (> 1 (random-integer 10))
    '()
    (let ((a (flatten (consrnd rwholenote2))))
      (append a
              a
              (refrain)))))

(define (flatten l)
  (if (null? l)
    '()
    (append (car l)
            (flatten (cdr l)))))

(define (string->bytes str)
  (map char->ascii (string->list str)))

(write (chord (rpitch))) (newline)

(define midi (midifile (list (track (append (shortchannelevent #xC 2 29)
                                            (music))))))

(display (length midi)) (newline)
(let ((port (open-output-file "m.mid")))
  (map (lambda (c) (write-char (integer->char c) port))
     midi)
  (close-port port))
