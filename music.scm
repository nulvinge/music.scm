(define (crnd)
  (ascii->char (+ (char->ascii #\A)
                  (random (- (char->ascii #\Z)
                             (char->ascii #\A)
                             1)))))

(define notes
  '(C Cis D Dis E F Fis G Gis A Ais H))
(define nnotes
  (length notes))

(define (nrnd)
  (random nnotes))

;makes an exponential number between 0 and (l^2)
;with the highest probability around 0
(define (random2 l)
  (let ((r (random l)))
    (* r r)))
;makes an exponential number between -(l^2) and (l^2)
;with the highest probability around 0
(define (randomn l)
  (let ((l2 (* l 2)))
    (let ((r (random l2)))
      (* (- r l)
         (/ r 2)))))
;Uses the masraglia algorithm to make a random number
;with normal distribution of my=0 and sigma^2=1
;This gives a spread of about from -4 to 4
;and with 0.4 probability of becoming close to 0
(define (marsaglia)
  (let ((x (random 1.0))
        (y (random 1.0)))
    (let ((s (+ (* x x) (* y y))))
      (if (> 1 s)
        (let ((s2 (sqrt (/ (* -2 (log s)) s))))
          (* x s2)) ;(* y s2) is unused
        (marsaglia)))))
(define (chop x a)
  (max a
       (min x
            (- a))))
(define (chopmars)
  (let ((a (marsagla)))
    (chop (car a) 4)))
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
    (- 1 (sqrt (random 1.0))))


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
(define (consrndc) (consrnd crnd))
(define (consrndn) (consrnd nrnd))


(define (combine-loop a b absa absb)
  (define nbytes 3)
  (define (addvarlen l n)
    (if (null? l)
      '()
      (let ((len (varlength l))
            (data (readvarlen l)))
        (append (num->varlen (+ data n))
                (list-tail (list-head l (+ len nbytes)) len)
                (list-tail l (+ len nbytes))))))
  (if (null? a)
    (addvarlen b (- absb absa))
    (if (null? b)
      (addvarlen a (- absa absb))
      (let ((la (readvarlen a))
            (lb (readvarlen b)))
        (if (> (+ absa la) (+ absb lb))
          (combine-loop b a absb absa)
          (let ((bytes (+ (varlength a) nbytes))
                (diff (if (> absb absa)
                        (- (+ absa la) absb)
                        la)))
            (append (num->varlen diff)
                    (list-tail (list-head a bytes) (- bytes nbytes))
                    (combine-loop b
                                  (list-tail a bytes)
                                  absb
                                  (+ absa la)))))))))
(define (combine-start a b)
  (combine-loop a b 0 0))
(define (combine . lists)
  (fold-left combine-start (car lists) (cdr lists))) 

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

(define ppqn 120)

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
  (if (> percent (random 100)) ;50%
    (rposdelta)
    (- (rposdelta))))
(define (rposdelta)
  (if (>= 8 (random 10)) ;20%
    (random 3) ;0-2
    (floor->exact (* 12 (randomk)))))

(define (rlength)
  (rlength2))
; 1/(2^(r 0-8)) * ppqn*4 ;whole note devided by random note
; ppqn*4/(2^(r 0-8))
; ppqn*4 >> (r 0-8)
(define (rlength1)
  (fix:lsh (* ppqn 4) (- (mars->int 3 2))))
(define (note->time n)
  (floor->exact (/ (* ppqn 4) n)))
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
        (if (zero? (random 1)) 2 9)
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
  (if (zero? (random 1))
    (note 0
          l
          2
          (mars->int 60 30)
          (mars->int 100 30))
    (let ((l2 (fix:lsh l -1)))
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
  (rnote (* ppqn 4)))
(define (rwholenote2)
  (define (loop absl)
    (let ((l (rlength)))
      (let ((d (- (+ absl l) (* 4 ppqn))))
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
  (define beats 1)
  (define (geninstruments count)
    (if (zero? count)
      '()
      (append (list (+ 35 (random 46)))
              (geninstruments (- count 1)))))
  (define instruments (geninstruments ninstruments))
  (define (beat)
    (note 0
          (/ ppqn beats)
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
  (loop (* n 1) (frame (* 4 beats))))



(define (chord pitch)
  (define intervals '(uni min2 maj2 min3 maj3 per4 dim5 per5 min6 maj6 min7 maj7 octave))
  (define chordlist '((minor (uni min3 per5))
                      (major (uni maj3 per5))
                      (maj7  (uni maj3 per5 maj7))
                      (dim07 (uni min3 dim5 maj6))
                      (sus4  (uni maj4 per5))))

  (define (rnotewpitch i)
    (note 0
          (* ppqn 4)
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


(define (generate-list generator n)
  (define (loop n)
    (if (zero? n)
      '()
      (append (generator)
              (loop (- n 1)))))
  (loop n))



;(define n (+ 2 (random 4)))
(define n 3)
(define (music) (combine (nframe n)
                         ;(chordedmelodyframes n)
                         ;(drumline n)
                         ))


;(write (flatten (consrnd randnote)))
(define (refrain)
  (if (> 1 (random 10))
    '()
    (let ((a (flatten (consrnd rwholenote2))))
      (append a
              a
              (refrain)))))


;The following function is called append in the standard
(define (concat a b)
  (if (null? a)
    b
    (cons (car a) (concat (cdr a) b))))

(define (bin->note bin)
  (list-ref notes bin))
(define (bins->notes bins)
  (map bin->note bins))

;(set-current-output-port! (open-binary-output-file "m.midi"))
;(map write-char (append (consrndc) (consrndc)))
;(close-all-open-files)

;the following line proves that zeroes are writeable
;(write-char (ascii->char 0))

(define (num->intel16 num)
  (list (fix:and num #xff)
        (fix:lsh (fix:and num #xff00) -8)))
(define (num->intel32 num)
  (let ((a (integer-divide num #x10000)))
    (append (num->intel16 (cdr a))
            (num->intel16 (car a)))))

(define (num->motor16 num)
  (list (fix:lsh (fix:and num #xff00) -8)
        (fix:and num #xff)))
(define (num->motor32 num)
  (let ((a (integer-divide num #x10000)))
    (append (num->motor16 (car a))
            (num->motor16 (cdr a)))))


;format: 0: single track
;        1: many tracks that starts simulatiously
;        2: many tracks that doesn't start simulatiously
;division: number of Pulses (i.e. clocks) Per Quarter Note

(define hexchars (string->list "0123456789ABCDEF"))
(define (byte->hex num)
  (list->string (list (list-ref hexchars (fix:lsh (fix:and num #xf0) -4))
                      (list-ref hexchars (fix:and num #xf)))))

(define (num->varlen num)
  (define (varlen num)
    (if (zero? num)
      '()
      (let ((a (integer-divide num #x80)))
        (append (varlen (car a))
                (list (+ #x80 (cdr a)))))))
  (if (< #xFFFFFFF num)
    'Error
    (let ((a (integer-divide num #x80)))
      (append (varlen (car a))
              (list (cdr a))))))
;(write (map byte->hex (num->varlen #xFFFFFFF)))

(define (readvarlen l)
  (if (zero? (fix:and #x80 (car l)))
    (car l)
    (+ (* #x80 (fix:and #x7f (car l)))
       (readvarlen (cdr l)))))
(define (varlength l)
  (define (loop n l)
    (if (zero? (fix:and #x80 (car l)))
      n
      (loop (+ 1 n) (cdr l))))
  (loop 1 l))

(define (flatten l)
  (if (null? l)
    '()
    (append (car l)
            (flatten (cdr l)))))

(define endoftrack (list #x00 #xFF #x2F #x0))
(define (track data)
  (makechunk "MTrk" (append data
                            endoftrack)))
(define (mthdchunk format numtracks division)
  (append (num->motor16 format)
          (num->motor16 numtracks)
          (num->motor16 division)))

(define (makechunk header data)
  (append (map char->ascii (string->list header))
          (num->motor32 (length data))
          data))
(define (midifile tracks)
  (append (makechunk "MThd"
                     (mthdchunk (if (< 1 (length tracks)) 1 0)
                                (length tracks)
                                ppqn)) ;120 pulses/quarter note
          (flatten tracks)))

(define (metaevent type text)
  (append (num->varlen 0) ;delay
          (list #xff
                type)
          (num->varlen (length text)))
          text)
(define (instrumentEvent instrument)
  (metaevent 4 instrument))
(define (channelPrefixEvent channel event)
  (append (metaevent 32 (list channel))
          event))

(define (channelevent delta type channel par1 par2)
  (append (num->varlen delta)
          (list (fix:or (fix:lsh type 4) channel)
                par1
                par2)))
(define (shortchannelevent type channel par)
  (list 0
        (fix:or (fix:lsh type 4) channel)
        par))
(define (noteon delta channel note velocity)
  (channelevent delta
                #x9
                channel
                note
                velocity))
(define (noteoff delta channel note velocity)
  (channelevent delta
                #x8
                channel
                note
                velocity))

(define (note delta length channel note velocity)
  (append (noteon  delta  channel note velocity)
          (noteoff (+ 10 length) channel note velocity)))


(define (string->bytes str)
  (map char->ascii (string->list str)))


(display (chord (rpitch)))
(define midi (midifile (list (track (append (shortchannelevent #xC 2 29)
                                            (music))))))

(display (length midi))
(set-current-output-port! (open-binary-output-file "m.mid"))
(map (lambda (c) (write-char (ascii->char c)))
     midi)
;(write (map byte->hex (midifile (list (track (flatten (consrnd randnote)))))))
;(channelprefixevent 1 (instrumentEvent (string->bytes "Electric Guitar")))
(close-all-open-files)

