(define endoftrack (list #x00 #xFF #x2F #x0))
(define (track data)
  (makechunk "MTrk" (append data
                            endoftrack)))
;format: 0: single track
;        1: many tracks that starts simulatiously
;        2: many tracks that doesn't start simulatiously
;division: number of Pulses (i.e. clocks) Per Quarter Note
(define (mthdchunk format numtracks division)
  (append (num->motor16 format)
          (num->motor16 numtracks)
          (num->motor16 division)))

(define (makechunk header data)
  (append (map char->integer (string->list header))
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
(define (instrumentevent channel instrument)
  (shortchannelevent #xC channel instrument))

(define (volume channel value)
  (channelevent 0
                #xB
                channel
                7
                value))
(define (balance channel value)
  (channelevent 0
                #xB
                channel
                8
                value))
(define (channelPrefixEvent channel event)
  (append (metaevent 32 (list channel))
          event))

(define (channelevent delta type channel par1 par2)
  (append (num->varlen delta)
          (list (fxior (fxarithmetic-shift-left type 4) channel)
                par1
                par2)))
(define (shortchannelevent type channel par)
  (list 0
        (fxior (fxarithmetic-shift type 4) channel)
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
          (noteoff length channel note velocity)))

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

(define (num->varlen num)
  (define (varlen num)
    (if (zero? num)
      '()
      (append (varlen (arithmetic-shift num -7))
              (list (+ #x80 (bitwise-and num #x7f))))))
  (if (< #xFFFFFFF num)
    'Error
    (append (varlen (arithmetic-shift num -7))
            (list (bitwise-and num #x7f)))))

(define (readvarlen l)
  (if (zero? (fxand #x80 (car l)))
    (car l)
    (+ (* #x80 (fxand #x7f (car l)))
       (readvarlen (cdr l)))))
(define (varlength l)
  (define (loop n l)
    (if (zero? (fxand #x80 (car l)))
      n
      (loop (+ 1 n) (cdr l))))
  (loop 1 l))

;a ni is like this:
;(length note)
;note = -1 means pause
(define (ni->midinote delta channel ni)
  (note delta
        (car ni)
        channel
        (cadr ni)
        (+ 60 (random-integer 60))))
(define (nis->midinotes nis channel)
 (define delta 0)
 (set! delta 0)
 (make-loop append
            (lambda(ni)
              (if (= (cadr ni) -1) ;pause
                (begin
                  (set! delta (+ delta (car ni)))
                  '())
                (let ((mid (ni->midinote delta channel ni)))
                  (set! delta 0)
                  mid)))
            nis))

(define (ni l n)
  (list l n))

(define (pause l)
  (ni l -1))

(define (make-midi nis-list channel-list other-list)
  (define (loop n c o)
    (if (null? n)
      '()
      (append (list (track (append (car o)
                                   (nis->midinotes (car n) (car c)))))
              (loop (cdr n)
                    (cdr c)
                    (cdr o)))))
  (midifile (loop nis-list channel-list other-list)))

(define (ni-length nis)
  (if (null? nis)
    0
    (+ (caar nis)
       (ni-length (cdr nis)))))

(define (form->midi form lines channels other)
  (define line-lengths '())
  (define (make-lines element lines len)
    (if (null? element)
      '()
      (let ((line (if (zero? (car element))
                    (list (pause (car len)))
                    (car lines))))
        (cons line
              (make-lines (cdr element) (cdr lines) (cdr len))))))
  (define (compose)
    (map (lambda (element)
           (make-lines element
                       lines
                       line-lengths))
         form))
  (define (loop l)
    (if (null? (cdr l))
      (car l)
      (map (lambda(l1 l2) (append l1 l2))
           (car l)
           (loop (cdr l)))))
  (set! line-lengths (map ni-length
                          lines))

  (write "length-list") (write line-lengths) (newline)
  (make-midi (loop (compose))
             channels
             other))
