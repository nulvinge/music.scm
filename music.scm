(load "systemfuncs.scm")
(load "math.scm")
(load "midi.scm")

(define ppqn 120)
(define ppf  (* ppqn 4))
(define pphn (* ppqn 2))

(load "scale.scm")
(load "drums.scm")
(load "form.scm")


(define (make-note n)
  (ni ppqn (+ 60 n)))

(define (chord-break index)
  (chord-break-random (list-ref chord-list index)))

(define (chord-break-random c)
  (cons-generated (lambda ()
                   (make-note (list-ref c (random-integer (length c)))))
                  (+ 1 (random-integer 3))))

(define (chord index)
  (make-loop combine
             make-note
             (list-ref chord-list index)))

(define (chords root len nprogs)
  (define chord-list
    (map chord->notes
         (chord-progression nprogs)))
  (define nlen (/ ppf (arithmetic-shift 1 len)))
  (define (make-chord c)
    (list (cons nlen
                (map (lambda(n) (+ n root))
                     c))))
  (write (/ (* 4 ppf) (* nlen nprogs)))
  (multiply (make-loop append
                       make-chord
                       chord-list)
            (/ (* 4 ppf) (* nlen nprogs))))

(define (melody root len)
  (define rpitch-pitch 0)
  (define percent 50)
  (define (rposdelta)
    (if (>= 8 (random-integer 10)) ;20%
      (random-integer 3) ;0-2
      (floor->exact (* (randomk) (vector-length scale)))))
  (define (rdelta)
    (if (> percent (random-integer 100)) ;50%
      (rposdelta)
      (- (rposdelta))))
  (define (rpitch)
    (set! rpitch-pitch
      (+ rpitch-pitch
         (rdelta)))
    rpitch-pitch)
  (define (rnote l)
    (list (ni l
              (+ root (scale-ref (rpitch))))))
  (define (part)
    (rlen ppf rnote len))

  (set! percent 85)
  (let ((one (part)))
    (set! percent 15)
    (append one
            (part)
            one
            (part))))

(define (patternone what)
  (pattern what '(1 2 1 3)))

(define form (make-form 6 3))
(define form '(
(1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0)
(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
(0 0 1 1 0 1 0 0 0 1 0 1 1 0 1 0 0)
(0 0 0 1 0 0 0 0 0 0 1 0 1 0 0 0 0)
(0 0 0 0 1 1 0 1 0 0 0 0 1 1 0 0 0)
(0 0 0 0 0 1 0 1 1 0 0 0 1 1 1 0 0)
    ))

(map (lambda(l) (write l) (newline))
     form)

(define (rand-octave)
    (* 12
       (mars->int 4 4)))

(define midi
(form->midi form
            3 ;longest index
            (list (multiply (patternone drumline) 2)
                  (multiply (patternone bassline) 2)
                  (chords (rand-octave) 1 8)
                  ;(melody 60 2)
                  (melody (rand-octave) 2) ;(generate-list (lambda() (melody 72 1)) 2)
                  (melody (rand-octave) 3) ;(chords 60 2 8)
                  (melody (rand-octave) 4))
            '(9 8 1 2 3 4)
            (list (volume 9 96)
                  (instrumentevent 8 (random-integer 40)) ;33)
                  (append (volume 1 64)
                          (instrumentevent 1 (random-integer 40))) ;19
                  (instrumentevent 2 (random-integer 40)) ;40)
                  (append (instrumentevent 3 (random-integer 40)) ;30
                          (balance 3 #x00))
                  (append (instrumentevent 4 (random-integer 40)) ;27)
                          (balance 4 #x7F)))))

(display (list "length " (length midi) " bytes")) (newline)
(let ((port (open-output-file "m.mid")))
  (map (lambda (c) (write-char (integer->char c) port))
     midi)
  (close-port port))
