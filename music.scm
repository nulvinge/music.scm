(load "systemfuncs.scm")
(load "math.scm")
(load "midi.scm")

(define ppqn 120)
(define ppf  (* ppqn 4))
(define pphn (* ppqn 2))

(load "scale.scm")
(load "drums.scm")


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


;(define chord-pattern '(1 4 5 2))
(define chord-pattern 
  (generate-list (lambda ()
                   (list (random-integer (length chord-list))))
                 8))
;(write chord-pattern) (newline)

(define (chords)
  (make-loop append
             chord-break
             chord-pattern))
(define (music)
  (let ((progression (chords)))
    (append (chords)
            progression)))

(define (melody)
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
              (+ 60 (scale-ref (rpitch))))))
  (define (part)
    (rlen ppf rnote 3))

  (set! percent 85)
  (let ((one (part)))
    (set! percent 15)
    (append one
            (part)
            one
            (part))))

(define (patternone what)
  (pattern what '(1 2 1 3)))

(define midi
(form->midi '((0 0 1)
              (0 0 1)
              (0 1 1)
              (0 1 1)
              (1 1 1)
              (1 1 1)
              (1 0 0)
              (1 0 0)
              (1 0 1)
              (1 0 1))
            (list (multiply (patternone drumline) 2)
                  (multiply (patternone bassline) 2)
                  (melody))
            '(9 2 1)
            (list (instrumentevent 2 33)
                  (volume 9 96)
                  (append (instrumentevent 1 30)
                          (balance 1 #x10))
                  ;(append (instrumentevent 1 27)
                  ;        (balance 1 #x7F))
                  )))

(display (length midi)) (newline)
(let ((port (open-output-file "m.mid")))
  (map (lambda (c) (write-char (integer->char c) port))
     midi)
  (close-port port))
