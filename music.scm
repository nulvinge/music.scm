(load "systemfuncs.scm")
(load "math.scm")
(load "midi.scm")

(define ppqn 120)
(define ppf (* ppqn 4))

(load "scale.scm")
(load "drums.scm")


(define (make-note n)
  (ni ppqn 1 (+ 60 n)))

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

(define midi (make-midi (list (cons (pause (* 0 ppf))
                                    (multiply (bassline) 12))
                              ;(cons (pause (* 8 ppf))
                              ;      (music))
                              ;(cons (pause (* 11 ppf)) 
                              ;      (music))
                               (multiply (drumline) 16))
                        (list (instrumentevent 2 33)
                              ;(append (instrumentevent 1 26)
                              ;        (balance 1 #x00))
                              ;(append (instrumentevent 1 27)
                              ;        (balance 1 #x7F))
                              (volume 9 96))))

(display (length midi)) (newline)
(let ((port (open-output-file "m.mid")))
  (map (lambda (c) (write-char (integer->char c) port))
     midi)
  (close-port port))
