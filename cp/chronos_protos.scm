(load "../systemfuncs.scm")
(load "../math.scm")
(load "../midi.scm")

; 120 BPM => 480 QNPM
(define ppqn 12)
(define ppf  (* ppqn 4))
(define pphn (* ppqn 2))

(load "../scale.scm")
(load "../drums.scm")
(load "../form.scm")

(define (make-rythm from to)
    (define (lstonis l)
            (list (ni l 60)
                  (ni l -1)))
    (make-loop append
               lstonis
               (reverse (map (lambda (n) (+ n from))
                             (generate-sequence (+ from to))))))


(define midi
 (make-midi (list (make-rythm 0 10))
            '(1)
            (list '())))

(display (list "length " (length midi) " bytes")) (newline)
(let ((port (open-output-file "m.mid")))
  (map (lambda (c) (write-char (integer->char c) port))
     midi)
  (close-port port))

