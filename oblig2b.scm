;; Oblig2B.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)

;; 1. Innkapsling, lokal tilstand og omgivelsesmodellen
;; a) 

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (begin
        (set! count (+ count 1))
        count))))

(define count 42)

(define c1 (make-counter))

(define c2 (make-counter))

;; b) Se vedlagt "1b-omgivelse.png"



