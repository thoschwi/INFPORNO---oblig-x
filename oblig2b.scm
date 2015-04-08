;; Oblig2B.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)

;; 1. Innkapsling, lokal tilstand og omgivelsesmodellen
;; a) 

(define count 42)

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (begin
        (set! count (+ count 1))
        count))))

(define c1 (make-counter))
(define c2 (make-counter))

;; b) Se vedlagt "1b-omgivelse.png"


;; 2 Innkapsling, lokal tilstand og message passing.
;; a)

(define (make-stack init)
  (let ((stack init))
               
    (lambda (message . items) ;; Items er en argumentliste av variabel lengde.
      (case message
        ('pop! (if (null? stack)
                   '() ;; Aner ikke hvordan man bare gjor "ingenting"...
                   (set! stack (cdr stack))))
        ('push! (set! stack (append (reverse items) stack)))
        ('stack stack)
        (else "Invalid message!")))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

;; b) 

;; 3 Strukturdeling og sirkulære lister
;; a)

;; Se vedlagt "3a-boks-peker.png".
;; Returverdiene til list-ref kommer av at listen er sirkulaer. (list-ref bar 5) 
;; og oppover ville ikke vaert gyldige indekser i den opprinnelige, asykliske 
;; listen, men her starter den "paa nytt". Elementet "e" finnes ikke lenger fordi
;; set-cdr! destruktivt satte (b c d)(som naa ikke er terminert, derav det 
;; sirkulaere resultatet) der istedet.