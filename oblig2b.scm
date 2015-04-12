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
                   (values) ;; Aner ikke hvordan man bare gjor "ingenting"...
                   (set! stack (cdr stack))))
        ('push! (set! stack (append (reverse items) stack)))
        ('stack stack)
        (else "Invalid message!")))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))


;; b)

(define (pop! stack)
  (stack 'pop! stack))

(define (push! stack . items)
  (stack 'push! items))

(define (stack stack)
  (stack 'stack))

(pop! s1)
;;(stack s1)
(push! s1 'dune 'atreides);; En cons for mye?
;;(stack s1)

;; 3 Strukturdeling og sirkulære lister
;; a)

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
;; Se vedlagt "3a-boks-peker.png".
;; Returverdiene til list-ref kommer av at listen er blitt sirkulaer etter 
;; kallet paa set-cdr!. (list-ref bar 5) og oppover ville ikke vaert gyldige
;; indekser i den opprinnelige, asykliske listen, men her starter den "paa nytt".
;; Elementet "e" finnes ikke lenger fordi set-cdr! satte (b c d) der istedet.


;; b)

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
;; Se vedlagt "3b-boks-peker.png".
;; I det forste kallet paa set-car!, settes car og cdr til 
;; aa peke paa ett og samme OBJEKT (bevist ved eq?). Naar
;; vi etterpaa kaller set-car! paa car, blir det forste elementet
;; likt i begge "instanser" av listen paa utskriften.


;; c)

(define (cycle? items) ;; Konstant minnebruk.
  (define (race tortoise hare)
    (cond ((null? hare) #f)
          ((null? (cdr hare)) #f)
          ((eq? tortoise hare) #t)
          (else (race (cdr tortoise) (cddr hare)))))
  (race items (cdr items)))

;;(cycle? '(hey ho))
;;(cycle? bar)

;; d)

;; Lister har pr definisjon en endelig lengde og er terminert av den
;; tomme listen (null). Sykliske "lister" er aapenbart ikke terminert.
;; list? detekterer tydeligvis sykluser i tillegg til enkeltsymboler.

;; e)

;; Vi definerer en ring for aa vaere en lenket liste av tripler som alle har
;; pekere til baade det forrige og neste elementet i listen.
;; En triple er en liste med 3 elementer som har en verdi som forste element, og 
;; pekere til venstre og hoyre triple som henholdsvis 2 og 3 element. Forste
;; triple (top) sin venstre peker paa triplet som inneholder det siste elementet
;; i listen som man startet med. Tilsvarende peker det siste triplet sin hoyre
;; til top sin venstre.

;;Abstraksjonsbarriere 
;;Konstruktor
(define (make-triple value left right)
  (list value left right))
;;Selektorer
(define (value triple)
  (car triple))  
(define (left triple)
  (cdr triple))
(define (right triple)
  (cddr triple))

(define (make-ring items)
  (define top (make-triple (car items)'o 'o));;o er plassholder
  (define (o? symbol)
    (if (equal? symbol 'o)
        #t
        #f))  

  (define (build-triples items triple)
    (cond ((o? (right top)) ;;(begin set! triple...
           (build-triples (cdr items)
                                (make-triple (cadr items) top 'o)))
          ;; Her sluttes ringen...
          ((null? items) (begin
                           (set! triple (left top)) 
                           (set! top (right triple))
                           top))
          (else ;;(begin set! triple...
           (build-triples (cdr items)
                               (make-triple (car items) triple 'o)))))
  
  (let ((first (build-triples items top)))  
    (lambda ()
      first)))

;;(define (top ring)
;;  ())

(define r1 (make-ring '(1 2 3 4)))

;; f)