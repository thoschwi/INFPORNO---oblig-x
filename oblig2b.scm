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

;; b) 

;;Se vedlagt "1b-omgivelse.png"


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
  (define (iter items)
    (if (null? items)
        "Push done!"
        (begin 
          (stack 'push! (car items))
               (iter (cdr items)))))
  (iter items))

(define (stack stack)
  (stack 'stack))

(pop! s1)
;;(stack s1)

(push! s1 'dune 'atreides 'harkoonen);; En cons for mye?
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
;; list? skiller tydeligvis mellom lister, sykluser og enkeltsymboler.


;; e)

;; Vi definerer en ring for aa vaere en lenket liste av tripler som alle har
;; pekere til baade det forrige og neste elementet i listen.
;; En triple er en liste med 3 elementer som har en verdi som forste element, 
;; og pekere til venstre og hoyre triple som henholdsvis 2 og 3 element. 
;; Forste triple (top) sin venstre peker paa triplet som inneholder det siste
;; elementet i listen som man startet med. Tilsvarende peker det siste triplet
;; sin hoyre til top sin venstre.
;; Se gjerne vedlagt "3c-illus.png" for illustrasjon av strukturen.

;;***ABSTRAKSJONSBARRIERE***
;; Noen av disse burde kanskje staa innenfor make-ring. 
;;Konstruktor for triple:
(define (make-triple value left right)
  (list value left right))
;;Selektorer:
(define (value triple)
  (car triple))  
(define (left triple)
  (cadr triple))
(define (right triple)
  (caddr triple))
;;Mutatorer:
(define (top ring)
  (ring 'top))
(define (delete! ring)
  (ring 'del))
(define (insert! ring value)
  (ring 'ins value))
(define (right-rotate ring)
  (ring 'r))
(define (left-rotate ring)
  (ring 'l))
(define (set-left! triple value)
  (set-car! (cdr triple) value))
(define (set-right! triple value)
  (set-car! (cddr triple) value))

(define (make-ring items)
  ;; Top er forste triple og starter med (car items) som verdi.
  ;; L/R starter tomme fordi vi ikke har mer.
  (define top (make-triple (car items) 'o 'o))
  ;; Invariant: R av triple starter alltid tom.
  (define (build-triples items triple)
    (if (null? items) ;; Base-case
         (begin  ;; Her sluttes ringen...
           (set-left! top triple)
           (set-right! triple top)
           top)
        (begin 
           ;; Setter R av triple til en NY triple og sender DEN videre.
           ;; Merk at den nye triplen faar triple (forrige) som sin L.
           (set-right! triple (make-triple (car items) triple '()))
           (build-triples (cdr items)(right triple)))))

  (let ((first (build-triples (cdr items) top)))
    (lambda (message . arg)
      (case message
        ('top (value first))
        ('del (begin
                (set-right! (left first) (right first))
                (set-left! (right first) (left first))
                (set! first (left first))
                (value first)))
        ('ins (begin ;; Denne er litt stygg--hold tunga rett i munnen...
                (set-right! (left first) 
                            (make-triple (car arg) (left first) first))
                (set-left! first (right (left first)))
                (set! first (right (left first)))))
        ('r (begin
              (set! first (left first))
            (value first)))
        ('l (begin
              (set! first (right first))
              (value first)))
      (else "Invalid message!")))))

(define r1 (make-ring '(1 2 3 4)))
(define r2 (make-ring '(a b c d)))

;; f)

;; right-rotate og left-rotate, samt delete og insert, tar konstant tid (O(1)).
;; Det er bare snakk om aa oppdatere 1 eller 3 pekere. 