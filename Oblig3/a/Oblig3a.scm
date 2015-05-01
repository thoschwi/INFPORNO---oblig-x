;; Oblig3a.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)
(load "prekode3a.scm")

;;1. PROSEDYRER FOR BEDRE PROSEDYRER

;;a - b)

;; Denne losningen takler dessverre ikke memoization av
;; variadiske prosedyrer som godtar 0 argumenter (der 0'te arg
;; faktisk har betydning), slik som test-proc. 
(define (mem message proc)
  
  (case message
    ('memoize
     (let ((table (make-table)))
       (lambda x 
         (if (null? x)
             proc         
             (let ((prev-result (lookup x table)))
               (or prev-result
                   (let ((result (apply proc x)))
                     (insert! x result table)
                     result)))))))
     ('unmemoize    
      (proc))))

;; Vi fant en losning paa http://stackoverflow.com/a/16318121 som vi forsokte
;; aa re-engineere, men klarte ikke aa forstaa hvordan den fungerer. Vi 
;; vurderte opprinnelig aa levere en lignende losning, men fikk moralske kvaler.
;; Vennligst vaer obs paa at noen kan ha levert losningen fra stackoverflow,
;; uten aa ha faktisk forstaatt den (vi vet at mange har slitt med 
;; denne oppgaven).


;;c)

(define mem-fib (mem 'memoize fib))
(mem-fib 3)
(mem-fib 3)
(mem-fib 2)
;; I eksemplene i oppgave b), endres den globale definisjonen av fib
;; ved aa destruktivt binde lambda-uttrykket fra mem til prosedyrenavnet
;; fib. fib blir altsaa redefinert og den opprinnelige definisjonen eksisterer
;; ikke lenger i det globale miljøet.
;; I koden over, derimot, bindes returverdien av mem til en NYTT prosedyrenavn
;; "mem-fib". Det er naa viktig aa merke seg at den nye prosedyren mem-fib 
;; kaller fib, som er uendret, og at fib er rekursiv.
;; Det som gaar galt i koden over er at fib kaller seg selv (i mem-fib) 
;; med den opprinnelige, ikke-memoiserte definisjonen av seg selv. Kun
;; returverdiene til kjedene av fib-kallene er synlige for mem-fib, saa bare
;; disse blir memoisert.


;;d)

;; Hjelpeprosedyren til greet. Tar inn en liste og lagrer elementene i en
;; tabell, to og to argumenter knyttet sammen. Det forventes at argumentene
;; folger formatet '<key> '<value>... (eksempelvis 'time "morning").
;; Den ferdige tabellen returneres naar man har enten naadd bunnen
;; av listen, eller listen har kun ett element igjen (hvis listen har et
;; odde antall elementer). 
(define (named-args args)
  
  (define table (make-table))
  
  (define (iter args table)
    (if (or (null? args)(null? (cdr args)))
        table
        (begin 
          (insert! (car args)(cadr args) table)
          (iter (cddr args) table))))
  
  (iter args table))

(define (greet . args)
  
  (define argtable (named-args args))
  (define time "day")
  (define title "friend")
  
  (if (lookup 'time argtable)
      (set! time (lookup 'time argtable)))
  
  (if (lookup 'title argtable)
      (set! title (lookup 'title argtable)))
  
  (display "Top of the ")
  (display time)(display " ")
  (display title)
  (newline))

(display "\nTesting greet...\n")
(greet)
(greet 'time "evening")
(greet 'time "evening" 'title "lord")
(greet 'title "guv'nor" 'time "morning") 

;; 2 STROMMER
;;a)

(define (list-to-stream l)
    (if (null? l)
        the-empty-stream
        (cons-stream (car l)
                     (list-to-stream (cdr l)))))

(define (stream-to-list s . n)
  (define (rec s i)
    (cond ((stream-null? s) '())
          ((= i 0) '())
          (else 
           (cons (stream-car s)
                 (rec (stream-cdr s)(- i 1))))))
  
  (rec s (if (null? n) 20 (car n))))

(display "\n\t** 1d) **\nTesting stream-to-list...\n")
(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)

;;b) 

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;;c)

;; Vi implementerer forslaget til Petter Smart...
(define (stream-memq item x)
  (cond ((stream-null? x) #f)
        ((eq? item (stream-car x)) x)
        (else (stream-memq item (stream-cdr x)))))

(define (stream-remove-duplicates lst)
  (cond ((stream-null? lst) the-empty-stream)
        ((not (stream-memq (stream-car lst)(stream-cdr lst)))
         (cons-stream (stream-car lst) 
                      (stream-remove-duplicates (stream-cdr lst))))
        (else (stream-remove-duplicates (stream-cdr lst)))))
;;...og tester det:
(display "\n** 2c) **\nTesting stream-remove-duplicates...\n")
(define test-stream (list-to-stream '(1 1 2 2 3 3 4 4)))
(define no-duplicates (stream-remove-duplicates test-stream))
(show-stream no-duplicates) 
;; Testen viser at losningen fungerer med en endelig strom, men hva 
;; skjer hvis... 
;; (stream-remove-duplicates nats)
;; Kallet over er kommentert ut fordi det resulterer i rekursjonsbronn.
;; Losningen over benytter seg nemlig ikke av utsatt evaluering,
;; og fungerer derfor ikke med en uendelig strom. Den prover aa sjekke alt som 
;; finnes videre i strommen opp i mot hva som allerede er blitt sett, noe som
;; er umulig fordi strommen aldri stopper. I losningen paa d) ser man hvordan
;; det skal gjores...

;;d)

(define (in-stream? stream)
  (lambda (x)
    (not (eq? x (stream-car stream))))) 

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream) 
                   (remove-duplicates 
                    (stream-filter (in-stream? stream)
                                   (stream-cdr stream))))))

(display "\n\t2d)\nTesting...\n")
(define nats-no-duplicates (remove-duplicates nats))
(set! no-duplicates (remove-duplicates test-stream))
(show-stream nats-no-duplicates 5)
(show-stream nats-no-duplicates)
(show-stream no-duplicates)

;;e)

(define (show x)
(display x)
(newline)
x)

(display "\n\t** 2e **\n")
(define x
  (stream-map show
              (stream-interval 0 10)))

(stream-ref x 5); 1 2 3 4 5 5

(stream-ref x 7); 6 7 7

;; Man ser at uttrykket i x ikke evalueres paa nytt for hver av verdiene som 
;; allerede er blitt evaluert av det forste kallet paa stream-ref. Dette er
;; fordi cons-stream (via delay) memoiserer resultater av tidligere evaluerte 
;; uttrykk (her skjer dette i stream-map).

;;f)

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(display "\n\t** 2d) **\nTesting mul-stream...\n")
(define multiplied (mul-streams test-stream test-stream));; (1 2 3 4)*(1 2 3 4)
(show-stream multiplied)
(define factorials (cons-stream 1 (mul-streams nats factorials)))


