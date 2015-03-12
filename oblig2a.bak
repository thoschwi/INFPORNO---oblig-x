;;1. Diverse
;; a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car pair)
  (pair (lambda (x y) x)))

(define (p-cdr pair)
  (pair (lambda (x y) y)))

;; b)
(define foo 30)

;;"La x vaere lik foo (30) og y vaere lik 20."
;; 30 + 30 + 20 = 80.
((lambda (x y)
   (+ foo x y))
 foo 20)

;; "La foo vaere lik 10 og x vaere lik foo (fortsatt 10), og y vaere lik 20."
;; 10 + 10 + 20 = 40.
((lambda (foo)
   ((lambda (x y)
      (+ foo x y))
    foo 20))10)
;; Det andre let-uttrykket evaluerer til noe annet enn det forste fordi foo er
;; her en ny lokal variabel med sin egen verdi. Den globale foo brukes ikke.

;; c)
(define a1 (list 1 2 3 4))
(define a2 (list + - * /))
(define a3 (list 5 6 7 8))
(map (lambda (x y z) (y x z))
     a1 a2 a3)
;; Map traverserer her de tre listene paralellt. For hvert rekursive kall, blir
;; de tre car-verdiene sendt som argumenter til den anonyme prosedyren som i
;; sin tur returnerer dem. Verdien fra a2, som tilfeldigvis
;; er en operator, er den forste i rekken slik at returverdien danner et
;; gyldig prosedyrekall. Slik brukes map til aa anvende en liste av operatorer 
;; (eller andre prosedyrer) paa to lister av operander.
;; Her er et par eksempler paa hva den anonyme prosedyren faktisk gjor:
((lambda (x y z)(y x z)) 
  1 + 2) ;; 3

((lambda (x y z)(y x z)) 
  1 cons 2) ;; (1 . 2)


;; 2. Huffmankoding
;; a)
(define (member? symbol list)
  (cond ((null? list) #f)
        ((eq? symbol (car list)) #t)
        (else (cdr list))))
