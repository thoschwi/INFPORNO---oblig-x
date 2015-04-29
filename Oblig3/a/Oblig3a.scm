;; Oblig3a.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)
(load "prekode3a.scm")

;;1. PROSEDYRER FOR BEDRE PROSEDYRER

;;a - b)


(define mem
  
  (let ((trigger (list 'trigger))
        (check (list 'check)))
    (lambda (message proc)
      (case message
        ('memoize
         (let ((table (make-table)))
           (lambda x 
             (if (eq? trigger check)
                 proc
                 (let ((prev-result (lookup x table)))
                   (or prev-result
                       (let ((result (apply proc x)))
                         (insert! x result table)
                         result)))))))
        ('unmemoize
         (let ((c check))
           (set! check trigger)
           (let ((p (proc)))
             (set! check c)
             proc)))))))

;;c)

(define mem-fib (mem 'memoize fib))
(mem-fib 3)
(mem-fib 3)
(mem-fib 2)
;; I eksemplene i oppgave b), endres den globale definisjonen av fib
;; ved aa destruktivt binde lambda-uttrykket fra mem til prosedyrenavnet
;; fib. fib blir altsaa redefinert og den opprinnelige definisjonen eksisterer
;; ikke lenger.
;; I koden over, derimot, bindes returverdien av mem til en NYTT prosedyrenavn
;; "mem-fib". Det er naa viktig aa merke seg at den nye prosedyren mem-fib 
;; kaller fib, som er uendret, og at fib er rekursiv.
;; Det som gaar galt i koden over er at fib kaller seg selv (i mem-fib) 
;; med den opprinnelige, ikke-memoiserte definisjonen av seg selv. Kun
;; returverdiene til kjedene av fib-kallene er synlige for mem-fib, saa bare
;; disse blir memoisert.