;; Oblig3a.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)
(load "prekode3a.scm")

;;1. PROSEDYRER FOR BEDRE PROSEDYRER

;;a - b)


(define (mem message proc)
  
  (define (memoize f)
    (let ((table (make-table)))
      (case message
        ('memoize
         (lambda x 
           (let ((previously-computed-result
                  (lookup x table)))
             (or previously-computed-result
                 (let ((result (apply f x)))
                   (insert! x result table)
                   result)))))
        ('unmemoize
         (lambda x
           (apply proc x))))));; Her maa "proc" erstattes med original-proc
  
  (memoize proc))

;;c)

(define mem-fib (mem 'memoize fib))
(mem-fib 3)
(mem-fib 3)
(mem-fib 2)
;; Her bindes returverdien av mem til en NYTT prosedyrenavn "mem-fib". I eksemplene
;; i oppgave b), derimot, endres definisjonen av fib ved aa destruktivt
;; binde lambda-uttrykket fra memoize til prosedyrenavnet fib. Det som gaar
;; galt i koden over er at fib kaller seg selv (i memo-fib) med den ikke-memoiserte
;; definisjonen av fib, slik at ikke alle verdiene blir memoisert. Definisjonen
;; av memo-fib er naturligvis ikke synlig for fib--fib kjenner bare seg selv. 