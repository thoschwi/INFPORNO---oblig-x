;; Oblig3a.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)
(load "prekode3a.scm")

;;1. PROSEDYRER FOR BEDRE PROSEDYRER

;;a - b)
(define (mem message proc)
      
    (define (memoize f)
      (let ((table (make-table)))
        ;; Memoizer-koden fra SICP oppgave 3.27. Fungerer ikke med 
        ;; prosedyrer som tar inn vilkaarlig antall argumenter.
        (lambda (x) 
          (let ((previously-computed-result
                 (lookup x table)))
            (or previously-computed-result
                (let ((result (f x)))
                  (insert! x result table)
                  result))))))
  
     (let ((original-proc proc))
       (case message
         ('memoize (memoize proc))
         ('unmemoize original-proc)))) ;; Unmemoize funker ikke.
    
    