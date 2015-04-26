;; Oblig3a.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)
(load "prekode3a.scm")

;;1. PROSEDYRER FOR BEDRE PROSEDYRER

;;a - b)
;;(define (mem message proc);; Den gamle.
  
;;  (define (memoize f)
  ;;  (let ((table (make-table)))
    ;;  (lambda x
      ;;  (let ((previously-computed-result
        ;;       (lookup x table)))
          ;;(or previously-computed-result
            ;;  (let ((result (apply f x)))
              ;;  (insert! x result table)
                ;;result))))))
  
  ;;(let ((original-proc proc))
    ;;  (case message
      ;;  ('memoize (memoize proc))
        ;;('unmemoize original-proc))))) ;; Unmemoize funker ikke.

(define (mem message proc)
  
  (define (memoize f)
    (let ((table (make-table))
          (original-proc proc))
          (lambda x
            (case message 
              ('memoize 
               (let ((previously-computed-result
                      (lookup x table)))
                 (or previously-computed-result
                     (let ((result (apply f x)))
                       (insert! x result table)
                       result))))
               ('umemoize
                original-proc)))))
            
            (memoize proc))