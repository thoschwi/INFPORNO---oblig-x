;;;; Diverse hjelpekode for innlevering 3a i INF2810, 2015.
;;;; erikve [at] ifi.uio.no


;; Tabell-abstraksjon fra seksjon 3.3.3 i SICP:

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (and record (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table 
		  (cons (cons key value) (cdr table))))))


;; mem-testprosedyre 1; fibonacci-tallene
(define (fib n)
  (display "computing fib of ")
  (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


;; mem-testprosedyre 2; tar virkårlig mange argumenter (null eller flere).
;; (Returnerer summen av argumentenes kvadrerte forskjell fra 42.)
(define (test-proc . args)
  (display "computing test-proc of ")
  (display args) (newline)
  (if (null? args)
      0
      (+ (expt (- 42 (car args)) 2)
	 (apply test-proc (cdr args)))))


;; Under definerer vi et grensesnitt som lar oss jobbe med strømmer på
;; samme måte som i seksjon 3.4 i SICP.

(define-syntax
  cons-stream
  (syntax-rules ()
    ((cons-stream head tail) (cons head (delay tail)))))

(define (stream-car stream) 
    (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? stream) 
  (null? stream))

;; Merk at `force' (prosedyre) og `delay' (special form) er innebygd i
;; R5RS. Konseptuelt kan vi tenke oss at de er definert som følger;
;;
;; (define (memo proc)
;;   (let ((forced? #f) 
;;         (result #f))
;;     (lambda ()
;;       (if (not forced?)
;; 	  (begin (set! result (proc))
;; 		 (set! forced? #t)))
;;       result)))
;;
;; (define-syntax
;;   delay
;;   (syntax-rules ()
;;     ((delay exp) (memo (lambda () exp)))))
;;
;; (define (force promise)
;;   (promise))


;; Under følger en del hjelpeprosedyrer til de ulike deloppgave, og
;; diverse listeoperasjoner tilpasset strømmer;


;; En prosedyre som lar oss titte på de n første elementene i en strøm:
(define (show-stream stream . n)
  (define (ss-rec stream i)
    (cond ((= i 0) (display "...\n"))
          ((stream-null? stream)  (newline))
          (else (display (stream-car stream))
                (display " ")
                (ss-rec (stream-cdr stream) (- i 1)))))
  (ss-rec stream (if (null? n) 15 (car n))))


;; brukes bl.a. i oppgave 2d:

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; brukes / nevnes i oppgave 2e:

(define (show x) 
  (display x)
  (newline)
   x)

(define (add-streams s1 s2)
  ;; forutsetter at generalisert `stream-map' er implementert:
  (stream-map + s1 s2))


;; brukes i oppgave 2 a og f:

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;; naturlige tall / natural numbers:
(define nats (integers-starting-from 1))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-interval (+ low 1) high))))
