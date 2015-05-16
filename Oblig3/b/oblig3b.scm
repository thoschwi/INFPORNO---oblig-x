;; oblig3b.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)

(load "evaluator.scm")
(define (repl)
  (read-eval-print-loop))

(set! the-global-environment (setup-environment))

;;1. BLI KJENT MED EVALUATOREN
;; a)

;;> (foo 2 square) 
;;0
;; Den LOKALE variablen "cond" innenfor foo er her 2, så uttrykket evaluerer til
;; 0 på grunn av det første caset i cond-klausulen. Det kan være det samme hva
;; man sender som 2. argument i dette tilfellet.

;;> (foo 4 square)
;;16
;; Her er 1. argument (formelt "cond") ikke 2 men 4, så foo kaller isteden 2. argument 
;; (formelt "else", men aktuelt prosedyren "square") med 4 som argument: 
;; (square 4) = (* 4 4) = 16. 

;;>(cond ((= cond 2) 0)
;;  (else (else 4)))
;;2
;; Her gjelder den GLOBALE "cond", som er 3 og ikke 2, så den kaller den GLOBALE 
;; "else" med 4 som argument:
;; (else 4) = (/ 4 2) = 2

;; For å oppsummere: evaluatoren tolker de ulike variablene til det de er bundet
;; til innenfor hvert enkelt miljø, der det inneværende miljøets bindinger gjelder først.
;; I det globale miljøet har man to cond'er: spesialformen og variablen vi definerte 
;; til å være 3. I foo sitt miljø har man også en "cond", men denne er en lokal variabel. 
;; Spesialformen cond gjelder når man kaller den som en prosedyre (den andre cond er
;; selvevaluerende og vil tolkes deretter). 
;; Det samme gjelder for prosedyrene "else" og "square". Meta-REPLet fungerer
;; med andre ord som standard REPLet.


;;2. PRIMITIVER/INNEBYGDE PROSEDYRER
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
;;     Under her har vi lagt til flere primitiver.
;;      2a) 
        (list '1+ 
              (lambda (x) (+ x 1)))
        (list '1- 
              (lambda (x) (- x 1)))
        ))

;;b)

;; Tolker oppgaven slik at install-primitive! skal kunne kjøres kun fra scheme-REPLet
;; og IKKE meta-REPLet (oppgaveteksten er uklar her).

(define (install-primitive! name exp)
  (let ((new-primitive (list (list name exp))))
    (set! primitive-procedures ;;Legger til exp i listen over primitiver for testformål.
          (append primitive-procedures new-primitive))
    (define-variable! name (list 'primitive exp) the-global-environment)))


;;3. NYE SPECIAL FORMS OG ALTERNATIV SYNTAKS
;;a)

;;Kopi
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ;;3a) And og Or:
        ((and? exp)(eval-and exp env))
        ((or? exp)(eval-or exp env))))

;;Kopi
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;3a) And og Or:
        ((and? exp) #t)
        ((or? exp) #t)
        (else #f)))

(define (and? exp)(tagged-list? exp 'and))

(define (eval-and exp env)
  ;; Egne selektorer for lesbarhetens skyld:
  (define (and-first exps)
    (car exps))
  (define (and-rest exps)
    (cdr exps))  
  ;; Evaluerer ett og ett uttrykk helt til noe blir false, eller til vi er gått 
  ;; tom for operander:
  (define (eval-ops exps)
    (if (null? exps) 
        'true
        (let ((first (mc-eval (and-first exps) env))
              (rest (and-rest exps)))
          (if (false? first)
              'false
              (if (no-operands? rest)
                  first
                  (eval-ops rest))))))
  (eval-ops (and-rest exp)))
  

(define (or? exp)(tagged-list? exp 'or))

(define (eval-or exp env)
  (define (first-or exps)
    (car exps))
  (define (or-rest exps)
    (cdr exps))
  
  (define (eval-ops exps)
    (if (null? exps)
        'false
        (let ((first (mc-eval (first-or exps) env))
              (rest (or-rest exps)))
          (if (true? first)
              first
              (eval-ops rest)))))
  (eval-ops (or-rest exp)))

