;; oblig3b.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)

(load "evaluator.scm")
(define (repl)
  (read-eval-print-loop))

(set! the-global-environment (setup-environment))

;;-----Kopier av kode fra evaluator.scm, med våre endringer-----
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
        (list '> >)
        (list '< <)))

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ;;3e) While:
        ((while? exp)(eval-while exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ;;3b) Alternativ if:
        ((alt-if? exp)(eval-alt-if exp env))
        ((if? exp) (eval-if exp env))
        ;;3c) let:
        ((let? exp)(mc-eval (let->lambda exp) env))
        ;;3d) Alternativ let:
        ((alt-let? exp)(mc-eval (alt-let exp) env))
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

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ;;3e) While:
        ((while? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ;;3c) let:
        ((let? exp) #t)
        ;;3d) Alternativ let:
        ((alt-let? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;3a) And og Or:
        ((and? exp) #t)
        ((or? exp) #t)
        ;;3b) Alternativ if:
        ((alt-if? exp) #t)
        (else #f)))
;;------------------------------- Kopier slutt.

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
;;a) 
;;Se kopien av primitive-procedures.

;;b)

;; Tolker oppgaven slik at install-primitive! skal kunne kjøres kun av scheme
;; og IKKE meta-REPLet (oppgaveteksten er uklar her).
(define (install-primitive! name exp)
  (let ((new-primitive (list (list name exp))))
    (set! primitive-procedures ;;Legger til exp i listen over primitiver for testformål.
          (append primitive-procedures new-primitive))
    (define-variable! name (list 'primitive exp) the-global-environment)))


;;3. NYE SPECIAL FORMS OG ALTERNATIV SYNTAKS
;;a)

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
  ;;Selektorer:
  (define (first-or exps)
    (car exps))
  (define (or-rest exps)
    (cdr exps))
  ;; Evaluer til noe blir true, eller til vi er gått tom for operander:
  (define (eval-ops exps)
    (if (null? exps)
        'false
        (let ((first (mc-eval (first-or exps) env))
              (rest (or-rest exps)))
          (if (true? first)
              first
              (eval-ops rest)))))
  (eval-ops (or-rest exp)))

;;b)

(define (alt-if? exp)(and (not (null? (cdr exp))) 
                          (tagged-list? (cddr exp) 'then)
                          (tagged-list? exp 'if)))
;;Syntaks: (if <predikat> then <utfall> elsif <predikat> then <utfall> ... else <utfall>)
;;elsif er ikke obligatorisk, alle andre ledd er obligatoriske.
;;Test-uttrykk til meta-REPLet eval-alt-if (copy-paste dem inn):
;;Test-exp 0: (if #f then 'bleh else 'feh)
;;Test-exp 1: (if #t then 'bleh elsif #t then 'meh else 'feh)
;;Test-exp 2: (if #f then 'bleh elsif #t then 'meh else 'feh)
;;Test-exp 3: (if #f then 'bleh elsif #f then 'meh else 'feh)
;;Test-exp 4: (if #f then 'bleh elsif #f then 'meh elsif #f then 'eh else 'DEAL_WITH_IT.)
;;Returverdi exp 1 - 3: Utfallet etter "#t then". 
;;Returverdi exp 0 og 4: Utfallet etter "else".

;;Selektorer og base-case predikat (disse er globale for å kunne testes):
(define (alt-if-conseq exps);; if-consequent funker ikke pga then; vi må hoppe over.
  (car (cdr (cdr (cdr exps)))))
(define (next-pred exps);; Hopp til neste predikat.
  (cdr (cdr (cdr (cdr exps)))))
(define (else? exps);; else markerer slutten av uttrykket.
  (eq? (car exps) 'else))

(define (eval-alt-if exp env)
  ;; Merk at det ikke tas høyde for at else utelates (dette fører til rekursjonsbrønn).
  (define (eval-ops exps)
    (if (else? exps)
        (mc-eval (cadr exps) env)
        (if (true? (mc-eval (if-predicate exps) env))
            (mc-eval (alt-if-conseq exps) env)
            (eval-ops (next-pred exps)))));; Predikatet var false; vi prøver videre.
  (eval-ops exp))

;;c)

(define (let? exp)(and (tagged-list? exp 'let)
                       (pair? (cadr exp))));; MERK: Vi støtter både vanlig og alternativ let.

;;Copy-paste returverdien av (let->lambda <test-exp>) inn i meta-REPLet for å verifisere.

(define let-test-exp0 '(let ((one (- 2 1))(two (+ 1 1)))(+ two one)))
;;Returverdi: 3, ekvivalent lambda: ((lambda (one two)(+ two one))(- 2 1)(+ 1 1))

(define (let->lambda exp)
  (let ((bindings (cadr exp)) ;; Listen over par av bindinger, <var><exp>...
        (body (cddr exp)));; Selve prosedyrekroppen. 
    (let ((parameters (map car bindings));; Plukker ut parameterne (<var>) fra bindingsparene...
          (expressions (map cadr bindings))) ;; ...og uttrykkene (<exp>)...
      ;; ... og klistrer det hele sammen til et lambda-uttrykk med uttrykkene som argumenter.
      (cons (make-lambda parameters body) 
            expressions))))

;;d) 

(define (alt-let? exp)(tagged-list? exp 'let))
;;Merk at let? evalueres FØR alt-let? i eval-special-form.

;;Syntaks: (let <var1> = <exp1> and <var2> = <exp2> and ... <varn> = <expn> in <body>)
;;and er ikke obligatorisk, alle andre ledd er obligatoriske.

;;Copy-paste returverdiene av (alt-let <test-exp>) inn i meta-replet for å verifisere.
(define al-test-exp0 '(let one = 1 and two = 2 and three = 3 in (+ one two three)))
;;Returverdi: 6, ekvivalent lambda: ((lambda (one two three) (+ one two three)) 1 2 3)

(define al-test-exp1 '(let one = 1 in (+ one one)))
;;Returverdi: 2, ekvivalent lambda: ((lambda (one) (+ one one)) 1)

(define al-test-exp2 '(let three = (- 4 1) and two = (+ 1 1) in (* three two)))
;;Returverdi: 6, ekvivalent lambda: ((lambda (three two) (* three two)) (- 4 1) (+ 1 1))

;;***Abstraksjonsbarriere***
;;Selektorer:
(define (variable exp)
  (cadr exp))
(define (expression exp)
  (car (cdr (cdr (cdr exp)))))
(define (next-var exp)
  (cdr (cdr (cdr (cdr exp)))))

;;Base-case for konstruktorene:
(define (body? exp)
  (eq? (car exp) 'in))

;;Konstruktorer
(define (make-parameters exp)
  (if (body? exp)
      '()
      (cons (variable exp)
            (make-parameters (next-var exp)))))
(define (make-expressions exp)
  (if (body? exp)
      '()
      (cons (expression exp)
            (make-expressions (next-var exp)))))
(define (get-body exp)
  (if (body? exp)
      (cdr exp)
      (get-body (cdr exp))))
               
(define (alt-let exp)
  ;;Samme prosedyre som vanlig let, bare at vi ikke har bindingene.
  (let ((parameters (make-parameters exp))
        (expressions (make-expressions exp))
        (body (get-body exp)))
    (cons (make-lambda parameters body) 
          expressions)))

;;e)

(define (while? exp)(tagged-list? exp 'while))
;; Syntaks: (while <pred> <body>)

(define (while-pred exp)
  (cadr exp))
(define (while-body exp)
  (cddr exp))

(define (eval-while exp env)
  (if (mc-eval (while-pred exp) env)
      (begin
        (mc-eval (while-body exp) env)
        (mc-eval exp env))))

;;Kjøreeksempler til meta-REPLet:
;;(define counter 10)
;;(while (> counter 0)(lambda () (display "\nCounter:")(display counter)(set! counter (- counter 1))))

;;Kjøreeksemplet illusterer hvordan while må brukes: man må ha en ekstern variabel som 
;;må oppdateres for hver iterasjon i body (som selvsagt må være et prosedyrekall eller 
;;et lambda-uttrykk). Med andre ord, while her fungerer slik som while fungerer i f.eks
;;Java eller C. 