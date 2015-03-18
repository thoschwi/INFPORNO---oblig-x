(load "huffman.scm")

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
        (else (member? symbol (cdr list)))))

;; b)

;;Ser ut til at den interne prosedyren bare er til for å opprette 
;;en lokal variabel til den grenen man traverserer, slik at man ikke ødelegger hele treet.

;; c) Iterativ losning av decode. 
;; Tar vare paa hver lovnodes liste ved aa slaa den sammen med accumulator istedenfor at
;; cons venter paa returverdien til de rekursive kallene.
(define (decode-tail bits tree)
  (define (decode-2 bits current-branch accumulator)
    (if (null? bits)
        (reverse accumulator) ;; Listen settes sammen baklengs og maa snus.
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-2 (cdr bits) tree 
                        (cons (symbol-leaf next-branch) accumulator))
              (decode-2 (cdr bits) next-branch accumulator)))))
  (decode-2 bits tree '()))


;; d)
;;(decode sample-code sample-tree)
;;(decode-tail sample-code sample-tree)
;; Begge returnerer 'ninjas fight ninjas by night'.

;; e)
(define (encode message tree)
  (define (encode-1 message branch code)
    (if (null? message)
        (reverse code)
        (cond ((leaf? branch) (encode-1 (cdr message) tree code))
              ((member? (car message)(symbols (left-branch branch))) 
               (encode-1 message (left-branch branch)(cons 0 code)))
              (else (encode-1 message (right-branch branch)(cons 1 code))))))
  (encode-1 message tree '()))            

;; f)

;; Har vi gjort f riktig?

(define (grow-huffman-tree freqs)
    (if (= (length freqs) 1)
        (car freqs)
        (grow-huffman-tree (adjoin-set (make-code-tree (car freqs)(cadr freqs)) 
                                       (cddr freqs)))))
  

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree (make-leaf-set freqs)))
;;(decode (encode '(a b c) codebook) codebook)
;;(decode (encode '(d e f) codebook) codebook)

;; g)

(define gecks '((ninjas 57) (samurais 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
(define codebook (grow-huffman-tree (make-leaf-set gecks)))

(encode '(ninjas fight) codebook)
(encode '(ninjas fight ninjas) codebook)
(encode '(ninjas fight samurais) codebook)
(encode '(samurais fight) codebook)
(encode '(samurais fight ninjas) codebook)
(encode '(ninjas fight by night) codebook)

;; h)

(define (huffman-leaves tree)
    (if(leaf? tree)
       (list(list (symbol-leaf tree)(weight-leaf tree)))
  (append (huffman-leaves(right-branch tree))
          (huffman-leaves(left-branch tree)))))

(huffman-leaves sample-tree)

;; i)