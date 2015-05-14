;; oblig3b.scm av Benjamin A. Thomas (benjamat) og Thomas Schwitalla (thoschwi)

(load "evaluator.scm")

(set! the-global-environment (setup-environment))
(read-eval-print-loop)

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
;; Spesialformen cond gjelder når man kaller den med riktig syntaks. 
;; Det samme gjelder for prosedyrene "else" og "square". Meta-REPL'en fungerer
;; med andre ord som standard REPL'en.

