; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions

(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
;;(load "test-dictionary.ss")

(load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

(define num-true
  (lambda (l)
    (reduce + (map (lambda (e) (if (equal? e #t) 1 0)) l) 0) ))

(define index-of
  (lambda (l i)
    (if (null? l) 
      -1
      (if (equal? i (car l))
        0
        (+ 1 (index-of (cdr l) i))))))

;; counts the number of letters in a word
(define num-letters
  (lambda (letter)
    (lambda (l)
      (if (null? l) 0
        (if (equal? (car l) letter)
          (+ 1 ((num-letters letter) (cdr l)))
          (+ 0 ((num-letters letter) (cdr l))))))))

(define to-e
  (lambda (x)
    (+ (- 26 x) 4)))

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
    (if (member w dictionary) #t #f)))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
      (map vtl
           (map (lambda (x) (modulo (+ n x) 26)) 
                (map ltv w))))))


;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (if (null? d) '() 
        (append (cons (map encoder (car d)) '()) 
                (encode-d (cdr d) encoder)))))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    (define result
      (lambda (x)
        (encode-n x)))
    (define valid-words
      (map num-true
          (list
            (map spell-checker (map (encode-n 0) p))
            (map spell-checker (map (encode-n 1) p))
            (map spell-checker (map (encode-n 2) p))
            (map spell-checker (map (encode-n 3) p))
            (map spell-checker (map (encode-n 4) p))
            (map spell-checker (map (encode-n 5) p))
            (map spell-checker (map (encode-n 6) p))
            (map spell-checker (map (encode-n 7) p))
            (map spell-checker (map (encode-n 8) p))
            (map spell-checker (map (encode-n 9) p))
            (map spell-checker (map (encode-n 10) p))
            (map spell-checker (map (encode-n 11) p))
            (map spell-checker (map (encode-n 12) p))
            (map spell-checker (map (encode-n 13) p))
            (map spell-checker (map (encode-n 14) p))
            (map spell-checker (map (encode-n 15) p))
            (map spell-checker (map (encode-n 16) p))
            (map spell-checker (map (encode-n 17) p))
            (map spell-checker (map (encode-n 18) p))
            (map spell-checker (map (encode-n 19) p))
            (map spell-checker (map (encode-n 20) p))
            (map spell-checker (map (encode-n 21) p))
            (map spell-checker (map (encode-n 22) p))
            (map spell-checker (map (encode-n 23) p))
            (map spell-checker (map (encode-n 24) p))
            (map spell-checker (map (encode-n 25) p))))) 
    (result (index-of valid-words (apply max valid-words))) ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    (define result
      (lambda (i)
        (encode-n i)
    )) 
    (define lettercounts
      (list (reduce + (map (num-letters 'a)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'b)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'c)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'd)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'e)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'f)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'g)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'h)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'i)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'j)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'k)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'l)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'm)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'n)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'o)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'p)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'q)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'r)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 's)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 't)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'u)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'v)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'w)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'x)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'y)(reduce append (list p) '())) 0)
            (reduce + (map (num-letters 'z)(reduce append (list p) '())) 0)))
    (result (to-e (index-of lettercounts (apply max lettercounts))))))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    (if (null? d) '()
      (append (cons (map decoder (car d)) '()) 
              (Code-Breaker (cdr d) decoder)) )))


;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
                    
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(define test (encode-d document add5))
;;(define decoderSP1 (Gen-Decoder-A (car test)))
;;(define decoderFA1 (Gen-Decoder-B (car test)))
;;(Code-Breaker test decoderFA1)
