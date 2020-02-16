#lang racket

; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2019                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains simple dictionary definition
(require "dictionary.rkt")

;; -----------------------------------------------------

;; some of these functions just helped me think through the problem
;;
(define (myReverse L) 
	(if (null? L)
		null
	(append (myReverse (rest L))(list (first L)))
))
(define (equalish? a b)
	(if (and (number? a) (number? b))
		(= a b)
		(equal?/recur a b equalish?)
))

(define hashTheDict
	(lambda (hashlist dict)
		(if (null? hashlist)
			'()
			(append (map (car hashlist) dict) (hashTheDict (cdr hashlist) dict)))
))


(define (myMember? x list)
	(if (null? list) #f
		(if (= x (car list)) #t
			(myMember? x (cdr list)))
))
;; checks the list for a valid number using numeric equality. Would have used built-in member function but that uses equals?, therefore, converted to numeric equality in myMember


(define rmExtras
	(lambda (bitvector)
		(cond ((null? bitvector) '())
			((myMember? (car bitvector) (cdr bitvector)) (rmExtras (cdr bitvector)))
		(else (cons (car bitvector) (rmExtras (cdr bitvector)))))
))

(define checkBits
	(lambda (bitvector word)
		(if (null? word) #t
			(cond
				((myMember? (car word) bitvector) '#t)
				(else '#f)))
))
			
;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
	(reduce
		(lambda (c k)
			(+ (* 29 k) c))
		(map ctv w) 5413) 
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 111037761665
;;   (key '(m a y))           = 132038724
;;   (key '(t r e e f r o g)) = 2707963878412931

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (k)
	(modulo (key k) size)) 
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (k)
	(floor (* size (- (* (key k) A) (floor (* (key k) A))))))
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

;;(define hash-1 (gen-hash-division-method 70111))
;;(define hash-2 (gen-hash-division-method 89989))
;;(define hash-3 (gen-hash-multiplication-method 700426))
;;(define hash-4 (gen-hash-multiplication-method 952))

;;(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
;;(define hashfl-2 (list hash-1 hash-3))
;;(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;; (hash-1 '(h e l l o))        ==> 26303
;; (hash-1 '(m a y))            ==> 19711
;; (hash-1 '(t r e e f r o g))  ==> 3010
;;
;; (hash-2 '(h e l l o))        ==> 64598
;; (hash-2 '(m a y))            ==> 24861
;; (hash-2 '(t r e e f r o g))  ==> 23090
;;
;; (hash-3 '(h e l l o))        ==> 313800.0
;; (hash-3 '(m a y))            ==> 317136.0
;; (hash-3 '(t r e e f r o g))  ==> 525319.0
;;
;; (hash-4 '(h e l l o))        ==> 426.0
;; (hash-4 '(m a y))            ==> 431.0
;; (hash-4 '(t r e e f r o g))  ==> 714.0

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
     (let ((bitvector (rmExtras (hashTheDict hashfunctionlist dict))))
	(lambda (w)
		(checkBits bitvector (hashTheDict hashfunctionlist (list w)))))
))

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

;;(define checker-1 (gen-checker hashfl-1 dictionary))
;;(define checker-2 (gen-checker hashfl-2 dictionary))
;;(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #f
