(define second cadr)
(define third caddr)
(define fourth cadddr)

(define (make-iterator-constructor get inc)
  (lambda (coll init-ref end?)
    (let ((ref init-ref))
      (case-lambda
	(() (get coll ref))
	((msg)
	 (case msg
	   ('next! (set! ref (inc ref)))
	   ('end? (end? coll ref))
	   ('reset! (set! ref init-ref))
	   (else (error "Iterator error"))))))))

(define-syntax define-iterator
  (er-macro-transformer
   (lambda (x r c)
     (let (;; Name of the iterator constructor. Follows the pattern
	   ;; MAKE-<type>-ITERATOR such as MAKE-LIST-ITERATOR
	   ;; or MAKE-HASH-ITERATOR
	   (ctor-name
	    (string->symbol
	     (string-append "make-"
			    (symbol->string (second x))
			    "-iterator")))
	   ;; Accessor function
	   (get (third x))
	   ;; Returns the next logical element within the container
	   (inc (fourth x)))
       ;; Expands to:
       ;; (define (MAKE-<type>-ITERATOR COLLECTION INITIAL-REF) ...)
       `(,(r 'define) ,ctor-name (make-iterator-constructor ,get ,inc))))))

;;; "Methods"

(define (next! itor)
  (begin (itor 'next!)
	 itor))

(define (end? itor)
  (itor 'end?))

(define (reset! itor)
  (itor 'reset!))

(define (iter fn itor)
  (if (end? itor)
      (begin (reset! itor)
	     '())
      (cons (fn (itor)) (iter fn (next! itor)))))

;;; Built-in type iterator constructors

(define-iterator list (lambda (_ r) (car r)) cdr)

(define-iterator vector vector-ref (lambda (i) (+ i 1)))

;;; Test cases


(define l (list 11 22 33 44))

(define li (make-list-iterator l l (lambda (_ r) (null? r))))

(define v (vector 11 22 33 44))

(define vi (make-vector-iterator v 0 (lambda (v i) (= i (vector-length v)))))


(define (map+10 itor)
  (iter (lambda (n) (+ n 10)) itor))
