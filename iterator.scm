(define (symbol-append . syms)
  (string->symbol (apply string-append (map symbol->string syms))))

(define (partial-apply f . args)
  (lambda rest-args
    (apply f (append args rest-args))))

(define-syntax define-iterator
  (er-macro-transformer
   (lambda (x r c)
     (let* ((itor-type (second x))
	    (coll-type (third x))
	    (ctor-name
	     (symbol-append 'make- itor-type '- coll-type '-iterator))
	    (ctor-type
	     (symbol-append 'make- itor-type '-iterator '-constructor))
	    (get (fourth x))
	    (inc (fifth x)))
       `(,(r 'define) ,ctor-name (,ctor-type ,get ,inc))))))

;;; Universal Iterator Functions

(define (end? itor)
  (itor 'end?))

;;; In-Place Iterator Macro & Functions

(define-syntax define-in-place-iterator
  (er-macro-transformer
   (lambda (x r c)
     (let ((coll-type (second x))
	   (get (third x))
	   (inc (fourth x)))
       `(define-iterator in-place ,coll-type ,get ,inc)))))

(define (make-in-place-iterator get inc coll init-ref end?)
  (let ((ref init-ref))
    (case-lambda
      (() (get coll ref))
      ((msg)
       (case msg
	 ('next! (set! ref (inc ref)))
	 ('reset! (set! ref init-ref))
	 ('end? (end? coll ref))
	 (else (error "In-Place Iterator Error")))))))

(define (make-in-place-iterator-constructor get inc)
  (partial-apply make-in-place-iterator get inc))

(define (next! ip-itor)
  (begin
    (ip-itor 'next!)
    ip-itor))

(define (reset! ip-itor)
  (begin
    (ip-itor 'reset!)
    ip-itor))

 
;;; Persistent Iterator Macro & Functions

(define-syntax define-persistent-iterator
  (er-macro-transformer
   (lambda (x r c)
     (let ((coll-type (second x))
	   (get (third x))
	   (inc (fourth x)))
       `(define-iterator persistent ,coll-type ,get ,inc)))))

(define (make-persistent-iterator get inc coll init-ref end?)
  (let ((ref init-ref))
    (case-lambda
      (() (get coll ref))
      ((msg)
       (case msg
	 ('next
	  (make-persistent-iterator get inc coll (inc ref) end?))
	 ('end? (end? coll ref))
	 (else (error "Persistent Iterator Error")))))))

(define (make-persistent-iterator-constructor get inc)
  (partial-apply make-persistent-iterator get inc))

(define (next p-itor)
  (p-itor 'next))


;;; Tests

(define mvi (define-persistent-iterator vector vector-ref (lambda (i) (+ i 1))))

(define v (vector 11 22 33 44))

(define vpi (make-persistent-vector-iterator v 0 (lambda (_ i) (= i 4))))

(define mli (define-in-place-iterator list (lambda (_ r) (car r)) (lambda (r) (cdr r))))

(define l (list 'a 'b 'c 'd))

(define lipi (make-in-place-list-iterator l l (lambda (_ r) (null? r))))