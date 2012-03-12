(use (srfi 1 69) vector-lib)

;; Redefines call-with-current-continuation to a shorter name call/cc
(define call/cc call-with-current-continuation)

;; Creates a (call/cc (lambda (...) ...)) form
(define-syntax let/cc
  (syntax-rules ()
    ((_ cont-name body ...)
     (call/cc (lambda (cont-name) body ...)))))

;; Creates an alist from the sublists provided
(define-syntax alist
  (syntax-rules ()
    ((_ (k0 ...) ...)
     (list (list k0 ...)
           ...))))

;; Pushes an object onto a list
(define-syntax push!
  (syntax-rules ()
    ((_ obj lst)
     (set! lst (cons obj lst)))))

;; Alist associating a collection predicate (ie list? vector? hash?)
;; with an internal iterator and an unpacking function, used to dynamically
;; dispatch make-iterator on a type.

(define coll-iterator-list
  (alist (list? for-each first) 
          (vector? vector-for-each second)
          ;(hash-table? (lambda (proc hash) (hash-table-walk hash proc)) (lambda x x))
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-generic-iterator : collection procedure procedure -> procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-generic-iterator takes in a collection, its internal iterator, and
;; an unpacking function and returns an external iterator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IN:
;;  coll -- A collection
;;  in-itor -- An internal iterator
;;  unpack -- An unpacking function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OUT:
;;  Returns an external iterator on the collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-generic-iterator coll in-itor unpack)
  (define (iter) (call/cc control-state))
  (define (control-state return)
    (in-itor
     (lambda current-state
       ;; Bind returning continuation to intermediate state
       (set! return
             ;; Create new intermediate state
             (let/cc resume-state
               ;; Redefine current continuation as intermediate state
               (set! control-state resume-state)
               ;; Return current state to calling continuation
               (return (unpack current-state)))))
     coll)
    ;; When continuation is exhausted return false
    (return #f))
  iter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-iterator : procedure procedure procedure -> side-effect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-iterator takes a type predicate, an internal iterator, and an unpacking
;; function and allows for run-time dispatching of make-iterator on a collection
;; that returns #t when the type predicate is applied to it. It does so by
;; pushing a list associating these procedures onto the coll-iterator-list alist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IN:
;;   type? -- type predicate
;;   in-itor -- internal iterator, must not return any values but only allow for
;;              side effects
;;   unpack -- unpacking function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OUT:
;;   Associates the type predicate with its internal iterator and unpacking
;;   functions which allows run-time dispatch of make-iterator on the collections
;;   of that type. Pushes the list containing the associated procedures onto
;;   coll-iterator-list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-iterator type? in-itor unpack)
  (push! (list type? in-itor unpack) coll-iterator-list)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-iterator : collection -> procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-iterator takes a collection, then creates the appropriate iterator for the
;; collection type by looking up the internal iterator and unpacking function
;; associated with its type predicate. If no type predicate returns true on the
;; collection, then an error is raised.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IN:
;;  coll -- A collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OUT:
;;  Returns an external iterator for the collection. If none has been defined,
;;  then it raises an error.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-iterator coll)
  (let loop ((itors coll-iterator-list))
    (if (null? itors)
        (error "No internal iterator for type")
        (let* ((head (first itors))
               (type? (first head))
               (in-itor (second head))
               (unpack (third head)))
          (if (type? coll)
              (make-generic-iterator coll in-itor unpack)
              (loop (cdr itors)))))))
