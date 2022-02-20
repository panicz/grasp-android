(define-alias make-weak-key-hash-table java.util.WeakHashMap)
(define-alias make-hash-table java.util.HashMap)

(define (hash-set! table::java.util.Map key value)
  (table:put key value))

(define (hash-ref table::java.util.Map key . default)
  (if (table:contains-key key)
      (table:get key)
      (if (not (null? default))
	  ((car default)))))

(define (hash-remove! table::java.util.Map key)
  (table:remove key))
  
;; hash-ref+ is like hash-ref, but it stores
;; the result of evaluating the "default" thunk
;; in the hash table
(define (hash-ref+ table::java.util.Map key . default)
  (if (table:contains-key key)
      (table:get key)
      (if (not (null? default))
	  (let ((value ((car default))))
	    (table:put key value)
	    value))))
