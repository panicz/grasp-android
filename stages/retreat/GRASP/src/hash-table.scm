(define-alias make-weak-key-hash-table java.util.WeakHashMap)
(define-alias make-hash-table java.util.HashMap)

;;(define-alias Pair gnu.lists.Pair)
;;(define-alias System java.lang.System)

(define (hash-set! table::java.util.Map key value)
  (table:put key value))

(define (hash-ref table::java.util.Map key . default)
  (if (table:contains-key key)
      (table:get key)
      (if (not (null? default))
	  ((car default)))))
