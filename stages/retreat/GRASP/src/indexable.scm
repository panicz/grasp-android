(import (define-interface))

;; Each tile can choose whatever it pleases to be its index
;; (except #!null, for the reason explained below)
;; For built-in types (boxes, combinators, atoms) indices are
;; typically either integers or characters or symbols.
;;
;; The special value #!null means the absence of an index

(define-alias Index java.lang.Object)

(define-alias Indexable* java.lang.Object)

(define-interface Indexable ()
  (has-children?)::boolean
  
  (part-at index::Index)::Indexable*
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index
)

(define (has-children? object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'has-children?))
	
	(else
	 #f)))

(define (part-at index::Index object)::Indexable*
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'part-at index))
	
	(else
	 (error "Don't know how to extract "index" from "object))))

(define (first-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'first-index))

	((string? object)
	 0)
	
	(else
	 (error "Don't know how to obtain first index from "object))))

(define (last-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'last-index))

	((string? object)
	 (string-length (as string object)))
	 
	(else
	 (error "Don't know how to obtain last index from "object))))

(define (next-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'next-index index))

	((string? object)
	 (min (string-length object) (+ index 1)))
	
	(else
	 (error "Don't know how to obtain next index to "index
		" in "object))))

(define (previous-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'previous-index index))

	((string? object)
	 (max 0 (- index 1)))
	
	(else
	 (error "Don't know how to obtain previous index to "index
		" in "object))))
