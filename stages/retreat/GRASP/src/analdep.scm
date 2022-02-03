(import (kawa regex))
(import (io))
(import (functions))
(import (infix))
(import (match))
(import (hash-table))

(define (matching string pattern)
  (regex-match pattern string))

(define (read-all #!optional (port (current-input-port)))
  (let ((first-expression (read port)))
    (if (eof-object? first-expression)
	'()
	(let ((result `(,first-expression)))
	  (define (read-into tail)
	    (let ((next-expression (read port)))
	      (cond ((eof-object? next-expression)
		     result)
		    (else
		     (set-cdr! tail `(,next-expression))
		     (read-into (cdr tail))))))
	  (read-into result)))))

(define (reach #;of graph #;from node)
  (define (walk #;from novel #;into visited)
    (let* ((vicinity (fold-left union '() (map graph novel)))
	   (visited (union visited novel))
	   (novel (difference vicinity visited)))
      (if (null? novel)
	  visited
	  (walk #;from novel #;into visited))))
  (walk #;from (graph node) #;into '()))

(define (callable hash)
  (lambda (key)
    (hash-ref hash key)))

(let* ((filenames (string-split (shell "ls") "\n"))
       (scheme-files (only (is _ matching "\\.scm$")
			   filenames))
       (files+imports (map (lambda (file)
			     (let* ((contents (with-input-from-file file read-all))
				    (imports (only (lambda (top-level)
						     (and-let* ((`(import . ,modules) top-level))))
						   contents))
				    (modules (apply append
						    (map (lambda (clause)
							   (map (lambda (module)
								  (match module
								    (`(rename ,module . ,_)
								     module)
								    (_
								     module)))
								(cdr clause)))
							 imports))))
			       `((,(string->symbol (string-drop-right file 4))) ,modules)))
			   scheme-files))
       (dependencies (let ((dependencies (make-hash-table)))
		       (for-each (lambda (module+dependencies)
				   (match module+dependencies
				     (`(,module ,imports)
				      (hash-set! dependencies module imports))))
				 files+imports)
		       dependencies))
       (leaves (map (lambda (module+imports)
		      (match module+imports
			(`(,module ,imports)
			 module)))
		    files+imports)))
    
  ;; no dobra, to teraz tak: zaczynajac od lisci, sledzimy zaleznosci zaleznosci
  ;; do czasu, az osiagniemy nasycenie
  
  (for-each (lambda (leaf)
	      (let ((chain (reach (callable dependencies) leaf)))
		(when (is leaf in chain)
		  (display "circular dependency from ")
		  (display leaf)
		  (display": ")
		  (display chain)
		  (newline))))
	      leaves))
