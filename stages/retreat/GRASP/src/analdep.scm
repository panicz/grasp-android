(import (kawa regex))
(import (io))
(import (functions))
(import (infix))
(import (match))
(import (hash-table))
(import (mapping))
(import (print))
(import (for))

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

(define (all-scm-files-from-current-directory)
  (only (is _ matching "\\.scm$") (string-split (shell "ls") "\n")))

(define files
  (match (command-line)
    (`(,command)
     (all-scm-files-from-current-directory))
    (`(,command . ,args)
     args)))

(define (dependency-graph files)
  (let ((dependencies (mapping (module) '())))
    (for file in files
      (let* ((contents (with-input-from-file file read-all))
	     (imports (apply
		       append
		       (map (lambda (expression)
			      (match expression
				(`(import . ,modules)
				 (map (lambda (module-spec)
					(match module-spec
					  (`(rename ,module . ,_)
					   module)
					  (_
					   module-spec)))
				      modules))
				(_
				 '()))) contents)))
	     (source-module `(,(string->symbol
				(string-drop-right file 4)))))
	(set! (dependencies source-module) imports)))
    dependencies))

(define files-dependency-graph (dependency-graph files))

(for module in (keys files-dependency-graph)
  (let ((dependencies (reach files-dependency-graph module)))
    (when (is module in dependencies)
      (print "circular dependency from "module": "dependencies))))
