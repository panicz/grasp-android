(import (define-type))
(import (define-interface))
(import (for))
(import (examples))
(import (screen))
(import (parse))

(define (draw-parenthesized! proc/object+screen object screen::Screen)::Extent
  (let* ((paren-width (screen:paren-width))
         (extent (with-translation screen (paren-width 0)
                   (as Extent (proc/object+screen object screen)))))
    (screen:open-paren! extent:height 0 0)
    (screen:close-paren! extent:height (+ paren-width extent:width) 0)
    (Extent width: (+ paren-width extent:width paren-width)
            height: extent:height)))

(define (draw! object #!optional (screen::Screen (current-screen)))::Extent
  (cond 
   ((pair? object)
    (draw-parenthesized! draw-sequence! object screen))
   ((symbol? object)
    (screen:draw-atom! (symbol->string object)))
   ((number? object)
    (screen:draw-atom! (number->string object)))
   ((string? object)
    (screen:draw-string! object 0 0))
   ((instance? object Tile)
    ((as Tile object):draw! screen))
   (else
    (error "Unsupported object type: " object))))

(define (draw-sequence! elems #!optional (screen::Screen (current-screen)))::Extent
  (let ((max-width 0)
        (max-line-height 1)
        (top 0)
        (left 0))

    (define (skip-spaces! spaces::string)::void
      (for char in spaces
           (cond ((eq? char #\newline)
                  (set! top (+ top max-line-height))
                  (set! left 0)
                  (set! max-line-height 1))
                 (else
                  (set! left (+ left 1))
                  (set! max-width (max max-width left))))))

    (define (advance! extent::Extent)::void
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height max-line-height))
      (set! max-width (max left max-width)))

    (define (draw-empty-list! spaces::string)::Extent
      (draw-parenthesized! (lambda (spaces screen)
                             (string-extent spaces))
                           spaces screen))

    (define (draw-head! pair)::Extent
      (with-translation screen (left top)
        (if (null? (head pair))
            (draw-empty-list! (null-head-space pair))
            (draw! (head pair) screen))))

    (define (draw-dotted-tail! pair)::Extent
      ;; trzeba narysowac "kropke", czyli
      ;; albo kreske pionowa, albo pozioma
      
      (with-translation screen (left top)
        (if (null? (tail pair))
            (draw-empty-list! (null-tail-space pair))
            (draw! (tail pair) screen)))
      (skip-spaces! (post-tail-space pair)))

    (let draw-pair! ((pair elems))
      (skip-spaces! (pre-head-space pair))
      (advance! (draw-head! pair))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (draw-dotted-tail! pair))
            ((pair? (tail pair))
             (draw-pair! (tail pair)))
            (else
             (Extent width: max-width
                     height: (+ top max-line-height)))))

    ))
