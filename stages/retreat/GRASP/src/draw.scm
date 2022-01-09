(import (cell-display-properties))
(import (rename (keyword-arguments) (define/kw define*)))
(import (define-type))
(import (define-interface))
(import (for))
(import (examples))
(import (screen))
(import (infix))
(import (match))
(import (extent))
(import (string-extras))


;; Jaki mamy teraz pomysl
;; funkcja renderujaca bedzie miala dwa dodatkowe argumenty:
;; - kontekst, ktory jest odwroconym kursorem wyrazenia
;; - biezacy kursor, o ile renderowane wyrazenie lezy na sciezce
;;   tego kursora, albo #f w przeciwnym przypadku



(define (parenthesized! proc/object+screen object screen::Screen)::Extent
  (let* ((paren-width (screen:paren-width))
         (extent (with-translation screen (paren-width 0)
                   (as Extent (proc/object+screen object screen)))))
    (screen:open-paren! extent:height 0 0)
    (screen:close-paren! extent:height (+ paren-width extent:width) 0)
    (Extent width: (+ paren-width extent:width paren-width)
            height: extent:height)))

(define* (draw! object::Tile
                on: screen::Screen := (current-screen)
                ;;within: context::List := '()
                ;;at: cursor::Cursor := #f
                )
  ::Extent
  (object:draw! screen #;context #;cursor))

(define* (draw-sequence! elems
                         on: screen :: Screen := (current-screen)
                         ;;within: context::List := '()
                         ;;at: cursor::Cursor := #f
                         )
  ::Extent
  (let ((max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0))

    (define (skip-spaces! spaces::string)::void
      (for char in spaces
           (cond ((eq? char #\newline)
                  ;; powinnismy dodac wszystkie obiekty z tej linii
                  ;; do detekcji
                  (set! top (+ top max-line-height))
                  (set! left 0)
                  (set! max-line-height (screen:min-line-height)))
                 (else
                  (set! left (+ left 1))
                  (set! max-width (max max-width left))))))


    (define (advance! extent::Extent)::void
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height max-line-height))
      (set! max-width (max left max-width)))

    (define (draw-empty-list! spaces::string)::Extent
      (parenthesized! (lambda (spaces screen)
                             (string-extent spaces))
                           spaces screen))

    (define (draw-head! pair)::Extent
      (with-translation screen (left top)
        (if (null? (head pair))
            (draw-empty-list! (null-head-space pair))
            (draw! (head pair) on: screen))))

    (define (should-draw-horizontal-bar? dotted-pair)
      (and (string-index (post-head-space dotted-pair) (is _ eq? #\newline))
           (string-index (pre-tail-space dotted-pair) (is _ eq? #\newline))))

    (define (draw-dotted-tail! pair)::Extent
      (cond ((should-draw-horizontal-bar? pair)
             => (lambda (post-bar-newline-index)
                  (let ((bottom (+ top max-line-height))
                        (post-bar-space (pre-tail-space pair)))
                    (skip-spaces! (post-head-space pair))
                    (skip-spaces! (string-drop post-bar-space
                                               (+ 1 post-bar-newline-index)))
                    (advance! (with-translation screen (left top)
                                (if (null? (tail pair))
                                    (draw-empty-list! (null-tail-space pair))
                                    (draw! (tail pair) on: screen))))
                    (skip-spaces! (post-tail-space pair))
                    (with-translation screen (0 bottom)
                      (screen:draw-horizontal-bar! max-width)))))
            (else
             (skip-spaces! (post-head-space pair))
             (let ((previous-left left)
                   (previous-top top))
               (advance! (Extent width: (screen:vertical-bar-width)
                                 height: 0))
               (skip-spaces! (pre-tail-space pair))
               (advance! (with-translation screen (left top)
                           (if (null? (tail pair))
                               (draw-empty-list! (null-tail-space pair))
                               (draw! (tail pair) on: screen))))
               (skip-spaces! (post-tail-space pair))
               (with-translation screen (previous-left previous-top)
                 (screen:draw-vertical-bar! max-line-height)))))
      (skip-spaces! (post-tail-space pair))
      (Extent width: max-width
              height: (+ top max-line-height)))

    (define (draw-next! pair)
      (skip-spaces! (pre-head-space pair))
      (advance! (draw-head! pair))
      (cond ((dotted? pair)
             (draw-dotted-tail! pair))

            ((pair? (tail pair))
             (skip-spaces! (post-head-space pair))
             (draw-next! (tail pair)))

            (else
             (skip-spaces! (post-head-space pair))
             (Extent width: max-width
                     height: (+ top max-line-height)))))
    
    (draw-next! elems)
    ))



