(import (screen))
(import (draw))
;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

(define-simple-class cons (gnu.lists.Pair Tile)
  ((*init* a d) (invoke-special gnu.lists.Pair (this) '*init* a d))
  ((equals object) ::boolean (eq? object (this)))
  ((hash-code) ::int (java.lang.System:identity-hash-code (this)))
  ((draw! screen::Screen)::Extent
   (parenthesized! (lambda (object screen)
                     (draw-sequence! object on: screen))
                   (this) screen)))

;; Docelowo bedziemy musieli patchowac Kawe, chociazby po to,
;; zeby takie obiekty, jak Symbol czy FString (IString? MString?)
;; implementowaly Tile.
;;
;; Na razie jednak zrobmy tak, zeby nie uzywac tych typow
;; bezposrednio w edytorze, a zamiast tego korzystac sobie
;; z owijek

(define-simple-class Symbol (Tile)
  (name :: string)
  ((*init* source::string)
   (set! name source))
  ((toString)::String
   (name:toString))
  ((draw! screen::Screen)::Extent
   (screen:draw-atom! name)))
