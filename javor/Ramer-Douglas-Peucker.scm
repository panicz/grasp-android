(use-modules (grand scheme))

(define ((perpendicular-distance [x1 y1] [x2 y2]) [x y])
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (/ (abs (+ (* dy x) (* -1 dx y) (* x2 y1) (* -1 y2 x1)))
       (sqrt (+ (* dx dx) (* dy dy))))))

(define (Ramer-Douglas-Peucker points epsilon)
  (if (is (length points) <= 2)
      points
      (let* (([first middle ... last] points)
             (furthest distance index-1 (apply argmax
                                               (perpendicular-distance first last)
                                               middle))
             (index (+ index-1 1)))
        (if (is distance > epsilon)
            (let* ((left right (split-at points (+ index 1)))
                   ([_ ... pivot] left)
                   (left* (Ramer-Douglas-Peucker left epsilon))
                   ([pivot . right*] (Ramer-Douglas-Peucker `(,pivot . ,right) epsilon)))
              `(,@left* ,@right*))
            `(,first ,last)))))

(e.g. ;; cf. https://rosettacode.org/wiki/Ramer-Douglas-Peucker_line_simplification
 (Ramer-Douglas-Peucker
  '((0 0) (1 0.1) (2 -0.1) (3 5) (4 6)
    (5 7) (6 8.1) (7    9) (8 9) (9 9))
  1.0) ===> ((0 0) (2 -0.1) (3 5) (7 9) (9 9))) 
