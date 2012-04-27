(module exblend mzscheme

(require "../util.ss"
         "../keyboard.ss"
         (prefix image- "../image.ss"))

(define (real->int i)
  (inexact->exact (round i)))

(define *dir*
  (let-syntax ((current-module-directory
		 (lambda (stx)
		   (datum->syntax-object
		     stx (current-load-relative-directory)))))
    (current-module-directory)))

(provide run)
(define (run)
  (easy-init 640 480 16)
  (set-color-conversion! TOTAL)
  (let ((picture-1 (image-create-from-file (path->string (build-path *dir* "images/allegro.pcx"))))
	(picture-2 (image-create-from-file (path->string (build-path *dir* "images/mysha.pcx"))))
	(counter 0))
    (game-loop
      (lambda ()
	(set! counter (add1 counter))
	(keypressed? 'ESC))
      (lambda (buffer)
	(let ((x1 (real->int (+ 160 (* 160 (sin (/ counter 32))))))
	      (y1 (real->int (- 140 (* 140 (cos (/ counter 32))))))
	      (r (real->int (- 127 (* 127 (cos (/ counter 6))))))
	      (g (real->int (- 127 (* 127 (cos (/ counter 7))))))
	      (b (real->int (- 127 (* 127 (cos (/ counter 8))))))
	      (a (real->int (- 127 (* 127 (cos (/ counter 9)))))))
	  (set-trans-blender! r g b 0)
	  (image-draw-lit buffer picture-1 x1 y1 a)
	  (image-print buffer 0 0 (image-color r g b) 0 (format "light: ~a" a)))
	(let ((x1 (real->int (+ 160 (* 160 (sin (/ counter 25))))))
	      (y1 (real->int (- 140 (* 140 (cos (/ counter 25))))))
	      (r (real->int (- 127 (* 127 (cos (/ counter 6))))))
	      (g (real->int (- 127 (* 127 (cos (/ counter 7))))))
	      (b (real->int (- 127 (* 127 (cos (/ counter 8))))))
	      (a (real->int (- 127 (* 127 (cos (/ counter 4)))))))
	  (set-trans-blender! 0 0 0 a)
	  (image-draw-translucent buffer picture-2 x1 y1)
	  (image-print buffer 0 8 (image-color a a a) 0
		       (format "alpha ~a" a))))
      (fps 15)))
  (easy-exit))

(run)
)
