(module simple mzscheme

(require "../game.ss")
(require (prefix image- "../image.ss"))
(require (lib "etc.ss"))
;; (require (lib "errortrace.ss" "errortrace"))

(define WIDTH 1000)
(define HEIGHT 1000)
(define BORDER 5)

;; my-buffer is a very large bitmap which all the objects actually draw on
;; the buffer that is passed to the (draw) method is ignored by all objects
;; except the 'show' and 'clear' object which copy the current visible screen
;; from my-buffer onto the passed in buffer.
(define my-buffer #f)

(define-object bullet (x y) (dx dy shape)

  (constant radius 2)

  (define (create)
    (set! shape (make Circle (radius radius))))

  (define (can-collide obj)
    (if (is-a? obj bullet)
      #f
      #t))

  (define (touch world obj)
    (say world remove me))

  (define (shapes)
    (list shape))

  (define (draw world buffer)
    (image-circle-fill my-buffer (round* x) (round* y)
		       radius (image-color 0 255 0)))

  (define (tick world)
    (set! x (+ x dx))
    (set! y (+ y dy))
    (when (or (< x 0)
	      (> x WIDTH)
	      (< y 0)
	      (> y HEIGHT))
      (say world remove me)))
  )

;; (define player-size 6)
(define-object player (x y) (dx dy ang left-click score counter)

  (constant player-size 6)

  (define (create)
    (set! ang 0)
    (set! left-click 0)
    (set! counter 0)
    (set! score 0)
    (set! dx 0)
    (set! dy 0))

  (define (set-coordinates mx my)
    (set! x mx)
    (set! y my))

  (define (get-score)
    score)

  (define (can-collide obj)
    (not (is-a? obj bullet)))

  (define (kill)
    (set! score (- score 50))
    (set-coordinates (/ WIDTH 2) (/ HEIGHT 2)))

  (define (touch world obj)
    (cond
      ((is-a? obj enemy) (kill))
      ((is-a? obj crystal) (set! score (+ score 25)))))

  (define (shapes)
    (list (make Circle (radius player-size))))

  (define (key world keys)
    (for-each (lambda (k)
		(case k
		  ((F12) (image-save-screen "screenshot.png"))))
	      keys))

  (define (tick world)
    (when (or (< x BORDER)
	      (> x (- WIDTH BORDER))
	      (< y BORDER)
	      (> y (- HEIGHT BORDER)))
      (kill))
    (call-with-values
      (lambda () (get-mouse-movement))
      (lambda (mx my)
	(set! dx (+ (/ mx 6.0) dx))
	(set! dy (+ (/ my 6.0) dy))))
    (set! x (+ x dx))
    (set! y (+ y dy))
    (set! counter (add1 counter))
    (when (> counter 20)
      (set! counter 0)
      (when (> score 0)
	(set! score (sub1 score))))
    (set! ang (modulo (+ ang 3) 360))
    (cond
      ((and (left-clicking?) (not left-click))
       (begin
	 (set! left-click #t)
	 (add-object world (make bullet (x x) (y y)
				 (dx (* 1.2 dx)) (dy (* 1.2 dy))))))
      ((and (not (left-clicking?)) left-click)
       (set! left-click #f)))
    )

  (define (draw world buffer)
    (define (segment angle)
      (let ((x1 (round* x))
	    (y1 (round* y))
	    (x2 (round* (+ x (* (Cosine angle) player-size))))
	    (y2 (round* (+ y (* (Sine angle) player-size)))))
      (image-line my-buffer x1 y1 x2 y2 (image-color 255 0 0))))
    (image-circle my-buffer (round* x) (round* y)
		  player-size (image-color 255 255 255))
    (for-each (lambda (n) (segment (+ ang n)))
	      (list 0 90 180 270)))
)

(define-object crystal (x y) (ang)

  (constant size 8)

  (define (create)
    (set! ang (random 360)))

  (define (shapes)
    (list (make Circle (radius size))))

  (define (can-collide obj)
    (is-a? obj player))

  (define (touch world obj)
    (say world remove me))

  (define (tick world)
    (set! ang (modulo (+ ang 4) 360)))

  (define (draw world buffer)
    (let ((draw-side (lambda (xang)
		       (let ((x2 (round* (+ x (* size (Cosine (+ xang ang))))))
			     (color (round* (- 158 (* 50 (Cosine (+ 90 xang ang)))))))
			 (image-line my-buffer x (+ y size) x2 y 
				     (image-color color color color))
			 (image-line my-buffer x (- y size) x2 y 
				     (image-color color color color))))))
      (for-each (lambda (x) (draw-side x)) (list 0 90 180 270))))

  )

(define-object star (x y phase) (color)

   (define (create)
     (set! phase -1)
     (set! color (random 255)))

   (define (can-collide obj) #f)

   (define (draw world buffer)
     (image-circle-fill my-buffer x y 1 (image-color color color color)))

   )

(define-object enemy (x y) (ang speed)

  (define (create)
    (set! speed (/ (+ 1 (random 10)) 3))
    (set! ang (random 360)))

  (define (can-collide obj)
    (or (is-a? obj bullet)
	(is-a? obj player)))

  (define (shapes)
    (list (make Circle (radius 5))))

  (define (draw world buffer)
    (image-circle-fill my-buffer (round* x) (round* y)
		       5 (image-color 255 32 0)))

  (define (touch world obj)
    (say world remove me))

  (define (tick world)
    (let ((dx (* (Cosine ang) speed))
	  (dy (* (Sine ang) speed)))
      (set! x (+ x dx))
      (set! y (+ y dy))
      (set! ang (modulo (+ ang (- (random 5) 2)) 360))
      (when (eq? (random 30) 0)
	(set! ang (random 360)))
      (when (or (< x 0)
		(> x WIDTH)
		(< y 0)
		(> y HEIGHT))
	(set! ang (modulo (+ ang 180) 360)))))
  )



(define-object level (x y) (player buffer border)

  (define (can-collide obj) #f)

  (define (create)
    (set! border BORDER))

  (define (tick world)
    (when (eq? 0 (random 30))
      (add-object world (make enemy (x (random WIDTH))
			      (y (random HEIGHT))))))

  (define (draw world buffer)
    (let loop ((x 0))
      (when (< x border)
	(let* ((c (- 255 (* x (round* (/ 255 border)))))
	       (col (image-color c c c)))
	  (image-line my-buffer x x (- WIDTH x 1) x col)
	  (image-line my-buffer x (- HEIGHT x 1) (- WIDTH x 1) (- HEIGHT x 1) col)
	  (image-line my-buffer (- WIDTH x 1) x (- WIDTH x 1) (- HEIGHT x 1) col)
	  (image-line my-buffer x x x (- HEIGHT x 1) col))
	(loop (+ x 1)))))

  )

(define-object border% (x y phase) (crystals)

  (define (create)
    (set! phase 1)
    (set! crystals 0))

  (define (can-collide obj)
    (is-a? obj player))

  (define (touch world player)
    (if (alive?)
      (say player set-coordinates (/ WIDTH 2) (/ HEIGHT 2))
      ;; you beat this level!
      (begin
	(say player gain-score 100)
	(new-level world player))))

  (define (shapes)
    (list (make Rectangle (width 50) (height 6))))

  (define (death world obj)
    (when (is-a? obj crystal)
      (set! crystals (sub1 crystals))))

  (define (add-crystal crystal)
    (set! crystals (add1 crystals)))

  (define (alive?)
    (> crystals 0))

  (define (draw world buffer)
    (image-rectangle-fill my-buffer
			  (- x 25) (- y 3)
			  (+ x 25) (+ y 3)
			  (if (alive?)
			    (image-color 255 0 0)
			    (image-color 0 0 0))))

  )

;; these two objects only show the part of the screen currently displayed
(define-object clear (phase) (player)
   (define (create)
     (set! phase -100))

   (define (draw world buffer)
     (let ((world-width (say world get-width))
	   (world-height (say world get-height)))
       (let ((x (round* (let ((min-x (/ world-width 2))
			      (max-x (- WIDTH (/ world-width 2))))
			  (min (max (say player get-x) min-x) max-x))))
	     (y (round* (let ((min-y (/ world-height 2))
			      (max-y (- HEIGHT (/ world-height 2))))
			  (min (max (say player get-y) min-y) max-y)))))
	 (image-rectangle-fill my-buffer
			       (- x (/ world-width 2))
			       (- y (/ world-height 2))
			       (+ x (- (/ world-width 2))
				  (image-width buffer))
			       (+ y (- (/ world-height 2))
				  (image-height buffer))
			       (image-color 0 0 0)))))
   )

(define-object show (phase) (player)

   (define (create)
     (say player set-coordinates (/ WIDTH 2) (/ HEIGHT 2))
     (set! phase 100))

   (define (draw world buffer)
     (let ((world-width (say world get-width))
	   (world-height (say world get-height)))
       (let ((x (round* (let ((min-x (/ world-width 2))
			      (max-x (- WIDTH (/ world-width 2))))
			  (min (max (say player get-x) min-x) max-x))))
	     (y (round* (let ((min-y (/ world-height 2))
			      (max-y (- HEIGHT (/ world-height 2))))
			  (min (max (say player get-y) min-y) max-y)))))
	 (image-copy buffer my-buffer
		     (- x (/ world-width 2))
		     (- y (/ world-height 2))
		     (image-width buffer)
		     (image-height buffer)))
       (image-print-translucent buffer 0 0 (image-color 255 255 255) 192
				(format "Score: ~a" (say player get-score)))))

   )

(define (make-level player)
  (make level (player player)))

(define (new-level world player)
  (let ((border (make border%
		      (x (/ WIDTH 2))
		      (y (round* (/ BORDER 2))))))
    (say world remove-all)
    (add-object world player)
    (add-object world border)
    (add-object world (make-level player))
    (for-each (lambda (n)
		(let ((c (make crystal
			       (x (+ 10 (random (- WIDTH 20))))
			       (y (+ 10 (random (- HEIGHT 20)))))))
		  (say border add-crystal c)
		  (add-object world c)))
	      (build-list (+ 20 (random 10)) (lambda (n) n)))
    (for-each (lambda (n)
		(add-object world (make star
					(x (random WIDTH))
					(y (random HEIGHT)))))
	      (build-list 250 (lambda (x) x)))
    (add-object world (make show (player player)))
    (add-object world (make clear (player player)))))

(provide run)
(define (run)
  (define world (make-world 800 600))
  (define player* (make player))
  (start world (lambda (world)
		 (set! my-buffer (image-create WIDTH HEIGHT))
		 (new-level world player*))
	 (lambda (world)
	   (image-destroy my-buffer))))

(run)

)
