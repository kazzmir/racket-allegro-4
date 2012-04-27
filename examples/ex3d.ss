#|
Shows some 3d boxes bounce up and down in various drawing modes
|#

(module ex3d mzscheme

(require "../util.ss"
         "../keyboard.ss"
         (prefix image- "../image.ss")
         scheme/generator)

; (require (lib "errortrace.ss" "errortrace"))

(define max-z 1024)

(define (real->int i)
  (inexact->exact (round i)))

(define (make-num-list min max)
  (let loop ((nums '())
	     (n min))
    (if (> n max)
      (reverse nums)
      (loop (cons n nums) (add1 n)))))

(define-struct quad (v1 v2 v3 v4))
(define-struct vertex (x y z))

(define-struct shape (x y z       ;; positions
		      rx ry rz    ;; rotations
		      dz          ;; speed of movement
		      drx dry drz ;; speed of rotation
		      ))

(define points '((-32 -32 -32)
		 (-32 32 -32)
		 (32 32 -32) 
		 (32 -32 -32)
		 (-32 -32 32)
		 (-32 32 32)
		 (32 32 32)
		 (32 -32 32)))

(define (flatten lst)
  (if (null? lst)
    null
    (let ((x (car lst)))
      (cond
	((pair? x) (append (flatten x) (flatten (cdr lst))))
	(else (cons x (flatten (cdr lst))))))))

(define (get-points num)
  (list-ref points num))

(define (get-points-x num)
  (car (get-points num)))

(define (get-points-y num)
  (cadr (get-points num)))

(define (get-points-z num)
  (caddr (get-points num)))

(define faces '((0 3 2 1)
		(4 5 6 7)
		(0 1 5 4)
		(2 3 7 6)
		(0 4 7 3)
		(1 2 6 5)))

(define (get-face-1 num)
  (car (list-ref faces num)))

(define (get-face-2 num)
  (cadr (list-ref faces num)))

(define (get-face-3 num)
  (caddr (list-ref faces num)))

(define (get-face-4 num)
  (cadddr (list-ref faces num)))

(define (init-shape)
  (let ((x (- (bitwise-and (random 256) 255) 128))
	(y (- (bitwise-and (random 256) 255) 128))
	(z 768)
	(rx 0)
	(ry 0)
	(rz 0)
	(dz (+ 0.2 (/ (- (random 100) 9) 4)))
	(drx (/ (- (random 31) 16) 3))
	(dry (/ (- (random 31) 16) 3))
	(drz (/ (- (random 31) 16) 3)))
    (make-shape x y z rx ry rz dz drx dry drz)))

(define (move-shape shape)
  (let ((z (+ (shape-z shape) (shape-dz shape)))
	(rx (+ (shape-rx shape) (shape-drx shape)))
	(ry (+ (shape-ry shape) (shape-dry shape)))
	(rz (+ (shape-rz shape) (shape-drz shape))))
    (when (or (> z max-z) (< z 192))
      (set-shape-dz! shape (- (shape-dz shape))))
    (set-shape-z! shape z)
    (set-shape-rx! shape rx)
    (set-shape-ry! shape ry)
    (set-shape-rz! shape rz)))

(define (mid x1 x2 x3)
  (max x1 (min x2 x3)))

(define (vertex-color vt1 vt2)
  (let ((z (/ (+ (vertex-z vt1) (vertex-z vt2)) 2)))
    (real->int (mid 150 
		    (- 255
		       (/ (* 128 (- z 192))
			  (- 1024 192)))
		    255))))

(define (wire buffer wire-colors vt1 vt2)
  (let ((color (let ((z (/ (+ (vertex-z vt1) (vertex-z vt2)) 2)))
		 (list-ref wire-colors
			   (- 128 (real->int (mid 1 128 (/ (* z 128) max-z))))))))
    (image-line buffer
		(real->int (vertex-x vt1))
		(real->int (vertex-y vt1))
		(real->int (vertex-x vt2))
		(real->int (vertex-y vt2))
		color)))

;; qsort
(define (sort lst bigger?)
  (cond
    ((<= (length lst) 1) lst)
    (else
      (let ((pivot (car lst)))
	(let loop ((rest (cdr lst))
		   (big '())
		   (small '()))
	  (cond
	    ((null? rest) (append
			    (sort small bigger?)
			    (list pivot)
			    (sort big bigger?)))
	    ((bigger? (car rest) pivot) (loop (cdr rest)
					(cons (car rest) big)
					small))
	    (else (loop (cdr rest) big (cons (car rest) small)))))))))

(define next-mode
  (let ()
    (define modes '(wire-frame POLYTYPE-FLAT POLYTYPE-GCOL
                               POLYTYPE-GRGB POLYTYPE-ATEX POLYTYPE-PTEX
                               POLYTYPE-ATEX-TRANS POLYTYPE-PTEX-TRANS
                               POLYTYPE-ATEX-MASK POLYTYPE-PTEX-MASK
                               POLYTYPE-ATEX-MASK-TRANS POLYTYPE-PTEX-MASK-TRANS
                               POLYTYPE-ATEX-LIT POLYTYPE-PTEX-LIT
                               POLYTYPE-ATEX-MASK-LIT POLYTYPE-PTEX-MASK-LIT))
    (generator
      (let loop ([current modes])
        (cond
          [(null? current) (loop modes)]
          [else (yield (car current))
                (loop (cdr current))])))))

(define (draw-quad buffer texture quad mode color1 color2 color3 color4)
  (let ((vtx1 (let ((q (quad-v1 quad)))
		(image-make-v3d (vertex-x q)
				(vertex-y q)
				(vertex-z q)
				0 0 color1)))
	(vtx2 (let ((q (quad-v2 quad)))
		(image-make-v3d (vertex-x q)
				(vertex-y q)
				(vertex-z q)
				32 0 color2)))
	(vtx3 (let ((q (quad-v3 quad)))
		(image-make-v3d (vertex-x q)
				(vertex-y q)
				(vertex-z q)
				32 32 color3)))
	(vtx4 (let ((q (quad-v4 quad)))
		(image-make-v3d (vertex-x q)
				(vertex-y q)
				(vertex-z q)
				0 32 color4))))
    (when (>= (polygon-z-normal vtx1 vtx2 vtx3) 0)
      (image-quad3d buffer
		    mode
		    texture
		    vtx1
		    vtx2
		    vtx3
		    vtx4))))

(provide run)
(define (run)
  (define max-shapes 8)
  (define points #f)
  (define max-vertices 8)
  (define mode (next-mode))
  (define space-pressed 0)
  (easy-init 640 480 16)
  
  (set-trans-blender! 0 0 0 128)

  (let ((texture (let ((image (image-create 32 32))
		       (frame-color (image-color 255 0 0))
		       (text-color (image-color 0 255 0)))
		   (image-clear image (image-mask-color image))
		   (image-line image  0 0 31 31 frame-color)
		   (image-line image 0 31 31 0 frame-color)
		   (image-rectangle image 0 0 31 31 frame-color)
		   (image-print image 0 0 text-color -1 "dead")
		   (image-print image 0 8 text-color -1 "pigs")
		   (image-print image 0 16 text-color -1 "cant")
		   (image-print image 0 24 text-color -1 "fly.")
		   image))
	(shapes (map (lambda (num)
		       (init-shape))
		     (make-num-list 1 max-shapes)))
	(wire-colors (blend-palette (image-color 0 0 0)
				    (image-color 255 255 255)
				    128)))
    (set-projection-viewport 0 0 screen-x screen-y)
    (game-loop
      (lambda ()
	;; flip to the next mode
	(when (and (keypressed? 'SPACE) (= space-pressed 0))
	  (set! space-pressed 4)
	  (set! mode (next-mode)))
	(when (> space-pressed 0)
	  (set! space-pressed (sub1 space-pressed)))

	;; in the end points will be a list of list of vertices
	;; points = (list points1 points2 points3 ...)
	;; points<X> = (list vt1 vt2 vt3 ...)
	;; vt<X> = (list x y z)
	(set! points
	  (map (lambda (shape)
		 (move-shape shape)
		 (let ((matrix 
			 (get-transformation-matrix 
			   1.0 
			   (shape-rx shape)
			   (shape-ry shape)
			   (shape-rz shape)
			   (shape-x shape)
			   (shape-y shape)
			   (shape-z shape))))
		   (map 
		     (lambda (num)
		       (let-values 
			 (((x y z)
			   (apply-matrix matrix 
					 (get-points-x num)
					 (get-points-y num)
					 (get-points-z num))))
			 (let-values (((px py) (persp-project x y z)))
				     (list px py z))))
		     (make-num-list 0 (sub1 max-vertices)))))
		  shapes))
	(keypressed? 'ESC))
      (lambda (buffer)
	(image-print buffer 5 10 (image-color 128 128 128) -1 (format "Mode: ~a" mode))
	(image-print buffer 5 20 (image-color 128 128 128) -1 "Press spacebar to goto the next mode")
	(for-each (lambda (quad)
		    (let ((current-mode mode))
		      (case current-mode
			((wire-frame) (let ((draw-wire (lambda (v1 v2)
							 (wire buffer wire-colors v1 v2))))
					(draw-wire (quad-v1 quad) (quad-v2 quad))
					(draw-wire (quad-v2 quad) (quad-v3 quad))
					(draw-wire (quad-v3 quad) (quad-v4 quad))
					(draw-wire (quad-v4 quad) (quad-v1 quad))))
			((POLYTYPE-ATEX-LIT
			  POLYTYPE-PTEX-LIT
			  POLYTYPE-ATEX-MASK-LIT
			  POLYTYPE-PTEX-MASK-LIT
			  POLYTYPE-ATEX
			  POLYTYPE-PTEX
			  POLYTYPE-ATEX-TRANS
			  POLYTYPE-PTEX-TRANS
			  POLYTYPE-ATEX-MASK
			  POLYTYPE-PTEX-MASK
			  POLYTYPE-ATEX-MASK-TRANS
			  POLYTYPE-PTEX-MASK-TRANS
			  POLYTYPE-GCOL
			  POLYTYPE-FLAT
			  POLYTYPE-GRGB)
			 (let-values (((c1 c2 c3 c4)
				       (case current-mode
					 ((POLYTYPE-ATEX-LIT
					   POLYTYPE-PTEX-LIT
					   POLYTYPE-ATEX-MASK-LIT
					   POLYTYPE-PTEX-MASK-LIT)
					  (let ((get-color (lambda (z)
							     (real->int (- 255 (mid 0 (/ z 4) 255))))))
					    (values
					      (get-color (vertex-z (quad-v1 quad)))
					      (get-color (vertex-z (quad-v2 quad)))
					      (get-color (vertex-z (quad-v3 quad)))
					      (get-color (vertex-z (quad-v4 quad))))))
					 ((POLYTYPE-ATEX
					   POLYTYPE-PTEX
					   POLYTYPE-ATEX-TRANS
					   POLYTYPE-PTEX-TRANS
					   POLYTYPE-ATEX-MASK
					   POLYTYPE-PTEX-MASK
					   POLYTYPE-ATEX-MASK-TRANS
					   POLYTYPE-PTEX-MASK-TRANS)
					  (values 0 0 0 0))
					 ((POLYTYPE-FLAT)
					  (let ((c (let ((z (/ (+ (vertex-z (quad-v1 quad)) (vertex-z (quad-v2 quad))) 2)))
						     (list-ref wire-colors
							       (- 128 (real->int (mid 1 128 (/ (* z 128) max-z))))))))
					    (values c c c c)))
					 ((POLYTYPE-GCOL)
					  (values (image-color 255 255 255)
						  (image-color 128 128 128)
						  (image-color 128 128 128)
						  (image-color 0 0 0)))
					 ((POLYTYPE-GRGB)
					  (values #x000000
						  #x7F0000
						  #xFF0000
						  #x7F0000)))))
                               (draw-quad buffer texture quad current-mode c1 c2 c3 c4))))))
		      (sort 
			(flatten
			  (map (lambda (num)
				 (let ((vertices (list-ref points num)))
				   (map (lambda (face-num)
					  (define (make lst)
					    (let ((x (car lst))
						  (y (cadr lst))
						  (z (caddr lst)))
					      (make-vertex x y z)))
					  (let ((v1 (make (list-ref vertices
								    (get-face-1 face-num))))
						(v2 (make (list-ref vertices
								    (get-face-2 face-num))))
						(v3 (make (list-ref vertices
								    (get-face-3 face-num))))
						(v4 (make (list-ref vertices
								    (get-face-4 face-num)))))
					    (make-quad v1 v2 v3 v4)))
					(make-num-list 0 5))))
			       (make-num-list 0 (sub1 max-shapes))))
			(lambda (quad1 quad2)
			  (let ((z1 (+ (vertex-z (quad-v1 quad1))
				       (vertex-z (quad-v2 quad1))
				       (vertex-z (quad-v3 quad1))
				       (vertex-z (quad-v4 quad1))))
				(z2 (+ (vertex-z (quad-v1 quad2))
				       (vertex-z (quad-v2 quad2))
				       (vertex-z (quad-v3 quad2))
				       (vertex-z (quad-v4 quad2)))))
			    (> z2 z1))))))
      (fps 25)))
  (easy-exit))

(run)
)
