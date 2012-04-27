(module image mzscheme

  (require (lib "foreign.ss")) (unsafe!)
  (require (prefix a: "private/allegro.ss"))

  (define-struct image (bitmap width height))

  (define-for-syntax (format-syntax stx str . args)
    (datum->syntax-object
      stx
      (string->symbol
	(apply format str
	       (map (lambda (x)
		      (symbol->string
			(syntax-object->datum x)))
		    args)))))

  ;; All functions in this module that work on an image
  ;; have the first argument the image that is to be modified.
  ;; Any extra image parameters come after.

  ;; defines 'name' and 'name-screen'
  ;; name = (lambda image args ...)
  ;; name-screen = (name (screen) args ...)
  (define-syntax (define/screen stx)
    (syntax-case stx ()
      ((_ (name image args ...) bodys ...)
       (with-syntax ((name-screen (format-syntax #'name "~a-screen" #'name)))
         #'(begin
	     (define (name image args ...) bodys ...)
	     (define (name-screen args ...)
	       (name (screen) args ...)))))
      ((_ name bodys ...)
       (with-syntax ((name-screen (format-syntax #'name "~a-screen" #'name)))
         #'(begin
	     (define name bodys ...)
	     (define (name-screen . args)
	       (apply name (screen) args)))))))

  ;; im sure this can be combined with the above
  (define-syntax (define/screen* stx)
    (syntax-case stx ()
      ((_ (name image args ...) bodys ...)
       (with-syntax ((name-screen (format-syntax #'name "~a-screen" #'name)))
         #'(begin
	     (provide name name-screen)
	     (define/screen (name image args ...) bodys ...))))
      ((_ name bodys ...)
       (with-syntax ((name-screen (format-syntax #'name "~a-screen" #'name)))
         #'(begin
	     (provide name name-screen)
	     (define/screen name bodys ...))))))

  ;; skip a few steps
  (define-syntax (define/screen/translucent* stx)
    (syntax-case stx ()
      ((_ (name image args ...) bodys ...)
       (let ((convert (lambda (str) (format-syntax #'name str #'name))))
	 (with-syntax ((name-screen (convert "~a-screen"))
		       (name-translucent (convert "~a/translucent"))
		       (name-screen-translucent (convert "~a-screen/translucent")))
	 #'(begin
	     (provide name name-screen
		      name-translucent name-screen-translucent)
	     (define/screen (name image args ...) bodys ...)
	     (define (name-translucent image red green blue alpha args ...)
	       (a:set-trans-blender red green blue alpha)
	       (a:drawing-mode 'TRANSLUCENT)
	       bodys ...
	       (a:drawing-mode 'SOLID))
	     (define (name-screen-translucent red green blue alpha args ...)
	       (apply name (screen) red green blue alpha args ...))))))))

  (provide (rename image-width width)
	   (rename image-height height)
	   image?)
  
  (provide (rename a:acquire-screen acquire-screen)
	   (rename a:release-screen release-screen))

  (define (int->fix i)
    (arithmetic-shift i 16))

  (define (float->fix f)
    (inexact->exact (round (+ (* f 65536)
			      (if (< f 0) -0.5 0.5)))))


  (provide create)
  (define create
    (case-lambda
      ((width height) (create-from-bitmap
			(a:create-bitmap width height)))
      ((width height color-depth)
       (create-from-bitmap (a:create-bitmap-ex
			     color-depth width height)))))

  (provide color)
  (define (color r g b)
    (a:makecol r g b))

  (provide get-rgb)
  (define (get-rgb color)
    (values (a:getr color)
	    (a:getg color)
	    (a:getb color)))

  (provide create-from-file)
  (define (create-from-file filename)
    (if (string? filename)
      (create-from-bitmap (a:load-bitmap filename #f))
      (error (format "~a must be a filename" filename))))

  (define create-from-bitmap
    (case-lambda
      ((bitmap)
       (if (not bitmap)
	 #f
	 (make-image bitmap (a:BITMAP-w bitmap) (a:BITMAP-h bitmap))))
      ((bitmap x y width height) 
       (if (not bitmap)
	 #f
	 (make-image (a:create-sub-bitmap bitmap x y width height) 
		     width height)))))

  (define/screen* (create-sub image x y width height)
    (create-from-bitmap (image-bitmap image) x y width height))

  #;
  (provide make-palette)
  (define (make-palette)
    ; (malloc _byte 680)
    (let ((m (malloc a:_RGB 256)))
      (cpointer-push-tag! m a:RGB-tag)
      m))

  #;
  (provide get-palette-color)
  #;
  (define (get-palette-color num)
    (ptr-ref (a:palette-color) _int num)
    #;
    (let ((rgb (ptr-ref a:palette-color a:_RGB num)))
      (color (a:RGB-r rgb)
	     (a:RGB-g rgb)
	     (a:RGB-b rgb))))

  #;
  (provide get-desktop-color)
  #;
  (define (get-desktop-color num)
    (let ((rgb (ptr-ref a:desktop-palette a:_RGB num)))
      (color (a:RGB-r rgb)
	     (a:RGB-g rgb)
	     (a:RGB-b rgb))))

  (provide make-v3d)
  (define (make-v3d x y z u v c)
    (a:make-v3d (+ x 0.0)
		(+ y 0.0)
		(+ z 0.0)
		(+ u 0.0)
		(+ v 0.0)
		c))

  (provide (rename a:v3d-x v3d-x)
	   (rename a:v3d-y v3d-y)
	   (rename a:v3d-z v3d-z)
	   (rename a:v3d-u v3d-u)
	   (rename a:v3d-v v3d-v)
	   (rename a:v3d-c v3d-c))

  (define/screen/translucent* (quad3d image1 type image2 v1 v2 v3 v4)
    (a:quad3d (image-bitmap image1)
	      type
	      (image-bitmap image2)
	      v1 v2 v3 v4))

  (define/screen/translucent* (polygon3d image type texture v3ds)
    (a:polygon3d (image-bitmap image) type texture
		 (length v3ds)
		 (list->cblock v3ds a:_v3d)))

  (define/screen/translucent* (triangle3d image type texture v3d1 v3d2 v3d3)
    (a:triangle3d (image-bitmap image) type texture
		  v3d1 v3d2 v3d3))

  #;
  (provide rgb-map)
  #;
  (define rgb-map
    (case-lambda
      (() a:rgb-map)
      ((new) (a:set-rgb-map! new))))

  (define/screen* (mask-color image)
    (a:bitmap-mask-color (image-bitmap image)))

  #;
  (provide color-map)
  #;
  (define color-map
    (case-lambda
      (() a:color-map)
      ((new) (a:set-color-map! new))))

  (provide (rename a:create-rgb-table create-rgb-table))
  (provide (rename a:create-light-table create-light-table))

  ; (provide unwrite-line)
  (define/screen* (unwrite-line image)
    (a:bmp-unwrite-line (image-bitmap image)))

  (provide (rename read-line- read-line)
	   (rename read-line--screen read-line-screen))
  (define/screen (read-line- image y)
    (a:bmp-read-line (image-bitmap image) y))

  ;; (provide write-line)
  (define/screen* (write-line image y)
    (a:bmp-write-line (image-bitmap image) y))

  #|
  (provide image-read32)
  (define (image-read32 pointer)
    (bmp-read32 pointer))

  (provide image-write32)
  (define (image-write32 pointer c)
    (bmp-write32 pointer c)) 
  |#

  #;
  (provide get-palette)
  (define (get-palette p num)
    (let ((rgb (ptr-ref p a:_RGB num)))
      (color (a:RGB-r rgb)
	     (a:RGB-g rgb)
	     (a:RGB-b rgb))))

  #;
  (provide set-palette!)
  (define set-palette!
    (case-lambda
      ((palette)
       (a:set-palette palette))
      ((palette num r g b)
       (let ((rgb (ptr-ref palette a:_RGB num)))
	 (a:set-RGB-r! rgb r)
	 (a:set-RGB-g! rgb g)
	 (a:set-RGB-b! rgb b)))))

  #;
  (define (create-image-bitmap bitmap)
    (make-image bitmap (BITMAP-w bitmap) (BITMAP-h bitmap)))

  (provide destroy)
  (define (destroy image)
    (a:destroy-bitmap (image-bitmap image)))

  (define/screen* (duplicate image)
    (let ((i (create (image-width image) (image-height image))))
      (copy i image)
      i))


  ; (provide line)
  (define/screen/translucent* (line image x1 y1 x2 y2 color)
    (a:line (image-bitmap image) x1 y1 x2 y2 color))

  (define/screen/translucent* (fastline image x1 y1 x2 y2 color)
    (a:fastline (image-bitmap image) x1 y1 x2 y2 color))

  ;; (polygon image '(50 50 100 100 75 75))
  (define/screen/translucent* (polygon image lst color)
    (a:polygon (image-bitmap image) (/ (length lst) 2)
	       (list->cblock lst _int) color))

  (define/screen/translucent* (arc image x y angle1 angle2 radius color)
    (a:arc (image-bitmap image) x y (float->fix angle1)
	   (float->fix angle2) radius color))

  (provide calculate-spline)
  (define (calculate-spline x1 y1 x2 y2 x3 y3 x4 y4 points)
    (a:calc-spline (list x1 y1 x2 y2 x3 y3 x4 y4) points))

  (define/screen/translucent* (spline image x1 y1 x2 y2 x3 y3 x4 y4 color)
    (a:spline (image-bitmap image) (list->cblock (list x1 y1 x2 y2 x3 y3 x4 y4)
						 _int)
	      color))

  ;; translucent floodfill?? maybe..
  (define/screen/translucent* (floodfill image x y color)
    (a:floodfill (image-bitmap image) x y color))

  (define/screen/translucent* (triangle image x1 y1 x2 y2 x3 y3 color)
    (a:triangle (image-bitmap image) x1 y1 x2 y2 x3 y3 color))
  
  #|
   ;; triangle is already filled, apparently if you want a hollow triangle
   ;; you should call line() 3 times. Maybe this should be changed..
  (provide image-triangle-fill)
  (define (image-triangle-fill image x1 y1 x2 y2 x3 y3 color)
    (triangle-fill (image-bitmap image) x1 y1 x2 y2 x3 y3 color))
  |#

  ; (provide circle)
  (define/screen/translucent* (circle image x1 y1 radius color)
    (a:circle (image-bitmap image) x1 y1 radius color))
  
  ; (provide circle-fill)
  (define/screen/translucent* (circle-fill image x1 y1 radius color)
    (a:circlefill (image-bitmap image) x1 y1 radius color))

  (define/screen/translucent* (ellipse image x y rx ry color)
    (a:ellipse (image-bitmap image) x y rx ry color))

  (define/screen/translucent* (ellipse-fill image x y rx ry color)
    (a:ellipsefill (image-bitmap image) x y rx ry color))

  ; (provide rectangle)
  (define/screen/translucent* (rectangle image x1 y1 x2 y2 color)
    (a:rect (image-bitmap image) x1 y1 x2 y2 color))
  
  ; (provide rectangle-fill)
  (define/screen/translucent* (rectangle-fill image x1 y1 x2 y2 color)
    (a:rectfill (image-bitmap image) x1 y1 x2 y2 color))

  ; (provide putpixel putpixel-screen)
  (define/screen/translucent* (putpixel image x1 y1 color)
    (a:putpixel (image-bitmap image) x1 y1 color))

  ; (provide getpixel)
  (define/screen* (getpixel image x1 y1)
    (a:getpixel (image-bitmap image) x1 y1))
  
  (provide (rename print- print)
	   (rename print--screen print-screen))
  (define/screen (print- image x y color bgcolor message)
    (a:textout-ex (image-bitmap image)
		  (a:default-font)
		  message x y color bgcolor))

  ; (provide print-center)
  (define/screen* (print-center image x y color bgcolor message)
    (a:textout-centre-ex (image-bitmap image)
			 (a:default-font)
			 message x y color bgcolor))

  (define/screen* (print-translucent image x y color* alpha message)
    (let ((area (create (a:text-length (a:default-font) message)
			(a:text-height (a:default-font)))))
      (clear area (color 255 0 255))
      (print- area 0 0 color* -1 message)
      (a:set-trans-blender 0 0 0 alpha)
      (draw-translucent image area x y)
      (destroy area)))

  ; (provide clear)
  (define/screen* clear
    (case-lambda
      ((image)
       (clear image (color 0 0 0)))
      ((image color)
       (a:clear-to-color (image-bitmap image) color))))

  #;
  (define (draw-alpha draw)
    (letrec ((routine (case-lambda
			((image1 image2)
			 (routine image1 image2 0 0 0))
			((image1 image2 x1 y1)
			 (draw 
			   (image-bitmap image1)
			   (image-bitmap image2)
			   x1 y1)))))
      routine))

  #;
  (define/screen* draw-trans (draw-alpha a:draw-trans-sprite))
  #;
  (define/screen* draw-lit (draw-alpha a:draw-lit-sprite))

  (provide draw-translucent)
  (define draw-translucent
    (case-lambda
      ((image1 image2)
       (draw-translucent image1 image2 0 0))
      ((image1 image2 x1 y1)
       (a:draw-trans-sprite 
	 (image-bitmap image1)
	 (image-bitmap image2)
	 x1 y1))))

  (provide draw-lit)
  (define draw-lit
    (case-lambda
      ((image1 image2)
       (draw-lit image1 image2 0 0 0))
      ((image1 image2 x1 y1 a)
       (a:draw-lit-sprite 
	 (image-bitmap image1)
	 (image-bitmap image2)
	 x1 y1 a))))

  (define/screen* (draw image1 image2 x y)
    (a:draw-sprite (image-bitmap image1)
		   (image-bitmap image2)
		   x y))

  (define/screen* (draw-vertical-flip image1 image2 x y)
    (a:draw-sprite-v-flip (image-bitmap image1)
			  (image-bitmap image2)
			  x y))

  (define/screen* (draw-horizontal-flip image1 image2 x y)
    (a:draw-sprite-h-flip (image-bitmap image1)
			    (image-bitmap image2)
			    x y))

  (define/screen* (draw-vertical-horizontal-flip image1 image2 x y)
    (a:draw-sprite-vh-flip (image-bitmap image1)
			   (image-bitmap image2)
			   x y))

  (define/screen* (draw-gouraud image1 image2 x y color1 color2 color3 color4)
    (a:draw-gouraud-sprite (image-bitmap image1) (image-bitmap image2)
			   x y color1 color2 color3 color4))

  (define/screen* (draw-character image1 image2 x y color background)
    (a:draw-character-ex (image-bitmap image1) (image-bitmap image2)
			 x y color background))

  (define/screen* (draw-rotate image1 image2 x y angle)
    (a:rotate-sprite (image-bitmap image1) (image-bitmap image2)
		     x y (float->fix angle)))

  (define/screen* (draw-rotate-vertical-flip image1 image2 x y angle)
    (a:rotate-sprite-v-flip (image-bitmap image1) (image-bitmap image2)
			    x y (float->fix angle)))

  (define/screen* (draw-rotate-scaled image1 image2 x y angle scale)
    (a:rotate-scaled-sprite (image-bitmap image1) (image-bitmap image2)
			    x y (float->fix angle) (float->fix scale)))

  (define/screen* (draw-rotate-scaled-vertical-flip image1 image2 x y angle scale)
    (a:rotate-scaled-sprite-v-flip (image-bitmap image1) (image-bitmap image2)
				   x y (float->fix angle) (float->fix scale)))

  (define/screen* (draw-pivot image1 image2 x y center-x center-y angle)
    (a:pivot-sprite (image-bitmap image1) (image-bitmap image2)
		    x y center-x center-y (float->fix angle)))

  (define/screen* (draw-pivot-vertical-flip image1 image2 x y center-x center-y angle)
    (a:pivot-sprite-v-flip (image-bitmap image1) (image-bitmap image2)
			   x y center-x center-y (float->fix angle)))

  (define/screen* (draw-pivot-scaled image1 image2 x y center-x center-y angle scale)
    (a:pivot-scaled-sprite (image-bitmap image1) (image-bitmap image2)
			   x y center-x center-y
			   (float->fix angle) (float->fix scale)))

  (define/screen* (draw-pivot-scaled-vertical-flip image1 image2 x y center-x center-y angle scale)
    (a:pivot-scaled-sprite-v-flip (image-bitmap image1) (image-bitmap image2)
				  x y center-x center-y
				  (float->fix angle) (float->fix scale)))

  (define (blit-routine blit)
    (letrec ((routine
	       (case-lambda
		 ((image1 image2)
		  (routine image1 image2 0 0))
		 ((image1 image2 x1 y1)
		  (routine image1 image2 x1 y1
			   (image-width image2)
			   (image-height image2)))
		 ((image1 image2 x1 y1 width height)
		  (routine image1 image2 x1 y1 width height 0 0))
		 ((image1 image2 x1 y1 width height dest-x dest-y)
		  (blit (image-bitmap image2)
			(image-bitmap image1)
			x1 y1
			dest-x dest-y
			width height)))))
      routine))

  (define/screen* copy (blit-routine a:blit))
  (define/screen* copy-masked (blit-routine a:masked-blit))

  ; (provide copy)
  #;
  (define/screen* copy
    (case-lambda
      ((image1 image2)
       (copy image1 image2 0 0))
      ((image1 image2 x1 y1)
       (copy image1 image2 x1 y1 (image-width image2) (image-height image2)))
      ((image1 image2 x1 y1 width height)
       (copy image1 image2 x1 y1 width height 0 0))
      ((image1 image2 x1 y1 width height dest-x dest-y)
       (a:blit (image-bitmap image2)
	       (image-bitmap image1)
	       x1 y1 dest-x dest-y
	       width height))))

  #;
  (define/screen* copy-masked
    (case-lambda
      ((image1 image2)
       (copy-masked image1 image2 0 0))
      ((image1 image2 x1 y1)
       (copy-masked image1 image2 x1 y1
		    (image-width image2)
		    (image-height image2)))
      ((image1 image2 x1 y1 width height)
       (copy-masked image1 image2 width height 0 0))
      ((image1 image2 x1 y1 width height dest-x dest-y)
       (a:masked-blit (image-bitmap image2)
		      (image-bitmap image1)
		      x1 y1 width height
		      dest-x dest-y))))

  (define/screen* (copy-stretch image1 image2 source-x source-y source-width source-height dest-x dest-y dest-width dest-height)
    (a:stretch-blit (image-bitmap image2)
		    (image-bitmap image1)
		    source-x source-y source-width source-height
		    dest-x dest-y dest-width dest-height))

  (define/screen* (copy-masked-stretch image1 image2 source-x source-y source-width source-height dest-x dest-y dest-width dest-height)
    (a:masked-stretch-blit (image-bitmap image2)
			   (image-bitmap image1)
			   source-x source-y source-width source-height
			   dest-x dest-y dest-width dest-height))

  (define/screen* (draw-stretched image1 image2 x y width height)
    (a:stretch-sprite image1 image2 x y width height))

  (provide screen)
  (define (screen)
    (create-from-bitmap (a:screen)))

  (define/screen* (save image name)
    (a:save-bitmap name (image-bitmap image) #f))

  #|
  (provide copy-to-screen)
  (define copy-to-screen
    (case-lambda
      ((image) (copy image (screen)))
      ((image x1 y1)
       (copy image (screen) x1 y1))))
  |#

  ;; collision detection with a binary space partition, sort of
  ;; this was basically copied from a C++ implemenation
  (define-struct equad (quads parent min-x min-y width height full))

  (define (full? quad)
    (equad-full quad))

  (define (num-quads quad)
    (length (equad-quads quad)))

  (define (make-equad-from-image image min-size mask-pixel min-x min-y parent)
    (let ((width (image-width image))
	  (height (image-height image))
	  (bitmap (image-bitmap image)))
      (let ((quad (make-equad null parent min-x min-y 
			      width height #f)))
	(if (andmap (lambda (x) (> x min-size)) (list width height))
	  (let ((w (inexact->exact (round (/ width 2))))
		(h (inexact->exact (round (/ height 2)))))
	    (set-equad-quads! 
	      quad
	      (map (lambda (x-y)
		     (let ((x (car x-y))
			   (y (cadr x-y)))
		       (let ((sub (create-from-bitmap bitmap
						      x y w h)))
			 (make-equad-from-image sub min-size mask-pixel x y quad))))
		   (list (list 0 0)
			 (list w 0)
			 (list 0 h)
			 (list w h))))
	    (if (andmap full? (equad-quads quad))
	      (begin
		(set-equad-quads! quad null)
		(set-equad-full! quad #t))
	      (set-equad-quads! 
		quad 
		(let loop ((sofar null)
			   (child-quads (equad-quads quad)))
		  (cond
		    ((null? child-quads) sofar)
		    ((= (num-quads (car child-quads)) 1) 
		     (let ((newquad (car (equad-quads (car child-quads))))
			   (min-x (equad-min-x (car child-quads)))
			   (min-y (equad-min-y (car child-quads))))
		       (set-equad-min-x! (+ (equad-min-x newquad) min-x))
		       (set-equad-min-y! (+ (equad-min-y newquad) min-y))
		       (set-equad-parent! quad)
		       (loop (cons newquad sofar) (cdr child-quads))))
		    (else (loop (cons (car child-quads) sofar)
				(cdr child-quads))))))))
	  (let ((total (let xloop ((x 0)
				   (total 0))
			 (if (< x width)
			   (let yloop ((y 0)
				       (total total))
			     (if (< y height)
			       (yloop (add1 y) 
				      (if (not (= (getpixel bitmap x y) mask-pixel))
					(add1 total)
					total))
			       (xloop (add1 x) total)))
			   total))))
	    (set-equad-full! quad (> (/ (* total 100) (* width height)) 50))))
	quad)))

  (define (touch-box? ax1 ay1 ax2 ay2 bx1 by1 bx2 by2)
    (cond
      ((and (< ax1 bx1) (< ax1 bx2)
	    (< ax2 bx1) (< ax2 bx2))
       #f)
      ((and (> ax1 bx1) (> ax1 bx2)
	    (> ax2 bx1) (> ax2 bx2))
       #f)
      ((and (< ay1 by1) (< ay1 by2)
	    (< ay2 by1) (< ay2 by2))
       #f)
      ((and (> ay1 by1) (> ay1 by2)
	    (> ay2 by1) (> ay2 by2))
       #f)
      (else #t)))

  (define (collide-equad? quad mx my x1 y1 x2 y2)
    (let* ((rx1 (+ mx (equad-min-x quad)))
	   (ry1 (+ my (equad-min-y quad)))
	   (rx2 (+ rx1 (equad-width quad)))
	   (ry2 (+ ry1 (equad-height quad))))
      (cond
	((not (touch-box? rx1 ry1 rx2 ry2 x1 y1 x2 y2)) #f)
	((ormap (lambda (q) 
		  (collide-equad? q rx1 ry1 x1 y1 x2 y2))
		(equad-quads quad))
	 #t)
	(else (equad-full quad)))))

#|
      (if (not (touch-box? rx1 ry1 rx2 ry2 x1 y1 x2 y2))
	#f
	(if (ormap (lambda (q) (collide-equad? rx1 ry1 x1 y1 x2 y2))
		   (equad-quads quad))
	  #t
	  (equad-full equad))))
|#

  (define (display-equad image equad x y color)
    (let* ((mx1 (+ x (equad-min-x equad)))
	   (my1 (+ y (equad-min-y equad)))
	   (mx2 (+ mx1 (equad-width equad)))
	   (my2 (+ my1 (equad-height equad))))
      (for-each (lambda (e) (display-equad image e mx1 my1 color)) (equad-quads equad))
      (when (full? equad)
	(rectangle image mx1 my1 mx2 my2 color))))

  (define-struct ebox (head))

  (define MIN-SIZE 8)

  (provide make-ebox-from-image)
  (define make-ebox-from-image
    (case-lambda
      ((image) (make-ebox (make-equad-from-image image MIN-SIZE (a:bitmap-mask-color (image-bitmap image)) 0 0 #f)))
      ((image mask-pixel)
       (make-ebox (make-equad-from-image image MIN-SIZE mask-pixel 0 0 #f)))))

  (define (ebox-width ebox)
    (equad-width (ebox-head ebox)))

  (define (ebox-height ebox)
    (equad-height (ebox-head ebox)))

  (provide ebox-collide?)
  (define (ebox-collide? ebox1 mx my ebox2 ax ay)
    (if (not (collide-equad? (ebox-head ebox1) mx my
			     ax ay 
			     (+ (ebox-width ebox2) ax)
			     (+ (ebox-height ebox2) ay)))
      #f
      (let ((x1 (max mx ax))
	    (y1 (max my ay))
	    (x2 (min (+ (ebox-width ebox1) mx)
		     (+ (ebox-width ebox2) ax)))
	    (y2 (min (+ (ebox-height ebox1) my)
		     (+ (ebox-height ebox2) ay))))
	(and (collide-equad? (ebox-head ebox1) mx my
			     x1 y1 x2 y2)
	     (collide-equad? (ebox-head ebox2) mx my
			     x1 y1 x2 y2)))))

  (provide display-ebox)
  (define (display-ebox image ebox x y color)
    (display-equad image (ebox-head ebox) x y color))

)
