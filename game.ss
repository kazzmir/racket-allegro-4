(module game mzscheme

  (require (lib "list.ss"))
  (require "util.ss")
  (require "keyboard.ss")
  (require (prefix mouse- "mouse.ss"))
  (require (prefix image- "image.ss"))

  (provide (all-from "image.ss"))

  (require (lib "math.ss"))
  (require (lib "etc.ss"))

  (require (lib "class.ss"))

  (define (round* i) (inexact->exact (round i)))
  (provide round*)

  (provide (rename calculate-normal-angle calculate-angle))

  ;; send a method to an object but if the method
  ;; doesn't exist return void
  (provide send**)
  (define-syntax (send** stx)
    (syntax-case stx ()
      ((_ obj method args ...)
       (with-syntax ((num-args (length (syntax->list (syntax (args ...))))))
         #`(begin
	     #;
	     (printf "~a has method ~a ~a = ~a\n" obj 'method num-args
		     (object-method-arity-includes? obj 'method num-args))
	     (if (object-method-arity-includes? obj 'method num-args)
	       (send obj method args ...)
	       (void)))))))

  (provide Basic)
  (define Basic
    (class* object% ()

	    (init-field (phase 0))
	    (init-field (x 0))
	    (init-field (y 0))

	    ;; return #t if this object can collide with obj
	    (define/public (can-collide obj) #t)

	    ;; return a list of shapes representing this object
	    (define/public (shapes) (list))

	    ;; perform a side-affect with a list of keys
	    (define/public (key world keys) (void))

	    ;; this object touched `obj'
	    (define/public (touch world obj) (void))

	    ;; perform a side-affect on this descrete moment in time
	    (define/public (tick world) (void))

	    ;; draw the current object on the image `buffer'
	    (define/public (draw world buffer) (void))
       
	    (define/public (get-x) x)
	    (define/public (get-y) y)

	    (begin
	      (super-new))
	    ))

  ;; min-x : () x coordinate furthest to the left
  ;; max-x : () x coordinate furthest to the right
  ;; min-y : () y coordinate furthest to the top( decreasing )
  ;; max-y : () y coordinate furthest to the bottom( increasing )
  ;; collide : (x y shape shape-x shape-y) #t if this touches `shape', else #f
  ;; inside : (x y sx sy) #t if sx sy is inside this shape
  (provide shape^)
  (define shape^
    (interface () min-x max-x min-y max-y collide inside))

  (define Shape
    (class* object% ()
      (super-new)
      (init-field (center-x 0) (center-y 0))))

  (provide Rectangle)
  (define Rectangle
    (class* Shape (shape^)
      (super-new)

      (init-field (width 0) (height 0))

      (define/public (inside x y sx sy)
        (and (<= (- x width) sx)
	     (>= (+ x width) sx)
	     (<= (- y height) sy)
	     (>= (+ y height) sy)))

      (define/public (collide x y shape sx sy)
        (cond
	  ((is-a? shape Point) (inside x y sx sy))
	  ((is-a? shape Rectangle)
	   (let ((shape-w (get-field width shape))
		 (shape-h (get-field height shape)))
	     (or
	       ;; upper right
	       (inside x y (+ sx shape-w) (- sy shape-h))
	       ;; upper left
	       (inside x y (- sx shape-w) (- sy shape-h))
	       ;; lower right
	       (inside x y (+ sx shape-w) (+ sy shape-h))
	       ;; lower left
	       (inside x y (- sx shape-w) (+ sy shape-h)))))
	  (else
	    ;; check some points around the perimeter and the middle
	    (let ((check (lambda (mx my) (send shape inside sx sy mx my))))
	      (or
		;; check middle of him
		(inside x y sx sy)
		;; check middle of us
		(check x y)
		(check (+ x width) (- y height))
		(check (+ x width) y)
		(check (+ x width) (+ y height))
		(check (- x width) (- y height))
		(check (- x width) y)
		(check (- x width) (+ y height))
		(check x (+ y height))
		(check x (- y height)))))))

      (define/public (min-x) (- width))
      (define/public (max-x) (+ width))
      (define/public (min-y) (- height))
      (define/public (max-y) (+ height))

      ))

  (provide Circle)
  (define Circle
    (class* Shape (shape^)

       (super-new)

       (init-field (radius 0))

       (define (dist x1 y1 x2 y2)
	 (let ((x (- x1 x2))
	       (y (- y1 y2)))
	   (sqrt (+ (* x x) (* y y)))))

       (define/public (inside x y sx sy)
          (< (dist x y sx sy) radius))

       (define/public (collide x y shape sx sy)
          (cond
	    ((is-a? shape Point) (inside x y sx sy))
	    ;; let the rectangle handle collision detection
	    ((is-a? shape Rectangle) (send shape collide sx sy this x y))
	    ((is-a? shape Circle)
	     (< (dist x y sx sy)
		(+ radius (get-field radius shape))))
	    (else
	      ;; check a handful of points
	      (call/cc (lambda (n)
			 (for-each
			   (lambda (r)
			     (for-each
			       (lambda (ang)
				 (let-values
				   (((cx cy) (values
					       (+ x (* r (cos (/ (* pi ang) 180))))
					       (+ y (* r (sin (/ (* pi ang) 180)))))))
				   (when (send shape inside sx sy cx cy)
				      (n #t))))
			       (build-list 10 (lambda (q) (* q 36)))))
			   (build-list (round* (/ radius 2)) (lambda (q) (* q 2))))
			 (n #f))))))

       (define/public (min-x) (- radius))
       (define/public (max-x) (+ radius))
       (define/public (min-y) (- radius))
       (define/public (max-y) (+ radius))

       ))

  (provide Point)
  (define Point
    (class* Shape (shape^)
       (super-new)

       (define/public (inside x y sx sy)
         (and (= x sx)
	      (= y sy)))


       (define/public (collide x y shape sx sy)
         (send shape inside sx sy x y))

       (define/public (min-x) 0)
       (define/public (max-x) 0)
       (define/public (min-y) 0)
       (define/public (max-y) 0)))

  (provide Animation)
  (define Animation
    (class* object% ()
      (super-new)

      (init-field (speed 0))

      (field (pics '())
	     (counter 0)
	     (current pics))

      (define/public (add-animation image)
        (if (null? pics)
	  (begin
	    (set! pics (list image))
	    (set! current pics))
	  (set! pics (append pics (list image)))))

      (define/public (draw buffer x y)
	(when (not (null? current))
	  (image-draw buffer (car current)
		      (- x (/ (image-width (car current)) 2))
		      (- y (/ (image-height (car current)) 2)))))

      (define/public (next-animation)
	(set! counter (add1 counter))
	(when (>= counter speed)
	  (set! counter 0)
	  (if (null? current)
	    (set! current pics)
	    (set! current (cdr current)))))

      ))

  (provide make-animation-from-files)
  (define (make-animation-from-files files speed)
    (let ((a (new Animation (speed speed))))
      (for-each (lambda (f)
		  (send a add-animation (image-create-from-file f)))
		files)
      a))

  (define World
    (class* Basic ()

	    (field (objects '()))
	    (field (collider (new binary-space-partition%)))

	    (init-field (width 640)
			(height 480)
			(depth 16)
			(mode 'WINDOWED))

	    (define/public (get-width) width)
	    (define/public (get-height) height)
	    (define/public (get-depth) depth)
	    (define/public (get-mode) mode)

	    (define/override (key keys)
               (for-each (lambda (o) (send** o key this keys))
			 objects))

	    (define/public (add obj)
              (set! objects (cons obj objects)))

	    #;
	    (define/public (start)
               (for-each (lambda (o) (send** o start this)) objects))

	    (define/override (tick)
              (for-each (lambda (o)
			  (send** o tick this)
			  (send collider add-object o))
			objects))

	    (define/override (draw buffer)
              (for-each (lambda (o) (send** o draw this buffer))
			(sort objects
			      (lambda (a b)
				(<= (get-field phase a)
				    (get-field phase b)))
			      )))

	    (define/public (remove obj)
              (set! objects (filter (lambda (x) (not (eqv? x obj))) objects))
	      (for-each (lambda (n) (send** n death this obj)) objects))

	    (define/public (remove-all)
              (set! objects '()))

	    (define/public (get-objects)
              objects)

	    (define/public (get-object pred)
              (let loop ((objs objects))
		(cond
		  ((null? objs) '())
		  ((pred (car objs)) (car objs))
		  (else (loop (cdr objs))))))

	    (define/public (reset-collisions)
              (send collider clear))

	    (define (collision x1 y1 shapes1 x2 y2 shapes2)
	      (call/cc (lambda (k)
			 (for-each
			   (lambda (s1)
			     (for-each
			       (lambda (s2)
				 (when (send s1 collide
					     (+ x1 (get-field center-x s1))
					     (+ y1 (get-field center-y s1))
					     s2
					     (+ x2 (get-field center-x s2))
					     (+ y2 (get-field center-y s2)))
				   (k #t)))
			       shapes2))
			   shapes1)
			 (k #f))))

	    (define/public (collide)
              ;; `pairs' keeps track of objects that touch each other
	      ;; it could be the case that two objects are in two seperate
	      ;; partitions and intersect with each other in both partitions
	      ;; this would result in the 'touch' message being recieved
	      ;; twice by each object. To remedy this each time two objects
	      ;; collide they are stored in a hash table and can only collide
	      ;; if the two objects dont appear in the hash table
              (let ((pairs (make-hash-table 'equal)))
		(send collider iterate
		      (lambda (a b)
			(if (and (send a can-collide b)
				 (send b can-collide a)
				 (not (hash-table-get pairs (cons a b) #f))
				 (not (hash-table-get pairs (cons b a) #f)))
			  (when (collision
				  (get-field x a)
				  (get-field y a)
				  (send** a shapes)
				  (get-field x b)
				  (get-field y b)
				  (send** b shapes))
			    (hash-table-put! pairs (cons a b) #t)
			    (hash-table-put! pairs (cons b a) #t)
			    (send** a touch this b)
			    (send** b touch this a)))))))

	    (begin
	      (super-new))

	    ))

  (define binary-space-partition%
    (class* object% ()
	    (super-new)
	    (public add-object iterate)

	    ;; partitions is a list of things where each thing is
	    ;; a cons cell of x,y coordinates and a list of elements
	    ;; (list (cons x y) obj1 obj2 obj3 ...)
	    (define partitions '())

	    (define partition-size 100)

	    (define/public (clear)
              (set! partitions '()))

	    ;; iterate through the partitions and call some function
	    ;; on each element
	    (define (iterate fun)
	      (for-each (lambda (partition)
			  (let loop ((objs (cdr partition)))
			    (when (not (null? objs))
			      (let loop2 ((objs2 (cdr objs)))
				(when (not (null? objs2))
				  (begin
				    (fun (car objs) (car objs2))
				    (loop2 (cdr objs2)))))
			      (loop (cdr objs)))))
			partitions))

	    ;; lazily make new partitions and/or return a partition
	    ;; for a given coordinate pair (x,y)
	    (define (get-partition x y)
	      (let loop ((ps partitions))
		(cond
		  ((null? ps) (let ((space (list (cons x y))))
				(set! partitions (cons space partitions))
				space))
		  ((let ((coords (caar ps)))
		     (and (= (car coords) x)
			  (= (cdr coords) y)))
		   (car ps))
		  (else (loop (cdr ps))))))

	    (define (add-element-to-partition x y element)
	      (set! partitions
		(let loop ((ps partitions)
			   (so-far '()))
		  (cond
		    ((null? ps) so-far)
		    ((let ((coords (caar ps)))
			(and (= (car coords) x)
			     (= (cdr coords) y)))
		     (loop (cdr ps)
			   (cons (append (car ps) (list element)) so-far)))
		    (else (loop (cdr ps) (cons (car ps) so-far)))))))

	    (define (add-object element)
	      ;; calculate maximum coordinates in each direction
	      ;; minimum x( left )
	      ;; maximum x( right )
	      ;; minimum y( top )
	      ;; maximum y( bottom )
	      (when (not (null? (send** element shapes)))
		(let-values (((left right top bottom)
			      ;; at the end of this loop left, right, top, bottom will
			      ;; correspond to the maximum dimensions this object
			      ;; spans.
			      ;; -inf < left <= 0
			      ;; 0 <= right < +inf
			      ;; -inf < top <= 0
			      ;; 0 <= bottom <= +inf
			      (let loop ((shapes (send** element shapes))
					 (left 0)
					 (right 0)
					 (top 0)
					 (bottom 0))
				(if (null? shapes)
				  (values left right top bottom)
				  (let ((s (car shapes))
					(rest (cdr shapes)))
				    (let ((cx (get-field center-x s))
					  (cy (get-field center-y s)))
				      (loop rest
					    (min left (+ cx (send s min-x)))
					    (max right (+ cx (send s max-x)))
					    (min top (+ cy (send s min-y)))
					    (max bottom (+ cy (send s max-y))))))))))
			    (let* ((ex (round* (get-field x element)))
				   (ey (round* (get-field y element)))
				   (x1 (+ ex left))
				   (y1 (+ ey top))
				   (x2 (+ ex right))
				   (y2 (+ ey top))
				   (x3 (+ ex left))
				   (y3 (+ ey bottom))
				   (x4 (+ ex right))
				   (y4 (+ ey bottom))
				   (pairs (let ((make-pair (lambda (x y)
						      (cons (quotient x partition-size)
							    (quotient y partition-size)))))
					    (let loop ((ps (list
							     (make-pair x1 y1)
							     (make-pair x2 y2)
							     (make-pair x3 y3)
							     (make-pair x4 y4)))
						       (x x1)
						       (y y1))
					      (cond
						((> x x2) ps)
						((> y y4) (loop ps (+ x partition-size) y1))
						(else
						  (loop (cons (make-pair x y)
							      ps)
							x
							(+ y partition-size)))))))

				   ;; list of (x1 mod partition-size, y1 mod partition-size) ...
				   #;
				   (pairs (let* ((q (lambda (n) (quotient n partition-size)))
						 (m (lambda (x y) (cons (q x) (q y)))))
					    (list (m x1 y1)
						  (m x2 y2)
						  (m x3 y3)
						  (m x4 y4)))))
			      ;; filter list into unique coordinate pairs
			      (let ((coordinates (foldl (lambda (x y)
							  (let loop ((ys y))
							    (cond
							      ((null? ys) (cons x y))
							      ((or (not (= (car x) (caar ys)))
								   (not (= (cdr x) (cdar ys))))
							       (loop (cdr ys)))
							      (else y))))
							'()
							pairs)))
				;; for each pair of coodinates add the element to the 
				;; corresponding partition space
				(for-each (lambda (c)
					    (let ((x (car c))
						  (y (cdr c)))
					      ;; TODO - cheap hack to create partition
					      (get-partition (car c) (cdr c))
					      (add-element-to-partition x y element))
					    #;
					    (let ((p (get-partition (car c) (cdr c))))
					      (append! p (list element))))
					  coordinates))))))
	    ))

   (provide make-world)
   (define make-world
     (case-lambda
       (() (new World))
       ((width height) (new World (width width) (height height)))
       ((width height depth) (new World
				  (width width)
				  (height height)
				  (depth depth)))
       ((width height depth mode) (new World
				  (width width)
				  (height height)
				  (depth depth)
				  (mode mode)))))
         
  (provide add-object)
  (define (add-object world n)
    (send world add n))

  (provide (rename mouse-x get-mouse-x)
	   (rename this me)
	   Cosine Sine
	   (rename mouse-y get-mouse-y)
	   (rename mouse-left-click? left-clicking?)
	   (rename mouse-right-click? right-clicking?)
	   (rename mouse-get-mickeys get-mouse-movement))

  (provide constant)
  (define-syntax (constant stx)
    (syntax-case stx ()
      ((_ id val)
       #'(begin
	   (define val* val)
	   (define-syntax id
	     (syntax-id-rules (set!)
               ((set! _ not-allowed)
		(error "Cannot set! constant" 'id))
	       (_ val*)))))))

  ;; a basic object is simply a list of fields and methods
  (provide define-object)
  (define-syntax (define-object stx)
    (syntax-case stx (define)
      ((_ name (inherit ...) (var ...)
	  remaining ...)
       ;; set all fields as #f to begin with
       (with-syntax (((inits ...)
		      (map (lambda (n) (list n #f))
			   (syntax->list #'(var ...))))
		     ((rest ...)
		      (map
			(lambda (stx)
			  ;; #t if the method name is in the base class
			  (define (is-special-method? m)
			    (memq m '(shapes can-collide key
				      touch tick draw)))
			  (syntax-case stx (define)
			    ((define (method args ...) body ...)
			     ;; use define/override or define/public depending
			     ;; on what the method name is
			     (with-syntax
			       ((def (if (is-special-method? 
					   (syntax-e (syntax method)))
				       (syntax define/override)
				       (syntax define/public))))
			       #'(def (method args ...) body ...))
			     #;
			     (syntax-case (syntax method) (shapes can-collide)
			       (shapes #'(define/override (shapes args ...) body ...))
			       (can-collide #'(define/override (can-collide args ...)
							       body ...))
			       (else #'(define/public (method args ...) body ...))))
			    (else stx)))
			(syntax->list (syntax (remaining ...))))))
         #'(define name
	     (class* Basic ()
	       (inherit-field inherit ...)
	       (init-field inits ...)
	       rest ...

	       ;; constructor
	       (begin
		 (super-new)
		 (send** this create))))))
      ))

  
  #|
   The 'generator' syntax provides a convienent object that executes a function
   every N logic loops. For example in the following code every 10 loops "Hello"
   will be printed and every 50 loops "Goodbye" will be printed.

  (define-generator foo
	     (every 10 (lambda (world)
			 (printf "Hello\n")))
	     (every 50 (lambda (world)
			 (printf "Goodbye\n"))))

  The form is (generator <name> (every N func) ...)
  |#

  (provide define-generator)
  (define-syntax (define-generator stx)
    (syntax-case stx (every)
      ((_ name (every time generate) ...)
       (with-syntax (((times ...) 
		      (generate-temporaries
			(syntax->list #'(time ...)))))
         #`(define-object name () (times ...)
                (define (create)
		  (set! times time) ...)

		(define (can-collide obj) #f)

		(define (tick world)
		  (begin
		    (set! times (sub1 times))
		    (when (< times 0)
		      (set! times time)
		      (generate world))) ...)
		)))))

  (provide (rename send** say)
	   is-a?
	   (rename new make))
   
  (define (start-real world first done)
    (easy-init (get-field width world)
	       (get-field height world)
	       (get-field depth world)
	       (get-field mode world))
    (first world)
    (game-loop
      (lambda ()
	;; (printf "Current keys = ~a\n" (current-keys))
	(send world reset-collisions)
	(let ((keys (current-keys)))
	  (when (not (null? keys))
	    (send** world key keys)))
	(send** world tick)
	(send** world collide)
	(keypressed? 'ESC))
      (lambda (buffer)
	(send** world draw buffer))
      (fps 30))
    (done world)
    (easy-exit))

  (provide start)
  (define start
    (case-lambda
      ((world) (start world (lambda (world) (void))))
      ((world first) (start world first (lambda (world) (void))))
      ((world first last) (start-real world first last))))

)
