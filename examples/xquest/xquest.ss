(module xquest mzscheme

(require (lib "class.ss"))
(require (lib "list.ss"))

(require (lib "pregexp.ss"))

;; (require "levels.ss")

(require "../../util.ss"
         "../../sound.ss"
         "../../keyboard.ss"
         (prefix image- "../../image.ss")
         (prefix mouse- "../../mouse.ss"))
; (require (lib "foreign.ss"))

(define screen-width 640)
(define screen-height 480)

(define max-width 1000)
(define max-height 1000)

(define (real->int i)
  (inexact->exact (round i)))

(define (get-fps) (frames-per-second 30))

(define explosion%
  (class* object% ()
    (public draw! act! dead?)
    (init-field (x 0) (y 0) (size 0))

    (define life (real->int (/ size 12)))

    (define (dead?)
      (>= life size))

    (define (act!)
      (set! life (add1 life))
      #;(set! life (real->int (+ life (/ (/ size life) 2)))))

    (define max-colors 20)

    (define explosion-colors (blend-palette 
			       (image-color 230 230 0)
			       (image-color 128 0 0)
			       max-colors))

    ;; draws cocentric circles of varying shade
    (define (draw! buffer) 
      (let loop ((radius life))
	(when (and (> radius 0) (>= radius (/ life 3)))
	  (image-circle-fill buffer x y 
			     (real->int radius)
			     (list-ref explosion-colors
				       (real->int (/ (* (sub1 max-colors) 
							radius)
						     size))))
	  (loop (- radius (/ life 8)))))
      ;; blacken out the middle
      (image-circle-fill buffer x y 
			 (real->int (/ life 3)) (image-color 0 0 0)))

    (super-new)
    ))

(define star-interface (interface () draw!))
(define star%
  (class* object% (star-interface)
    (public draw!)
    (init-field (x 0) (y 0) (color 0))

    (define (draw! buffer)
      (image-putpixel buffer x y (image-color color color color)))

    (super-new)))


(define (make-list num func)
  (let loop ((current '())
	     (num num))
    (if (> num 0)
      (loop (cons (func) current)
	    (- num 1))
      current)))

(define binary-space-partition%
  (class* object% ()
    (super-new)
    (public add-object iterate)

    ;; partitions is a list of things where each thing is
    ;; a cons cell of x,y coordinates and a list of elements
    ;; (list (cons x y) obj1 obj2 obj3 ...)
    (define partitions '())

    (define partition-size 100)

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

    (define (add-object element)
      (let* ((ex (get-field x element))
	     (ey (get-field y element))
	     (size (get-field size element))
	     (x1 (- ex size))
	     (y1 (- ey size))
	     (x2 (+ ex size))
	     (y2 (- ey size))
	     (x3 (- ex size))
	     (y3 (+ ey size))
	     (x4 (- ex size))
	     (y4 (+ ey size))
	     (pairs (list (cons (quotient x1 partition-size) 
				(quotient y1 partition-size))
			  (cons (quotient x2 partition-size) 
				(quotient y2 partition-size))
			  (cons (quotient x3 partition-size) 
				(quotient y3 partition-size))
			  (cons (quotient x4 partition-size) 
				(quotient y4 partition-size)))))
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
                  (let ((p (get-partition (car c) (cdr c))))
                    (append! p (list element))))
                coordinates))))
    ))

(define universe-interface (interface () draw! act!))

(define universe%
  (class* object% (universe-interface)
    ;; draw! act! no soup for you!
    (public draw! act!)
    (super-new)

    (public restrict-x restrict-y)

    ;; our one and only player
    (init-field (player #f) (width max-width) (height max-height) (level #f))

    (define (reset-player obj)
      ((class-field-mutator player% life) obj 1)
      ((class-field-mutator player% x) obj (/ width 2))
      ((class-field-mutator player% y) obj (/ height 2)))

    (define gate #f)

    (define/public (open-gate) (set! gate #t))

    ;; constructor
    (begin
      (reset-player player))

    (define (restrict-x x)
      (min (max x 0) width))

    (define (restrict-y y)
      (min (max y 0) height))

    ;; camera position
    (define x (/ width 2))
    (define y (/ height 2))

    ;; size of game border
    (define border 13)

    ;; false if the player is still inside the zone, true otherwise
    (define complete #f)

    (define/public (complete?) complete)

    ;; length of the gate in each direction
    (define gate-length 40)

    ;; list of explosions
    (define explosions '())

    ;; list of objects in the universe
    (define objects (list player))

    ;; super large buffer to hold everything in the universe
    (define my-buffer (image-create width height))

    (define stars (make-list (real->int (sqrt (* width height)))
			     (lambda ()
			       (new star% 
				       (x (random width))
				       (y (random height))
				       (color (+ 128 (random 64)))))))

    (define/public (enemy-port)
      (if (= (random 2) 1)
	(values (+ border 10) (/ height 2))
	(values (- width border 10) (/ height 2))))

    (define/public (out-of-bounds-x x)
      (or (<= x border) (>= x (- width border))))

    (define/public (out-of-bounds-y y)
      (or (<= y border) (>= y (- height border))))

    (define/public (add-explosion x y size) 
      (set! explosions (cons (new explosion% (x x) (y y) (size (* 3 size)))
			     explosions)))

    (define (draw-stars! buffer)
      (for-each (lambda (s) (send s draw! buffer)) stars))
			
    (define (draw-lines! buffer)
      (let loop ((x 0))
	(when (< x border)
	  (let* ((c (- 255 (* x (real->int (/ 255 border)))))
		 (col (image-color c c c)))
	    (image-line buffer x x (- width x 1) x col)
	    (image-line buffer x (- height x 1) (- width x 1) (- height x 1) col)
	    (image-line buffer (- width x 1) x (- width x 1) (- height x 1) col)
	    (image-line buffer x x x (- height x 1) col))
	  (loop (+ x 1)))))

    (define/public (add-object obj)
      (set! objects (cons obj objects)))

    (define (draw-gate! buffer)
      (let ((mx (/ width 2)))
	(if gate
	  ;; open
	  (image-rectangle-fill buffer 
				(- mx gate-length) 0
				(+ mx gate-length) border
				(image-color 0 0 0))
	  ;; closed
	  (image-rectangle-fill buffer 
				(- mx gate-length) 0
				(+ mx gate-length) border
				(image-color 255 0 0)))))

    (define (draw! buffer) 
      ;; creating sub bitmaps is very inexpensive
      (let ((sub (image-create-sub my-buffer 
				   (- x (/ (image-width buffer) 2))
				   (- y (/ (image-height buffer) 2))
				   (image-width buffer) 
				   (image-height buffer))))
	;(image-clear my-buffer)
	;; only have to clear the part ther user is going to see anyway
	(image-clear sub)
	(draw-stars! my-buffer)
	(draw-lines! my-buffer)
	(draw-gate! my-buffer)
	(for-each (lambda (e) (send e draw! my-buffer)) explosions)
	(for-each (lambda (o) (send o draw! my-buffer)) objects)
	;; copy the viewable area to the screen buffer
      	(image-copy buffer sub)
	(image-destroy sub)))

    (define (move-camera cx cy)
      (set! x (let ((min-x (/ screen-width 2))
		    (max-x (- width (/ screen-width 2))))
		(min (max cx min-x) max-x)))
      (set! y (let ((min-y (/ screen-height 2))
		    (max-y (- height (/ screen-height 2))))
		(min (max cy min-y) max-y))))

    (define (act!) 
      (send level act! this)
      (for-each (lambda (e) (send e act!)) explosions)
      (set! explosions (filter (lambda (e) (not (send e dead?))) explosions))
      (let ((bsp (new binary-space-partition%)))
	(for-each (lambda (o) 
		    (send o act! this)
		    (send bsp add-object o)
		    (let ((x (get-field x o))
			  (y (get-field y o))
			  (size (get-field size o)))
		      (when (or (< (- x size) border)
				(< (- y size) border)
				(> (+ size x) (- width border))
				(> (+ size y) (- height border)))
			(send o out-of-bounds this))))
		  objects)
	(send bsp iterate (lambda (e1 e2)
			    (when (send e1 collision? e2)
			      (send e1 collide e2)
			      (send e2 collide e1)))))
      (set! objects (filter (lambda (o) 
			      (let ((isdead? (send o dead?)))
				(when isdead?
				  (send o died this))
				(not isdead?)))
			    objects))
      (when player
	;; follow the player around
	(move-camera (get-field x player) (get-field y player))
	(when (and gate 
		   (<= (- (get-field y player) (get-field size player)) border)
		   (<= (+ (get-field x player) (get-field size player)) 
		      (+ (/ width 2) gate-length))
		   (>= (- (get-field x player) (get-field size player))
		      (- (/ width 2) gate-length)))
	  (set! complete #t))
	(when (send player dead?)
	  (reset-player player)
	  (add-object player)
	  #;(set! player #f)
	  )))

    ))

(define element-interface (interface () draw! act! dead? collision?))

(define element%
  (class* object% (element-interface)
    (public draw! act! dead? collision? out-of-bounds died hurt)
    (define (draw! buffer) #f)
    (define (act! universe) #f)
    (init-field (life 0) (x 0) (y 0) (size 0) (alliance 'none))

    (define (hurt) 0)

    (define/public (collide element) #f)

    (define (died universe) 0)

    (define (collision? element)
      (if (not (eq? alliance (get-field alliance element)))
	(circle-touch x y size
		      (get-field x element)
		      (get-field y element)
		      (get-field size element))
	#f))

    (define (distance x1 y1 x2 y2)
      (let ((xs (- x2 x1))
	    (ys (- y2 y1)))
	(sqrt (+ (* xs xs) (* ys ys)))))

    (define (rectangle-touch ax1 ay1 ax2 ay2 bx1 by1 bx2 by2)
      (not (or (and (< ax1 bx1) (< ax1 bx2)
		     (< ax2 bx1) (< ax2 bx2))
	       (and (> ax1 bx1) (> ax1 bx2)
		     (> ax2 bx1) (> ax2 bx2))
	       (and (< ay1 by1) (< ay1 by2)
		     (< ay2 by1) (< ay2 by2))
	       (and (> ay1 by1) (> ay1 by2)
		     (> ay2 by1) (> ay2 by2)))))

    (define/public (circle-touch x1 y1 size1 x2 y2 size2)
      (let ((ax1 (- x1 size1))
	    (ax2 (+ x1 size1))
	    (ay1 (- y1 size1))
	    (ay2 (+ y1 size1))
	    (bx1 (- x2 size2))
	    (bx2 (+ x2 size2))
	    (by1 (- y2 size2))
	    (by2 (+ y2 size2)))
	(and (rectangle-touch ax1 ay1 ax2 ay2 bx1 by1 bx2 by2)
	     (< (distance x1 y1 x2 y2) (+ size1 size2)))))

    (define (out-of-bounds this)
      (set! life 0))

    (define (dead?)
      (<= life 0))

    (super-new)))

(define mine%
  (class* element% ()
    (super-new (size 9) (alliance 'enemy) (life 1))
    (override draw! collision? died act! collide hurt)

    (inherit-field x y size alliance life)

    (define (hurt) 1)

    (define ang (random 360))

    (define (collide element)
      (set! life 0))

    (define (get-positions ang len)
      (let ((diff 15))
	(values 
	  (real->int (+ x (* len (Cosine (- ang diff)))))
	  (real->int (+ y (* len (Sine (- ang diff)))))
	  (real->int (+ x (* len (Cosine (+ ang diff)))))
	  (real->int (+ y (* len (Sine (+ ang diff)))))
	  (real->int (+ x (* (+ 3 len) (Cosine ang))))
	  (real->int (+ y (* (+ 3 len) (Sine ang)))))))

    (define (draw! buffer) 
      (image-circle-fill buffer x y (- size 3) (image-color 255 0 0))
      (for-each (lambda (q)
		  (let-values (((x1 y1 x2 y2 x3 y3) (get-positions (+ q ang) (- size 3))))
		    (image-triangle buffer x1 y1 x2 y2 x3 y3 (image-color 255 255 255))))
		(list 0 60 120 180 240 300)))

    (define (act! universe)
      (set! ang (modulo (+ ang 2) 360)))

    (define (collision? element)
      (if (not (is-a? element bullet%))
	(super collision? element)
	#f))

    (define (died universe)
      (send universe add-explosion x y size))
    ))

(define bullet%
  (class* element% ()
    (super-new (size 2) (life 200))
    (override draw! act! collide collision? hurt)

    (define (hurt) 1)

    (init-field (dx 0) (dy 0)) 
    (inherit-field x y size alliance life)

    (define (collision? element) 
      (if (is-a? element breakable-interface)
	(super collision? element)
	#f))
    
    (define (collide element)
      (set! life 0))

    (define fx x)
    (define fy y)

    (define (draw! buffer)
      (image-circle-fill buffer x y size (image-color 255 255 255)))

    (define (act! universe)
      (set! life (sub1 life))
      (set! fx (+ fx dx))
      (set! fy (+ fy dy))
      (set! x (real->int fx))
      (set! y (real->int fy)))))

(define blue-bullet%
  (class* bullet% ()
    (override draw!)
    (super-new)
    (inherit-field x y size)

    (define (draw! buffer)
      (image-circle-fill buffer x y size (image-color 0 64 232)))))

(define breakable-interface (interface ()))

(define player%
  (class* element% (breakable-interface)
    (override draw! act! died collide)

    (public damage!)

    (inherit-field life x y size alliance)
    (super-new (size 10) (life 1) (alliance 'player))

    (define (collide element)
      (damage! (send element hurt)))

    (define (died universe)
      (send universe add-explosion x y size))
    
    (define (damage! damage)
      (set! life (- life damage)))

    ;; speed
    (define dx 0)
    (define dy 0)

    ;; coordinates represented as doubles for extreme accuracy
    (define real-x x)
    (define real-y y)
    
    ;; angle of spinner inside the player's circle
    (define spinner 0)

    (define (draw! buffer)
      (image-circle buffer 
		    x y size
		    (image-color 255 255 255))
      (set! spinner (let ((n (+ 3 spinner)))
		      (if (> n 360) 0 (- n 360))))
      (image-line buffer 
		  (real->int (+ x (* size (Cosine (+ 90 spinner)))))
		  (real->int (+ y (* size (Sine (+ 90 spinner)))))
		  (real->int (- x (* size (Cosine (+ 90 spinner)))))
		  (real->int (- y (* size (Sine (+ 90 spinner)))))
		  (image-color 255 0 0))
      (image-line buffer 
		  (real->int (+ x (* size (Cosine spinner))))
		  (real->int (+ y (* size (Sine spinner))))
		  (real->int (- x (* size (Cosine spinner))))
		  (real->int (- y (* size (Sine spinner))))
		  (image-color 255 0 0)))

    (define (make-bullet)
      (let ((boost 1.5))
	(new blue-bullet% 
	     (alliance alliance)
	     (dx (* boost dx)) (dy (* boost dy))
	     (x x) (y y))))

    (define pressing-left-click #f)

    (define (act! universe)
      (when (and (not pressing-left-click) (mouse-left-click?))
	(set! pressing-left-click #t)
	(send universe add-object (make-bullet)))

      (when (not (mouse-left-click?))
	(set! pressing-left-click #f))

      (let-values (((mx my) (mouse-get-mickeys)))
       (set! dx (+ dx (/ mx 25.0)))
       (set! dy (+ dy (/ my 25.0)))
       (set! real-x (send universe restrict-x (+ x dx)))
       (set! real-y (send universe restrict-y (+ y dy)))
       (set! x (real->int real-x))
       (set! y (real->int real-y))))
    ))

(define crystal%
  (class* element% ()
    (super-new (alliance 'enemy) (size 8) (life 1))

    (override draw! act! collide died)

    (inherit-field x y life size)

    (define ang (random 360))

    (define (collide element)
      (set! life 0))

    (define (draw! buffer) 
      (let ((draw-side (lambda (xang)
			 (let ((x2 (real->int (+ x (* size (Cosine (+ xang ang))))))
			       (color (real->int (- 158 (* 50 (Cosine (+ 90 xang ang)))))))
			   (image-line buffer x (+ y size) x2 y 
				       (image-color color color color))
			   (image-line buffer x (- y size) x2 y 
				       (image-color color color color))))))
	(for-each (lambda (x) (draw-side x)) (list 0 90 180 270))))

    (define (act! universe)
      (set! ang (modulo (+ ang 4) 360)))

    (define (died universe) 0)

    ))

(define level-interface (interface () create-universe complete? get-description))

(define enemy%
  (class* element% (breakable-interface)
    (super-new (alliance 'enemy))
    (override collide hurt act! out-of-bounds died)

    (define (hurt) 1)

    (inherit-field life x y size)

    (init-field (dx 0) (dy 0))

    (define (died universe)
      (send universe add-explosion x y size))

    (define (out-of-bounds universe)
      (when (send universe out-of-bounds-x x)
	(set! dx (- dx)))
      (when (send universe out-of-bounds-y y)
	(set! dy (- dy))))

    (define fx x)
    (define fy y)

    (define (act! universe)
      (set! fx (+ fx dx))
      (set! fy (+ fy dy))
      (set! x (real->int fx))
      (set! y (real->int fy)))

    (define (collide element)
      (set! life 0))
    ))

(define stupid-enemy%
  (class* enemy% ()
    (define (random-speed)
      (* (+ 1 (* 2 (- (random 2) 1)))
	 (+ (/ (random 10) (+ 1 (random 10))) 0.1)))

    (super-new (life 1) (size 8) (dx (random-speed)) (dy (random-speed)))

    (override draw!)

    (inherit-field x y size)

    (define (draw! buffer)
      (image-circle-fill buffer x y size (image-color 255 0 0)))

    ))

(define (make-mine)
  (new mine%
       (x (random max-width))
       (y (random max-height))))

(define (get-levels player)
  (define (get-level-1 player)
    (define crystals 0)

    (define crystal-1%
      (class* crystal% ()
	      (super-new)

	      (override died)

	      (define (died universe)
		(set! crystals (sub1 crystals)))
	      ))

    (define (make-enemy universe)
      (let-values (((x y) (send universe enemy-port)))
		  (new stupid-enemy% (x x) (y y))))

    (define level-1%
      (class* object% (level-interface)
	      (super-new)
	      (init-field (player #f))

	      (define universe #f)

	      (define/public (complete?)
			     (send universe complete?))

	      (define/public (get-description)
"Welcome to Jon Rafkinds XQuest remake in Scheme!
Each level will have a different objective listed on this screen.
For level 1 your objective is to collect all the crystals, which look
like spinning cubes, and make your way to the exit at the top of the
level.
Use the mouse to move around the circle. Left click shoots. Dont
touch anything other than a crystal or you will die.
Press ESC to continue")

			     (define (make-crystal universe)
			       (new crystal-1% 
				    (x (random (get-field width universe)))
				    (y (random (get-field height universe)))))

			     (define/public (act! universe)
					    (when (= (random 30) 4)
					      (send universe add-object (make-enemy universe)))
					    (when (= crystals 0)
					      (send universe open-gate)))

			     (define/public (create-universe)
					    (let ((u (new universe% (width 800) (height 600) (player player) (level this))))
					      (for-each (lambda (n) (send u add-object n))
							(make-list 20 (lambda () (make-mine))))
					      (for-each (lambda (n) 
							  (send u add-object n)
							  (set! crystals (add1 crystals)))
							(make-list 20 (lambda () (make-crystal u))))
					      (set! universe u)
					      u))))

	      (new level-1% (player player)))

      (define (get-level-2 player)
	(define crystals 0)

	(define crystal-1%
	  (class* crystal% ()
		  (super-new)

		  (override died)

		  (define (died universe)
		    (set! crystals (sub1 crystals)))
		  ))

	(define red-bullet%
	  (class* bullet% ()
		  (override draw!)
		  (super-new)
		  (inherit-field x y size)

		  (define (draw! buffer)
		    (image-circle-fill buffer x y size (image-color 255 0 0)))))

	(define shooter-enemy%
	  (class* enemy% ()

		  (define (random-speed)
		    (* (+ 1 (* 2 (- (random 2) 1)))
		       (+ (/ (random 10) (+ 1 (random 10))) 0.1)))

		  (super-new (size 8) (life 1) (dx (random-speed)) (dy (random-speed)))

		  (override draw! act!)

		  (inherit-field x y dx dy size alliance)

		  (define (draw! buffer)
		    (image-circle-fill buffer x y size (image-color 192 64 10)))

		  (define (make-bullet)
		    (let ((dx (random-speed))
			  (dy (random-speed)))
		      (new red-bullet% (x x) (y y) (dx dx) (dy dy) (alliance alliance))))

		  (define (act! universe)
		    (super act! universe)
		    (when (= (random 80) 0)
		      (send universe add-object (make-bullet))))
		  ))

	(define (make-enemy universe)
	  (let-values (((x y) (send universe enemy-port)))
		      (new shooter-enemy% (x x) (y y))))

	(define level-2%
	  (class* object% (level-interface)
		  (super-new)
		  (init-field (player #f))

		  (define universe #f)

		  (define/public (complete?)
				 (send universe complete?))

		  (define/public (get-description)
"Wow you made it past level 1!
Level 2 is basically the same thing except the enemies shoot back at you.
This level is fun because its not just a test to see if you can collect
all the crystals but also due to the large amount of objects created
its also a test of who can use more CPU power. The garbage collector or 
the actual game engine? Find out!
Press ESC to continue")

				 (define (make-crystal universe)
				   (new crystal-1% 
					(x (random (get-field width universe)))
					(y (random (get-field height universe)))))

				 (define/public (act! universe)
						(when (= (random 30) 4)
						  (send universe add-object (make-enemy universe)))
						(when (= crystals 0)
						  (send universe open-gate)))

				 (define/public (create-universe)
						(let ((u (new universe% (width 800) (height 800) (player player) (level this))))
						  (for-each (lambda (n) (send u add-object n))
							    (make-list 10 (lambda () (make-mine))))
						  (for-each (lambda (n) 
							      (send u add-object n)
							      (set! crystals (add1 crystals)))
							    (make-list 30 (lambda () (make-crystal u))))
						  (set! universe u)
						  u))))

		  (new level-2% (player player)))

	  (list get-level-1 get-level-2))

(provide run)
(define (run)
  (easy-init screen-width screen-height 16 'WINDOWED)
  (let* ((player (new player%)))
    (let main-loop ((levels (get-levels player))
		    (continue? #t))
      (when (and continue? (not (null? levels)))
	(let ((level ((car levels) player)))
	  (let ((strs (pregexp-split "\n" (send level get-description))))
	    (game-loop (lambda ()
			 (keypressed? 'ESC))
		       (lambda (buffer)
			 (let loop ((y (/ (image-height buffer) 2))
				    (str strs))
			   (when (not (null? str))
			     (image-print buffer 
					  50 y (image-color 255 255 255) -1
					  (car str))
			     (loop (+ 10 y) (cdr str)))))
		       (fps 20)))
	  (let loop ((x #t))
	    (when x
	      (sleep 0.1)
	      (loop (keypressed? 'ESC))))
	  (let ((universe (send level create-universe)))
	    (game-loop (lambda()
			 #;
			 (when (keypressed? 'S)
			   (save-screen "xquest.bmp"))
			 (send universe act!)
			 (or (keypressed? 'ESC)
			     (send level complete?)))
		       (lambda (buffer)
			 (send universe draw! buffer))
		       (get-fps))))
	(main-loop (cdr levels) (not (keypressed? 'ESC))))))
  (easy-exit))

)
