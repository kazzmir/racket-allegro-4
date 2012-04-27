(module util mzscheme

  (require (lib "foreign.ss")) (unsafe!)
  (require (prefix a: "private/allegro.ss"))
  (require (prefix image- "image.ss"))

  (provide makeColor)
  (define makeColor
    (case-lambda
      ((sym)
       (let-values (((r g b) (case sym
			       ((RED) (values 255 0 0))
			       ((DARK-RED) (values 128 0 0))
			       ((GREEN) (values 0 255 0))
			       ((DARK-GREEN) (values 0 128 0))
			       ((BLUE) (values 0 0 255))
			       ((DARK-BLUE) (values 0 0 128))
			       ((WHITE) (values 255 255 255))
			       (else (values 0 0 0)))))
		   (makeColor r g b)))
      ((r g b)
       (image-color r g b))))

  (provide blend-palette)
  (define (blend-palette start-color end-color num-color)
    (let-values ([(sr sg sb) (image-get-rgb start-color)]
                 [(er eg eb) (image-get-rgb end-color)])
      (let loop ([q 0])
        (if (< q num-color)
          (let* ([j (/ q num-color)]
                 [f_r (round (+ sr (* (- er sr) j)))]
                 [f_g (round (+ sg (* (- eg sg) j)))]
                 [f_b (round (+ sb (* (- eb sb) j)))])
            (cons (image-color f_r f_g f_b) (loop (+ q 1))))
          null))))

  (provide (rename a:set-gfx-mode set-gfx-mode)
	   (rename a:set-color-conversion set-color-conversion!)
	   (rename a:set-projection-viewport set-projection-viewport)
	   (rename a:get-transformation-matrix get-transformation-matrix)
	   (rename a:get-camera-matrix get-camera-matrix)
	   (rename a:apply-matrix apply-matrix)
	   (rename a:persp-project persp-project)
	   (rename a:set-color-depth set-color-depth))

  (provide (rename a:set-alpha-blender set-alpha-blender!)
	   (rename a:set-write-alpha-blender set-write-alpha-blender!)
	   (rename a:set-trans-blender set-trans-blender!)
	   (rename a:set-add-blender set-add-blender!)
	   (rename a:set-burn-blender set-burn-blender!)
	   (rename a:set-color-blender set-color-blender!)
	   (rename a:set-difference-blender set-difference-blender!)
	   (rename a:set-dissolve-blender set-dissolve-blender!)
	   (rename a:set-dodge-blender set-dodge-blender!)
	   (rename a:set-hue-blender set-hue-blender!)
	   (rename a:set-invert-blender set-invert-blender!)
	   (rename a:set-luminance-blender set-luminance-blender!)
	   (rename a:set-multiply-blender set-multiply-blender!)
	   (rename a:set-saturation-blender set-saturation-blender!)
	   (rename a:set-screen-blender set-screen-blender!))

  (provide (rename a:polygon-z-normal polygon-z-normal))

  (provide (rename a:DITHER DITHER)
	   (rename a:TOTAL TOTAL))

  (provide allegro-init)
  (define (allegro-init)
    (a:install-allegro))

  (provide (rename a:allegro-exit allegro-exit))

  (provide set-drawing-mode-translucent!
	   set-drawing-mode-solid!
	   set-drawing-mode-xor!)

  (define (set-drawing-mode-translucent!)
    (a:drawing-mode 'TRANSLUCENT))

  (define (set-drawing-mode-solid!)
    (a:drawing-mode 'SOLID))

  (define (set-drawing-mode-xor!)
    (a:drawing-mode 'XOR))

  (provide Cosine Sine)
  (provide M-PI)
  (define M-PI 3.14159265358979323846)
  (define (Cosine x) (cos (/ (* x M-PI) 180)))
  (define (Sine x) (sin (/ (* x M-PI) 180)))

  (provide calculate-angle)
  (define (calculate-angle x1 y1 x2 y2)
    (if (eq? x1 x2)
      (atan 9999999999)
      (let-values ([(rx1 rx2) (if (> x2 x1) (values x2 x1) (values x1 x2))])
                  (atan (/ (- y1 y2) (- rx1 rx2))))))

  (provide calculate-normal-angle)
  (define (calculate-normal-angle x1 y1 x2 y2)
    (let ((ang (if (eq? x1 x2)
                 (atan 9999999999)
                 (cond
                   ((and (< x1 x2) (<= y1 y2)) (* 180 (/ (atan (/ (- y1 y2) (- x1 x2))) M-PI)))
                   ((and (< x1 x2) (> y1 y2)) (+ 360 (* 180 (/ (atan (/ (- y1 y2) (- x1 x2))) M-PI))))
                   ((and (> x1 x2) (<= y1 y2)) (+ 180 (* 180 (/ (atan (/ (- y1 y2) (- x1 x2))) M-PI))))
                   ((and (> x1 x2) (> y1 y2)) (+ 180 (* 180 (/ (atan (/ (- y1 y2) (- x1 x2))) M-PI))))))))
    ang))

  (provide for-each-pixel)
  (define (for-each-pixel image func)
    (let yloop ((y 0))
      (when (< y (image-height image))
	(let xloop ((x 0))
	  (when (< x (image-width image))
	    (func x y)
	    (xloop (add1 x))))
	(yloop (add1 y)))))

  (define current-start 0)
  (define GetTicks current-milliseconds)

  (define osx-running #f)

  (define screen-x 0)
  (define screen-y 0)

  (provide screen-x screen-y)

  (provide easy-init easy-exit)
  (define easy-init
    (case-lambda
      ((width height depth)
       (easy-init width height depth 'WINDOWED))
      ((width height depth mode)
       (begin
       	 (when (eq? (system-type) 'macosx)
	   (set! osx-running #t)
	   (a:osx-startup "mzscheme")
	   #;
	   (a:osx-begin-update)
	   (thread
	    (lambda ()
	     (let loop ((running osx-running))
	      (when running
	       (a:osx-update)
	       (sleep 0.001)
	       (loop osx-running))))))
	 (allegro-init)
	 (a:install-timer)
	 (a:loadpng-init)
	 (a:install-keyboard)
	 (a:install-mouse)
	 (a:install-sound 'AUTODETECT 'NONE)
	 (a:set-color-depth depth)
	 (a:set-gfx-mode mode width height 0 0)
	 (case (system-type)
	   ((macosx) (a:set-display-switch-mode 'NONE))
	   ((windows) (a:set-display-switch-mode 'BACKGROUND)))
	 (set! screen-x width)
	 (set! screen-y height)))))
    
  (define (easy-exit)
    (when (eq? 'macosx (system-type))
      (set! osx-running #f))
    (a:allegro-exit))

  ; (define mouse-x 0)
  ; (define mouse-y 0)
  (provide game-loop)
  (define (game-loop logic! draw! game-delay)
    (let ([buffer (image-create screen-x screen-y)])
      (let loop ([game-time (GetTicks)]
			    [done? #f])
        (when (not done?)
          (let ([now (GetTicks)])
            (if (< (- now game-time) game-delay)
              (begin
                (sleep 0.01)
                (loop game-time done?))
              (let loop2 ([diff (- now game-time)]
                                [xtime game-time]
                                [end? done?]
                                [draw? #f])
                (if (or (< diff game-delay) end?)
                  (begin
                    (when draw?
		      (draw! buffer)
		      (image-copy-screen buffer)
		      #;
		      (when (eq? 'macosx (system-type))
			(a:osx-update-screen))
		      (image-clear buffer))
                    (loop xtime end?))
		  (loop2 (- diff game-delay)
			 (+ xtime game-delay)
			 (logic!)
			 #t)))))))
      (image-destroy buffer)))

  (provide fps frames-per-second)
  (define (fps num)
    (/ 1000 num))
  (define frames-per-second fps)

)
