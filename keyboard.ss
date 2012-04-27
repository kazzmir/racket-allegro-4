(module keyboard mzscheme

(require (prefix a: "private/allegro.ss"))
(require (lib "list.ss"))

(provide keypressed?)
(define (keypressed? key)
  (a:key-array key))

(define (key-modifiers) (a:key-shifts))

(provide current-keys)
(define (current-keys)
  (append
    (key-modifiers)
    (filter (lambda (x) x)
	    (map (lambda (n) (if (a:key-array (car n)) (car n) #f))
		 a:key-list))))

(provide readkey)
(define (readkey)
  (let loop ((key (a:keypressed)))
    (if key
      (a:readkey)
      (begin
	(sleep 0.001)
	(loop (a:keypressed))))))

(provide key-modifiers
	 (rename a:clear-keybuf clear-keyboard)
	 (rename a:keypressed any-keypressed?)
	 (rename a:simulate-keypress simulate-keypress))

)

