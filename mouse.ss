(module mouse mzscheme
 
(require (prefix a: "private/allegro.ss"))

(provide left-click?)
(define (left-click?)
  (= (bitwise-and (a:mouse-b) 1) 1))

(provide right-click?)
(define (right-click?)
  (let ((a (= (bitwise-and (a:mouse-b) 2) 2)))
    #;(printf "right-click ~a\n" a)
    a))

(provide (rename a:mouse-x x)
	 (rename a:mouse-y y))

(provide (rename a:get-mouse-mickeys get-mickeys))

)
