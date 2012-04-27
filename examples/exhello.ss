(module exhello mzscheme

(require "../util.ss"
         "../keyboard.ss"
         (prefix image- "../image.ss"))

(provide run)
(define (run)
  (easy-init 320 240 16)
  (image-clear-screen (image-color 255 255 255))
  (image-print-screen 160 120 (image-color 0 0 0) -1 "Hello World!")
  (readkey)
  (easy-exit))

)
