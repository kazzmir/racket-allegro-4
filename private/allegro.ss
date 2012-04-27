(module allegro mzscheme

;; MzScheme bindings to Allegro( http://alleg.sf.net )

(require (lib "foreign.ss")) (unsafe!)
(require (lib "runtime-path.ss"))


;; use `this-expression-source-directory' at some point??

(define-runtime-path native-path
		     (build-path "../native"
				 (system-library-subpath #f)))

;; do any initialization stuff
(define (init)

  ;; OOHH! This is a bad hack..
  (putenv "ALLEGRO_MODULES" (path->string native-path))
  #; (putenv "DLYD_FALLBACK_LIBRARY_PATH" (path->string (build-path *dir* "../allegro-4.2.0/lib/unix")))

  ;; load extra libs
  (case (system-type)
    ;; for now png12 is compiled into the windows port
    #;
    ((windows) (begin
		 (ffi-lib "libz")
		 (ffi-lib (build-path native-path "png12"))))
    ((macosx) (begin
		(ffi-lib "libz")
		(ffi-lib (build-path native-path "libpng12.0.1.2.5"))))
    ((unix) (begin
	      (ffi-lib "libz")
	      (ffi-lib (build-path native-path "libpng12"))))))

(init)

(define libpath
  (let ((lib (case (system-type)
	       ((windows) "alleg42")
	       ((unix) "liballeg-4.2.0")
	       ((macosx) "liballeg-4.2.1"))))
    (build-path native-path lib)))

(define liballegro (ffi-lib libpath))

(define-syntax allegro-func
  (syntax-rules (:)
    [(_ id : x ...)
    (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_"))) liballegro (_fun x ...)) ]))

(define-syntax defallegro
  (syntax-rules (:)
   [(_ id : x ...) (define id (allegro-func id : x ...))]))

#;
(define-syntax defallegro
  (syntax-rules (:)
    [(_ id : x ...)
     (define id
       (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_"))) liballegro (_fun x ...))
       )]))

(define-syntax defallegro*
  (syntax-rules ()
    [(_ name : x ...) (begin (defallegro name : x ...) (provide name))]))

(define _fixed _int)

#|
typedef struct FONT
{
   void *data;
   int height;
   struct FONT_VTABLE *vtable;
} FONT;
|#

(define-cstruct _FONT
  ((data _pointer)
   (height _int)
   (vtable _pointer)))

#|
typedef struct BITMAP            /* a bitmap structure */
{
   int w, h;                     /* width and height in pixels */
   int clip;                     /* flag if clipping is turned on */
   int cl, cr, ct, cb;           /* clip left, right, top and bottom values */
   GFX_VTABLE *vtable;           /* drawing functions */
   void *write_bank;             /* C func on some machines, asm on i386 */
   void *read_bank;              /* C func on some machines, asm on i386 */
   void *dat;                    /* the memory we allocated for the bitmap */
   unsigned long id;             /* for identifying sub-bitmaps */
   void *extra;                  /* points to a structure with more info */
   int x_ofs;                    /* horizontal offset (for sub-bitmaps) */
   int y_ofs;                    /* vertical offset (for sub-bitmaps) */
   int seg;                      /* bitmap segment */
   ZERO_SIZE_ARRAY(unsigned char *, line);
} BITMAP;
|#

(define-cstruct _BITMAP
   ((w _int)
    (h _int)
    (clip _int)
    (cl _int)
    (cr _int)
    (ct _int)
    (cb _int)
    (vtable _pointer)
    (write_bank _pointer)
    (read_bank _pointer)
    (dat _pointer)
    (id _ulong)
    (extra _pointer)
    (x_ofs _int)
    (y_ofs _int)
    (seg _int)
    (line _pointer)))
(provide BITMAP-w BITMAP-h)

#|
typedef struct SAMPLE                  /* a sample */
{
   int bits;                           /* 8 or 16 */
   int stereo;                         /* sample type flag */
   int freq;                           /* sample frequency */
   int priority;                       /* 0-255 */
   unsigned long len;                  /* length (in samples) */
   unsigned long loop_start;           /* loop start position */
   unsigned long loop_end;             /* loop finish position */
   unsigned long param;                /* for internal use by the driver */
   void *data;                         /* sample data */
} SAMPLE;
|#

(define-cstruct _SAMPLE
  ((bits _int)
   (stereo _int)
   (freq _int)
   (priority _int)
   (len _ulong)
   (loop-start _ulong) 
   (loop-end _ulong)
   (param _ulong)
   (data _pointer)))

(define-cstruct _PALETTE
  ((hold _int)))

#|
typedef struct MATRIX_f          /* transformation matrix (floating point) */
{
   float v[3][3];                /* scaling and rotation */
   float t[3];                   /* translation */
} MATRIX_f;
|#
(define-cstruct _MATRIX
  ((data _bytes)))

(define-cstruct _RGB
  ((r _int8)
   (g _int8)
   (b _int8)
   (filler _int8)))
(provide _RGB RGB-tag set-RGB-r! set-RGB-g! set-RGB-b! RGB-r RGB-g RGB-b)

(define (make-list num)
  (let loop ((nums '())
	     (n 0))
    (if (>= n num)
      nums
      (loop (cons 0 nums) (add1 n)))))

(define-cstruct _COLORMAP
  ((data _pointer)))

(define (make-COLORMAP*)
  (let ((f (malloc _ubyte (* 256 256))))
    (cpointer-push-tag! f COLORMAP-tag)
    (set-COLORMAP-data! f f)
    #;
    (set-COLORMAP-data! f (list->cblock (make-list (* 256 256)) _ubyte))
    f)
  #;
  (make-COLORMAP (list->cblock (make-list (* 256 256)) _ubyte)))

(define-cstruct _RGBMAP
  ((data _pointer)))

(define (make-RGBMAP*)
  (let ((f (malloc _ubyte (* 32 32 32))))
    (cpointer-push-tag! f RGBMAP-tag)
    ; (set-RGBMAP-data! f f)
    ; (set-RGBMAP-data! f (malloc _ubyte 'raw (* 32 32 32)))
    ; (ptr-set! (RGBMAP-data f) _ubyte 0 3)
    ; (printf "Byte 0 = ~a\n" (ptr-ref (RGBMAP-data f) _ubyte 0))
    #; (set-RGBMAP-data! f (list->cblock (make-list (* 32 32 32)) _ubyte))
    f)
  #;
  (make-RGBMAP (list->cblock (make-list (* 32 32 32)) _ubyte)))

(define-cstruct _V3D_f
  ((hold _int)))

(define-cstruct _v3d
  ((x _float)
   (y _float)
   (z _float)
   (u _float)
   (v _float)
   (c _int)))

(provide make-v3d _v3d v3d-x v3d-y v3d-z v3d-u v3d-v v3d-c)

(define PolyType
  (_enum '(POLYTYPE-FLAT = 0
           POLYTYPE-GCOL = 1
           POLYTYPE-GRGB = 2
           POLYTYPE-ATEX = 3
           POLYTYPE-PTEX = 4
           POLYTYPE-ATEX-MASK = 5
           POLYTYPE-PTEX-MASK = 6
           POLYTYPE-ATEX-LIT = 7
           POLYTYPE-PTEX-LIT = 8
           POLYTYPE-ATEX-MASK-LIT = 9
           POLYTYPE-PTEX-MASK-LIT = 10
           POLYTYPE-ATEX-TRANS = 11
           POLYTYPE-PTEX-TRANS = 12
           POLYTYPE-ATEX-MASK-TRANS = 13
           POLYTYPE-PTEX-MASK-TRANS = 14
           POLYTYPE-MAX = 15
           POLYTYPE-FLAT/ZBUF = 16
           POLYTYPE-GCOL/ZBUF = 17
           POLYTYPE-GRGB/ZBUF = 18
           POLYTYPE-ATEX/ZBUF = 19
           POLYTYPE-PTEX/ZBUF = 20
           POLYTYPE-ATEX-MASK/ZBUF = 21
           POLYTYPE-PTEX-MASK/ZBUF = 22
           POLYTYPE-ATEX-LIT/ZBUF = 23
           POLYTYPE-PTEX-LIT/ZBUF = 24
           POLYTYPE-ATEX-MASK-LIT/ZBUF = 25
           POLYTYPE-PTEX-MASK-LIT/ZBUF = 26
           POLYTYPE-ATEX-TRANS/ZBUF = 27
           POLYTYPE-PTEX-TRANS/ZBUF = 28
           POLYTYPE-ATEX-MASK-TRANS/ZBUF = 29
           POLYTYPE-PTEX-MASK-TRANS/ZBUF = 30)))

(provide ColorConversion)
(define ColorConversion
  (_bitmask '(NONE       = #x0000000
	      8-TO-15    = #x0000001
	      8-TO-16    = #x0000002
	      8-TO-24    = #x0000004
	      8-TO-32    = #x0000008
	      15-TO-8    = #x0000010
	      15-TO-16   = #x0000020
	      15-TO-24   = #x0000040
	      15-TO-32   = #x0000080
	      16-TO-8    = #x0000100
	      16-TO-15   = #x0000200
	      16-TO-24   = #x0000400
	      16-TO-32   = #x0000800
	      24-TO-8    = #x0001000
	      24-TO-15   = #x0002000
	      24-TO-16   = #x0004000
	      24-TO-32   = #x0008000
	      32-TO-8    = #x0010000
	      32-TO-15   = #x0020000
	      32-TO-16   = #x0040000
	      32-TO-24   = #x0080000
	      32A-TO-8   = #x0100000
	      32A-TO-15  = #x0200000
	      32A-TO-16  = #x0400000
	      32A-TO-24  = #x0800000
	      DITHER-PAL = #x1000000
	      DITHER-HI  = #x2000000
	      KEEP-TRANS = #x4000000)))

(define DITHER (list 'DITHER-HI 'DITHER-PAL))
(provide DITHER)

(define EXPAND-256
  '(8-TO-15
    8-TO-16
    8-TO-24
    8-TO-32))

(define REDUCE-TO-256
  '(15-TO-8
    16-TO-8
    24-TO-8
    32-TO-8
    32A-TO-8))

(define EXPAND-15-TO-16
  '(15-TO-16))

(define REDUCE-16-TO-15
  '(16-TO-15))

(define EXPAND-HI-TO-TRUE
  '(15-TO-24
    15-TO-32
    16-TO-24
    16-TO-32))

(define REDUCE-TRUE-TO-HI
  '(24-TO-15
    24-TO-16
    32-TO-15
    32-TO-16))

(define 24-EQUALS-32
  '(24-TO-32
    32-TO-24))

(provide TOTAL)
(define TOTAL
  (append 
    EXPAND-256
    REDUCE-TO-256
    EXPAND-15-TO-16
    REDUCE-16-TO-15
    EXPAND-HI-TO-TRUE
    REDUCE-TRUE-TO-HI
    24-EQUALS-32
    '(32A-TO-15
      32A-TO-16
      32A-TO-24)))

(define PARTIAL
  '(EXPAND-15-TO-16
    REDUCE-16-TO-15
    24-EQUALS-32))

(define MOST
  '(EXPAND-15-TO-16
    REDUCE-16-TO-15
    EXPAND-HI-TO-TRUE
    REDUCE-TRUE-TO-HI
    24-EQUALS-32))

;; This one needs some work..
#;
(define COLORCONV_KEEP_ALPHA
  '(COLORCONV_TOTAL
     & ~(COLORCONV_32A_TO_8
         COLORCONV_32A_TO_15
         COLORCONV_32A_TO_16
         COLORCONV_32A_TO_24)))

(define (al-id a b c d)
  (define (x n)
    (char->integer (string-ref (symbol->string n) 0)))
  (bitwise-ior (arithmetic-shift (x a) 24)
	       (arithmetic-shift (x b) 16)
	       (arithmetic-shift (x c) 8)
	       (arithmetic-shift (x d) 0)))

; #define AL_ID(a,b,c,d)     (((a)<<24) | ((b)<<16) | ((c)<<8) | (d))
; #define GFX_SAFE           AL_ID('S','A','F','E')
;; this is the definition of safe, if you want it
(define safe (al-id 'S 'A 'F 'E))
(define Gfx-Mode
  (_enum '(TEXT = -1
           AUTO = 0
           FULLSCREEN = 1
           WINDOWED = 2
           SAFE = 1396786757)))

(define SwitchMode
  (_enum '(NONE = 0
           PAUSE = 1
           AMNESIA = 2
           BACKGROUND = 3
           BACKAMNESIA = 4)))

(provide Sound-Mode)
(define Sound-Mode
  (_enum '(AUTODETECT = -1
           NONE = 0))) 

(provide Midi-Mode)
(define Midi-Mode
  (_enum '(AUTODETECT = -1
           NONE = 0)))

(provide mask-color)
(define (mask-color)
  (makecol 255 0 255))

; AL_INLINE(int, install_allegro, (int system_id, int *errno_ptr, AL_METHOD(int, atexit_ptr, (AL_METHOD(void, func, (void))))),
; (defallegro -install-allegro : _int _pointer (_fun (_fun -> _void) -> _int) -> _int)

(defallegro -install-allegro : 
	    (_int = 0) (_pointer = #f) (_pointer = #f) -> _int)

(define (install-allegro)
  (-install-allegro))
(provide install-allegro)

#;
(provide desktop-palette palette-color
	 color-map rgb-map default-font
	 screen mouse-y mouse-x mouse-b)

(provide screen mouse-y mouse-x mouse-b default-font key-shifts)

(define (allegro-parameter name type)
  (make-c-parameter name liballegro type))

#;
(define desktop-palette (ffi-obj-ref "desktop_palette" liballegro))
; (define palette-color (ffi-obj-ref "palette_color" liballegro))
#;
(define palette-color (allegro-parameter "palette_color" _pointer))

;; Allocate rgb-map and color-map once, then copy blocks of 
;; memory when you want to set a new one
;; TODO: free these objects when allegro goes away. use a custodian?
#;
(define rgb-map (let ((f (malloc _RGBMAP (* 32 32 32))))
		  (cpointer-push-tag! f RGBMAP-tag)
		  (set-RGBMAP-data! f f)
		  (set-ffi-obj! "rgb_map" liballegro _RGBMAP-pointer f)
		  f))

#;
(define color-map (let ((f (malloc _ubyte 'raw (* 256 256))))
		    (cpointer-push-tag! f COLORMAP-tag)
		    (set-COLORMAP-data! f f)
		    (set-ffi-obj! "color_map" liballegro _COLORMAP-pointer f)
		    f))

#;
(define memcpy
  (case (system-type)
    ((windows) (lambda (dest src bytes)
		 (let loop ((pos 0))
		   (when (< pos bytes)
		     (ptr-set! dest _byte pos (ptr-ref src _byte))
		     (loop (add1 pos))))))
    ((unix macosx)    (get-ffi-obj "memcpy" #f
			    (_fun _pointer _pointer _int -> _pointer)))))

#;
(provide set-rgb-map!)
#;
(define (set-rgb-map! new)
  (memcpy rgb-map new (* 32 32 32)))

#;
(provide set-color-map!)
#;
(define (set-color-map! new)
  (memcpy color-map new (* 256 256)))

(define KeyShifts
  (_bitmask '(SHIFT    = #x0001
	      CTRL     = #x0002
	      ALT      = #x0004
	      LWIN     = #x0008
	      RWIN     = #x0010
	      MENU     = #x0020
	      COMMAND  = #x0040
	      SCROLOCK = #x0100
	      NUMLOCK  = #x0200
	      CAPSLOCK = #x0400
	      INALTSEQ = #x0800
	      ACCENT1  = #x1000
	      ACCENT2  = #x2000
	      ACCENT3  = #x4000
	      ACCENT4  = #x8000)
	    _int))

(define DrawingMode
  (_enum '(SOLID = 0
           XOR = 1
	   TRANSLUCENT = 5)))

(define default-font (allegro-parameter "font" _FONT-pointer))
(define screen (allegro-parameter "screen" _BITMAP-pointer))
(define key-shifts (allegro-parameter "key_shifts" KeyShifts))
(define mouse-x (allegro-parameter "mouse_x" _int))
(define mouse-y (allegro-parameter "mouse_y" _int))
(define mouse-b (allegro-parameter "mouse_b" _int))

(provide key-list)
(define key-list 
     '([A 1] [B 2] [C 3] [D 4] [E 5] [F 6] [G 7] [H 8] [I 9] [J 10] [K 11]
       [L 12] [M 13] [N 14] [O 15] [P 16] [Q 17] [R 18] [S 19] [T 20] [U 21]
       [V 22] [W 23] [X 24] [Y 25] [Z 26] [NUM-0 27] [NUM-1 28] [NUM-2 29]
       [NUM-3 30] [NUM-4 31] [NUM-5 32] [NUM-6 33] [NUM-7 34] [NUM-8 35]
       [NUM-9 36] [PAD-0 37] [PAD-1 38] [PAD-2 39] [PAD-3 40] [PAD-4 41]
       [PAD-5 42] [PAD-6 43] [PAD-7 44] [PAD-8 45] [PAD-9 46] [F1 47]
       [F2 48] [F3 49] [F4 50] [F5 51] [F6 52] [F7 53] [F8 54] [F9 55]
       [F10 56] [F11 57] [F12 58] [ESC 59] [TILDE 60]
       [MINUS 61] [EQUALS 62] [BACKSPACE 63] [TAB 64] [OPENBRACE 65]
       [CLOSEBRACE 66] [ENTER 67] [COLON 68] [QUOTE 69] [BACKSLASH 70]
       [BACKSLASH2 71] [COMMA 72] [STOP 73] [SLASH 74] [SPACE 75] [INSERT 76]
       [DEL 77] [HOME 78] [END 79] [PGUP 80] [PGDN 81] [LEFT 82] [RIGHT 83]
       [UP 84] [DOWN 85] [SLASH_PAD 86] [ASTERISK 87] [MINUS_PAD 88]
       [PLUS_PAD 89] [DEL_PAD 90] [ENTER_PAD 91] [PRTSCR 92] [PAUSE 93]
       [ABNT_C1 94] [YEN 95] [KANA 96] [CONVERT 97] [NOCONVERT 98] [AT 99]
       [CIRCUMFLEX 100] [COLON2 101] [KANJI 102]
       [EQUALS_PAD 103]  ;; MacOS X
       [BACKQUOTE 104]  ;; MacOS X
       [SEMICOLON 105]  ;; MacOS X
       [COMMAND 106]  ;; MacOS X
       [UNKNOWN1 107]
       [UNKNOWN2 108]
       [UNKNOWN3 109]
       [UNKNOWN4 110]
       [UNKNOWN5 111]
       [UNKNOWN6 112]
       [UNKNOWN7 113]
       [UNKNOWN8 114]

       [MODIFIERS 115]

       [LSHIFT 115]
       [RSHIFT 116]
       [LCONTROL 117]
       [RCONTROL 118]
       [ALT 119]
       [ALTGR 120]
       [LWIN 121]
       [RWIN 122]
       [MENU 123]
       [SCRLOCK 124]
       [NUMLOCK 125]
       [CAPSLOCK 126]
       [MAX 127]))

(define Key 
  (let ((codes (make-hash-table)))
    (for-each
     (lambda (pair)
       (hash-table-put! codes (car pair) (cadr pair))
       (hash-table-put! codes (cadr pair) (car pair)))
     key-list)
    (make-ctype _int
		(lambda (sym)
		  (arithmetic-shift (hash-table-get codes sym) 8))
		(lambda (num)
		  (hash-table-get codes (arithmetic-shift num -8))))))

(define key-array
  (let ([keys (ffi-obj-ref "key" liballegro)]
        [codes (make-hash-table)])
    (for-each
     (lambda (pair) (hash-table-put! codes (car pair) (cadr pair)))
     key-list)
    (lambda (key)
      (not (zero? (ptr-ref keys _byte (hash-table-get codes key)))))))

(provide key-array)

#;
(define key-shifts
  (make-c-parameter "key_shifts" liballegro _int))
#;
(provide key-shifts)

(defallegro* allegro-exit : -> _void)

(defallegro* loadpng-init : -> _int)

(defallegro* get-mouse-mickeys :
	     (x : (_ptr o _int))
	     (y : (_ptr o _int))
	     -> _void
	     -> (values x y))

;; osx stuff
(define-values (osx-startup osx-update osx-update-event osx-update-screen)
	       (if (eq? 'macosx (system-type))
		 (values (allegro-func osx-startup : _string -> _void)
		         (allegro-func osx-update : -> _void)
			 (allegro-func osx-update-event : -> _void)
			 (allegro-func osx-update-screen : -> _void))
		 (values (lambda () (void))
			 (lambda () (void))
			 (lambda () (void))
			 (lambda () (void)))))
(provide osx-startup osx-update osx-update-event osx-update-screen)

; (int, install_keyboard, (void));
(defallegro* install-keyboard : -> _int)

(defallegro* install-timer : -> _int)

; (void, remove_keyboard, (void));
(defallegro remove-keyboard : -> _void)

; (int, poll_keyboard, (void));
(defallegro poll-keyboard : -> _int)

; (int, keyboard_needs_poll, (void));
(defallegro keyboard-needs-poll : -> _int)

; (int, keypressed, (void));
(defallegro* keypressed : -> _bool)

; (int, readkey, (void));
(defallegro* readkey : -> Key)

; (int, ureadkey, (int *scancode));
(defallegro ureadkey : _pointer -> _int)

; (void, simulate_keypress, (int keycode));
(defallegro* simulate-keypress : Key -> _void)

; (void, simulate_ukeypress, (int keycode, int scancode));
(defallegro simulate-ukeypress : _int _int -> _void)

; (void, clear_keybuf, (void));
(defallegro* clear-keybuf : -> _void)

; (void, set_leds, (int leds));
(defallegro set-leds : _int -> _void)

; (void, set_keyboard_rate, (int delay, int repeat));
(defallegro set-keyboard-rate :
   _int _int -> _void)

; (int, scancode_to_ascii, (int scancode));
(defallegro scancode-to-ascii :
   _int -> _int)

; (AL_CONST char *, scancode_to_name, (int scancode));
(defallegro scancode-to-name :
   _int -> _string)

(defallegro* bitmap-mask-color :
	     _BITMAP-pointer -> _int)

(defallegro* position-mouse : _int _int -> _int)

; (int, install_mouse, (void));
(defallegro* install-mouse : -> _int)

; (int, remove_mouse, (void));
(defallegro remove-mouse : -> _int)

; (int, getpixel, (BITMAP *bmp, int x, int y)
(defallegro* getpixel :
   _BITMAP-pointer _int _int -> _int)

; (void, putpixel, (BITMAP *bmp, int x, int y, int color)
(defallegro* putpixel :
   _BITMAP-pointer _int _int _int -> _void)

; (void, vline, (BITMAP *bmp, int x, int y_1, int y2, int color)
(defallegro -allegro-vline :
   _BITMAP-pointer _int _int _int _int -> _void)

; (void, hline, (BITMAP *bmp, int x1, int y, int x2, int color)
(defallegro -allegro-hline :
   _BITMAP-pointer _int _int _int _int -> _void)

; (void, line, (BITMAP *bmp, int x1, int y_1, int x2, int y2, int color)
(defallegro* line :
   _BITMAP-pointer _int _int _int _int _int -> _void)

; (void, fastline, (BITMAP *bmp, int x1, int y_1, int x2, int y2, int color)
(defallegro* fastline :
   _BITMAP-pointer _int _int _int _int _int -> _void)

; (void, rectfill, (BITMAP *bmp, int x1, int y_1, int x2, int y2, int color)
(defallegro* rectfill :
   _BITMAP-pointer _int _int _int _int _int -> _void)

; (void, triangle, (struct BITMAP *bmp, int x1, int y_1, int x2, int y2, int x3, int y3, int color)
(defallegro* triangle :
   _BITMAP-pointer _int _int _int _int _int _int _int -> _void)

; (void, polygon, (BITMAP *bmp, int vertices,  int *points, int color)
(defallegro* polygon :
   _BITMAP-pointer _int _pointer _int -> _void)

; (void, rect, (BITMAP *bmp, int x1, int y_1, int x2, int y2, int color)
(defallegro* rect :
   _BITMAP-pointer _int _int _int _int _int -> _void)

(defallegro* drawing-mode :
   DrawingMode (_BITMAP-pointer/null = #f) (_int = 0) (_int = 0) -> _void)

; (void, circle, (BITMAP *bmp, int x, int y, int radius, int color)
(defallegro* circle :
   _BITMAP-pointer _int _int _int _int -> _void)

; (void, circlefill, (BITMAP *bmp, int x, int y, int radius, int color)
(defallegro* circlefill :
   _BITMAP-pointer _int _int _int _int -> _void)

; (void, ellipse, (BITMAP *bmp, int x, int y, int rx, int ry, int color)
(defallegro* ellipse :
   _BITMAP-pointer _int _int _int _int _int -> _void)

; (void, ellipsefill, (BITMAP *bmp, int x, int y, int rx, int ry, int color)
(defallegro* ellipsefill :
   _BITMAP-pointer _int _int _int _int _int -> _void)

; (void, arc, (BITMAP *bmp, int x, int y, fixed ang1, fixed ang2, int r, int color)
(defallegro* arc :
   _BITMAP-pointer _int _int _fixed _fixed _int _int -> _void)

(defallegro calc-spline :
   _pointer (num : _int) (xs : _pointer) (ys : _pointer) -> _void ->
   (let ((xs* (cblock->list xs _int num))
	 (ys* (cblock->list ys _int num)))
     (map (lambda (x y)
	    (cons x y))
	  xs* ys*)))

(provide (rename calc-spline* calc-spline))
(define (calc-spline* points num)
  (let ((xs (malloc _int num))
	(ys (malloc _int num)))
    (calc-spline (list->cblock points _int) num xs ys)))

; (void, spline, (BITMAP *bmp,  int points[8], int color)
(defallegro* spline :
   _BITMAP-pointer _pointer -> _void)

; (void, floodfill, (BITMAP *bmp, int x, int y, int color)
(defallegro* floodfill :
   _BITMAP-pointer _int _int _int -> _void)

; (void, polygon3d, (BITMAP *bmp, int type, struct BITMAP *texture, int vc, V3D *vtx[])
#;
(defallegro polygon3d :
   _BITMAP-pointer _int _BITMAP-pointer _int _v3d-pointer -> _void)

; (void, polygon3d_f, (BITMAP *bmp, int type, BITMAP *texture, int vc, V3D_f *vtx[])
(provide (rename polygon3d-f polygon3d))
(defallegro* polygon3d-f :
   _BITMAP-pointer PolyType _BITMAP-pointer _int _v3d-pointer -> _void)

; (void, triangle3d, (BITMAP *bmp, int type, BITMAP *texture, V3D *v1, V3D *v2, V3D *v3)
#;
(defallegro triangle3d :
   _BITMAP-pointer _int _BITMAP-pointer _v3d-pointer _v3d-pointer _v3d-pointer -> _void)

; (void, triangle3d_f, (BITMAP *bmp, int type, BITMAP *texture, V3D_f *v1, V3D_f *v2, V3D_f *v3)
(provide (rename triangle3d-f triangle3d))
(defallegro triangle3d-f :
   _BITMAP-pointer PolyType _BITMAP-pointer _v3d-pointer _v3d-pointer _v3d-pointer -> _void)

(defallegro* set-projection-viewport :
   _int _int _int _int -> _void)

; (void, quad3d, (BITMAP *bmp, int type, BITMAP *texture, V3D *v1, V3D *v2, V3D *v3, V3D *v4)
#;
(defallegro quad3d :
   _BITMAP-pointer _int _BITMAP-pointer _v3d-pointer _v3d-pointer _v3d-pointer _v3d-pointer -> _void)

; AL_FUNC(float, polygon_z_normal_f, (AL_CONST V3D_f *v1, AL_CONST V3D_f *v2, AL_CONST V3D_f *v3));
(provide (rename polygon-z-normal-f polygon-z-normal))
(defallegro polygon-z-normal-f :
	    _v3d-pointer _v3d-pointer _v3d-pointer -> _float)

; (void, quad3d_f, (BITMAP *bmp, int type, BITMAP *texture, V3D_f *v1, V3D_f *v2, V3D_f *v3, V3D_f *v4)
(provide (rename quad3d-f quad3d))
(defallegro quad3d-f :
   _BITMAP-pointer PolyType _BITMAP-pointer _v3d-pointer _v3d-pointer _v3d-pointer _v3d-pointer -> _void)

; (void, draw_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y)
(defallegro* draw-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int -> _void)

; (void, draw_sprite_v_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y)
(defallegro* draw-sprite-v-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int -> _void)
; (void, draw_sprite_h_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y)
(defallegro* draw-sprite-h-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int -> _void)
; (void, draw_sprite_vh_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y)
(defallegro* draw-sprite-vh-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int -> _void)

; (void, draw_trans_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y)
(defallegro* draw-trans-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int -> _void)

; (void, draw_lit_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y, int color)
(defallegro* draw-lit-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _int -> _void)

; (void, draw_gouraud_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y, int c1, int c2, int c3, int c4)
(defallegro* draw-gouraud-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _int _int -> _void)

; (void, draw_character_ex, (BITMAP *bmp, BITMAP *sprite, int x, int y, int color, int bg)
(defallegro* draw-character-ex :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int -> _void)

; (void, rotate_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y, fixed angle)
(defallegro* rotate-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _fixed -> _void)

; (void, rotate_sprite_v_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y, fixed angle)
(defallegro* rotate-sprite-v-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int _fixed -> _void)

; (void, rotate_scaled_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y, fixed angle, fixed scale)
(defallegro* rotate-scaled-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _fixed _fixed -> _void)

; (void, rotate_scaled_sprite_v_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y, fixed angle, fixed scale)
(defallegro* rotate-scaled-sprite-v-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int _fixed _fixed -> _void)

; (void, pivot_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y, int cx, int cy, fixed angle)
(defallegro* pivot-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _fixed -> _void)

; (void, pivot_sprite_v_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y, int cx, int cy, fixed angle)
(defallegro* pivot-sprite-v-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _fixed -> _void)

; (void, pivot_scaled_sprite, (BITMAP *bmp, BITMAP *sprite, int x, int y, int cx, int cy, fixed angle, fixed scale)
(defallegro* pivot-scaled-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _fixed _fixed -> _void)

; (void, pivot_scaled_sprite_v_flip, (BITMAP *bmp, BITMAP *sprite, int x, int y, int cx, int cy, fixed angle, fixed scale)
(defallegro* pivot-scaled-sprite-v-flip :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _fixed _fixed -> _void)

; AL_FUNC(void, blit, (struct BITMAP *source, struct BITMAP *dest, int source_x, int source_y, int dest_x, int dest_y, int width, int height));
(defallegro* blit :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _int _int -> _void)

; AL_FUNC(void, masked_blit, (struct BITMAP *source, struct BITMAP *dest, int source_x, int source_y, int dest_x, int dest_y, int width, int height));
(defallegro* masked-blit :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _int _int -> _void)

; AL_FUNC(void, stretch_blit, (struct BITMAP *s, struct BITMAP *d, int s_x, int s_y, int s_w, int s_h, int d_x, int d_y, int d_w, int d_h));
(defallegro* stretch-blit :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _int _int _int _int -> _void)

; AL_FUNC(void, masked_stretch_blit, (struct BITMAP *s, struct BITMAP *d, int s_x, int s_y, int s_w, int s_h, int d_x, int d_y, int d_w, int d_h));
(defallegro* masked-stretch-blit :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int _int _int _int _int -> _void)

; AL_FUNC(void, stretch_sprite, (struct BITMAP *bmp, struct BITMAP *sprite, int x, int y, int w, int h));
(defallegro* stretch-sprite :
   _BITMAP-pointer _BITMAP-pointer _int _int _int _int -> _void)

; (void, _putpixel, (BITMAP *bmp, int x, int y, int color)
(defallegro -putpixel :
   _BITMAP-pointer _int _int _int -> _void)

; (int, _getpixel, (BITMAP *bmp, int x, int y)
(defallegro -getpixel :
   _BITMAP-pointer _int _int -> _int)

; (void, _putpixel15, (BITMAP *bmp, int x, int y, int color)
(defallegro -putpixel15 :
   _BITMAP-pointer _int _int _int -> _void)

; (int, _getpixel15, (BITMAP *bmp, int x, int y)
(defallegro -getpixel15 : _BITMAP-pointer _int _int -> _int)

; (void, _putpixel16, (BITMAP *bmp, int x, int y, int color)
(defallegro -putpixel16 : _BITMAP-pointer _int _int _int -> _void)

; (int, _getpixel16, (BITMAP *bmp, int x, int y)
(defallegro -getpixel16 : _BITMAP-pointer _int _int -> _int)

; (void, _putpixel24, (BITMAP *bmp, int x, int y, int color)
(defallegro -putpixel24 :
   _BITMAP-pointer _int _int _int -> _void)

; (int, _getpixel24, (BITMAP *bmp, int x, int y)
(defallegro -getpixel24 : _BITMAP-pointer _int _int -> _int)

; (void, _putpixel32, (BITMAP *bmp, int x, int y, int color)
(defallegro -putpixel32 : _BITMAP-pointer _int _int _int -> _void)

; (int, _getpixel32, (BITMAP *bmp, int x, int y)
(defallegro -getpixel32 : _BITMAP-pointer _int _int -> _int)

(defallegro* bmp-unwrite-line : _BITMAP-pointer -> _void)
(defallegro* bmp-write-line : _BITMAP-pointer _int -> _pointer)
(defallegro* bmp-read-line : _BITMAP-pointer _int -> _pointer)

; AL_FUNC(void, destroy_gfx_mode_list, (GFX_MODE_LIST *gfx_mode_list));

(defallegro destroy-gfx-mode-list : _pointer -> _void)

; AL_FUNC(void, set_color_depth, (int depth));
(defallegro* set-color-depth : _int -> _void)

; AL_FUNC(int, get_color_depth, (void));
(defallegro get-color-depth : -> _int)

; AL_FUNC(void, set_color_conversion, (int mode));
(defallegro* set-color-conversion : ColorConversion -> _void)

; AL_FUNC(int, get_color_conversion, (void));
(defallegro get-color-conversion : -> ColorConversion)

; AL_FUNC(void, request_refresh_rate, (int rate));
(defallegro request-refresh-rate : _int -> _void)

; AL_FUNC(int, get_refresh_rate, (void));
(defallegro get-refresh-rate : -> _int)

; AL_FUNC(int, set_gfx_mode, (int card, int w, int h, int v_w, int v_h));
(defallegro* set-gfx-mode : Gfx-Mode _int _int _int _int -> _int)

(defallegro* set-display-switch-mode : SwitchMode -> _int)

; AL_FUNC(int, scroll_screen, (int x, int y));
(defallegro scroll-screen : _int _int -> _int)

; AL_FUNC(int, request_scroll, (int x, int y));
(defallegro request-scroll : _int _int -> _int)

; AL_FUNC(int, poll_scroll, (void));
(defallegro poll-scroll : -> _int)

; AL_FUNC(int, show_video_bitmap, (BITMAP *bitmap));
(defallegro show-video-bitmap : _BITMAP-pointer -> _int)

; AL_FUNC(int, request_video_bitmap, (BITMAP *bitmap));
(defallegro request-video-bitmap : _BITMAP-pointer -> _int)

; AL_FUNC(int, enable_triple_buffer, (void));
(defallegro enable-triple-buffer : -> _int)

; AL_FUNC(BITMAP *, create_bitmap, (int width, int height));
(defallegro* create-bitmap : _int _int -> _BITMAP-pointer)

; AL_FUNC(BITMAP *, create_bitmap_ex, (int color_depth, int width, int height));
(defallegro* create-bitmap-ex : _int _int _int -> _BITMAP-pointer)

; AL_FUNC(BITMAP *, create_sub_bitmap, (BITMAP *parent, int x, int y, int width, int height));
(defallegro* create-sub-bitmap :
   _BITMAP-pointer _int _int _int _int -> _BITMAP-pointer)

; AL_FUNC(BITMAP *, create_video_bitmap, (int width, int height));
(defallegro create-video-bitmap : _int _int -> _BITMAP-pointer)

; AL_FUNC(BITMAP *, create_system_bitmap, (int width, int height));
(defallegro create-system-bitmap : _int _int -> _BITMAP-pointer)

; AL_FUNC(void, destroy_bitmap, (BITMAP *bitmap));
(defallegro* destroy-bitmap : _BITMAP-pointer -> _void)

; AL_FUNC(struct BITMAP *, load_bitmap, (AL_CONST char *filename, struct RGB *pal));
(defallegro* load-bitmap : _string _RGB-pointer/null -> _BITMAP-pointer/null)

(defallegro* save-bitmap : _string _BITMAP-pointer _RGB-pointer/null -> _int)

; AL_FUNC(void, set_clip_rect, (BITMAP *bitmap, int x1, int y_1, int x2, int y2));
(defallegro set-clip-rect :
   _BITMAP-pointer _int _int _int _int -> _void)

; AL_FUNC(void, add_clip_rect, (BITMAP *bitmap, int x1, int y_1, int x2, int y2));
(defallegro add-clip-rect :
   _BITMAP-pointer _int _int _int _int -> _void)

; AL_FUNC(void, clear_bitmap, (BITMAP *bitmap));
(defallegro* clear-bitmap : _BITMAP-pointer -> _void)

; AL_METHOD(void, clear_to_color, (struct BITMAP *bitmap, int color));
(defallegro* clear-to-color : _BITMAP-pointer _int -> _void)

(defallegro* acquire-screen : -> _void)
(defallegro* release-screen : -> _void)

; AL_FUNC(void, vsync, (void));
(defallegro vsync : -> _void)

; AL_FUNC(void, set_color, (int idx, AL_CONST RGB *p));
(defallegro set-color :
   _int _RGB-pointer -> _void)

; AL_FUNC(void, set_palette, ( PALETTE * p));
(defallegro* set-palette :
   _RGB-pointer -> _void)

; AL_FUNC(void, set_palette_range, ( PALETTE * p, int from, int to, int retracesync));
(defallegro set-palette-range :
   _RGB-pointer _int _int _int -> _void)

; AL_FUNC(void, get_color, (int idx, RGB *p));
(defallegro get-color :
   _int _RGB-pointer -> _void)

; AL_FUNC(void, get_palette, (PALETTE * p));
(defallegro* get-palette :
   _RGB-pointer -> _void)

; AL_FUNC(void, get_palette_range, (PALETTE * p, int from, int to));
(defallegro get-palette-range :
   _PALETTE-pointer _int _int -> _void)

; AL_FUNC(void, fade_interpolate, ( PALETTE * source,  PALETTE * dest, PALETTE * output, int pos, int from, int to));
(defallegro fade-interpolate :
   _PALETTE-pointer _PALETTE-pointer _PALETTE-pointer _int _int _int -> _void)
; AL_FUNC(void, fade_from_range, ( PALETTE * source,  PALETTE * dest, int speed, int from, int to));
(defallegro fade-from-range :
   _PALETTE-pointer _PALETTE-pointer _int _int _int -> _void)
; AL_FUNC(void, fade_in_range, ( PALETTE * p, int speed, int from, int to));
(defallegro fade-in-range :
   _PALETTE-pointer _int _int _int -> _void)
; AL_FUNC(void, fade_out_range, (int speed, int from, int to));
(defallegro fade-out-range :
   _int _int _int -> _void)
; AL_FUNC(void, fade_from, ( PALETTE * source,  PALETTE * dest, int speed));
(defallegro fade-from :
   _PALETTE-pointer _PALETTE-pointer _int -> _void)
; AL_FUNC(void, fade_in, ( PALETTE * p, int speed));
(defallegro fade-in :
   _PALETTE-pointer _int -> _void)
; AL_FUNC(void, fade_out, (int speed));
(defallegro fade-out :
   _int -> _void)

; AL_FUNC(void, select_palette, ( PALETTE * p));
(defallegro select-palette :
   _PALETTE-pointer -> _void)
; AL_FUNC(void, unselect_palette, (void));
(defallegro unselect-palette : -> _void)

; AL_FUNC(void, generate_332_palette, (PALETTE * pal));
(defallegro generate-332-palette :
   _PALETTE-pointer -> _void)
; AL_FUNC(int, generate_optimized_palette, (struct BITMAP *image, PALETTE * pal,  signed char rsvdcols[256]));
(defallegro generate-optimized-palette :
   _BITMAP-pointer _PALETTE-pointer _pointer -> _int)

; AL_FUNC(void, create_rgb_table, (RGB_MAP *table,  PALETTE * pal, AL_METHOD(void, callback, (int pos))));
(defallegro create-rgb-table :
   _RGBMAP-pointer _RGB-pointer _pointer -> _void)

(provide (rename create-rgb-table- create-rgb-table))
(define (create-rgb-table- pal)
  (let ((rgb (make-RGBMAP*)))
    (create-rgb-table rgb pal #f)
    rgb))

; AL_FUNC(void, create_light_table, (COLOR_MAP *table,  PALETTE * pal, int r, int g, int b, AL_METHOD(void, callback, (int pos))));
(defallegro create-light-table :
   _COLORMAP-pointer _RGB-pointer _int _int _int _pointer -> _void)

(provide (rename create-light-table- create-light-table))
(define (create-light-table- pal r g b)
  (let ((map (make-COLORMAP*)))
    (create-light-table map pal r g b #f)
    map))

; AL_FUNC(void, create_trans_table, (COLOR_MAP *table,  PALETTE * pal, int r, int g, int b, AL_METHOD(void, callback, (int pos))));
(defallegro create-trans-table :
   _int -> _void)
; AL_FUNC(void, create_color_table, (COLOR_MAP *table,  PALETTE * pal, AL_METHOD(void, blend, ( PALETTE * pal, int x, int y, RGB *rgb)), AL_METHOD(void, callback, (int pos))));
(defallegro create-color-table :
   _int -> _void)
; AL_FUNC(void, create_blender_table, (COLOR_MAP *table,  PALETTE * pal, AL_METHOD(void, callback, (int pos))));
(defallegro create-blender-table :
   _int -> _void)

; typedef AL_METHOD(unsigned long, BLENDER_FUNC, (unsigned long x, unsigned long y, unsigned long n));

; AL_FUNC(void, set_blender_mode, (BLENDER_FUNC b15, BLENDER_FUNC b16, BLENDER_FUNC b24, int r, int g, int b, int a));
#;
(defallegro set-blender-mode : 
   _BLENDER_FUNC _BLENDER_FUNC _BLENDER_FUNC _int _int _int _int -> _void)
; AL_FUNC(void, set_blender_mode_ex, (BLENDER_FUNC b15, BLENDER_FUNC b16, BLENDER_FUNC b24, BLENDER_FUNC b32, BLENDER_FUNC b15x, BLENDER_FUNC b16x, BLENDER_FUNC b24x, int r, int g, int b, int a));
#;
(defallegro set-blender-mode-ex :
   _BLENDER_FUNC _BLENDER_FUNC _BLENDER_FUNC _BLENDER_FUNC _BLENDER_FUNC _BLENDER_FUNC _BLENDER_FUNC _int _int _int _int -> _void)

; AL_FUNC(void, set_alpha_blender, (void));
(defallegro* set-alpha-blender : -> _void)
; AL_FUNC(void, set_write_alpha_blender, (void));
(defallegro* set-write-alpha-blender : -> _void)
; AL_FUNC(void, set_trans_blender, (int r, int g, int b, int a));
(defallegro* set-trans-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_add_blender, (int r, int g, int b, int a));
(defallegro* set-add-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_burn_blender, (int r, int g, int b, int a));
(defallegro* set-burn-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_color_blender, (int r, int g, int b, int a));
(defallegro* set-color-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_difference_blender, (int r, int g, int b, int a));
(defallegro* set-difference-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_dissolve_blender, (int r, int g, int b, int a));
(defallegro* set-dissolve-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_dodge_blender, (int r, int g, int b, int a));
(defallegro* set-dodge-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_hue_blender, (int r, int g, int b, int a));
(defallegro* set-hue-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_invert_blender, (int r, int g, int b, int a));
(defallegro* set-invert-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_luminance_blender, (int r, int g, int b, int a));
(defallegro* set-luminance-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_multiply_blender, (int r, int g, int b, int a));
(defallegro* set-multiply-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_saturation_blender, (int r, int g, int b, int a));
(defallegro* set-saturation-blender :
   _int _int _int _int -> _void)
; AL_FUNC(void, set_screen_blender, (int r, int g, int b, int a));
(defallegro* set-screen-blender :
   _int _int _int _int -> _void)

; AL_FUNC(void, hsv_to_rgb, (float h, float s, float v, int *r, int *g, int *b));
(defallegro hsv-to-rgb :
   _float _float _float _pointer _pointer _pointer -> _void)
; AL_FUNC(void, rgb_to_hsv, (int r, int g, int b, float *h, float *s, float *v));
(defallegro rgb-to-hsv :
   _int _int _int _pointer _pointer _pointer -> _void)

; AL_FUNC(int, bestfit_color, ( PALETTE * pal, int r, int g, int b));
(defallegro bestfit-color :
   _PALETTE-pointer _int _int _int -> _int)

; AL_FUNC(int, makecol, (int r, int g, int b));
(defallegro* makecol :
   _int _int _int -> _int)
; AL_FUNC(int, makecol8, (int r, int g, int b));
(defallegro makecol8 :
   _int _int _int -> _int)
; AL_FUNC(int, makecol_depth, (int color_depth, int r, int g, int b));
(defallegro makecol-depth :
   _int _int _int _int -> _int)

; AL_FUNC(int, makeacol, (int r, int g, int b, int a));
(defallegro makeacol :
   _int _int _int _int -> _int)

; AL_FUNC(int, makeacol_depth, (int color_depth, int r, int g, int b, int a));
(defallegro makeacol-depth :
   _int _int _int _int _int -> _int)

; AL_FUNC(int, makecol15_dither, (int r, int g, int b, int x, int y));
(defallegro makecol15-dither :
   _int _int _int _int _int -> _int)
; AL_FUNC(int, makecol16_dither, (int r, int g, int b, int x, int y));
(defallegro makecol16-dither :
   _int _int _int _int _int -> _int)

; AL_FUNC(int, getr, (int c));
(defallegro* getr : _int -> _int)

; AL_FUNC(int, getg, (int c));
(defallegro* getg : _int -> _int)

; AL_FUNC(int, getb, (int c));
(defallegro* getb : _int -> _int)

; AL_FUNC(int, geta, (int c));
(defallegro* geta : _int -> _int)

; AL_FUNC(int, getr_depth, (int color_depth, int c));
(defallegro getr-depth :
   _int _int -> _int)
; AL_FUNC(int, getg_depth, (int color_depth, int c));
(defallegro getg-depth :
   _int _int -> _int)
; AL_FUNC(int, getb_depth, (int color_depth, int c));
(defallegro getb-depth :
   _int _int -> _int)
; AL_FUNC(int, geta_depth, (int color_depth, int c));
(defallegro geta-depth :
   _int _int -> _int)

; AL_FUNC(void, reserve_voices, (int digi_voices, int midi_voices));
(defallegro reserve-voices :
   _int _int -> _void)

; AL_FUNC(void, set_volume_per_voice, (int scale));
(defallegro set-volume-per-voice :
   _int -> _void)

; AL_FUNC(int, install_sound, (int digi, int midi, AL_CONST char *cfg_path));
(defallegro* install-sound : Sound-Mode Midi-Mode (_string = "") -> _int)

; AL_FUNC(void, remove_sound, (void));
(defallegro remove-sound : -> _void)

; AL_FUNC(int, install_sound_input, (int digi, int midi));
(defallegro install-sound-input :
   _int _int -> _int)

; AL_FUNC(void, remove_sound_input, (void));
(defallegro remove-sound-input : -> _void)

; AL_FUNC(void, set_volume, (int digi_volume, int midi_volume));
(defallegro set-volume :
   _int _int -> _void)

; AL_FUNC(void, set_hardware_volume, (int digi_volume, int midi_volume));
(defallegro set-hardware-volume :
   _int _int -> _void)

; AL_FUNC(void, set_mixer_quality, (int quality));
(defallegro set-mixer-quality :
   _int -> _void)

; AL_FUNC(int, get_mixer_quality, (void));
(defallegro get-mixer-quality : -> _int)

; AL_FUNC(int, get_mixer_frequency, (void));
(defallegro get-mixer-frequency : -> _int)

; AL_FUNC(int, get_mixer_bits, (void));
(defallegro get-mixer-bits : -> _int)

; AL_FUNC(int, get_mixer_channels, (void));
(defallegro get-mixer-channels : -> _int)

; AL_FUNC(int, get_mixer_voices, (void));
(defallegro get-mixer-voices : -> _int)

; AL_FUNC(int, get_mixer_buffer_length, (void));
(defallegro get-mixer-buffer-length : -> _int)

; AL_FUNC(int, detect_digi_driver, (int driver_id));
(defallegro detect-digi-driver :
   _int -> _int)

; AL_FUNC(SAMPLE *, load_sample, (AL_CONST char *filename));
(defallegro* load-sample :
   _string -> _SAMPLE-pointer/null)

; AL_FUNC(SAMPLE *, load_wav, (AL_CONST char *filename));
(defallegro* load-wav :
   _string -> _SAMPLE-pointer/null)

; AL_FUNC(SAMPLE *, load_wav_pf, (struct PACKFILE *f));
(defallegro load-wav-pf :
   _pointer -> _SAMPLE-pointer)

; AL_FUNC(SAMPLE *, load_voc, (AL_CONST char *filename));
(defallegro* load-voc :
   _string -> _SAMPLE-pointer)

; AL_FUNC(SAMPLE *, load_voc_pf, (struct PACKFILE *f));
(defallegro load-voc-pf :
   _pointer -> _SAMPLE-pointer)

; AL_FUNC(int, save_sample, (AL_CONST char *filename, SAMPLE *spl));
;; Writes a sample into a file. The output format is determined from the
;; filename extension. At present Allegro does not natively support the
;; writing of any sample formats, so you must register a custom saver routine
;; with register_sample_file_type().
(defallegro save-sample :
   _string _SAMPLE-pointer -> _int)

; AL_FUNC(SAMPLE *, create_sample, (int bits, int stereo, int freq, int len));
(defallegro create-sample :
   _int _int _int _int -> _SAMPLE-pointer)

; AL_FUNC(void, destroy_sample, (SAMPLE *spl));
(defallegro* destroy-sample :
   _SAMPLE-pointer -> _void)

; AL_FUNC(int, play_sample, (AL_CONST SAMPLE *spl, int vol, int pan, int freq, int loop));
(defallegro* play-sample :
   _SAMPLE-pointer _int _int _int _int -> _int)

; AL_FUNC(void, stop_sample, (AL_CONST SAMPLE *spl));
(defallegro* stop-sample :
   _SAMPLE-pointer -> _void)

; AL_FUNC(void, adjust_sample, (AL_CONST SAMPLE *spl, int vol, int pan, int freq, int loop));
(defallegro adjust-sample :
   _SAMPLE-pointer _int _int _int _int -> _void)

; AL_FUNC(int, allocate_voice, (AL_CONST SAMPLE *spl));
(defallegro allocate-voice :
   _SAMPLE-pointer -> _int)

; AL_FUNC(void, deallocate_voice, (int voice));
(defallegro deallocate-voice :
   _int -> _void)

; AL_FUNC(void, reallocate_voice, (int voice, AL_CONST SAMPLE *spl));
(defallegro reallocate-voice :
   _int _SAMPLE-pointer -> _void)

; AL_FUNC(void, release_voice, (int voice));
(defallegro release-voice :
   _int -> _void)

; AL_FUNC(void, voice_start, (int voice));
(defallegro voice-start :
   _int -> _void)

; AL_FUNC(void, voice_stop, (int voice));
(defallegro voice-stop :
   _int -> _void)

; AL_FUNC(void, voice_set_priority, (int voice, int priority));
(defallegro voice-set-priority :
   _int _int -> _void)

; AL_FUNC(SAMPLE *, voice_check, (int voice));
(defallegro voice-check :
   _int -> _SAMPLE-pointer)

; AL_FUNC(void, voice_set_playmode, (int voice, int playmode));
(defallegro voice-set-playmode :
   _int _int -> _void)


; AL_FUNC(int, voice_get_position, (int voice));
(defallegro voice-get-position :
   _int -> _int)

; AL_FUNC(void, voice_set_position, (int voice, int position));
(defallegro voice-set-position :
   _int _int -> _void)


; AL_FUNC(int, voice_get_volume, (int voice));
(defallegro voice-get-volume :
   _int -> _int)

; AL_FUNC(void, voice_set_volume, (int voice, int volume));
(defallegro voice-set-volume :
   _int _int -> _void)

; AL_FUNC(void, voice_ramp_volume, (int voice, int tyme, int endvol));
(defallegro voice-ramp-volume :
   _int _int _int -> _void)

; AL_FUNC(void, voice_stop_volumeramp, (int voice));
(defallegro voice-stop-volumeramp :
   _int -> _void)


; AL_FUNC(int, voice_get_frequency, (int voice));
(defallegro voice-get-frequency :
   _int -> _int)

; AL_FUNC(void, voice_set_frequency, (int voice, int frequency));
(defallegro voice-set-frequency :
   _int _int -> _void)

; AL_FUNC(void, voice_sweep_frequency, (int voice, int tyme, int endfreq));
(defallegro voice-sweep-frequency :
   _int _int _int -> _void)

; AL_FUNC(void, voice_stop_frequency_sweep, (int voice));
(defallegro voice-stop-frequency-sweep :
   _int -> _void)


; AL_FUNC(int, voice_get_pan, (int voice));
(defallegro voice-get-pan :
   _int -> _int)

; AL_FUNC(void, voice_set_pan, (int voice, int pan));
(defallegro voice-set-pan :
   _int _int -> _void)

; AL_FUNC(void, voice_sweep_pan, (int voice, int tyme, int endpan));
(defallegro voice-sweep-pan :
   _int _int _int -> _void)

; AL_FUNC(void, voice_stop_pan_sweep, (int voice));
(defallegro voice-stop-pan-sweep :
   _int -> _void)


; AL_FUNC(void, voice_set_echo, (int voice, int strength, int delay));
(defallegro voice-set-echo :
   _int _int _int -> _void)

; AL_FUNC(void, voice_set_tremolo, (int voice, int rate, int depth));
(defallegro voice-set-tremolo :
   _int _int _int -> _void)

; AL_FUNC(void, voice_set_vibrato, (int voice, int rate, int depth));
(defallegro voice-set-vibrato :
   _int _int _int -> _void)

; AL_FUNC(int, get_sound_input_cap_bits, (void));
(defallegro get-sound-input-cap-bits : -> _int)

; AL_FUNC(int, get_sound_input_cap_stereo, (void));
(defallegro get-sound-input-cap-stereo : -> _int)

; AL_FUNC(int, get_sound_input_cap_rate, (int bits, int stereo));
(defallegro get-sound-input-cap-rate :
   _int _int -> _int)

; AL_FUNC(int, get_sound_input_cap_parm, (int rate, int bits, int stereo));
(defallegro get-sound-input-cap-parm :
   _int _int _int -> _int)

; AL_FUNC(int, set_sound_input_source, (int source));
(defallegro set-sound-input-source :
   _int -> _int)

; AL_FUNC(int, start_sound_input, (int rate, int bits, int stereo));
(defallegro start-sound-input :
   _int _int _int -> _int)

; AL_FUNC(void, stop_sound_input, (void));
(defallegro stop-sound-input : -> _void)

; AL_FUNC(int, read_sound_input, (void *buffer));
(defallegro read-sound-input :
   _pointer -> _int)

; AL_FUNC(void, lock_sample, (struct SAMPLE *spl));
(defallegro lock-sample :
   _SAMPLE-pointer -> _void)

;; void get_transformation_matrix_f(MATRIX_f *m, float scale, float xrot, yrot, zrot, x, y, z);
(defallegro get-transformation-matrix-f :
   _MATRIX-pointer _float _float _float 
   _float _float _float _float -> _void)

(define (make-matrix)
  (let ((m (malloc _MATRIX (* 9 (ctype-sizeof _float)))))
    (cpointer-push-tag! m MATRIX-tag)
    m))

(provide get-transformation-matrix)
(define (get-transformation-matrix scale xrot yrot zrot x y z)
  (let ((m (make-matrix)))
    (get-transformation-matrix-f m scale
				 (+ xrot 0.0)
				 (+ yrot 0.0)
				 (+ zrot 0.0)
				 (+ x 0.0)
				 (+ y 0.0)
				 (+ z 0.0))
    m))

(defallegro get-camera-matrix-f :
   _MATRIX-pointer _float _float _float 
   _float _float _float 
   _float _float _float 
   _float _float -> _void)

(provide get-camera-matrix)
(define (get-camera-matrix x y z xfront yfront zfront
			     xrot yrot zrot field-of-view
			     aspect-ratio)
  (let ((m (make-matrix)))
    (get-camera-matrix-f m x y z xfront yfront zfront
			 xrot yrot zrot field-of-view
			 aspect-ratio)
    m))

(provide apply-matrix)
(define (apply-matrix matrix x y z)
  (apply-matrix-f matrix (+ x 0.0) (+ y 0.0) (+ z 0.0)))

; (provide (rename apply-matrix-f apply-matrix))
(defallegro apply-matrix-f :
  _MATRIX-pointer _float _float _float 
  (x : (_ptr o _float))
  (y : (_ptr o _float))
  (z : (_ptr o _float))
  -> _void
  -> (values x y z))

(provide persp-project)
(define (persp-project x y z)
  (persp-project-f (+ x 0.0) (+ y 0.0) (+ z 0.0)))

; (provide (rename persp-project-f persp-project))
(defallegro persp-project-f :
  _float _float _float 
  (x : (_ptr o _float))
  (y : (_ptr o _float))
  -> _void
  -> (values x y))

(define-syntax textfunction
  (syntax-rules ()
    ((_ id ...)
     (begin
       (defallegro* id : 
		    _BITMAP-pointer _FONT-pointer _string _int _int _int _int -> _void)
       ...))))

; AL_FUNC(void, textout_ex, (struct BITMAP *bmp, AL_CONST struct FONT *f, AL_CONST char *str, int x, int y, int color, int bg)); 
; (defallegro textout-ex : _BITMAP-pointer _FONT-pointer _string _int _int _int _int -> _void)

(textfunction textout-ex textout-centre-ex
	      textout-right-ex textout-justify-ex)

#|
AL_PRINTFUNC(void, textprintf_ex, (struct BITMAP *bmp, AL_CONST struct FONT *f, int x, int y, int color, int bg, AL_CONST char *format, ...), 7, 8);
AL_PRINTFUNC(void, textprintf_centre_ex, (struct BITMAP *bmp, AL_CONST struct FONT *f, int x, int y, int color, int bg, AL_CONST char *format, ...), 7, 8);
AL_PRINTFUNC(void, textprintf_right_ex, (struct BITMAP *bmp, AL_CONST struct FONT *f, int x, int y, int color, int bg, AL_CONST char *format, ...), 7, 8);
AL_PRINTFUNC(void, textprintf_justify_ex, (struct BITMAP *bmp, AL_CONST struct FONT *f, int x1, int x2, int y, int diff, int color, int bg, AL_CONST char *format, ...), 9, 10);
AL_FUNC(int, text_length, (AL_CONST struct FONT *f, AL_CONST char *str));
AL_FUNC(int, text_height, (AL_CONST struct FONT *f));
AL_FUNC(void, destroy_font, (struct FONT *f));
|#

(defallegro* text-length : (font : _FONT-pointer) _string -> _int)
(defallegro* text-height : (font : _FONT-pointer) -> _int)

)

