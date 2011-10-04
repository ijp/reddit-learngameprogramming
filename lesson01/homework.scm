;; Copyright (C) 2011 Ian Price <ianprice90@googlemail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             (rnrs control))

(define-syntax-rule (+= x s)
  (set! x (+ x s)))

(define-syntax-rule (-= x s)
  (set! x (- x s)))

(define (draw-surface/wrapping dest src x y)
  (let* ((dw (SDL:surface:w dest))
         (dh (SDL:surface:h dest))
         (sw (SDL:surface:w src))
         (sh (SDL:surface:h src))
         (h (+ sh y))
         (w (+ sw x)))
    (cond ((and (> h dh) (> w dw))
           ;; if it is too tall and too wide for the current (x,y)
           ;; coordinates, then wrap around all four corners
           (let* ((h2 (- h dh))
                  (h1 (- sh h2))
                  (w2 (- w dw))
                  (w1 (- sw w2))
                  ;; bottom-right rectangles
                  (src-rect1 (SDL:make-rect 0 0 w1 h1))
                  (dest-rect1 (SDL:make-rect x y w1 h1))
                  ;; top-left rectangles
                  (src-rect2 (SDL:make-rect w1 h1 w2 h2))
                  (dest-rect2 (SDL:make-rect 0 0 w2 h2))
                  ;; top-right rectangles
                  (src-rect3 (SDL:make-rect w1 0 w2 h1))
                  (dest-rect3 (SDL:make-rect 0 y w2 h1))
                  ;; bottom-left rectangles
                  (src-rect4 (SDL:make-rect 0 h1 w1 h2))
                  (dest-rect4 (SDL:make-rect x 0 w1 h2)))
             (SDL:blit-surface src src-rect1 dest dest-rect1)
             (SDL:blit-surface src src-rect2 dest dest-rect2)
             (SDL:blit-surface src src-rect3 dest dest-rect3)
             (SDL:blit-surface src src-rect4 dest dest-rect4)))
          ((> h dh)
           ;; if it is too tall for the current y coordinate, then
           ;; only wrap around the bottom
           (let* ((h2 (- h dh))
                  (h1 (- sh h2))
                  ;; bottom rectangles
                  (src-rect1 (SDL:make-rect 0 0 sw h1))
                  (dest-rect1 (SDL:make-rect x y sw h1))
                  ;; top rectangles
                  (src-rect2 (SDL:make-rect 0 h1 sw h2))
                  (dest-rect2 (SDL:make-rect x 0 sw h2)))
             (SDL:blit-surface src src-rect1 dest dest-rect1)
             (SDL:blit-surface src src-rect2 dest dest-rect2)))
          ((> w dw)
           ;; if it is too wide for the current x coordinate, then
           ;; wrap around the side
           (let* ((w2 (- w dw))
                  (w1 (- sw w2))
                  ;; right hand side rectangles
                  (src-rect1 (SDL:make-rect 0 0 w1 sh))
                  (dest-rect1 (SDL:make-rect x y w1 sh))
                  ;; left hand side rectangles
                  (src-rect2 (SDL:make-rect w1 0 w2 sh))
                  (dest-rect2 (SDL:make-rect 0 y w2 sh)))
             (SDL:blit-surface src src-rect1 dest dest-rect1)
             (SDL:blit-surface src src-rect2 dest dest-rect2)))
          (else
           ;; otherwise just one whole rectangle
           (let ((src-rect (SDL:make-rect 0 0 w h))
                 (dest-rect (SDL:make-rect x y w h)))
             (SDL:blit-surface src src-rect dest dest-rect))))))

(define (loop millisecs thunk)
  (define usecs (* millisecs 1000))
  (define (now)
    (let ((time (gettimeofday)))
      (+ (* (car time) 1000000)
         (cdr time))))
  (let loop ()
    (let ((start (now)))
      (thunk)
      (let* ((end (now))
             (difference (- usecs (- end start))))
        (when (positive? difference)
          (usleep difference))
        (loop)))))

(define frame-rate 30)
(define window #f)
(define icon #f)
(define bg-color #f)
(define speed 15)
(define x-speed 0)
(define y-speed 0)
(define player-x 0)
(define player-y 0)
(define x-max 800)
(define y-max 600)

(define (setup)
  (SDL:init '(SDL_INIT_VIDEO))
  (set! window (SDL:set-video-mode x-max y-max 16 '(SDL_HWSURFACE SDL_DOUBLEBUF)))
  (set! bg-color (SDL:map-rgb (SDL:surface-get-format window) 0 0 0))
  (let ((image (SDL:load-image "monster_01.png")))
    (if image
        (set! icon image)
        (begin
          (display "Error: Couldn't load player icon\n")
          (exit -1)))))


(define (handle e)
  (case (SDL:event:type e)
    ((SDL_KEYDOWN)
     (case (SDL:event:key:keysym:sym e)
       ((SDLK_LEFT) (-= x-speed speed))
       ((SDLK_RIGHT) (+= x-speed speed))
       ((SDLK_UP) (-= y-speed speed))
       ((SDLK_DOWN) (+= y-speed speed))))
    ((SDL_KEYUP)
     (case (SDL:event:key:keysym:sym e)
       ((SDLK_LEFT) (+= x-speed speed))
       ((SDLK_RIGHT) (-= x-speed speed))
       ((SDLK_UP) (+= y-speed speed))
       ((SDLK_DOWN) (-= y-speed speed))))
    ((SDL_QUIT)
     (SDL:quit)
     (exit 0))))

(define (update!)
  (let ((new-x (modulo (+ player-x x-speed) x-max))
        (new-y (modulo (+ player-y y-speed) y-max)))
    (set! player-x new-x)
    (set! player-y new-y)))

(define (render)
  (SDL:fill-rect window #f bg-color)
  (draw-surface/wrapping window icon player-x player-y))

(define (main)
  (setup)
  (loop (floor-quotient 1000 frame-rate)
        (lambda ()
          (let ((e (SDL:make-event)))
            (while (SDL:poll-event e)
              (handle e)))
          (update!)
          (render)
          (SDL:flip))))

(main)
