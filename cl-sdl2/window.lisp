;; cl-sdl2をロード
(ql:quickload :sdl2)

(defpackage my-game
  (:use :cl
        :sdl2))
(in-package :my-game)

(defparameter *screen-width* 640)  ; 幅
(defparameter *screen-height* 480) ; 高さ

;; メイン関数
(defun main (&key (delay-time 2000))
  (with-init (:everything)
    (with-window (window :title "SDL2 Sample" :w *screen-width* :h *screen-height*)
      (let ((screen-surface (get-window-surface window)))
        (fill-rect screen-surface
                   nil
                   (map-rgb (surface-format screen-surface) #xFF #xFF #xFF))
        (update-window window)
        (delay delay-time)))))

;; 実行
(main)
