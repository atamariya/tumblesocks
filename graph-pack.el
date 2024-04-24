;;; graph-pack.el --- Convenient graph-pack for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Anand Tamariya <atamarya@gmail.com>
;; Keywords: graph, data visualization

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
(require 'graph-draw)
(require 'graph-enclose)

(defun graph-pack--place (b a c)
  (let* ((dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a)))
	 (d2 (+ (* dx dx) (* dy dy)))
	 a2 b2 x y)
    (if (> d2 0)
	(progn
          ;; desired distance sq between a, c
	  (setq a2 (+ (point-r a) (point-r c))
		a2 (* a2 a2))
	  ;; desired distance sq between b, c
	  (setq b2 (+ (point-r b) (point-r c))
		b2 (* b2 b2))

	  ;; place circle c 
	  (if (> a2 b2)
	      (progn
		(setq x (/ (+ d2 b2 (- a2)) (* 2.0 d2))
		      y (sqrt (max 0 (- (/ b2 d2 1.0) (* x x)))))
		(setf (point-x c) (- (point-x b) (* x dx) (* y dy))) 
		(setf (point-y c) (- (point-y b) (* x dy) (* y dx -1))))
	    (setq x (/ (+ d2 a2 (- b2)) (* 2.0 d2))
		  y (sqrt (max 0 (- (/ a2 d2 1.0) (* x x)))))
	    (setf (point-x c) (+ (point-x a) (* x dx) (* y dy -1))) 
	    (setf (point-y c) (+ (point-y a) (* x dy) (* y dx)))))

      ;; if b and a are the same point just put c somewhere sensible
      (setf (point-x c) (+ (point-x a) (point-r a) (point-r c))) 
      (setf (point-y c) (point-y a))      
      )))

(defun graph-pack--intersects (a b)
  (let* ((dr (+ (point-r a) (point-r b) -1e-3))
	 (dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a))))
    (and (> dr 0) (> (* dr dr) (+ (* dx dx) (* dy dy))))))

;;;###autoload
(defun graph-pack (pos &optional image)
  (let* ((n (length pos))
	 (points pos)
	 (a (pop pos))
	 (b (pop pos))
	 (c (pop pos))
	 ;; (c (car pos))
	 (j 4)
	 p d front done)
    (when (> n 0)
      ;; place the first circle
      (setf (point-x a) 0)    
      (setf (point-y a) 0)
      ;; (setq front (point-r a))
      (push a front)

      ;; place second circle
      (when b
	(setf (point-x a) (- (point-r b)))    
	(setf (point-x b) (point-r a))
	(setf (point-y b) 0)
	;; enclosing circle has radius of sum of the 2 radii
	;; (setq front (+ (point-r a) (point-r b)))
	(push b front))

      (when c
	;; place third circle
	;; b is second circle, a is first, c is third
	(graph-pack--place b a c)

	;; initialize front-chain using the first 3 circles
	(setq front (list c b a))
	(setq d a)

	;; place each circle in turn
	(while (setq p (car pos))
	  (catch 'restart
	    (setq done nil
		  c (pop front)
		  b (car front))
	    (while (not done)
	      ;; attempt to place
	      (graph-pack--place c a p)
	      ;; (message "Placing %d: %s" j p)

              ;; check if where we added c intersects any circles in the front-chain
	      (if (graph-pack--intersects p d)
		  (progn
		    ;; (message "1 %s\n%s\n%s\n%s\n%s" b c p a d)
		    (push c front)
		    (setq front (butlast front) a d)
		    (setq d (nth (- (length front) 2) front))		
		    (throw 'restart "restarting"))

		(setq front (append (list p c) front))
		(setq done t))

	      (graph-pack--draw image points front)
	      )
	    (pop pos)
	    (setq d (nth (- (length front) 2) front))		
	    (setq j (1+ j))
	    ;; (pp front)
	    ))
	))
    ;; Use front chain to calculate a circumscribing circle
    ;; (setq enc (graph-enclose front))
    front))

(defun graph-pack--draw1 (image circles _lines)
  "Draw circle packing with radial lines."
  (let* ((m 0)
	 (graph-draw--index 0)
	 p1)
    (when image
      (setq graph-draw-fill "red")
      (setq p1 (car circles))
      (graph-draw-lines-radial image p1 (cdr circles))

      (dolist (i circles)
	(setq m (1+ m))
	(setq graph-draw-fill (point-fill i))
	(graph-draw-circle image i)
	(svg-text image (number-to-string m) :x (point-x i) :y (point-y i)
		  :id graph-draw--index)
	(setq graph-draw--index (1+ graph-draw--index)))

      (svg-possibly-update-image image)
      (sit-for .1)
      )))

(defun graph-pack--draw (image circles lines)
  "Draw circle packing with front chain."
  (let* ((m 0)
	 (graph-draw--index 0)
	 p1 p2)
    (when image
      (dolist (i circles)
	(setq m (1+ m))
	(setq graph-draw-fill (point-fill i))
	(graph-draw-circle image i)
	(svg-text image (number-to-string m) :x (point-x i) :y (point-y i)
		  :id graph-draw--index)
	(setq graph-draw--index (1+ graph-draw--index)))

      (setq graph-draw-fill "red")
      (setq p1 (car lines)
	    p2 (graph-draw-lines-chain image lines))
      (setq graph-draw-fill "green")
      (graph-draw-line image p1 p2)

      (svg-possibly-update-image image)
      (sit-for .1)
      )))

;;;###autoload
(defun graph-pack-tree (root image)
  "Draw a bubble graph for ROOT in IMAGE."
  (let* (children circles p x y r c)
    ;; Get circumscribing circle
    (dolist (child root)
      (setq p (graph-enclose (graph-pack child)))
      (setf (point-old-x p) (point-x p))
      (setf (point-old-y p) (point-y p))
      (push p children))
    (setq children (nreverse children))

    ;; Place circumscribing circle
    (setq p (graph-enclose (graph-pack children)))
    (setq x (point-x p)
          y (point-y p)
          r (point-r p))
    (svg-circle image x y r
                :stroke-width 2
                :stroke "red"
                :id graph-draw--index
                :fill "none")
    (setq graph-draw--index (1+ graph-draw--index))

    ;; Draw circles using offset from circumscribing circle
    (dotimes (j (length children))
      (setq p (nth j children)
            circles (nth j root)
            x (point-x p)
            y (point-y p)
            r (point-r p)
            offset (if (> (length circles) 1)
                       (cons (- (point-old-x p) (point-x p))
                             (- (point-old-y p) (point-y p)))
		     `(0 . 0)))
      ;; (message "1 %s %s %s %s %s" x y r (- (point-old-x p) (point-x p)) (- (point-old-y p) (point-y p)))
      (svg-circle image x y r
                  :stroke-width 2
                  :stroke "red"
                  :id graph-draw--index
                  :fill "none")
      ;; (setq graph-draw--index (1+ graph-draw--index))
      ;; (setq graph-draw-fill "red")
      ;; (graph-draw-lines-radial image (car circles) (cdr circles) offset)

      (dolist (i circles)
	(setq graph-draw-fill (point-fill i))
	(setq c (graph-draw-circle image i offset))
	(setq graph-draw--index (1+ graph-draw--index))
	(svg-animate c "cx" "1"
		     :id graph-draw--index
		     :from (number-to-string (point-old-x i))
		     :to (number-to-string (- (point-x i) (car offset)))
		     :fill "freeze")
	(setq graph-draw--index (1+ graph-draw--index))
	(svg-animate c "cy" "1"
		     :id graph-draw--index
		     :from (number-to-string (point-old-y i))
		     :to (number-to-string (- (point-y i) (cdr offset)))
		     :fill "freeze")
	(setq graph-draw--index (1+ graph-draw--index)))

      ;; Keep text on the top to preserve legibility
      (dolist (i circles)
	(when (> (point-r i) 30)
	  (graph-draw-text image (point-title i) i offset))
        ;; (sit-for .1)
        ;; (svg-possibly-update-image image)
        ))))


(provide 'graph-pack)
