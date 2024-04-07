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

(defun graph-pack--distance (a b)
  (let* ((dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun graph-pack--score (a b)
  ;; b = a.next
  ;; weighted position (weighed based on rad)
  (let* ((ab (+ (point-r a) (point-r b) 0.0))
	 (dx (/ (+ (* (point-x a) (point-r b))
		   (* (point-x b) (point-r a))) ab))
	 (dy (/ (+ (* (point-y a) (point-r b))
		   (* (point-y b) (point-r a))) ab)))
    ;; distance squared from origin to weighted center
    (+ (* dx dx) (* dy dy))))

(defun graph-pack-enclose (pos &optional image)
  (let* ((n (length pos))
	 (a (pop pos))
	 (b (pop pos))
	 ;; (c (pop pos))
	 (c (car pos))
	 (enc 0)
	 (j 0)
	 p front done)
    (when (> n 0)
      ;; place the first circle
      (setf (point-x a) 0)    
      (setf (point-y a) 0)
      (setq enc (point-r a))

      ;; place second circle
      (when b
	(setf (point-x a) (- (point-r b)))    
	(setf (point-x b) (point-r a))
	(setf (point-y b) 0)
	;; enclosing circle has radius of sum of the 2 radii
	(setq enc (+ (point-r a) (point-r b))))

      (when c
	;; place third circle
	;; b is second circle, a is first, c is third
	;; (graph-pack--place b a c)
	;; (graph-pack--place a b c)
	(message "1 Scores: %s %s %s" a b c)
	(message "Scores: %s %s %s"
		   (graph-pack--score a b)
		   (graph-pack--score c b)
		   (graph-pack--score a c))

	;; initialize front-chain using the first 3 circles
	(setq front (list b a) restart t)
	;; (setq front (list c b a) restart t)

	;; place each circle in turn
	(while (setq p (car pos))
	  (catch 'restart
	    (setq done nil
		  c (pop front)
		  b (car front))
	    (message "Placing %d: %s" j p)
	    (while (not done)
	      ;; attempt to place
	      (graph-pack--place b c p)
	      (graph-pack--draw image (append (list c p) front))
	      
              ;; check if where we added c intersects any circles in the front-chain
	      ;; (message "1 %s\n%s\n%s\n%s %s" b c p a (graph-pack--intersects p a))
	      (if (graph-pack--intersects p a)
		  (progn
		    (throw 'restart "restarting")
		    ;; (error "hit")
		    )
		(setq front (append (list c p) front))
		(setq done t))
	      )
	    (pop pos)
	    (setq j (1+ j))
	    (pp front))
	  ;; (message "2 %s\n%s\n%s\n%s" a b c p)
	  ;; (message "Scores: %s %s %s %s %s"
	  ;; 	   (graph-pack--score b p)
	  ;; 	   (graph-pack--score p c)
	  ;; 	   (and a (graph-pack--score a b))
	  ;; 	   (graph-pack--score b c)
	  ;; 	   (and a (graph-pack--score a c)))
	  ;; (if (and a (< (graph-pack--score b p) (graph-pack--score p c)))
	  ;;     (setq c p) ;; swap a and c
	  ;;   (setq b p)
	  ;;   )
	  ;; (message "3 %s\n%s\n%s\n%s" a b c p)
	  ;; (unless front
	  ;;   (if (> (graph-pack--distance c f) (graph-pack--distance c g))
	  ;; 	(setq a f)
	  ;;     (setq a g)
	  ;;     ))
	  )))
    enc))

(defun graph-pack--draw (image front)
  ""
  (let* ((m 0)
	 (n (length front))
	 p1 p2)
    (when image
      (dolist (i svg--bubble-points)
	(setq m (1+ m))
	(svg-circle image (point-x i) (point-y i) (point-r i)
		    :fill (point-vel-x i)
		    :id m)))
    (dotimes (i n)
      (setq m (1+ m)
	    p1 (nth i front)
	    p2 (nth (if (< (1+ i) n) (1+ i) 0) front))
      (svg-line image
		(point-x p1) (point-y p1)
		(point-x p2) (point-y p2)
		:style "stroke:red;stroke-width:2"
		:id m))

    (svg-possibly-update-image image)
    (sit-for 1)
    ))

;; (defun graph-pack--intersects (a b)
;;   (let* ()
;;     ))

(provide 'graph-pack)
