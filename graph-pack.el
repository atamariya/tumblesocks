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
		(setq x (/ (+ d2 b2 (- a2)) (* 2 d2))
		      y (sqrt (max 0 (- (/ b2 d2) (* x x)))))
		(setf (point-x c) (- (point-x b) (* x dx) (* y dx))) 
		(setf (point-y c) (- (point-y b) (* x dx) (* y dx -1))))
	    (setq x (/ (+ d2 a2 (- b2)) (* 2 d2))
		  y (sqrt (max 0 (- (/ a2 d2) (* x x)))))
	    (setf (point-x c) (- (point-x a) (* x dx) (* y dx -1))) 
	    (setf (point-y c) (- (point-y a) (* x dx) (* y dx)))))

        ;; if b and a are the same point just put c somewhere sensible
	    (setf (point-x c) (+ (point-x a) (point-r a) (point-y c))) 
	    (setf (point-y c) (point-y a))      
	  )))

(defun graph-pack--intersects (a b)
  (let* ((dr (+ (point-r a) (point-r b) -1e-3))
	 (dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a))))
    (> (* dr dr) (+ (* dx dx) (* dy dy)))))

(defun graph-pack--score (a b)
  ;; b = a.next
  ;; weighted position (weighed based on rad)
  (let* ((ab (+ (point-r a) (point-r b)))
	 (dx (/ (+ (* (point-x a) (point-r b))
		   (* (point-x b) (point-r a))) ab))
	 (dy (/ (+ (* (point-y a) (point-r b))
		   (* (point-y b) (point-r a))) ab)))
    ;; distance squared from origin to weighted center
    (+ (* dx dx) (* dy dy))))

(defun graph-pack-enclose (pos)
  (let* ((n (length pos))
	 (a (pop pos))
	 (b (pop pos))
	 (c (pop pos))
	 enc count)
    ;; place the first circle
    (setf (point-x a) 0)    
    (setf (point-y a) 0)
    (setq enc (point-r a))

    ;; place second circle
    (when b
      (setf (point-x a) (- (point-r b)))    
      (setf (point-x b) (point-r a))
      (setf (point-r b) 0)
      ;; enclosing circle has radius of sum of the 2 radii
      (setq enc (+ (point-r a) (point-r b))))

    (when c
      ;; place third circle
      ;; b is second circle, a is first, c is third
      (graph-pack--place b a c)

      ;; initialize front-chain using the first 3 circles
      
      ;; place each circle in turn
      (dolist (p pos)
	(setq count 0)
	(while (> (setq count (1+ count))
		  (* 2 n))
	  ;; attempt to place
	  (graph-pack--place b a p)	

          ;; check if where we added c intersects any circles in the front-chain
	  )))
      enc))

;; (defun graph-pack--intersects (a b)
;;   (let* ()
;;     ))

(provide 'graph-pack)
