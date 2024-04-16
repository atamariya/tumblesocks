;;; graph-draw.el --- Convenient graph drawing functions for Emacs  -*- lexical-binding:t -*-

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
(defvar graph-draw--index 0)
(defvar graph-draw-fill nil)
(defvar graph-draw-padding 0)

(defun graph-draw-circle (image circle &optional offset)
  (let* ((href (point-href circle))
	 (x (or (car offset) 0))
	 (y (or (cdr offset) 0))
	 (anchor image))
    (when href
      (setq anchor (svg-node image 'a :xlink:href href
			     :xlink:title (point-title circle)
			     :id graph-draw--index))
      (setq graph-draw--index (1+ graph-draw--index)))
    (svg-circle anchor (- (point-x circle) x) (- (point-y circle) y)
		(- (point-r circle) graph-draw-padding)
		:fill (or graph-draw-fill (point-fill circle))
		:id graph-draw--index))
  (setq graph-draw--index (1+ graph-draw--index)))

(defun graph-draw-rect (image p)
  (let* ((href (point-href p))
	 (anchor image))
    (when href
      (setq anchor (svg-node image 'a :xlink:href href
			     :xlink:title (point-title p)
			     :id graph-draw--index))
      (setq graph-draw--index (1+ graph-draw--index)))
    (svg-rectangle anchor (point-x p) (point-y p) (point-width p) (point-height p)
		   :fill (or graph-draw-fill (point-fill p))
		   :id graph-draw--index)
    (setq graph-draw--index (1+ graph-draw--index))))

(defun graph-draw-line (image p1 p2)
  (when (and p1 p2)
    (svg-line image
	      (point-x p1) (point-y p1)
	      (point-x p2) (point-y p2)
	      :stroke graph-draw-fill
	      :stroke-width 2
	      :id graph-draw--index)
    (setq graph-draw--index (1+ graph-draw--index))))

(defun graph-draw-lines-chain (image lines)
  (let* (p)
    (while (and (setq p (pop lines)) lines)
      (graph-draw-line image p (car lines)))
    p))

(defun graph-draw-lines-radial (image center lines)
  (let* (p)
    (while (setq p (pop lines))
      (graph-draw-line image center p))
    p))

(defun graph-pack--place1 (b a c)
  ;; Place c wrt a (in center) and b
  ;; Consider placement in a 3x3 grid. Calculations below are based on screen coordinates.
  (if (not b)
      (progn
	(setf (point-x a) (- (point-width a)))
	(setf (point-x c) 0)
	(setf (point-y c) 0))

  (let* ((ax1 (point-x a))
	 (ay1 (point-y a))
	 (ax2 (+ (point-x a) (point-width a)))
	 (ay2 (+ (point-y a) (point-height a)))
	 (bx1 (point-x b))
	 (by1 (point-y b))
	 (bx2 (+ (point-x b) (point-width b)))
	 (by2 (+ (point-y b) (point-height b)))
	 x y)
    ;; (message "1 %s %s %s %s %s" b a c x y)
    (cond ((and (<= bx2 ax2) (<= by1 ay1))
	   ;; Place in top row
	   (setq x (if (< by1 ay1) bx2 bx1)
		 y (- ay1 (point-height b))))
	  ((and (> bx2 ax2) (< by2 ay2))
	   ;; Place on right
	   (setq x ax2
		 y ay1))
	  ((and (> bx1 ax1) (>= by2 ay2))
	   ;; Place in bottom row
	   (setq x (- (if (> by2 ay2) ax2 bx2) ;(if (> bx1 ax1) bx1 (if (> by2 ay2) ax2 bx2))
		      (point-width c))
		 y ay2))
	  ((<= bx1 ax1)
	   ;; Place on left
	   (setq x (- ax1 (point-width c))
		 y (- (if (< bx1 ax1) ay2 by2) (point-height c)))))
    ;; (message "2 %s %s %s %s %s" b a c x y)
    (setf (point-x c) x)
    (setf (point-y c) y)
    )))
  
(defun graph-pack--intersects1 (a b)
  (let* ((ax1 (point-x a))
	 (ay1 (point-y a))
	 (ax2 (+ (point-x a) (point-width a)))
	 (ay2 (+ (point-y a) (point-height a)))
	 (bx1 (point-x b))
	 (by1 (point-y b))
	 (bx2 (+ (point-x b) (point-width b)))
	 (by2 (+ (point-y b) (point-height b))))
    (or (and (< ax1 bx2) (< bx1 ax2) (< ay1 by2) (< ay2 by1))
	(and (= ax1 bx1) (= ay1 by1)))))

(defun graph-pack--draw1 (image circles lines)
  "Draw rectangle packing with front chain."
  (let* ((m 0)
	 (graph-draw--index 0)
	 p1 p2)
    (when image
      (dolist (i circles)
	(setq m (1+ m))
	(setq graph-draw-fill (point-fill i))
	(graph-draw-rect image i)
	(svg-text image (point-title i) :x (point-x i)
		  :y (+ (point-y i) (point-r i))
		  :font-size (point-r i)
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


(provide 'graph-draw)
