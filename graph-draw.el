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
(defstruct node value children)
(defstruct point x y r old-x old-y fill title href text)

(defvar graph-draw--index 0)
(defvar graph-draw-fill nil)
(defvar graph-draw-padding 0)

(defun graph-draw-circle (image circle &optional offset)
  (let* ((href (point-href circle))
	 (x (or (car offset) 0))
	 (y (or (cdr offset) 0))
	 (anchor image))
    (setq graph-draw--index (1+ graph-draw--index))
    (when href
      (setq anchor (svg-node image 'a
			     :xlink:href (xml-escape-string href)
			     :xlink:title (and (point-title circle)
					       (xml-escape-string (point-title circle)))
			     :text (and (point-text circle)
					(xml-escape-string (point-text circle)))
			     :id graph-draw--index))
      (setq graph-draw--index (1+ graph-draw--index)))
    (svg-circle anchor (- (point-x circle) x) (- (point-y circle) y)
		(- (point-r circle) graph-draw-padding)
		:fill (or graph-draw-fill (point-fill circle))
		:id graph-draw--index)))

(defun graph-draw-rect (image p)
  (let* ((href (point-href p))
	 (anchor image))
    (setq graph-draw--index (1+ graph-draw--index))
    (when href
      (setq anchor (svg-node image 'a :xlink:href href
			     :xlink:title (point-title p)
			     :id graph-draw--index))
      (setq graph-draw--index (1+ graph-draw--index)))
    (svg-rectangle anchor (point-x p) (point-y p) (point-width p) (point-height p)
		   :fill (or graph-draw-fill (point-fill p))
		   :id graph-draw--index)))

(defun graph-draw-line (image p1 p2 &optional offset)
  (let* ((x (or (car offset) 0))
	 (y (or (cdr offset) 0)))
    (when (and p1 p2)
      (setq graph-draw--index (1+ graph-draw--index))
      (svg-line image
		(- (point-x p1) x) (- (point-y p1) y)
		(- (point-x p2) x) (- (point-y p2) y)
		:stroke graph-draw-fill
		:stroke-width 2
		:id graph-draw--index))))

(defun graph-draw-text (image text p &optional offset)
  (let* ((x (- (point-x p) (or (car offset) 0)))
	 (y (- (point-y p) (or (cdr offset) 0)))
	 (fs 14))
    (dolist (i (split-string text "\n"))
      ;; Break text on new line
      (setq graph-draw--index (1+ graph-draw--index))
      (svg-text image i
		;; 14/2 * (0.5 * text length)
		:x (+ x (* -3.5 (length i)))
                :y y
		:font-size fs
		:font-family "Arial"
		;; :font-weight "Bold"
		:id graph-draw--index)
      (setq y (+ y fs))
      )))

(defun graph-draw-lines-chain (image lines)
  (let* (p)
    (while (and (setq p (pop lines)) lines)
      (graph-draw-line image p (car lines)))
    p))

(defun graph-draw-lines-radial (image center lines &optional offset)
  (let* (p)
    (while (setq p (pop lines))
      (graph-draw-line image center p offset))
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

(defun graph-draw--tree-internal (root image)
  "Draw a bubble graph for ROOT in IMAGE.
ROOT is a tree of NODEs."
  (let* (children p c offset)
    (when (node-p root)
      (setq children (node-children root))
      (dolist (i children)
	(graph-draw--tree-internal i image))

      (setq p (node-value root)
	    children (mapcar 'node-value (node-children root))
	    offset (cons (- (point-old-x p) (point-x p))
			 (- (point-old-y p) (point-y p))))

      (when children
	;; Only draw for non-leaf root
	(setq graph-draw--index (1+ graph-draw--index))
	(svg-circle image (point-x p) (point-y p) (point-r p)
                    :stroke-width 2
                    :stroke "red"
                    :id graph-draw--index
                    :fill "none"))

      ;; Draw circles using offset from circumscribing circle
      ;; We want to use circle fill color
      (setq graph-draw-fill nil)
      (dolist (i children)
	(svg-possibly-update-image image)
	(sit-for .1)
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
      (dolist (i children)
	(when (and (point-title i) (> (point-r i) 30))
	  (graph-draw-text image (point-title i) i offset)))
      )))

(defun graph-draw--tree-convert (root image &optional level)
  "Convert a tree of POINT to tree of NODE."
  (let* (children p nodes)
    (setq level (or level 0))
    (cond ((point-p root)
	   ;; Single root node
           (setq children (list root)
		 nodes (list (make-node :value p))))
          ((point-p (car root))
           (setq children root
		 nodes (mapcar (lambda (a) (make-node :value a)) root)))
          (t
           ;; Get circumscribing circle
           (dolist (child root)
             (setq p (graph-draw--tree-convert child image (1+ level)))
             (push p nodes)
	     (push (node-value p) children))
           (setq children (nreverse children))))

    ;; Place circumscribing circle
    (setq p (graph-enclose (graph-pack children)))
    (setf (point-old-x p) (point-x p))
    (setf (point-old-y p) (point-y p))
    (setf (point-fill p)  "none")
    (setq p (make-node :value p :children nodes))
    p))

;;;###autoload
(defun graph-draw-tree (root image)
  "Draw a bubble graph for ROOT in IMAGE."
  (graph-draw--tree-internal
   (graph-draw--tree-convert root image) image))


(provide 'graph-draw)
