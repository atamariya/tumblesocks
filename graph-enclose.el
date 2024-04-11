;;; graph-enclose.el --- Convenient graph-enclose for Emacs  -*- lexical-binding:t -*-

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

(defun graph-enclose--not (a b)
  (let* ((dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a)))
	 (dr (- (point-r a) (point-r b)))
	 (d2 (+ (* dx dx) (* dy dy))))
    (or (< dr 0) (< (* dr dr) d2))
    ))

(defun graph-enclose--weak (a b)
  (let* ((dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a)))
	 (dr (+ (point-r a) (- (point-r b)) (* (max (point-r a) (point-r b) 1) 1e-3)))
	 (d2 (+ (* dx dx) (* dy dy))))
    (and (> dr 0) (> (* dr dr) d2))
    ))

(defun graph-enclose--weak-all (a arr)
  (let* ((res t)
	 done)
    (while (and arr (not done))
      (if (not (graph-enclose--weak a (car arr)))
	  (setq res nil done t))
      (setq arr (cdr arr)))
    res))

(defun graph-enclose-2 (a b)
  (let* ((x1 (point-x a))
	 (y1 (point-y a))
	 (r1 (point-r a))
	 (x2 (point-x b))
	 (y2 (point-y b))
	 (r2 (point-r b))
	 (dx (- (point-x b) (point-x a)))
	 (dy (- (point-y b) (point-y a)))
	 (dr (- (point-r b) (point-r a)))
	 (d  (sqrt (+ (* dx dx) (* dy dy)))))
    (make-point :x (/ (+ x1 x2 (* (/ dx d) dr)) 2)
		:y (/ (+ y1 y2 (* (/ dy d) dr)) 2)
		:r (/ (+ d r1 r2) 2))
    ))

(defun graph-enclose-3 (a b c)
  (let* ((x1 (point-x a))
	 (y1 (point-y a))
	 (r1 (point-r a))
	 (x2 (point-x b))
	 (y2 (point-y b))
	 (r2 (point-r b))
	 (x3 (point-x c))
	 (y3 (point-y c))
	 (r3 (point-r c))
	 (a2 (- x1 x2))
	 (a3 (- x1 x3))
	 (b2 (- y1 y2))
	 (b3 (- y1 y3))
	 (c2 (- r2 r1))
	 (c3 (- r3 r1))
	 (d1 (+ (* x1 x1) (* y1 y1) (- (* r1 r1))))
	 (d2 (+ d1 (- (* x2 x2)) (- (* y2 y2)) (* r2 r2)))
	 (d3 (+ d1 (- (* x3 x3)) (- (* y3 y3)) (* r3 r3)))
	 (ab (- (* a3 b2) (* a2 b3)))
	 (xa (- (/ (- (* b2 d3) (* b3 d2)) (* 2 ab)) x1))
	 (ya (- (/ (- (* a3 d2) (* a2 d3)) (* 2 ab)) y1))
	 (xb (/ (- (* b3 c2) (* b2 c3)) ab))
	 (yb (/ (- (* a2 c3) (* a3 c2)) ab))
	 (A  (- (* xb xb) (* yb yb) 1))
	 (B  (* 2 (+ r1 (* xa xb) (* ya yb))))
	 (C  (+ (* xa xa) (* ya ya) (- (* r1 r1))))
	 (r  (- (if (> (abs A) 0)
		    (/ (+ B (sqrt (- (* B B) (* 4 A C)))) (* 2.0 A))
		  (/ C B 1.0)))))
    (make-point :x (+ x1 xa (* xb r))
		:y (+ y1 ya (* yb r))
		:r r)
    ))

(defun graph-enclose--triad (arr)
  (pcase (length arr)
    (1 (nth 0 arr))
    (2 (graph-enclose-2 (nth 0 arr) (nth 1 arr)))
    (3 (graph-enclose-3 (nth 0 arr) (nth 1 arr) (nth 2 arr)))
    ))

(defun graph-enclose--extend (arr p)
  (let* (res)
    (catch 'done
      (when (graph-enclose--weak-all p arr)
	(setq res (list p))
	(throw 'done "Done"))
      
      (dolist (i arr)
	(when (and (graph-enclose--not p i)
		   (graph-enclose--weak-all (graph-enclose-2 i p) arr))
	  (setq res (list i p))
	  (throw 'done "Done")))

      (dolist (i arr)
	(dolist (j (cdr arr))
	  (when (and (graph-enclose--not (graph-enclose-2 i j) p)
		     (graph-enclose--not (graph-enclose-2 i p) j)
		     (graph-enclose--not (graph-enclose-2 j p) i)
		     (graph-enclose--weak-all (graph-enclose-3 i j p) arr))
	    (setq res (list i j p))
	    (throw 'done "Done")
	    ))))
    res))

(defun graph-enclose (circles)
  (let* ((i 0)
	 (n (length circles))
	 arr p e)
    (while (< i n)
      (setq p (nth i circles))
      (if (and e (graph-enclose--weak e p))
	  (setq i (1+ i))
	(setq arr (graph-enclose--extend arr p)
	      e (graph-enclose--triad arr)
	      )))
    e))


(provide 'graph-enclose)
