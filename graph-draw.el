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
(defvar graph-draw-style nil)
(defvar graph-draw-padding 0)

(defun graph-draw-circle (image circle)
  (svg-circle image (point-x circle) (point-y circle)
	      (- (point-r circle) graph-draw-padding)
	      :fill graph-draw-fill
	      :id graph-draw--index)
  (setq graph-draw--index (1+ graph-draw--index)))

(defun graph-draw-line (image p1 p2)
  (svg-line image
	    (point-x p1) (point-y p1)
	    (point-x p2) (point-y p2)
	    :stroke graph-draw-fill
	    :stroke-width 2
	    :id graph-draw--index)
  (setq graph-draw--index (1+ graph-draw--index)))

(defun graph-draw-lines-chain (image lines)
  (let* (p)
    (while (and (setq p (pop lines)) lines)
      (graph-draw-line image p (car lines)))
    p))

(defun graph-draw-lines-radial (image center lines)
  (let* (p)
    (while (and (setq p (pop lines)) lines)
      (graph-draw-line image center p))
    p))


(provide 'graph-draw)
