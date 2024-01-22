;;; gallery.el --- Gallery image viewer for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Anand Tamariya <atamarya@gmail.com>
;; Keywords: gallery, image

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

;; Commentary:
;; Given a URL and an index, view a sequence of images in a gallery view.

(require 'eww)

(defvar gallery--index nil)
(defvar gallery--url nil)

(defun gallery-next ()
  (interactive)
  (setq gallery--index (1+ gallery--index))
  (gallery gallery--url gallery--index))

(defun gallery-previous ()
  (interactive)
  (setq gallery--index (1- gallery--index))
  (gallery gallery--url gallery--index))

(defun gallery (url i)
  (interactive "sURL: \nnIndex: ")
  (setq gallery--index i
	gallery--url url)
  (eww (format url i))
  (gallery-mode))

(defvar gallery-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" 'gallery-next)
    (define-key map "p" 'gallery-previous)
    map))

;;;###autoload
(define-minor-mode gallery-mode
  "Minor mode for viewing images in a gallery."
  :lighter " Gallery"
  :keymap gallery-mode-map
  )


(provide 'gallery)
