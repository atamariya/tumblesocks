;;; graph-brain.el --- Convenient graph manipulation functions for Emacs  -*- lexical-binding:t -*-

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
(require 'org-roam)
(require 'graph-pack)

(defvar graph-brain--doc nil)
(defvar graph-brain--buffer nil)
(defvar graph-brain--tags nil)
(defvar graph-brain--view 'graph-brain--all)

(defvar-local graph-brain--image nil)
(defvar-local graph-brain--points nil)

(defun graph-brain--shorten (title)
  "Split words in lines. Truncate second words onwards."
  (with-temp-buffer
    (insert title)
    (goto-char (point-min))
    (forward-word 2)
    (when (> (point-max) (point))
      (delete-region (point) (point-max))
      (insert "..."))
    (forward-word -2)
    (while (re-search-forward "[ -]" nil t)
      (replace-match "\n"))
    (buffer-string)))

(defun graph-brain--search ()
  "Highlight the nodes with a code to enable selection."
  (interactive)
  (let* ((image graph-brain--image)
	 ;; (l (length (dom-children image)))
	 (i 0)
	 g x y r off)
    (dolist (p (dom-by-tag image 'circle))
      (setq g (svg-group image nil :id (format "_g%d" i)))
      (setq i (1+ i))
      (setq r (dom-attr p 'r)
	    off (min (/ r 2) 15)
	    x (dom-attr p 'cx)
	    y (dom-attr p 'cy))
      (svg-rectangle g (- x (/ off 2)) (+ y off) off off
		     :id i
		     :fill "none"
		     :stroke "black"
		     :rx 4 :ry 4)
      (setq i (1+ i))
      (svg-text g (number-to-string i) :id i :x (- x 3) :y (+ y r)))
    (svg-possibly-update-image image)
    ))

(defun graph-brain--open (url)
  (interactive)
  (with-temp-buffer
    (insert url)
    (forward-char -1)
    (org-open-at-point)
    (setq graph-brain--doc (current-buffer))
    ))

(defun graph-brain--tag-add (tags)
  (interactive
   (list (let ((crm-separator "[ 	]*:[ 	]*"))
           (completing-read-multiple "Tag: " (org-roam-tag-completions)))))
  (let* ((points graph-brain--points))
    (dolist (p points)
      (graph-brain--open (point-href p))
      (with-current-buffer graph-brain--doc
	(org-roam-tag-add tags))
      )))

(defun graph-brain--tag-select (tags)
  (interactive
   (list (let ((crm-separator "[ 	]*:[ 	]*"))
           (completing-read-multiple "Tag: " (org-roam-tag-completions)
				     nil t graph-brain--tags))))
  (if (not (get-buffer-window graph-brain--buffer))
      (switch-to-buffer-other-window graph-brain--buffer))
  (setq graph-brain--tags tags)
  (funcall graph-brain--view))

(defun graph-brain--draw (points lines &optional group-fn)
  (interactive)
  (let* ((graph-draw-padding (if group-fn 0 10))
	 image)
    (setq graph-brain--points points
	  graph-brain--image image)
    (setq graph-draw-group group-fn
          graph-draw-group-fn group-fn)
    (setq svg-click-function 'graph-brain--open)

    (setq image (graph-draw-init))
    ;; (setq p (graph-draw-tree points image))
    (setq points (graph-draw--tree-convert points image))
    (dolist (p lines)
      (setq graph-draw-fill "red")
      (graph-draw-line image (car p) (cdr p)))
    (graph-draw--tree-internal points image)
    
    ;; (setq w (* 2 (point-r p))
    ;; 	  h w)
    ;; (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))    

    (svg-possibly-update-image image)
    ))

(defun graph-brain--group-tag ()
  (interactive)
  (let* ((nodes (org-roam-db-query [:select [id title] :from nodes]))
	 (tags  (if (not graph-brain--tags)
		    (org-roam-db-query [:select [tag node-id] :from tags])
		  (org-roam-db-query
		   (vconcat
		    [:select [tags:tag nodes:id] :from nodes
			     :left :join tags :on (= nodes:id tags:node-id)]
		    (vector :where (apply 'list 'and (mapcar (lambda (a) `(= tags:tag ,a))
							     graph-brain--tags)))
		    ))))
	 ;; (tags  (org-roam-db-query [:select [links:type nodes:id] :from nodes
	 ;; 				    :left :outer :join links :on (= nodes:id links:dest)
	 ;; 				    :where (= links:type "id")]))
	 (group (make-hash-table :test 'equal))
	 root points p names)
    (mapc (lambda (a)
	    (if (setq p (gethash (car a) group))
		(puthash (car a) (cons (nth 1 a) p) group)
	      (puthash (car a) (list (nth 1 a)) group)))
	  tags)
    (dolist (j (map-keys group))
      (setq points nil)
      (dolist (i (gethash j group))
	(dolist (p nodes)
	  (if (string= i (car p))
	      (push (make-point :x 0 :y 0 :r 30
				:old-x 0 :old-y 0
				:fill (random-color-html)
				:image (save-window-excursion
					 (with-current-buffer
					     (graph-brain--open (concat "id:" (nth 0 p)))
					   (car (org-property-values "IMAGE"))))
				:href (concat "id:" (nth 0 p))
				:text (nth 1 p)
				:title (graph-brain--shorten (nth 1 p)))
		    points))))
      (push j names)
      (push points root))
    ;; (pp group)
    ;; (pp root)
    ;; (pp names)
    (if (= (length root) 1)
	(setq root (car root)))

    (graph-brain--draw root nil (lambda (d i)
				  ;; (message "%s %s" d i)
				  (if (> d 0)
                                      (format "#%s" (or (nth i names) "Ungrouped")))
				  ))
    (setq graph-brain--view 'graph-brain--group-tag)
    ))

(defun graph-brain--all ()
  (interactive)
  (let* ((nodes (if (not graph-brain--tags)
		    (org-roam-db-query [:select [id title] :from nodes])
		  (org-roam-db-query
		   (vconcat
		    [:select [nodes:id nodes:title tags:tag] :from nodes
			     :left :join tags :on (= nodes:id tags:node-id)]
		    (vector :where (apply 'list 'and (mapcar (lambda (a) `(= tags:tag ,a))
							     graph-brain--tags)))
		    ))))
	 (links (org-roam-db-query [:select [source dest] :from links
					    :where (= type "id")]))
	 (group (make-hash-table :test 'equal))
	 points title pt id lines)
    (dolist (p nodes)
      (setq title (nth 1 p)
	    id (nth 0 p)
	    pt (make-point :x 0 :y 0 :r 30
			   :old-x 0 :old-y 0
			   :fill (random-color-html)
			   :image (save-window-excursion
				    (with-current-buffer
					(graph-brain--open (concat "id:" (nth 0 p)))
				      (car (org-property-values "IMAGE"))))
			   :href (concat "id:" id)
			   :text title
			   :title (graph-brain--shorten title)))
      (puthash id pt group)
      (push pt points))
    ;; (pp points)
    (dolist (p links)
      (push (cons (gethash (car p) group)
		  (gethash (cadr p) group))
	    lines))
    (graph-brain--draw points lines)
    (setq graph-brain--view 'graph-brain--all)
    ))

(defun graph--firefox-bookmarks-list (tag)
  (let* ((buf (get-buffer-create "*bookmarks*")))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (dolist (i (gethash tag graph-brain--doc))
      (insert (format "[[%s][%s]]\n" (cdr i) (car i))))
    (goto-char (point-min))
    ))

;;;###autoload
(defun graph-firefox-bookmarks (filename)
  (interactive "fFirefox bookmarks backup: ")
  ;; (setq filename "~/work/bookmarks-2024-05-06.json")
  (let* ((file (find-file-noselect filename))
	 (json (with-current-buffer file
		 (goto-char (point-min))
		 (json-parse-buffer)))
	 (nodes (json-resolve "children[1].children" json t))
	 (group (make-hash-table :test 'equal))
	 (max 0)
	 points p title)
    (dotimes (i (length nodes))
      (let* ((v (aref nodes i))
	     (tags (json-resolve "tags" v t))
	     (title (json-resolve "title" v t))
	     (uri (json-resolve "uri" v t))
	     (v (cons title uri)))
	(mapc (lambda (tag)
		(if (setq p (gethash tag group))
		    (progn
		      (if (not (string= tag "Ungrouped"))
			  (setq max (max max (1+ (length p)))))
		      (puthash tag (cons v p) group))
		  (puthash tag (list v) group)))
		(if (not tags) '("Ungrouped") (split-string tags ",")))))

    (maphash (lambda (k v)
	       ;; (message "%s %s %s" max k v)
	       (setq title (format "%s (%d)" k (length v)))
	       (push (make-point :x 0 :y 0 :r (+ 30 (length v)) ;(/ (* 100 (length v)) max) 
				 :old-x 0 :old-y 0
				 :fill (random-color-html)
				 :href k
				 :text title
				 :title title)
		     points))
	     group)
    (setq points (sort points (lambda (a b) (> (point-r a) (point-r b)))))

    (setq graph-brain--buffer (get-buffer-create "*graph*"))
    (switch-to-buffer-other-window graph-brain--buffer)
    (graph-brain-mode)
    
    (graph-brain--draw points nil)
    (setq svg-click-function 'graph--firefox-bookmarks-list
	  graph-brain--doc group)    
    ))

(defvar graph-brain-mode-map (let* ((map (make-sparse-keymap)))
			       (define-key map "/" 'graph-brain--search)
			       (define-key map "g" 'graph-brain--group-tag)
			       (define-key map "n" 'graph-brain--all)
			       (define-key map "s" 'graph-brain--tag-select)
			       (define-key map "t" 'graph-brain--tag-add)
			       map))
;;;###autoload
(define-derived-mode graph-brain-mode special-mode "Brain"
  "Major mode for managing graph-brain."
  (setq cursor-type nil))

;;;###autoload
(defun graph-brain ()
  (interactive)
  (setq graph-brain--buffer (get-buffer-create "*graph*"))
  (switch-to-buffer-other-window graph-brain--buffer)
  (graph-brain-mode)
  (funcall graph-brain--view))

(provide 'graph-brain)
