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
(defvar graph-brain--view 'graph-brain--view-node)
(defvar graph-brain--time nil)
(defvar graph-brain--back nil)

(defvar-local graph-brain--image nil)
(defvar-local graph-brain--selection nil)

(defun graph-brain--shorten (title)
  "Split words in lines. Truncate second words onwards."
  (with-temp-buffer
    (insert title)
    (when (> (point) 10)
      (goto-char (point-min))
      (forward-word 2)
      (when (> (- (point-max) (point)) 3)
	(delete-region (point) (point-max))
	(insert "..."))
      (forward-word -2)
      (while (re-search-forward "[ -]" nil t)
	(replace-match "\n")))
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

(defun graph-brain--refresh ()
  (if (not (get-buffer-window graph-brain--buffer))
      (switch-to-buffer-other-window graph-brain--buffer))
  (with-current-buffer graph-brain--buffer
    (funcall graph-brain--view)))

(defun graph-brain--open (url)
  (interactive)
  (cond ((string-prefix-p "tag:" url)
	 (setq graph-brain--tags
	       (if graph-brain--tags
		   (append graph-brain--tags (list (substring url 4)))
		 (push (substring url 4) graph-brain--tags)))
	 (graph-brain--refresh))
	((string-prefix-p "time:" url)
	 (let ((name (substring url 5)))
	   (if (or (and (eq graph-brain--view 'graph-brain--view-wheel-git)
			(< (length graph-brain--time) 4))
		   (and (eq graph-brain--view 'graph-brain--view-git-log)
			(not graph-brain--time))
		   (eq graph-brain--view 'graph-brain--view-wheel))
	       (push name graph-brain--time)))
	 (graph-brain--refresh))
	((string-prefix-p "rev:" url)
	 (let ((name (substring url 4)))
	   (shell-command (concat "git show " name))))
	(t
  (with-temp-buffer
    (insert url)
    (forward-char -1)
    (org-open-at-point)
    (setq graph-brain--doc (current-buffer))
    ))))

(defun graph-brain--node-delete ()
  (interactive)
  (let* ((points graph-brain--selection)
	 filter)
    (or points (error "No node selected"))

    (when (eq graph-brain--view 'graph-brain--view-tag)
      (mapc (lambda (a)
	      (let* ((tmp (split-string a ":"))
		     (tag (nth 1 tmp)))
		(delete tag graph-brain--tags)
		(setq filter (graph-brain--filter-internal tag))))
	    points)
      (setq points (mapcar (lambda (a) (concat "id:" a))
			   filter)))

    (dolist (p points)
      (with-current-buffer (graph-brain--open p)
	(when (yes-or-no-p (format "Note %s %s.  Delete? "
				   (buffer-name)
				   (if (buffer-modified-p)
				       "HAS BEEN EDITED" "is unmodified")))
	  (delete-file (buffer-file-name))
	  (kill-buffer))
	))
    (graph-brain--node-select-init)
    (org-roam-db-sync)
    (graph-brain--refresh)
    ))

(defun graph-brain--tag-add ()
  (interactive)
  (let* ((points graph-brain--selection)
	 tags)
    (or points (error "No node selected"))

    (setq tags (let ((crm-separator "[ 	]*:[ 	]*"))
		 (completing-read-multiple "Tag: " (org-roam-tag-completions))))
    (dolist (p points)
      (with-current-buffer (graph-brain--open p)
	(org-roam-tag-add tags)
	(save-buffer)
      ))
    (graph-brain--node-select-init)
    (org-roam-db-sync)
    (graph-brain--refresh)
    ))

(defun graph-brain--tag-select (tags)
  (interactive
   (list (let ((crm-separator "[ 	]*:[ 	]*"))
           (completing-read-multiple "Tag: " (org-roam-tag-completions) nil nil
				     (mapconcat 'identity graph-brain--tags ":")))))
  (let ((graph-brain--tags tags))
    (graph-brain--refresh))
  ;; This line is needed if graph-brain--tags is buffer local
  (setq graph-brain--tags tags))

(defun graph-brain--deselect ()
  (interactive)
  (pcase graph-brain--view
    ('graph-brain--view-tag
     (setq graph-brain--tags (butlast graph-brain--tags)))
    ((or 'graph-brain--view-wheel-git 'graph-brain--view-git-log)
     (pop graph-brain--time))
    ('graph-brain--view-wheel
     (pop graph-brain--time)))
  (setq graph-brain--back t)
  (graph-brain--refresh))

(defun graph-brain--node-select-init ()
  (interactive)
  (setq graph-brain--selection nil))

(defun graph-brain--node-select-internal (url)
  (message "Selected %s" url)
  (push url graph-brain--selection))

(defun graph-brain--node-select (e)
  (interactive "e")
  (let* ((svg-click-function 'graph-brain--node-select-internal))
    (svg-on-click e)
    ))

(defun graph-brain--draw (points &optional lines group-fn)
  (interactive)
  (let* ((graph-draw-padding (if group-fn 10 10))
	 image p r w root)
    (setq graph-brain--image image)
    (setq graph-draw-group group-fn
          graph-draw-group-fn group-fn)
    (setq svg-click-function 'graph-brain--open)

    (setq image (graph-draw-init))
    (when points
    ;; (setq p (graph-draw-tree points image))
    (setq root (graph-draw--tree-convert points image))
    (dolist (p lines)
      (setq graph-draw-fill "red")
      (graph-draw-line image (car p) (cdr p)))
    (graph-draw--tree-internal root image)
    
    (setq p (node-value root)
	  r (point-r p)
	  w (* 2 r))
    ;; (dom-set-attribute image 'viewBox
    ;; 		       (format "%d %d %d %d" (- (point-x p) r) (- (point-y p) r) w w))    

    (svg-possibly-update-image image)
    (set-buffer-modified-p nil)
    )))

(defun graph-brain--filter-internal (tag &optional initial)
  (vconcat
   (apply 'append
	  (org-roam-db-query
	   (vconcat
	    [:select [node-id] :from tags]
	    (vector :where (if initial
			       `(and (= tags:tag ,tag)
				     (in node-id ,initial))
			     `(= tags:tag ,tag))))))))

(defun graph-brain--filter ()
  "Filter nodes based on active tags."
  (let* (filter)
    (when graph-brain--tags
      (mapc (lambda (a)
	      (setq filter (graph-brain--filter-internal a filter)))
	    graph-brain--tags))
    filter))

(defun graph-brain--get-date-format (n)
  ;; (id . name)
  (pcase n
    (0 '("%Y" . "%Y"))
    (1 '("%Y-%m" . "%B"))
    (2 '("%Y-%m-%d" . "%d"))
    (3 '("%Y-%m-%d %H" . "%H"))
    ))

(defun graph-brain--view-wheel ()
  (interactive)
  (let* (group filter nodes points title pt id lines files tmp name v fmt flag r)
    (setq filter (graph-brain--filter))
    (setq files (org-roam-db-query
		 (vconcat
		  [:select [files:file mtime] :from files :inner :join nodes
			   :on (= files:file nodes:file)]
		  (when graph-brain--tags
		    (vector :where `(in id ,filter)))
		  )))
    (catch 'node-view
      (while (and files
		  (or (not nodes)
		      (and (= (length nodes) 1)
			   ;; (not graph-brain--back)
			   )))
	(when (= (length nodes) 1)
	  (if graph-brain--back
	      (pop graph-brain--time)
	    (push (car nodes) graph-brain--time)))

	(setq r (length graph-brain--time)
	      fmt (or (graph-brain--get-date-format r)
		      (and (setq flag t)
			   '("%Y-%m-%d %H" . "%H")))
	      filter (car graph-brain--time))

	(setq group (make-hash-table :test 'equal))
	(dolist (p files)
	  (when (or
		 (= r 0)
		 (string= filter (format-time-string
				  (if flag (car fmt)
				    (car (graph-brain--get-date-format (1- r))))
				  (nth 1 p))))
	    (setq name (format-time-string (car fmt) (nth 1 p)))
	    (setq v (list name (format-time-string (cdr fmt) (nth 1 p)) p))
	    (push p tmp)
	    (if (setq p (gethash name group))
		(puthash name (cons v p) group)
	      (puthash name (list v) group))))

	(setq nodes (sort (hash-table-keys group) 'string>)
	      files tmp)
	(if flag (throw 'node-view t)))

    (dolist (p nodes)
      (if flag
	  (setq id (nth 0 p)
		r 0
		title (nth 1 p))
	(setq id p
	      v (gethash p group)
	      r (length v)
	      title (format "%s (%d)" (nth 1 (car v)) r)))
      
      (setq pt (make-point :x 0 :y 0 :r (+ r 30)
			   :old-x 0 :old-y 0
			   :fill (random-color-html)
			   ;; :image (save-window-excursion
			   ;; 	    (with-current-buffer
			   ;; 		(graph-brain--open (concat "id:" (nth 0 p)))
			   ;; 	      (car (org-property-values "IMAGE"))))
			   :href (concat "time:" id)
			   :text title
			   :title (graph-brain--shorten title)))
      (push (cons pt (car points)) lines)
      (push pt points))
    ;; (pp points)

    (graph-brain--draw points (if (not flag) lines)
		       (lambda (_d _i)
			 (format "%s" (or (car graph-brain--time)
					  "Timeline")))))

    (when flag
      ;; Render leaves
      (setq filter (vconcat (mapcar (lambda (a)
				      (car (nth 2 a)))
				    (gethash (car graph-brain--time) group))))
      (setq nodes (org-roam-db-query
		   (vconcat
		    [:select [id title] :from nodes]
		    (vector :where `(in file ,filter)))
		   ))
      (graph-brain--view-node nodes))
    (setq graph-brain--view 'graph-brain--view-wheel
	  graph-brain--back nil)
    ))

(defun graph-brain--view-group ()
  (interactive)
  (let* ((group (make-hash-table :test 'equal))
	 root points p names name filter nodes tags)
	 ;; (nodes (org-roam-db-query [:select [id title] :from nodes]))
	 ;; (tags (org-roam-db-query [:select [tag node-id] :from tags]))
	 ;; (org-roam-db-query [:select [id title] :from nodes :where (in id ["854b72c9-5042-4362-8f23-af36a95a6180"])])
    (setq filter (graph-brain--filter))
    (setq nodes (org-roam-db-query
		 (vconcat
		  [:select [id title properties] :from nodes]
		  (when graph-brain--tags
		    (vector :where `(in id ,filter)))
		  ))
	  tags  (org-roam-db-query
		 (vconcat
		  [:select [tag node-id] :from tags]
		  (when graph-brain--tags
		    (vector :where `(and (in node-id ,filter)
					 (not-in tag ,(vconcat graph-brain--tags)))))
		  )))

    (setq points (mapcar 'car nodes))
    (mapc (lambda (a)
	    (setq name (car a))
	    (delete (nth 1 a) points)
	    (if (setq p (gethash name group))
		(puthash name (cons (nth 1 a) p) group)
	      (puthash name (list (nth 1 a)) group)))
	  tags)
    ;; Add a new entry for top level tag
    (puthash nil points group)
    (dolist (j (map-keys group))
      (setq points nil)
      (dolist (i (gethash j group))
	(dolist (p nodes)
	  (if (string= i (car p))
	      (push (make-point :x 0 :y 0 :r 30
				:old-x 0 :old-y 0
				:fill (random-color-html)
				:image (cdr (assoc "IMAGE" (nth 2 p)))
				;; (save-window-excursion
				;; 	 (with-current-buffer
				;; 	     (graph-brain--open (concat "id:" (nth 0 p)))
				;; 	   (car (org-property-values "IMAGE"))))
				:href (concat "id:" (nth 0 p))
				:text (nth 1 p)
				:title (graph-brain--shorten (nth 1 p)))
		    points))))
      (push j names)
      (if (and (not j) graph-brain--tags)
	  (progn
	    (pop names)
	    (mapc (lambda (_a) (push j names)) points)
	    (setq root (append points root)))
	(push points root)))
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
    (setq graph-brain--view 'graph-brain--view-group)
    ))

(defun graph-brain--view-node (&optional nodes)
  (interactive)
  (let* ((group (make-hash-table :test 'equal))
	 filter links points title pt id lines)
    (unless nodes
    (setq filter (graph-brain--filter)
	  nodes (org-roam-db-query
		 (vconcat
		  [:select [id title properties] :from nodes]
		  (when graph-brain--tags
		    (vector :where `(in id ,filter)))
		  ))))
    (setq links (org-roam-db-query [:select [source dest] :from links
					    :where (= type "id")]))
    (dolist (p nodes)
      (setq title (nth 1 p)
	    id (nth 0 p)
	    pt (make-point :x 0 :y 0 :r 30
			   :old-x 0 :old-y 0
			   :fill (random-color-html)
			   :image (cdr (assoc "IMAGE" (nth 2 p)))
			   ;; (save-window-excursion
			   ;; 	    (with-current-buffer
			   ;; 		(graph-brain--open (concat "id:" (nth 0 p)))
			   ;; 	      (car (org-property-values "IMAGE"))))
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
    (setq graph-brain--view 'graph-brain--view-node)
    ))

(defun graph-brain--view-tag ()
  (interactive)
  (let* (filter nodes points tag title pt r)
    (setq filter (graph-brain--filter))
    (setq nodes (org-roam-db-query
		 (vconcat
		  [:select [tag (funcall count tag)] :from tags]
		  (when graph-brain--tags
		    (vector :where `(and (in node-id ,filter)
					 (not-in tag ,(vconcat graph-brain--tags)))))
		  [:group-by tag]
		  )))

    (if (not nodes)
	(graph-brain--view-node)
    (dolist (p nodes)
      (setq tag (nth 0 p)
	    r (nth 1 p)
	    title (format "%s (%d)" tag r)
	    pt (make-point :x 0 :y 0 :r (+ 30 r)
			   :old-x 0 :old-y 0
			   :fill (random-color-html)
			   :href (concat "tag:" tag)
			   :text title
			   :title (graph-brain--shorten title)))
      (push pt points))
    ;; (pp points)
    
    (graph-brain--draw points))
    (setq graph-brain--view 'graph-brain--view-tag)
    ))

(defun graph-brain--view-wheel-git (&optional show-branches)
  (interactive)
  (let* ((group (make-hash-table :test 'equal))
	 (repo (string-trim
		(shell-command-to-string "git remote get-url origin")))
	 filter nodes points title pt id lines fmt flag r log branch color)
    (while (and (not log)
		(or (not nodes)
		    (and (= (length nodes) 1)
			 (not flag)
			 ;; (not graph-brain--back)
			 )))
      (when (= (length nodes) 1)
	(if graph-brain--back
	    (pop graph-brain--time)
	  (push (caar nodes) graph-brain--time)))

      (setq r (length graph-brain--time)
	    flag (> r 2)
	    nodes nil
	    fmt (graph-brain--get-date-format r)
	    filter (car graph-brain--time))
      (setq log (and show-branches (>= r 1)))
      (if log (setq r 0
		    flag t
		    branch (string-trim
			    (shell-command-to-string "git branch --show-current"))))

      (let (i j k before after time id title n cmd)
	(pcase r
	  (0 (setq after  "%s%s-01-01 00:00"
		   time (if log (string-to-number filter) (nth 5 (decode-time)))
		   filter ""
		   before "%s%s-01-01 00:00"))
	  (1 (setq after  "%s-%02d-01 00:00"
		   before "%s-%02d-31 00:00"
		   j 12))
	  (2 (setq after  "%s-%02d 00:00"
		   before "%s-%02d 24:00"
		   j 31))
	  (_ (setq after  "%s 00:00"
		   before "%s 24:00"
		   flag t
		   j 1)))
	(setq i (if (= r 0) time j)
	      k (if (= r 0) 1 0))
	(while (if log (numberp time)
		 (if (= r 0) (not (member n '("" "0")))
		   (> i 0)))
	  (setq time (format after  filter i)
		cmd (format (concat "git"
				    (if flag " log --format='%%h\t%%s (%%an)\t%%D\t;'"
				      " rev-list --count --all")
				    " --after='%s' --before='%s'")
			    time (format before filter (+ i k))))
	  (setq n (string-trim
		   (shell-command-to-string cmd)))
	  ;; (message "%s %s" cmd n)
	  (setq i (1- i))
	  (when (not (member n '("" "0")))
	    (if flag
		(mapc (lambda (a)
			(setq lines (split-string a "\t")) 
			(if (string-empty-p (nth 2 lines))
			    (setf (nth 2 lines) branch)
			  (setq branch (nth 2 lines)))
			(if (not (gethash branch group))
			    (puthash branch (random-color-html) group))
			(push lines nodes))
		      (split-string n "\n"))
	      (setq id (format-time-string (car fmt) (date-to-time time))
		    title (format-time-string (cdr fmt) (date-to-time time)))
	      (push (list id title n) nodes))))
	(when (> r 0)
	  (setq nodes (nreverse nodes)))))

    (setq lines nil)
    (dolist (p nodes)
	(if flag
	    (progn	      
	      (setq id (nth 0 p)
		    r 1
		    branch (nth 2 p)
		    color (gethash branch group)
		    title (format "%s\n(%s)" (nth 1 p) branch)))
	(setq id (nth 0 p)
	      r (string-to-number (nth 2 p))
	      color (random-color-html)
	      title (format "%s (%d)" (nth 1 p) r)))
      
      (setq pt (make-point :x 0 :y 0 :r (+ (log r) 30)
			   :old-x 0 :old-y 0
			   :fill color
			   :href (concat (if flag "rev:" "time:") id)
			   :text title
			   :title (graph-brain--shorten title)))
      (push (cons pt (car points)) lines)
      (push pt points))
    ;; (pp points)

    (graph-brain--draw points lines
		       (lambda (_d _i)
			 (format "%s\n%s" (or (car graph-brain--time) "Timeline")
				 repo)))

    (setq graph-brain--view 'graph-brain--view-wheel-git
	  graph-brain--back nil)
    ))

(defun graph-brain--view-git-log ()
  (interactive)
  (graph-brain--view-wheel-git t)
  (setq graph-brain--view 'graph-brain--view-git-log))

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

(defvar graph-brain-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "/" 'graph-brain--search)
    (define-key map "d" 'graph-brain--node-delete)
    (define-key map "G" 'graph-brain--view-wheel-git)
    (define-key map "L" 'graph-brain--view-git-log)
    (define-key map "w" 'graph-brain--view-wheel)
    (define-key map "g" 'graph-brain--view-group)
    (define-key map "n" 'graph-brain--view-node)
    (define-key map "T" 'graph-brain--view-tag)
    (define-key map "s" 'graph-brain--tag-select)
    (define-key map "l" 'graph-brain--deselect)
    (define-key map "t" 'graph-brain--tag-add)
    (define-key map (kbd "C-k") 'graph-brain--node-select-init)
    (define-key map [nil C-mouse-1] 'graph-brain--node-select)
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
