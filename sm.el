;;; sm.el --- Social media client for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Anand Tamariya <atamarya@gmail.com>
;; Keywords: social media

;; This file is part of GNU Emacs.

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

;; Glossary:
;; Channel name: Tumblr Blog name, sub reddit, Twitter user timeline
;; Like: Upvote
;; Share: Reblog, retweet
;; Comments: Notes (Tumblr)
;; Note_count: likes + reblogs (Tumblr), votes (Reddit)

(require 'parse-time)

(defconst sm--base-url-twitter "https://api.twitter.com")
(defconst sm--base-url-reddit "https://oauth.reddit.com")
(defconst sm--base-url-tumblr "https://api.tumblr.com/v2")

(defvar sm--client-type nil)

(defvar sm--date-format "%a %-e %b %Y")

(defvar sm--post-def-alist
  '((vars    . (channel-name author post-url type date title body num_likes liked
			     num_shared shared notes tags reblog_key))
    (tumblr  . (blog_name blog_name post_url type date title body like_count liked
			  reblog_count shared notes tags reblog_key
			  reply_count timestamp))
    (reddit  . (subreddit author permalink type date title selftext score likes
			  note_count shared notes tags reblog_key
			  created num_comments url url_overridden_by_dest))
    (twitter . (user.screen_name author_id urls type created_at title text
				 public_metrics.like_count favorited
				 public_metrics.retweet_count retweeted notes
				 entities.hashtags reblog_key)))
  ;; "An alist of variable name and JSON key mapping."
  )

(defmacro sm--with-post (post &rest body)
  "Bind each KEY to its associated value in PLIST and execute BODY."
  (let* ((temp post)
	 (temp1 body)
	 (vars (cdr (assoc 'vars sm--post-def-alist)))
	 (i -1)
	 (n (length vars)))
    `(pcase sm--client-type
       ('tumblr
	;; (message "11 %s a" sm--client-type)
	(let*
	    (;,(list temp post)
	     ,@(progn
		 (setq i -1)
		 (mapcar #'(lambda (v)
			     (setq i (1+ i))
			     ;; (message "%s" i)
			     (list
			      (if (< i n) (nth i vars) v)
			      `(gethash ,(symbol-name v) ,temp)))
			 (cdr (assoc 'tumblr sm--post-def-alist))))
	     (date (format-time-string sm--date-format
				       timestamp))
	     (num_comments reply_count))
	  . ,temp1))
       ('reddit
	;; (message "12 %s a" sm--client-type)
	(let*
	    (;(,temp post)
	     ,@(progn
		 (setq i -1)
		 (mapcar #'(lambda (v)
			     (setq i (1+ i))
			     ;; (message "%s" i)
			     (list
			      (if (< i n) (nth i vars) v)
			      `(gethash ,(symbol-name v) ,temp)))
			 (cdr (assoc 'reddit sm--post-def-alist))))
	     (type (if (eq (gethash "is_video" ,temp) t)
		       "video" "text" ))
	     (date (format-time-string sm--date-format
				       created)))
	  . ,temp1))
       ('twitter
	;; (message "12 %s a" sm--client-type)
	(let*
	    (;(,temp post)
	     ,@(progn
		 (setq i -1)
		 (mapcar #'(lambda (v)
			     (setq i (1+ i))
			     ;; (message "%s" i)
			     (list
			      (if (< i n) (nth i vars) v)
			      `(json-resolve ,(symbol-name v) ,temp t)))
			 (cdr (assoc 'twitter sm--post-def-alist))))
	     (type "text")
	     (num_comments (json-resolve "public_metrics.reply_count" post t))
	     (date (format-time-string sm--date-format
				       (encode-time
					(parse-time-string date))))
	     )
	  . ,temp1))
       (_ (let* ((channel-name "default")) . ,temp1))
       )))

(defun sm--api-user-info ()
  (unless tumblesocks-user
    (let (result)
      (pcase sm--client-type
	('tumblr
	 (setq result (tumblesocks-api-tumblr-get
		       (tumblesocks-api-url "/user/info") nil)
	       tumblesocks-user (json-resolve "response.user.name" result t)))
	('twitter
	 (setq result (tumblesocks-api-http-request-twitter
		       (tumblesocks-api-url "/2/users/me") nil "GET")
	       tumblesocks-user (json-resolve "data.id" result t)))
	))))

(defmacro sm--api-dashboard ()
  (let* ()
    `(pcase sm--client-type
       ('tumblr
	(tumblesocks-api-user-dashboard
         tumblesocks-posts-per-page
         tumblesocks-view-current-offset
	 nil nil nil nil))
       ('reddit
	(tumblesocks-api-user-dashboard-reddit
         tumblesocks-posts-per-page
         tumblesocks-view-current-offset
	 nil nil nil nil))
       ('twitter
	(tumblesocks-api-user-dashboard-twitter
         tumblesocks-posts-per-page
         tumblesocks-view-current-offset
	 nil nil nil nil))
       )))

(defmacro sm--api-post-details (post)
  (let* ((data post))
    `(pcase sm--client-type
       ('tumblr
	(tumblesocks-api-blog-posts
         nil (plist-get ,data :id) nil "1" nil nil "true" "html"))
       ('reddit
	(tumblesocks-api-post-details-reddit (plist-get ,data :uri)))
       ('twitter
	(tumblesocks-api-post-details-twitter (plist-get ,data :id)))
       )))

(defmacro sm--render-notes (notes)
  (let* ((data notes))
    `(pcase sm--client-type
       ('tumblr
	(tumblesocks-view-render-notes ,data))
       ((or 'reddit 'twitter)
	(tumblesocks-view-render-notes-reddit ,data))
       )))

(defmacro sm--get-id (post)
  ;; Services use different fields as id for fetching
  (let* ((data post))
    `(pcase sm--client-type
       ('tumblr
	(gethash "id" ,data))
       ('reddit
	(gethash "name" ,data))
       ('twitter
	(or (json-resolve "referenced_tweets[0].id" ,data t)
	    (gethash "id" ,data)))
       )))

(defmacro sm--get-url (post)
  ;; Services might need base to be added to the url
  (let* ((data post))
    `(pcase sm--client-type
       ('tumblr
	(gethash "post_url" ,data))
       ('reddit
	(concat "https://www.reddit.com"
		(gethash "permalink" ,data)))
       ('twitter
	(format "https://twitter.com/twitter/status/%s"
		(gethash "id" ,data)))
       )))

(defvar sm--reddit-offset nil)
(defvar sm--reddit-direction nil)
(defvar sm--extra-data nil)
(make-variable-buffer-local 'sm--extra-data)

(defmacro sm--get-list (data)
  ;; Fetch post list from response
  (let* ((temp data))
    `(pcase sm--client-type
       ('tumblr
	(json-resolve "response.posts" ,temp t))
       ('reddit
	(let ((before (json-resolve "data.before" ,temp t))
	      (after (json-resolve "data.after" ,temp t)))
	  (setq sm--reddit-offset
	      (list :before (if (eq before :null) nil before)
		    :after  (if (eq after :null) nil after))))
	(json-resolve "data.children" ,temp t))
       ('twitter
	(json-resolve "data" ,temp t))
       (_ ,temp)
       )))

(defmacro sm--get-post-from-list (data i)
  ;; Fetch ith post from list
  (let* ((temp data)
	 (temp1 i))
    `(pcase sm--client-type
       ((or 'tumblr 'twitter)
	(aref ,temp ,temp1))
       ('reddit
	(gethash "data" (aref ,temp ,temp1)))
       )))

(defmacro sm--get-post-from-details (data)
  ;; Fetch post from detail object
  (let* ((temp data))
    `(pcase sm--client-type
       ('tumblr
	(json-resolve "response.posts[0]" ,temp t))
       ('reddit
	(json-resolve "[0].data.children[0].data" ,temp t))
       ('twitter
	(json-resolve "main" ,temp t))
       )))

(defmacro sm--get-additional-data (data)
  ;; Fetch additional data from response
  (let* ((temp data))
    `(pcase sm--client-type
       ('twitter
	;; (pp (json-serialize (json-resolve "includes" ,temp t)))
	(json-resolve "includes" ,temp t))
       )))

(defmacro sm--get-comments-from-details (data)
  ;; Fetch comments from detail object
  (let* ((temp data))
    `(pcase sm--client-type
       ('tumblr
	(json-resolve "response.notes" ,temp t))
       ('reddit
	(json-resolve "[1].data.children" ,temp t))
       ('twitter
	(json-resolve "data" ,temp t))
       )))

(defun sm--find-field (collection key val data)
  (let* ((col (json-resolve collection data t))
	 (len (length col))
	 (i 0)
	 res)
    (while (and (not res)
		(< i len)
		col)
      (if (string= val
		   (json-resolve key (aref col i) t))
	  (setq res (aref col i)))
      (setq i (1+ i)))
    ;; (pp (json-resolve "name" res))
    res))

(defun sm--render-post (post &optional verbose-header)
  (let* ((begin-post-area (point))
	 tmp)
    ;; (pp (json-serialize post))
    (sm--with-post
     post
     ;; '(:blog_name "test" :author "a")
     ;; (pp channel-name)
     (when (eq sm--client-type 'twitter)
       (setq tmp (sm--find-field "users" "id" author sm--extra-data)
	     author (if tmp (json-resolve "username" tmp t) author)
	     ))
     (sm--render-header channel-name author (sm--get-url post) (or date "")
			(if (eq title :null) nil title) (or num_likes 0)
			(eq liked t)
			(or num_shared 0) shared tags (or num_comments 0)
			verbose-header)
     ;; (message "%s" body)
     (sm--render-body type body post)
     (if (and
	  (eq sm--client-type 'reddit)
	  ;; (boundp 'url)
	  (not (string-empty-p url))
	  url_overridden_by_dest)
	 ;; reddit photo
 	 (sm--render-body (if (member (file-name-extension url) '("png" "jpg"))
			      "photo" "url")
			  url))
     (when (eq sm--client-type 'twitter)
       ;; Twitter media - video urls are nil
       (let ((attachments (json-resolve "attachments.media_keys" post t)))
	 (dotimes (i (length attachments))
	   ;; (pp (json-resolve "url"
	   ;; 		     (sm--find-field "media"
	   ;; 				     "media_key"
	   ;; 				     (aref attachments i)
	   ;; 				     sm--extra-data)
	   ;; 		     t))
	   (sm--render-body "photo"
			    (json-resolve "url"
					  (sm--find-field "media"
							  "media_key"
							  (aref attachments i)
							  sm--extra-data)
					  t)))
 	 ))

     (insert "\n")
     ;; Record this post data so we know how to read it next
     (put-text-property begin-post-area (point)
                        'tumblesocks-post-data
			`(:title ,title
				 :id ,(sm--get-id post)
				 :uri ,post-url
				 :channel-name ,channel-name
				 :reblog_key ,reblog_key
				 :service ,sm--client-type)
                        ;; post
			))
    ))

(defun sm--render-body (type body &optional post)
  ""
  (cond
   ((string= type "text")
    (if body
	(sm--insert-text body)))
   ((string= type "photo")
    ;; Pic with a url
    (tumblesocks-view-insert-parsed-html-fragment
	  `(p nil (img ((src . ,body))))))
   ((string= type "image")
    ;; List of pics with alternate sizes - display one
    (sm--insert-photo post))
   ((string= type "url")
    (newline)
    (tumblesocks-view-insert-parsed-html-fragment
     `(a ((href . ,body)) ,body) t)
        (newline))
   ((string= type "blocks")
    ;; Tumblr NPF blocks
    (cl-mapc
     (lambda (a)
       (sm--render-body (json-resolve "type" a t)
			(json-resolve "text" a t)
			(json-resolve "media" a t)))
     (json-resolve "trail[0].content" post t)))
   (t (insert type)))
  )

(defun sm--format-num (n)
  (cond ((> n 1000000) (format "%dM" (/ n 1000000)))
	((> n 1000)    (format "%dK" (/ n 1000)))
	(t (format "%d" n))))

(defun sm--render-header (channel-name author post-url date title likes liked
				       note_count shared tags num_comments
				       &optional verbose)
  "Draw the header for the current post, optionally being verbose."
  (let (begin end_bname str)
    (setq begin (point))
    (setq str (if (eq sm--client-type 'twitter)
		  author channel-name))
    (insert (format "%-23s" (if (> (length str) 22)
				(substring str 0 21) str)))
    (setq end_bname (point))
    ;; Notes
    ;; (when (and note_count (> note_count 0))
    ;;   (insert " (" (format "%d" note_count) " vote"
    ;;           (if (= 1 note_count) "" "s") ")"))

    (insert (format " %s %-4s" (if liked "❤️" "🤍") (sm--format-num likes)))
    ;; (insert "👎 👍 🔁")
    ;; (setq p_shared (1+ (point)))
    (insert (format "\t%s %-4s" (if shared "🔂" "🔁")
		    (sm--format-num note_count)))
    (insert (format "\t💬 %-4s" (sm--format-num num_comments)))

    (unless verbose
      (insert "\t" date))
    ;; (insert (make-string (- (window-body-width) (current-column)) ? ))
    (insert (propertize " " 'display `(space . (:align-to ,(window-body-width)))))
    (insert "\n")
    (put-text-property end_bname (point) 'face
                       (list '(:weight bold)
                             'highlight))

    ;; Title
    (cond
     (title (tumblesocks-view-insert-html-fragment
	     (format "<p><b>%s</b>" title)))
     ;; (caption (tumblesocks-view-insert-html-fragment caption))
     ;; (question (tumblesocks-view-insert-html-fragment question))
     ;; (t (insert " "))
     )
    (when verbose
      (insert "\n")
      (insert "Date: " date)
      (when (and tags (> (length tags) 0))
	(insert
	 "\nTags: " (mapconcat #'(lambda (x)
				   (when (eq sm--client-type 'twitter)
				     (setq x (json-resolve "tag" x t)))
				  (propertize
				   (concat "#" x)
				   'tumblesocks-tag x))
			       tags ", ")))
      (insert
       "\nPermalink: ")
      (tumblesocks-view-insert-parsed-html-fragment
       `(a ((href . ,post-url)) ,post-url) t)
      (insert "\n"))
    (put-text-property begin end_bname 'face
                       (list '(:inverse-video t)
                             '(:weight bold)
                             font-lock-keyword-face))
    ))

(defun sm--insert-text (body)
  (tumblesocks-view-insert-html-fragment
   ;; (decode-coding-string (encode-coding-string body 'latin-1) 'utf-8)
   body
   ))

(defun sm--insert-photo (photos)
  (let ((photo-html-frag
         `(p () .
	     ,(let* ((alts photos)
                     (desired-size-alts
                      (delq nil
                            (cl-mapcar #'(lambda(alt)
                                           (and (= (gethash "width" alt)
                                                   tumblesocks-desired-image-size)
                                                alt))
                                       alts)))
                     (alt (or (car desired-size-alts)
			      (aref alts 0))))
                (list `(img ((src . ,(gethash "url" alt))))
		      ))

             ;; ,(apply 'append
             ;;         (mapcar
             ;;          #'(lambda (photodata)
             ;;             ;; There could be several photos here, and
             ;;             ;; each photo has several alternative sizes.
             ;;             ;; The first is usually the biggest, the
             ;;              ;; third is a good compromise
	     ;; 		  (message "%s" (json-serialize photodata))
             ;;             (let* ((alts photos)
             ;;                    (desired-size-alts
             ;;                     (delq nil
             ;;                      (cl-mapcar #'(lambda(alt)
             ;;                                 (and (= (gethash "width" alt)
             ;;                                         tumblesocks-desired-image-size)
             ;;                                      alt))
             ;;                              alts)))
             ;;                    (alt (or (car desired-size-alts)
	     ;; 				 (aref alts 0))))
             ;;               (list `(img ((src . ,(gethash "url" alt))))
             ;;                     ;;`(br)
             ;;                     (gethash "caption" photodata))))
             ;;          photos))
	     )))
    (tumblesocks-view-insert-parsed-html-fragment photo-html-frag)
    ;; (when caption
    ;;   (tumblesocks-view-insert-html-fragment caption))
    (insert "\n")))

(defun sm--reply-tree-reddit (notes)
  (let* (children note count author liked body)
    (dotimes (i (length (json-resolve "data" notes t)))
      (setq note (json-resolve "data" (aref notes i) t)
	    count (json-resolve "count" note t)
	    author (json-resolve "author" note t)
	    liked (json-resolve "likes" note t)
	    body (json-resolve "body" note t))
      (when body
	(with-temp-buffer
	  (tumblesocks-view-insert-html-fragment body)
	  (setq body (buffer-string))))
      (if author
	(push (apply 'widget-convert 'tree
		     :name (format "[%s%s] %s"
				   (propertize author
					       'face 'font-lock-comment-face)
				   (cond ((eq liked :null) "")
					 ((eq liked :false) " 👎")
					 (t " 👍"))
				   body)
		     (sm--reply-tree-reddit
		      (json-resolve "replies.data.children" note t)))
	      children)
	(push (widget-convert 'tree :name (format "%d more" count))
	      children)
	))
    (nreverse children)
    ))

(defun sm--reply-tree-twitter (notes)
  (let* (children note count author liked body tmp)
    (dotimes (i (length notes))
      (setq note (aref notes i)
	    count (json-resolve "count" note t)
	    author (json-resolve "author_id" note t)
	    liked (json-resolve "likes" note t)
	    body (json-resolve "text" note t))
      (setq tmp (sm--find-field "users" "id" author sm--extra-data)
	    author (if tmp (json-resolve "name" tmp t) author))
      (when body
	(with-temp-buffer
	  (tumblesocks-view-insert-html-fragment body)
	  (setq body (buffer-string))))
      (if author
	(push (apply 'widget-convert 'tree
		     :name (format "[%s%s] %s"
				   (propertize author
					       'face 'font-lock-comment-face)
				   (cond ((eq liked :null) "")
					 ((eq liked :false) " 👎")
					 (t " 👍"))
				   body)
		     (sm--reply-tree-reddit
		      (json-resolve "replies.data.children" note t)))
	      children)
	(push (widget-convert 'tree :name (format "%d more" (or count 0)))
	      children)
	))
    (nreverse children)
    ))

(defun tumblesocks-api-url (&rest args)
  (apply 'concat (pcase sm--client-type
		   ('tumblr sm--base-url-tumblr)
		   ('reddit sm--base-url-reddit)
		   ('twitter sm--base-url-twitter))
	 args))

;;;###autoload
(defun sm-tumblr ()
  (interactive)
  (let* ((sm--client-type 'tumblr))
    (tumblesocks-view-dashboard)))

;;;###autoload
(defun sm-reddit ()
  (interactive)
  (let* ((sm--client-type 'reddit)
	 (tumblesocks-blog nil))
    (tumblesocks-view-dashboard)))

;;;###autoload
(defun sm-twitter ()
  (interactive)
  (let* ((sm--client-type 'twitter))
    (tumblesocks-view-dashboard)))

(provide 'sm)
