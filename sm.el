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

(defconst sm--base-url-twitter "https://www.twitter.com")
(defconst sm--base-url-reddit "https://www.reddit.com")
(defconst sm--base-url-tumblr "https://www.tumblr.com")

(defvar sm--client-type 'tumblr)
(make-variable-buffer-local 'sm--client-type)

(defvar sm--post-def-alist
  '((vars    . (channel-name post-url type date title body likes liked
			     note_count shared notes tags))
    (tumblr  . (blog_name post_url type date title body note_count liked
			  note_count shared notes tags
			  reblog_key))
    (reddit  . (subreddit permalink type date title selftext score likes
			  note_count shared notes tags
			  created num_comments))
    (twitter . (sub_name post-url type date title body likes liked
			 note_count shared notes tags)))
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
			      `(plist-get ,temp
					  ,(intern (concat ":" (symbol-name v))))))
			 (cdr (assoc 'tumblr sm--post-def-alist))))
	     (num_comments note_count))
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
	     (date (format-time-string "%A %-e %B"
				       (decode-time created))))
	  . ,temp1))
       (_ (let* ((channel-name "default")) . ,temp1))
       )))

(defun sm--render-post (post &optional verbose-header)
  (let* ((begin-post-area (point)))
    (setq sm--client-type 'reddit)
    (sm--with-post
     post
     ;; '(:blog_name "test" :author "a")
     ;; (pp channel-name)
     (sm--render-header channel-name post-url date title likes liked
			note_count shared tags num_comments
			verbose-header)
     (cond
      ((null body) nil)
      ((string= type "text") (sm--insert-text body))
      ;; ((string= type "photo") (sm--insert-photo photos))
      (t (insert type)))
     (insert "\n")
     ;; Record this post data so we know how to read it next
     (put-text-property begin-post-area (point)
                        'tumblesocks-post-data
                        post))
    ))

(defun sm--render-header (channel-name post-url date title likes liked
				       note_count shared tags num_comments
				       &optional verbose)
  "Draw the header for the current post, optionally being verbose."
  (let (begin end_bname)
    (setq begin (point))
    (insert (format "%-22s" channel-name))
    (setq end_bname (point))
    ;; Notes
    (when (and note_count (> note_count 0))
      (insert " (" (format "%d" note_count) " vote"
              (if (= 1 note_count) "" "s") ")"))

    (insert (format " %s %-5d" (if liked "❤️" "🤍") likes))
    ;; (insert "👎 👍 🔁")
    (insert (format "\t💬 %d" num_comments) " comments")

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
     (title (tumblesocks-view-insert-html-fragment title))
     ;; (caption (tumblesocks-view-insert-html-fragment caption))
     ;; (question (tumblesocks-view-insert-html-fragment question))
     (t (insert " ")))
    (when verbose
      (insert "\n")
      (insert "Date: " date)
      (when tags
	(insert
	 "\nTags: " (mapconcat #'(lambda (x)
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
   (decode-coding-string (encode-coding-string body 'latin-1) 'utf-8)))

(defun sm--insert-photo (photos)
  (let ((photo-html-frag
         `(p () .
             ,(apply 'append
                     (mapcar
                      #'(lambda (photodata)
                         ;; There could be several photos here, and
                         ;; each photo has several alternative sizes.
                         ;; The first is usually the biggest, the
                         ;; third is a good compromise
                         (let* ((alts (plist-get photodata :alt_sizes))
                                (desired-size-alts
                                 (delq nil
                                  (mapcar #'(lambda(alt)
                                             (and (= (plist-get alt :width)
                                                     tumblesocks-desired-image-size)
                                                  alt))
                                          alts)))
                                (alt (car (or desired-size-alts alts))))
                           (list `(img ((src . ,(plist-get alt :url))))
                                 ;;`(br)
                                 (plist-get photodata :caption))))
                      photos)))))
    (tumblesocks-view-insert-parsed-html-fragment photo-html-frag)
    ;; (when caption
    ;;   (tumblesocks-view-insert-html-fragment caption))
    (insert "\n")))

(provide 'sm)
