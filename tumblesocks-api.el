;; tumblesocks-api.el -- functions for talking with tumblr
;; Copyright (C) 2012 gcr

(require 'oauth)
(require 'oauth2)
(require 'json)
(provide 'tumblesocks-api)

(defcustom tumblesocks-consumer-key
  "5xqkcJNRSGj3TokMQDJf3FzE8246DHvw8sNJWNn54fs2z0AhYr"
  "Our Tumblr OAuth consumer API key.

This goes hand-in-hand with `tumblesocks-secret-key'.

If you need to register your own app, do that at
http://www.tumblr.com/oauth/apps"
  :type 'string
  :group 'tumblesocks)

(defcustom tumblesocks-secret-key
  "juLG1T866ZG964ybgGCu1EntFMo5eQuHth1SKCqL2mdMzNIL1Q"
  "Our Tumblr OAuth consumer secret key.

This goes hand-in-hand with `tumblesocks-consumer-key'.

If you need to register your own app, do that at
http://www.tumblr.com/oauth/apps"
  :type 'string
  :group 'tumblesocks)

(defcustom tumblesocks-callback-url
  "http://www.sneakygcr.net/oauth-dummy-endpoint.htm"
  "Our Tumblr OAuth consumer secret key.

This goes hand-in-hand with `tumblesocks-consumer-key'.

If you need to register your own app, do that at
http://www.tumblr.com/oauth/apps"
  :type 'string
  :group 'tumblesocks)

(defvar tumblesocks-user nil
  "Logged in user.")

(defcustom tumblesocks-blog nil
  "Your blog name, like xxx.tumblr.com.

This variable affects many functions that depend on blogs. For
example, `tumblesocks-api-blog-posts' will consult this variable
to pick which block to list posts for, so if you want to temporarily ask for a different blog, rebind this."
  :type 'string
  :group 'tumblesocks)

(defcustom tumblesocks-token-file (concat (file-name-as-directory user-emacs-directory)
                                         "tumblr-oauth-token")
  "Location of the file containing the oath-token as returned by Tumblr."
  :type 'file
  :group 'tumblesocks)

(defvar tumblesocks-token nil)

(defun tumblesocks-api-forget-authentication ()
  "Forget our authentication and delete the token file. You must
call `tumblesocks-api-reauthenticate' after this."
  (interactive)
  (setq tumblesocks-token nil)
  (when (file-exists-p tumblesocks-token-file)
    (delete-file tumblesocks-token-file)))

(defun tumblesocks-api-reauthenticate ()
  "Read our tumblr token from the tumblr token file, or generate a new one."
  (when (or (not tumblesocks-secret-key)
            (not tumblesocks-consumer-key))
    (error "You MUST set both `tumblesocks-secret-key' and `tumblesocks-consumer-key' to use tumblesocks."))
  (let ((oauth-callback-url tumblesocks-callback-url))
    (when (file-exists-p tumblesocks-token-file)
      (save-excursion
        (find-file tumblesocks-token-file)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)"
                            (buffer-substring (point-min) (point-max)))
	      (push (cons 'tumblr
			  (make-oauth-access-token
			   :consumer-key tumblesocks-consumer-key
			   :consumer-secret tumblesocks-secret-key
			   :auth-t (make-oauth-t
				    :token (match-string 1 str)
				    :token-secret (match-string 2 str))))
		    tumblesocks-token)))
        (kill-this-buffer)))
    (unless (assoc 'tumblr tumblesocks-token)
      (push (cons 'tumblr
		  (oauth-authorize-app
		   tumblesocks-consumer-key
		   tumblesocks-secret-key
		   "https://www.tumblr.com/oauth/request_token"
		   "https://www.tumblr.com/oauth/access_token"
		   "https://www.tumblr.com/oauth/authorize"))
	    tumblesocks-token)
      (save-excursion
        (let ((token (oauth-access-token-auth-t
		      (cdr (assoc 'tumblr tumblesocks-token)))))
          (find-file tumblesocks-token-file)
          (erase-buffer)
          (insert (format "%s:%s\n"
                          (oauth-t-token token)
                          (oauth-t-token-secret token))))
        (save-buffer)
        (kill-this-buffer)))))

(defun tumblesocks-api-test-auth ()
  (interactive)
  (unless tumblesocks-blog (error "Please set the `tumblesocks-blog' variable. See https://github.com/gcr/tumblesocks for help getting Tumblesocks working."))
  (condition-case nil
      (message (concat "Hello, "
                       (plist-get (plist-get (tumblesocks-api-user-info)
                                             :user)
                                  :name)
                       "! Tumblesocks is working properly."))
    (error
     (if (yes-or-no-p "Looks like something broke. Try again? (yes/no) ")
         (progn
           (tumblesocks-api-forget-authentication)
           (tumblesocks-api-test-auth))
       (message "Please see http://github.com/gcr/tumblesocks for help.")))))

;; (defun tumblesocks-api-url (&rest args)
;;   (apply 'concat "https://api.tumblr.com/v2" args))

(defun tumblesocks-api-http-noauth-get (url)
  "Post to an unauthenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously url)
    (tumblesocks-api-process-response)))

(defun tumblesocks-plist-to-alist (plist)
  (when plist
    (let ((key (car plist))
          (value (cadr plist))
          (rest (cddr plist)))
      (cons (cons (if (stringp key)
		      key
		    ;; (intern (substring (symbol-name key) 1))
		    (substring (symbol-name key) 1))
		  value)
            (tumblesocks-plist-to-alist rest)))))

(defun tumblesocks-api-http-oauth-get (url params)
  "Post to an API-key-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, a keyword plist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (let ((oauth-callback-url tumblesocks-callback-url))
    (unless (assoc 'tumblr tumblesocks-token) (tumblesocks-api-reauthenticate))
    (with-current-buffer (oauth-url-retrieve
                          (cdr (assoc 'tumblr tumblesocks-token))
                          (concat url "?api_key=" tumblesocks-consumer-key
                                  (mapconcat
                                   #'(lambda (x)
                                      (concat "&" (url-hexify-string (format "%s" (car x)))
                                              "=" (url-hexify-string (format "%s" (cdr x)))))
                                   (tumblesocks-plist-to-alist params) "")))
      (tumblesocks-api-process-response))))

(defun tumblesocks-api-http-apikey-get (url params)
  "Post to an API-key-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, a keyword plist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously
                        (concat url "?api_key=" tumblesocks-consumer-key
                                (mapconcat
                                 #'(lambda (x)
                                    (concat "&" (url-hexify-string (format "%s" (car x)))
                                            "=" (url-hexify-string (format "%s" (cdr x)))))
                                 (tumblesocks-plist-to-alist params) "")))
    (tumblesocks-api-process-response)))

(defun tumblesocks-api-http-oauth-post (url params)
  "Post to an OAuth-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (unless (assoc 'tumblr tumblesocks-token) (tumblesocks-api-reauthenticate))
  (let ((oauth-callback-url tumblesocks-callback-url))
    (with-current-buffer
        (oauth-post-url
         (cdr (assoc 'tumblr tumblesocks-token)) url
         (mapcar #'(lambda (x)
                    (cons (format "%s" (car x))
                          (format "%s" (cdr x))))
                 (tumblesocks-plist-to-alist params)))
      (tumblesocks-api-process-response))))

(defvar tumblesocks-service-conf
  ;; (service . (client-id secret-key redirect-url))
  nil
  )

(defun tumblesocks-api-tumblr-get (url params)
  "Post to an OAuth2-authenticated Tumblr API endpoint (url),
using the given GET parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-tumblr-request url params "GET"))

(defun tumblesocks-api-tumblr-post (url params)
  "Post to an OAuth2-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-tumblr-request url params "POST"))

(defun tumblesocks-api-tumblr-request (url params method)
  "Post to an OAuth2-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-oauth2-request url params method
				  'tumblr "write offline_access"))

(defun tumblesocks-api-oauth-request (url params method service auth-scope
					   &optional headers)
  (let* ((oauth2-conf (assoc service oauth2-service-conf))
	 (conf (assoc service tumblesocks-service-conf))
	 (tumblesocks-consumer-key (nth 1 conf))
	 (tumblesocks-secret-key (nth 2 conf))
	 (oauth-callback-url (nth 3 conf)))
    (unless (assoc service tumblesocks-token)
      (push (cons 'tumblr
		  (oauth-authorize-app
		   tumblesocks-consumer-key
		   tumblesocks-secret-key
		   "https://www.tumblr.com/oauth/request_token"
		   "https://www.tumblr.com/oauth/access_token"
		   "https://www.tumblr.com/oauth/authorize"))
	    tumblesocks-token))
    (if (string= method "GET")
	(setq url (concat url "?"
			  (mapconcat
			   #'(lambda (x)
			       (and (cdr x)
				    (concat "&" (url-hexify-string (format "%s" (car x)))
					    "=" (url-hexify-string (format "%s" (cdr x))))))
			   (tumblesocks-plist-to-alist params) ""))))
    (with-current-buffer
	(if (string= method "GET")
	    (oauth-fetch-url (cdr (assoc service tumblesocks-token))
			     url)
	  (oauth-post-url (cdr (assoc service tumblesocks-token))
			  url (tumblesocks-plist-to-alist params)))
      (tumblesocks-api-process-response service))))

(defun tumblesocks-api-oauth2-request (url params method service auth-scope
					   &optional headers)
  (let* ((oauth2-conf (assoc service oauth2-service-conf))
	 (conf (assoc service tumblesocks-service-conf))
	 (tumblesocks-consumer-key (nth 1 conf))
	 (tumblesocks-secret-key (nth 2 conf))
	 (tumblesocks-callback-url (nth 3 conf))
	 (data nil))
    (unless (assoc service tumblesocks-token)
      (push (cons service
		  (oauth2-auth (nth 1 oauth2-conf)
			       (nth 2 oauth2-conf)
			       tumblesocks-consumer-key tumblesocks-secret-key
			       auth-scope "nil" tumblesocks-callback-url)
		  )
	    tumblesocks-token))
    (setq data (if headers
		   ;; Use content type as is
		   params
		 (if (string= method "POST")
		   (setq headers
			 '(("Content-Type" . "application/x-www-form-urlencoded"))))
		 (mapconcat
		  #'(lambda (x)
		      (and (cdr x)
			   (concat "&" (url-hexify-string (format "%s" (car x)))
				   "=" (url-hexify-string (format "%s" (cdr x))))))
		  (tumblesocks-plist-to-alist params) "")))
    (if (string= method "GET")
	(setq url (concat url "?" data)))
    (with-current-buffer
	(oauth2-url-retrieve-synchronously
	 (cdr (assoc service tumblesocks-token))
	 url method data headers)
      (tumblesocks-api-process-response service))))

(defun tumblesocks-api-reddit-get (url params)
  "Post to an OAuth2-authenticated Reddit API endpoint (url),
using the given GET parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-reddit-request url params "GET"))

(defun tumblesocks-api-reddit-post (url params)
  "Post to an OAuth2-authenticated Reddit API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-reddit-request url params "POST"))

(defun tumblesocks-api-reddit-request (url params method)
  "Post to an OAuth2-authenticated Reddit API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-oauth2-request url params method
				  'reddit "read,vote,submit"))

(defun tumblesocks-api-twitter-get (url params)
  "Post to an OAuth2-authenticated Reddit API endpoint (url),
using the given GET parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-http-request-twitter
   url (append '("tweet.fields" "created_at,public_metrics,referenced_tweets"
		 "media.fields" "url,variants"
		 "expansions" "author_id,attachments.media_keys")
	       params)
   "GET"))

(defun tumblesocks-api-twitter-post (url params)
  "Post to an OAuth2-authenticated Reddit API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-http-request-twitter
   url params "POST"
   '(("Content-Type" . "application/json"))))

(defun tumblesocks-api-http-request-twitter (url params method
						 &optional headers)
  "Post to an OAuth2-authenticated Reddit API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (tumblesocks-api-oauth2-request
   url params method
   'twitter
   (concat "tweet.read users.read tweet.write like.write like.read "
	   "offline.access follows.write follows.read")
   headers))

(defun tumblesocks-api-process-response (&optional service)
  "Process Tumblr's response in the current buffer,
returning JSON or signaling an error for other requests."
  (decode-coding-region (point-min) (point-max) 'utf-8-dos)
  ;; the following copied from url.el
  (goto-char (point-min))
  (skip-chars-forward " \t\n")		; Skip any blank crap
  (skip-chars-forward "HTTP/")		; Skip HTTP Version
  (skip-chars-forward "[0-9].")
  (let ((pointpos (point))
        (code (read (current-buffer))))
    (cond
     ((= code 100) ;; Gotta clean up the buffer and try again
      (search-forward-regexp "^$" nil t)
      (delete-region (point-min) (point))
      (tumblesocks-api-process-response))
     ((not (and (<= 200 code) (<= code 299)))
      (if (= code 401)
	  (delq (assoc service tumblesocks-token) tumblesocks-token))
      (error (buffer-substring pointpos
                               (line-end-position))))
     (t
      (search-forward-regexp "^$" nil t)
      ;; body
      (let* ((json-response (buffer-substring (1+ (point)) (point-max)))
             (json-object-type 'plist)
             (json-array-type 'list)
             (json-false nil))
        ;; (plist-get (json-read-from-string json-response)
        ;;            :response)
	(json-parse-string json-response)
	)))))



(defun tumblesocks-api-avatar-url (&optional size)
  (tumblesocks-api-url
   "/blog/" tumblesocks-blog "/avatar/"
   (format "%d" (or size 128))))

(defun tumblesocks-api-user-info ()
  "Gather information about the logged in user"
  (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/info") '()))

(defun tumblesocks-api-user-dashboard (&optional limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset))
               (and type `(:offset ,type))
               (and since_id `(:since_id ,since_id))
               (and reblog_info `(:reblog_info ,reblog_info))
               (and notes_info `(:notes_info ,notes_info)))))
    (if (not tumblesocks-blog)
    (tumblesocks-api-tumblr-post (tumblesocks-api-url "/user/dashboard") args)
    (tumblesocks-api-tumblr-get
     (tumblesocks-api-url "/blog/"
                          tumblesocks-blog
                          "/posts"
                          (if type (concat "/" type) ""))
     args))
    ))

(defun tumblesocks-api-user-likes (&optional limit offset)
  "Gather information about the logged in user's likes"
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset)))))
    (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/likes") args)))

(defun tumblesocks-api-user-following (&optional limit offset)
  "Gather information about which blogs are followed by the logged-in user"
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/user/following") args)))

(defun tumblesocks-api-user-follow (url)
  "Follow the given blog URL."
  (pcase sm--client-type
    ('tumblr
     (tumblesocks-api-tumblr-post (tumblesocks-api-url "/user/follow")
                                  `(:url ,url)))
    ('reddit
     (tumblesocks-api-reddit-post (tumblesocks-api-url "/api/subscribe")
                                  `(:action "sub" :action_source "o"
					    :sr ,url)))
    ('twitter
     (tumblesocks-api-twitter-post
      (tumblesocks-api-url "/2/users/" tumblesocks-user "/following")
      (format "{\"target_user_id\" : \"%s\"}" url)))
    ))

(defun tumblesocks-api-user-unfollow (url)
  "Unfollow the given blog URL."
  (pcase sm--client-type
    ('tumblr
     (tumblesocks-api-tumblr-post (tumblesocks-api-url "/user/unfollow")
                                  `(:url ,url)))
    ('reddit
     (tumblesocks-api-reddit-post (tumblesocks-api-url "/api/subscribe")
                                  `(:action "unsub" :action_source "o"
					    :sr ,url)))
    ('twitter
     (tumblesocks-api-http-request-twitter
      (tumblesocks-api-url "/2/users/" tumblesocks-user "/following/" url)
      nil
      "DELETE"))
    ))

(defun tumblesocks-api-user-like (id reblog_key)
  "Like a given post"
  (pcase sm--client-type
    ('tumblr
     (tumblesocks-api-tumblr-post (tumblesocks-api-url "/user/like")
                                      `(:id ,id :reblog_key ,reblog_key)))
    ('reddit
     (tumblesocks-api-reddit-post (tumblesocks-api-url "/api/vote")
                                  `(:id ,id :dir 1)))
    ('twitter
     (tumblesocks-api-twitter-post
      (tumblesocks-api-url "/2/users/" tumblesocks-user "/likes")
      (format "{\"tweet_id\" : \"%s\"}" id)))
    ))

(defun tumblesocks-api-user-unlike (id reblog_key)
  "Unlike a given post"
  (pcase sm--client-type
    ('tumblr
     (tumblesocks-api-tumblr-post (tumblesocks-api-url "/user/unlike")
                                      `(:id ,id :reblog_key ,reblog_key)))
    ('reddit
     (tumblesocks-api-reddit-post (tumblesocks-api-url "/api/vote")
                                  `(:id ,id :dir 0)))
    ('twitter
     (tumblesocks-api-http-request-twitter
      (tumblesocks-api-url "/2/users/" tumblesocks-user "/likes/" id)
      nil
      "DELETE"))
    ))

(defun tumblesocks-api-blog-info ()
  "Gather information about the blog listed in
`tumblesocks-blog'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (tumblesocks-api-http-apikey-get
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/info")
   '()))

;; TODO This returns actual image data, not JSON!
;; (defun tumblesocks-api-blog-avatar ()
;;   "Gathers info about the given blog's avatar. Defaults to `tumblesocks-blog'"
;;   (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
;;   (tumblesocks-api-http-noauth-get
;;    (tumblesocks-api-url "/blog/" tumblesocks-blog "/avatar")))

(defun tumblesocks-api-blog-followers ()
  "Gathers info about the `tumblesocks-blog''s followers.

See http://www.tumblr.com/docs/en/api/v2 for information about the returned JSON."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/followers") '()))



(defun tumblesocks-api-blog-posts (&optional type id tag limit offset reblog_info notes_info filter)
  "Gather info about the blog posts for `tumblesocks-blog'.

Type should be one of text, quote, link, answer, video, audio, photo, chat.

If given, retrieve just posts with the given attributes (args)

See http://www.tumblr.com/docs/en/api/v2 for information about
the returned JSON."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and id `(:id ,id))
               (and tag `(:tag ,tag))
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset))
               (and reblog_info `(:reblog_info ,reblog_info))
               (and notes_info `(:notes_info ,notes_info))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-tumblr-get
     (tumblesocks-api-url "/blog/"
                          tumblesocks-blog
                          "/posts"
                          (if type (concat "/" type) ""))
     args)))

(defun tumblesocks-api-blog-queued-posts (&optional offset filter)
  "Retrieve queued blog posts from `tumblesocks-blog'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and offset `(:offset ,offset))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/posts/queue")
     args)))

(defun tumblesocks-api-blog-draft-posts (&optional filter)
  "Retrieve draft blog posts from `tumblesocks-blog'. You need
write access to it!"
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (and filter `(:filter ,filter))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/posts/draft")
     args)))

(defun tumblesocks-api-blog-submission-posts (&optional offset filter)
  "Retrieve submission blog posts from `tumblesocks-blog'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and offset `(:offset ,offset))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/posts/submission")
     args)))

(defun tumblesocks-api-new-post (&optional args)
  "Create a new post, using Tumblr's post API. Post it to `tumblrsocks-blog'.
Args must be an alist of arguments to use.

If you're making a text post, for example, args should be something like
'(:type \"text\"
  :title \"How to use the Tumblr API\"
  :body \"...\")"
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/post")
   args))

(defun tumblesocks-api-edit-post (id &optional args)
  "Edit the post with the given id. args should be as in `tumblesocks-new-post'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (setq args (plist-put args :id id))
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/post/edit")
   args))

(defun tumblesocks-api-reblog-post (id reblog_key &optional comment)
  "Reblog a post with the given id and reblog key."
  (unless tumblesocks-user (error "Not logged in."))
  (let ((args (append `(:id  ,id :reblog_key ,reblog_key)
                      (and comment (not (string= comment ""))
                           `(:comment ,comment)))))
    (pcase sm--client-type
      ('tumblr
       (tumblesocks-api-tumblr-post
	(tumblesocks-api-url "/blog/" tumblesocks-user "/post/reblog")
	args))
      ('reddit
       (tumblesocks-api-reddit-post (tumblesocks-api-url "/api/submit")
                                    `(:id ,id :sr ,tumblesocks-blog
					  :kind "self")))
      ('twitter
       (tumblesocks-api-twitter-post
	(tumblesocks-api-url "/2/users/" tumblesocks-user "/retweets")
	(format "{\"tweet_id\" : \"%s\"}" id)))
      )))

(defun tumblesocks-api-reblog-undo (id reblog_key &optional comment)
  "Reblog a post with the given id and reblog key."
  (unless tumblesocks-user (error "Not logged in."))
  (let ((args (append `(:id  ,id :reblog_key ,reblog_key)
                      (and comment (not (string= comment ""))
                           `(:comment ,comment)))))
    (pcase sm--client-type
      ('tumblr
       (tumblesocks-api-tumblr-post
	(tumblesocks-api-url "/blog/" tumblesocks-user "/post/reblog")
	args))
      ('reddit
       (tumblesocks-api-reddit-post (tumblesocks-api-url "/api/submit")
                                    `(:id ,id :sr ,tumblesocks-blog
					  :kind "self")))
      ('twitter
       (tumblesocks-api-http-request-twitter
	(tumblesocks-api-url "/2/users/" tumblesocks-user "/retweets/" id)
	nil "DELETE"))
      )))

(defun tumblesocks-api-delete-post (id)
  "Delete the post with the given id. args should be as in `tumblesocks-new-post'."
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/post/delete")
   `(:id ,id)))

(defun tumblesocks-api-tagged (tag &optional before limit filter)
  (let ((args (append
               `(:tag ,tag)
               (and before `(:before ,before))
               (and limit `(:limit ,limit))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-apikey-get
     (tumblesocks-api-url "/tagged")
     args)))

(defun tumblesocks-api-user-dashboard-reddit (&optional limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  (let ((args (append
               (and limit `(:limit ,limit))
               (and (/= 0 offset)
		    `(,sm--reddit-direction ,(plist-get sm--reddit-offset
							sm--reddit-direction)))
               ;; (and type `(:offset ,type))
               ;; (and since_id `(:since_id ,since_id))
               ;; (and reblog_info `(:reblog_info ,reblog_info))
               ;; (and notes_info `(:notes_info ,notes_info))
	       )))
    ;; (tumblesocks-api-http-apikey-get "https://www.reddit.com/.json" args)
    (tumblesocks-api-reddit-get (tumblesocks-api-url
					(if tumblesocks-blog
					    (format "/r/%s" tumblesocks-blog))
					;; "r/emacs/"
					"/best")
				args)
    ))

(defun tumblesocks-api-post-details-reddit (&optional url limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  ;; (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset))
               (and type `(:offset ,type))
               (and since_id `(:since_id ,since_id))
               (and reblog_info `(:reblog_info ,reblog_info))
               (and notes_info `(:notes_info ,notes_info)))))
    (tumblesocks-api-reddit-get (tumblesocks-api-url url ".json") args)
    ))

(defun tumblesocks-api-user-dashboard-twitter (&optional limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  (let ((args (append
               ;; (and limit `(:limit ,limit))
               (and offset `(,sm--reddit-direction ,(plist-get sm--reddit-offset
							       sm--reddit-direction)))
               ;; (and type `(:offset ,type))
               ;; (and since_id `(:since_id ,since_id))
               ;; (and reblog_info `(:reblog_info ,reblog_info))
               ;; (and notes_info `(:notes_info ,notes_info))
	       ))
	res user)
    ;; Find userid
    (if tumblesocks-blog
	(setq res (tumblesocks-api-http-request-twitter
		   (tumblesocks-api-url "/2/users/by/username/" tumblesocks-blog)
		   nil "GET")
	      user (json-resolve "data.id" res t))
      (setq user tumblesocks-user))
    (if tumblesocks-blog
	(tumblesocks-api-twitter-get
	 (tumblesocks-api-url "/2/users/" user "/tweets")
	 args)
      (tumblesocks-api-twitter-get
       (tumblesocks-api-url "/2/users/" user "/timelines/reverse_chronological")
       ;; "https://api.twitter.com/2/users/3236721175/timelines/reverse_chronological"
       args))
    ))

(defun tumblesocks-api-post-details-twitter (&optional url limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  ;; (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
	       `(:ids ,url)
               ;; (and limit `(:limit ,limit))
               ;; (and offset `(:offset ,offset))
               ;; (and type `(:offset ,type))
               ;; (and since_id `(:since_id ,since_id))
               ;; (and reblog_info `(:reblog_info ,reblog_info))
               ;; (and notes_info `(:notes_info ,notes_info))
	       )))
    ;; "https://api.twitter.com/2/tweets/search/recent?query=conversation_id:1577882425971335168&tweet.fields=in_reply_to_user_id,author_id,created_at,conversation_id"
    (setq args (append
		`("query" ,(concat "conversation_id:" "1577882425971335168"))
		'("tweet.fields" "in_reply_to_user_id,author_id,created_at,conversation_id,public_metrics")
		'("expansions" "author_id")
		))
    (tumblesocks-api-http-request-twitter
     (tumblesocks-api-url "/2/tweets/search/recent") args "GET")
    ;; (tumblesocks-api-twitter-get "https://api.twitter.com/2/tweets" args)
    ))
