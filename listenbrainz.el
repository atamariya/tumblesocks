;;; listenbrainz.el --- ListenBrainz API interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Anand Tamariya <atamarya@gmail.com>
;; Keywords: social media, music, streaming

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Stream ListenBrainz.org music playlist
;; https://listenbrainz.readthedocs.io/

(require 'emms)
(require 'json)

(defconst listenbrainz-api-url "https://api.listenbrainz.org"
  "URL for listenbrainz API.
Documentation available at https://listenbrainz.readthedocs.io/")

(defcustom listenbrainz-api-token nil
  "An auth token is required for some functions.
Details can be found near https://listenbrainz.org/profile/"
  :type 'string
  :group 'listenbrainz)

(defcustom emms-listenbrainz-buffer-name "Listenbrainz"
  "Name of emms buffer listenbrainz"
  :type 'string
  :group 'listenbrainz)

(defun listenbrainz-api-request (url params method &optional headers)
  (let* ((data nil))
    (unless listenbrainz-api-token
      (setq listenbrainz-api-token (read-string "Token")))
    (setq data (if headers
		   ;; Use content type as is
		   params
		 (if (string= method "POST")
		   (setq headers
			 '(("Content-Type" . "application/x-www-form-urlencoded"))))
		 (url-build-query-string params)
		 ))
    (if (string= method "GET")
	(setq url (concat url "?" data)))
    (push `("Authorization" . ,(format "Token %s" listenbrainz-api-token))
	  headers)
    (with-current-buffer
	(let ((url-request-method method)
              (url-request-data data)
              (url-request-extra-headers headers))
	  (url-retrieve-synchronously url))
      (tumblesocks-api-process-response))))

(defun listenbrainz-api-get (url &optional params)
  "Post to a Token-authenticated Listenbrainz API endpoint (url),
using the given GET parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (listenbrainz-api-request url params "GET"))

(defun listenbrainz-api-post (url params)
  "Post to a Token-authenticated Listenbrainz API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (listenbrainz-api-request url params "POST"))

(define-emms-source listenbrainz ()
  "An EMMS source for listenbrainz for streaming."
  (interactive)
  (emms-playlist-insert-track (emms-track 'url "https://www.youtube.com/watch?v=DGVJtAHzzDQ")))

(defun listenbrainz--insert-playlist ()
  (interactive)
  ;; (let* ((res (listenbrainz-api-get "https://api.listenbrainz.org/1/playlist/7a521b1b-9c09-4054-a0fd-68985b7f65ad"))
  ;; 	 (tracks (json-resolve "playlist.track" res t))
  ;; 	 (n (length tracks))
  ;; 	 )
  ;;   (dotimes (i n)
  ;;     (setq track (aref tracks i))
  ;;     (pp track)
  ;;     (pp (json-resolve "title" track t))
  ;;     (pp (json-resolve "album" track t))
  ;;     (pp (json-resolve "creator" track t))
  ;;     ))
  (let* ((res (listenbrainz-api-get "https://api.listenbrainz.org/1/user/atamariya/listens"))
	 (tracks (json-resolve "payload.listens" res t))
	 (n (json-resolve "payload.count" res t))
	 data track
	 )
    (dotimes (i n)
      (setq data (aref tracks i)
	    track (json-resolve "track_metadata" data t))
      ;; (pp track)
      ;; (pp (json-resolve "track_name" track t))
      ;; (pp (json-resolve "release_name" track t))
      ;; (pp (json-resolve "artist_name" track t))
      ;; (pp (json-resolve "additional_info.origin_url" track t))
      (message "\n")
      (emms-playlist-insert-track
       `(*track* (type . url)
	     (name . ,(json-resolve "additional_info.origin_url" track t))
	     (metadata ,(json-resolve "track_name" track t))))
      ))
  )
    
(setq emms-track-description-function 'emms-track-meta-description)
(defun emms-track-meta-description (track)
  "Simple function to give a user-readable description of a track.
Same as `emms-track-meta-description' except it displays metadata for URLs."
  (let ((type (emms-track-type track)))
    (cond ((eq 'file type)
	   (emms-track-name track))
	  ((eq 'url type)
	   (emms-format-url-track-name (if (emms-track-get track 'metadata)
					   (car (emms-track-get track 'metadata))
					 (emms-track-name track))))
	  (t (concat (symbol-name type)
		     ": " (emms-track-name track))))))

;;;###autoload
(defun emms-listenbrainz ()
  "Stream ListenBrainz.org music playlist"
  (interactive)
  (let ((buf (get-buffer emms-listenbrainz-buffer-name)))
    (when (not buf)
      (with-current-buffer (get-buffer-create emms-listenbrainz-buffer-name)
	(setq buf (current-buffer))
	(emms-playlist-mode)
	(setq emms-playlist-buffer-p t)
	(emms-playlist-set-playlist-buffer (current-buffer))
	;; (emms-add-native-playlist emms-streams-file)
	(listenbrainz--insert-playlist)
	(goto-char (point-min))
	))
    (switch-to-buffer buf)))

(provide 'listenbrainz)

;;; listenbrainz.el ends here
