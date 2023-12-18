;;; musicbrainz.el --- MusicBrainz API interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Anand Tamariya <atamarya@gmail.com>
;; Keywords: music

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

;; musicbrainz.org API interface
;; Use musicbrainz and listenbrainz API to import JSON playlist into listenbrainz.
;; {
;;   "tracks": [
;;     {
;;       "artist": "Anuradha Paudwal",
;;       "album": "Sadak",
;;       "track": "Mohabbat Ki Hai Tumhare Liye",
;;       "uri": "spotify:track:3rS4GplDN0wt7JPIYfYV6R"
;;     }]
;; }
;;; Code:

(require 'url)

(defcustom musicbrainz-api-url "https://musicbrainz.org/ws/2"
  "URL for MusicBrainz API.
Documentation available at https://musicbrainz.org/doc/MusicBrainz_API"
  :type 'string
  :group 'musicbrainz)

;; https://musicbrainz.org/ws/2/release/80cdc014-5796-4ca4-8600-7c7237061241?inc=recordings
(defun musicbrainz-search (title &optional artist album date)
  (let* ((url (concat musicbrainz-api-url
		      (if album "/recording" "/release")
		      "/?limit=5&fmt=json&query="
		      (and title  (format "\"%s\"" title))
		      (and artist (format " AND artist:%s" artist))
		      (and album  (format " AND release:\"%s\"" album))
		      (and date (format " AND date:%s" date))
		      ))
	 (buffer (url-retrieve-synchronously url)))
    (pp url)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(tumblesocks-api-process-response)
	))
    ))

(defvar listen-counter 0)
(defun listen-submit(&optional track artist album)
  (let* (n data credits mb-results recording_mbid)
    (message "%s, %s, %s" track artist album)
    (setq mb-results (musicbrainz-search track artist album))
    (setq recording_mbid (json-resolve "recordings[0].id" mb-results t))
    (message "%d %s %s" listen-counter track recording_mbid)

    (when (zerop (length recording_mbid))
      (setq mb-results (musicbrainz-search track nil album))
      (setq recording_mbid (json-resolve "recordings[0].id" mb-results t))
      (message "%d %s %s" listen-counter track recording_mbid))

    (if (zerop (length recording_mbid)) (error "Not found"))
    (unless artist
      (setq data (json-resolve "recordings[0].artist-credit" mb-results t)
	    n (length data))
      (dotimes (i n)
	(setq credits (aref data i)
	      artist (concat artist
			     (json-resolve "name" credits t)
			     (json-resolve "joinphrase" credits t)
			     (if (json-resolve "joinphrase" credits t) " ")))))
    (message "%s, %s, %s" track artist album)

    (listenbrainz-api-request (concat listenbrainz-api-url "/1/submit-listens")
			      (encode-coding-string
			       (format "{ \"listen_type\": \"single\",\"payload\": [{ \"listened_at\": %d,\"track_metadata\": { \"additional_info\": { \"recording_mbid\": \"%s\"},\"artist_name\": \"%s\",\"track_name\": \"%s\",\"release_name\": \"%s\" } }] }
"
				       (time-convert nil 'integer) recording_mbid artist track album)
			       'utf-8)
			      "POST" '(("Content-Type" . "application/json; charset=utf-8")))
    (listenbrainz-api-request (concat listenbrainz-api-url "/1/feedback/recording-feedback") 
			      (json-serialize `((recording_mbid . ,recording_mbid)
						(score . 1))) 
			      "POST" '(("Content-Type" . "application/json")))
    
    ))

;;;###autoload
(defun listen-import(start)
  (interactive "P")
  (if start (setq listen-counter 0))
  (let* (listens n track album artist credits pos)
    (with-current-buffer (find-file-noselect "~/work/YourLibrary.json")
      (goto-char (point-min))
      (setq listens (json-resolve "tracks" (json-parse-buffer) t)
	    n (length listens))
      )
    (while (< listen-counter n)
      (setq credits (aref listens listen-counter)
	    track (json-resolve "track" credits t)
	    artist (json-resolve "artist" credits t)
	    album (json-resolve "album" credits t)
	    )
      (message "%s %s %s" track artist album)
      (when (setq pos (string-search " (Original Motion Picture Soundtrack)" album))
	(setq album (substring album 0 pos)))
      (when (setq pos (string-search " - From " track))
	(setq album (substring track (+ pos 9) (1- (length track)))
	      track (substring track 0 pos)))
      (when (setq pos (string-search " (From " track))
	(setq album (substring track (+ pos 8) (- (length track) 2))
	      track (substring track 0 pos)))
      (message "%s, %s, %s" track artist album)

      (listen-submit track artist album)
      (setq listen-counter (1+ listen-counter))
      )
    ))

(provide 'musicbrainz)

;;; musicbrainz.el ends here
