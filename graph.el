;;; graph.el --- Convenient graph for Emacs  -*- lexical-binding:t -*-

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

(require 'gnuplot)
(require 'rect)


(defvar gnuplot--flag-day nil)
(defvar gnuplot--flag-month nil)
(defvar gnuplot--flag-year nil)

(defun gnuplot--resolve-token (a)
  (cond ((= (length a) 4) (setq gnuplot--flag-year "%Y"))
        ((= (length a) 3) (setq gnuplot--flag-month "%b")) ;; Jan
        ((> (string-to-number a) 31) (setq gnuplot--flag-year "%y"))
        ((> (string-to-number a) 12) (setq gnuplot--flag-day "%d"))
        (t (setq gnuplot--flag-month "%m"))))

(defun gnuplot--gen-datefmt (datetime &optional datetime1)
  "Generate a gnuplot date format string or NIL if invalid."
  (let* (;(tokens (parse-time-string (org-read-date nil nil datetime)))
         (dash  (and (string-match-p "-" datetime) "-"))
         (slash (and (string-match-p "/" datetime) "/"))
         (time  (string-match-p ":" datetime))
         (sep   (or dash slash))
         (tokens1 (split-string datetime sep))
         (tokens2 (split-string datetime1 sep))
         res tokens3 out)
    (when (or sep time)
      (setq gnuplot--flag-day nil
	    gnuplot--flag-month nil
	    gnuplot--flag-year nil)
      ;; 1. Check for length
      (setq tokens3 (mapcar 'gnuplot--resolve-token tokens1))
      ;; (pp tokens3)

      ;; 2. Use second date to disambiguate
      (unless (and gnuplot--flag-day gnuplot--flag-month gnuplot--flag-year)
      (pcase-let
          ((`(,s1 ,s2 ,s3) tokens1)
           (`(,t1 ,t2 ,t3) tokens2))
        (if (= (length s1) 4) (push "%Y" tokens3)
          (if (string= s1 t1) (push "%m" tokens3) (push "%d" tokens3)))

        (if (= (length s1) 3) (push "%b" tokens3) ;; Jan
          (if (string= s2 t2) (push "%m" tokens3) (push "%d" tokens3)))

        (when s3
          (if (= (length s3) 4) (push "%Y" tokens3)
            (if (string= s3 t3) (push "%m" tokens3) (push "%d" tokens3)))
          ))
      ;; (pcase-let
      ;;     ((`(,s1 ,s2) tokens1)
      ;;      (`(,t1 ,t2) tokens2))
      ;;   (if (= (length s1) 4) (push "%Y" tokens3))
      ;;   (if (string= s1 t1)   (push "%m" tokens3) (push "%d" tokens3))
      ;;   (if (string= s2 t2)   (push "%m" tokens3) (push "%d" tokens3))
      ;;   (if (= (length s2) 4) (push "%Y" tokens3))
      ;;   )
      ;; (pp tokens)
      (setq tokens3 (nreverse tokens3)))
      (setq res (mapconcat 'identity tokens3 sep))
      ;; (pcase-let
      ;;     ((`(,sec ,min ,hour ,day ,mon ,year dow dst tz) tokens))
      ;;   (setq res (apply 'concat
      ;;                    (append (if mon  (list "%m" sep))
      ;;                            (if day  (list "%d" sep))
      ;;                            (if year (list year-str)))
      ;;                    )))

      ;; 3. Seek user help
      (pcase-let
          ((`(,s1 ,s2 ,s3) out))
        (when (or (string= s1 s2)
                  (string= s3 s2))
          (if (string= s3 s2)
              (if (string= s1 "Y")
                  (setq out (list (mapconcat 'identity '("Y" "m" "d") sep)
                                  (mapconcat 'identity '("Y" "d" "m") sep)
                                  ))))
          (if (string= s1 s2)
              (if (string= s3 "Y")
                  (setq out (list (mapconcat 'identity '("m" "d" "Y") sep)
                                  (mapconcat 'identity '("d" "m" "Y") sep)
                                  ))))
          (pp out)
          ))
      )
    res))

(defun gnuplot--draw (&optional title style)
  (or killed-rectangle (error "No tabular data"))
  (let* ((name (make-temp-file "plot"))
         (data killed-rectangle)
         (comma (and (string-match-p "," (car data)) ","))
         (pipe  (and (string-match-p "|" (car data)) "|"))
	 (sep   (or comma pipe))
         (cols (split-string (car data) sep))
         (n (length cols))
	 (str-check "^ *[a-zA-Z'\"]")
         buf xlabel ylabel header j labelcol datetime)
    ;; Extract plot details
    ;; Column header
    (when (string-match-p str-check (car cols))
      (if (= n 1)
          (if (string-match-p str-check (nth 1 data))
              (error "Bad data")
            (setq ylabel (car cols)
                  labelcol nil))
        (when (string-match-p str-check (nth 1 cols))
          (setq header cols
                ;; style "histograms"
                )
          (pcase (length header)
            (1 (setq ylabel (nth 1 header)))
            (2 (setq xlabel (nth 0 header)
                     ylabel (nth 1 header)))
            (_ (setq xlabel (nth 0 header))))
          ;; (pop killed-rectangle)
          )))
    ;; First column might be a label column or datetime
    (setq labelcol (string-match-p str-check (nth 1 data)))
    (setq datetime (gnuplot--gen-datefmt (car (split-string (nth 1 data) sep))
                                         (car (split-string (nth 2 data) sep))))

    (setq buf (find-file-noselect name))
    (with-current-buffer buf
      (yank-rectangle)
      (save-buffer))

    (setq style (or style "line"))
    ;; (setq style (if labelcol "histograms"))
    (with-temp-buffer
      (insert "unset xdata\n")
      (insert "unset ydata\n")
      (insert "unset xrange\n")
      (insert "unset yrange\n")
      (insert "unset format xy\n")
      (insert "unset timefmt\n")
      (insert "reset\n")

      (insert "set datafile separator "
	      (if sep (format "'%s'" sep) "whitespace") "\n")
      (insert "set style fill solid 0.5\n")
      (insert "set title  '" (or title  "Title")  "'\n")
      (insert "set xlabel '" (or xlabel "x-axis") "'\n")
      (insert "set ylabel '" (or ylabel "y-axis") "'\n")
      (when datetime
        (insert "set xdata time\n")
        (insert "set format x \"" (if (< (length data) 10) "%d-" "") "%b-%y\"\n")
        (insert "set timefmt \"" datetime "\"\n"))
      (insert "plot '" name "'")
      (setq j (if (or labelcol datetime (> n 1)) 1 0))
      (dotimes (i (- n j))
        (insert " using ")
        (when (not (string= style "histograms"))
          (insert (number-to-string
                   (if (or labelcol (= n 1)) 0 1))
                  ":"))
        (insert (number-to-string (+ i j 1)))
        (if (and labelcol (not datetime)) (insert ":xtic(1)"))
        (if (< i n) (insert " with " style))
        (if (> n 2)
            (insert " title columnhead")
          (insert " notitle"))
        (if (< i (- n j 1)) (insert ", ''")))
      (newline)
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))

    ;; Cleanup
    (kill-buffer buf)
    ;; (delete-file name)
    ))

;; styles lines, points, linespoints, impulses, dots, steps, errorbars (or
;; yerrorbars), xerrorbars, xyerrorbars, boxes, boxerrorbars, boxxyerrorbars
(defun gnuplot-rectangle (prefix)
  (interactive "P")
  (let ((styles '(("line" . "line")
		  ("bar" . "histograms")
		  ("scatter" . "circles")
		  ))
	title style)
    (when prefix
      (setq title (read-string "Title: "))
      (setq style (cdr (assoc (completing-read "Style: " styles) styles)))
      )
    (gnuplot--draw title style)))


(provide 'graph)
