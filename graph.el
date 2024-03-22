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

(defvar gnuplot--styles '(("line" . "line")
			  ("bar" . "histograms")
			  ("scatter" . "circles")
			  ("pie" . "pie")
			  ("spider" . "spider")
			  ))
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

(defun gnuplot--draw-line (style m n xlabel ylabel labelcol datetime)
  ""
  (let (j)
    (insert "set xlabel '" (or xlabel "x-axis") "'\n")
    (insert "set ylabel '" (or ylabel "y-axis") "'\n")
    (when datetime
      (insert "set xdata time\n")
      (insert "set format x \"" (if (< m 10) "%d-" "") "%b-%y\"\n")
      (insert "set timefmt \"" datetime "\"\n"))
    (insert "plot file")
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
    ))

(defun gnuplot--draw-pie (m header labelcol datetime)
  (if datetime (error "Date is not supported for piechart"))
  (let* ((col (if labelcol "2" "1"))
	 (rows (format "%d:%d" (if header 1 0)
		       (- m 1 (if header 1 0))))
	 (fmt1 "(sprintf('%s (%05.2f%%)', stringcolumn(1), percent($2)))")
	 (fmt2 "(sprintf('%05.2f%%', percent($1)))"))
    (insert "stats file using " col " nooutput prefix 'A'\n")
    (insert "angle(x)  = x*360/A_sum\n")
    (insert "percent(x)= x*100/A_sum\n")

    (insert "centerX = 0\n")
    (insert "centerY = 0\n")
    (insert "radius  = 1\n")
    (insert "pos     = 0\n")
    (insert "colour  = 0\n")

    (insert "yposmax = 0.95*radius\n")
    (insert "xpos    = 1.5*radius\n")
    (insert "ypos(i) = yposmax - i*(yposmax)/(1.0*A_records)\n")

    (insert "set xrange [-2:2]\n")
    (insert "set yrange [-2:2]\n")
    (insert "unset key\n")
    (insert "unset tics\n")
    (insert "unset border\n")
    
    (insert "plot file using "
    	    "(centerX):(centerY):(radius):(pos):(pos=pos+angle($"
    	    col ")):(colour=colour+1) "
    	    "with circle linecolor var notitle")
    (insert "\\\n, for [i=" rows "] file using (xpos):(ypos(i)):"
	    (if labelcol fmt1 fmt2)
	    " every ::i::i with labels left offset 3,0")
    (insert "\\\n, for [i=" rows "] '+' using (xpos):(ypos(i)) "
	    "with points pointtype 5 pointsize 4 linecolor i+1\n")
  ))

(defun gnuplot--draw-spider (n header labelcol)
  (let* ((j (if labelcol 1 0))
	 (colfmt (format "%d:%d" j (- n j))))
    (if (< (- n j) 3) (error "Atleast 3 axes needed"))

    (insert "set spiderplot\n")
    (insert "set style spiderplot fill transparent solid 0.30 border\n")
    (insert "set for [i=" colfmt "] paxis i range [0:100]\n")
    (if (not header)
	(insert "set for [i=" colfmt "] paxis i label sprintf('%d',i)\n"))
    (insert "set paxis 1 tics\n")
    (insert "set grid spider linetype black linewidth 0.2\n")

    (insert "plot for [i="
	    (format "%d:%d" (1+ j) n)
	    "] file using i:key(1)"
	    (if header " title columnhead" "")
	    "\n")    
    ))

(defun gnuplot--draw (&optional title style)
  (or killed-rectangle (error "No tabular data"))
  (let* ((name (make-temp-file "plot"))
         (data killed-rectangle)
         (comma (and (string-match-p "," (car data)) ","))
         (pipe  (and (string-match-p "|" (car data)) "|"))
	 (sep   (or comma pipe))
         (cols (split-string (car data) sep))
	 (m (length data))
         (n (length cols))
	 (str-check "^ *[a-zA-Z'\"]")
         buf xlabel ylabel header labelcol datetime)
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
          (pcase n
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
    ;; (setq style "spider")
    ;; (setq style (if labelcol "histograms"))
    ;; (setq style "pie")
    (with-temp-buffer
      ;; (insert "unset xdata\n")
      ;; (insert "unset ydata\n")
      ;; (insert "unset xrange\n")
      ;; (insert "unset yrange\n")
      ;; (insert "unset format xy\n")
      ;; (insert "unset timefmt\n")
      ;; First \n is to flush any pending commands
      (insert "\nreset\n")

      (insert "set datafile separator "
	      (if sep (format "'%s'" sep) "whitespace") "\n")
      (insert "set style fill solid 0.5\n")
      (insert "set title  '" (or title  "Title")  "'\n")
      (insert "file='" name "'\n")

      (pcase style
	("pie" (gnuplot--draw-pie m header labelcol datetime))
	("spider" (gnuplot--draw-spider n header labelcol))
	(_ (gnuplot--draw-line style m n xlabel ylabel labelcol datetime)))

      (newline)
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))

    ;; Cleanup
    (kill-buffer buf)
    ;; (delete-file name)
    ))

;; styles lines, points, linespoints, impulses, dots, steps, errorbars (or
;; yerrorbars), xerrorbars, xyerrorbars, boxes, boxerrorbars, boxxyerrorbars
;;;###autoload
(defun gnuplot-rectangle (prefix)
  (interactive "P")
  (let ((styles gnuplot--styles)
	title style)
    (when prefix
      (setq title (read-string "Title: "))
      (setq style (cdr (assoc (completing-read "Style: " styles) styles)))
      )
    (gnuplot--draw title style)))


(provide 'graph)
