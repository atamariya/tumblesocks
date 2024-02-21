;;; shop.el --- Shopping client for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Anand Tamariya <atamarya@gmail.com>
;; Keywords: shop, ecommerce

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

(require 'shr)

(defvar shop--cookies nil)
(defvar shop--services '(bb jio))
(defvar shop--items nil)

(defvar shop--jio-query-string "{\"requests\":[{\"indexName\":\"prod_mart_master_vertical\",\"params\":\"analyticsTags=%5B%22web%22%2C%22560064%22%2C%22PANINDIABEAUTY%22%2C%22PANINDIABOOKS%22%2C%22PANINDIACRAFT%22%2C%22PANINDIADIGITAL%22%2C%22PANINDIAFASHION%22%2C%22PANINDIAFURNITURE%22%2C%22TNJ0%22%2C%22PANINDIAGROCERIES%22%2C%22PANINDIAHOMEANDKITCHEN%22%2C%22PANINDIAHOMEIMPROVEMENT%22%2C%22PANINDIAJEWEL%22%2C%22SK36%22%2C%22PANINDIASTL%22%5D&attributesToHighlight=%5B%5D&attributesToRetrieve=%5B%22*%22%2C%22-algolia_facet%22%2C%22-alt_class_keywords%22%2C%22-available_stores%22%2C%22-avg_discount%22%2C%22-avg_discount_pct%22%2C%22-avg_discount_rate%22%2C%22-avg_mrp%22%2C%22-avg_selling_price%22%2C%22-search_keywords%22%5D&clickAnalytics=true&enableRules=true&facetFilters=%5B%5B%22category_tree.level0%3ACategory%22%5D%5D&facets=%5B%22avg_discount_pct%22%2C%22avg_selling_price%22%2C%22brand%22%2C%22category_level.level4%22%2C%22category_tree.level0%22%2C%22category_tree.level1%22%5D&filters=(NOT%20category_level.level1%3AWellness)%20AND%20availability_status%3AA%20%20AND%20(mart_availability%3AJIO%20OR%20mart_availability%3AJIO_INSTA%20OR%20mart_availability%3AJIO_WA%20OR%20mart_availability%3AJIO_INSTA_WA)%20AND%20(buybox_mrp.PANINDIABEAUTY.available%3Atrue%20OR%20buybox_mrp.PANINDIABOOKS.available%3Atrue%20OR%20buybox_mrp.PANINDIACRAFT.available%3Atrue%20OR%20buybox_mrp.PANINDIADIGITAL.available%3Atrue%20OR%20buybox_mrp.PANINDIAFASHION.available%3Atrue%20OR%20buybox_mrp.PANINDIAFURNITURE.available%3Atrue%20OR%20buybox_mrp.TNJ0.available%3Atrue%20OR%20buybox_mrp.PANINDIAGROCERIES.available%3Atrue%20OR%20buybox_mrp.PANINDIAHOMEANDKITCHEN.available%3Atrue%20OR%20buybox_mrp.PANINDIAHOMEIMPROVEMENT.available%3Atrue%20OR%20buybox_mrp.PANINDIAJEWEL.available%3Atrue%20OR%20buybox_mrp.SK36.available%3Atrue%20OR%20buybox_mrp.PANINDIASTL.available%3Atrue)%20AND%20(available_stores%3APANINDIABEAUTY%20OR%20available_stores%3APANINDIABOOKS%20OR%20available_stores%3APANINDIACRAFT%20OR%20available_stores%3APANINDIADIGITAL%20OR%20available_stores%3APANINDIAFASHION%20OR%20available_stores%3APANINDIAFURNITURE%20OR%20available_stores%3ATNJ0%20OR%20available_stores%3APANINDIAGROCERIES%20OR%20available_stores%3APANINDIAHOMEANDKITCHEN%20OR%20available_stores%3APANINDIAHOMEIMPROVEMENT%20OR%20available_stores%3APANINDIAJEWEL%20OR%20available_stores%3ASK36%20OR%20available_stores%3APANINDIASTL)%20AND%20((inventory_stores%3AALL%20OR%20inventory_stores%3ASVRR%20OR%20inventory_stores%3ASK8Q%20OR%20inventory_stores%3ASANS%20OR%20inventory_stores%3ASURR%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3ASANQ%20OR%20inventory_stores%3ASANS%20OR%20inventory_stores%3ASURR%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3ASANQ%20OR%20inventory_stores%3Aelectronics_zone%20OR%20inventory_stores%3AS236%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3AR696%20OR%20inventory_stores%3ASE40%20OR%20inventory_stores%3AR804%20OR%20inventory_stores%3AY331%20OR%20inventory_stores%3ASK1M%20OR%20inventory_stores%3ASD78%20OR%20inventory_stores%3ASJ93%20OR%20inventory_stores%3AR396%20OR%20inventory_stores%3ASLKO%20OR%20inventory_stores%3AY350%20OR%20inventory_stores%3Afashion_zone%20OR%20inventory_stores%3AS535%20OR%20inventory_stores%3ASLG2%20OR%20inventory_stores%3ASXJJ%20OR%20inventory_stores%3AR741%20OR%20inventory_stores%3ASURR%20OR%20inventory_stores%3AR300%20OR%20inventory_stores%3ASAFY%20OR%20inventory_stores%3ASLI1%20OR%20inventory_stores%3AV050%20OR%20inventory_stores%3ASH50%20OR%20inventory_stores%3AY524%20OR%20inventory_stores%3AR351%20OR%20inventory_stores%3ASJ14%20OR%20inventory_stores%3AV012%20OR%20inventory_stores%3ASL9F%20OR%20inventory_stores%3AR975%20OR%20inventory_stores%3AS402%20OR%20inventory_stores%3AS223%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3AV017%20OR%20inventory_stores%3ASL7L%20OR%20inventory_stores%3ASB41%20OR%20inventory_stores%3ASQ9C%20OR%20inventory_stores%3ASLTP%20OR%20inventory_stores%3ASANS%20OR%20inventory_stores%3ASL7Q%20OR%20inventory_stores%3ASG84%20OR%20inventory_stores%3ASANQ%20OR%20inventory_stores%3ASE87%20OR%20inventory_stores%3ASH62%20OR%20inventory_stores%3ASL7O%20OR%20inventory_stores%3ASH09%20OR%20inventory_stores%3ASK3Y%20OR%20inventory_stores%3AV027%20OR%20inventory_stores%3AS352%20OR%20inventory_stores%3A254%20OR%20inventory_stores%3A420%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3A60%20OR%20inventory_stores%3A270%20OR%20inventory_stores%3Agroceries_zone_non-essential_services%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3ATNJ0%20OR%20inventory_stores%3Agroceries_zone_essential_services%20OR%20inventory_stores%3ASANS%20OR%20inventory_stores%3ASURR%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3ASANQ%20OR%20inventory_stores%3ASANS%20OR%20inventory_stores%3ASURR%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3ASANQ%20OR%20inventory_stores%3ATMX5%20OR%20inventory_stores%3ASK36%20OR%20inventory_stores%3ASANS%20OR%20inventory_stores%3ASURR%20OR%20inventory_stores%3Ageneral_zone%20OR%20inventory_stores%3ASANQ%20OR%20inventory_stores_3p%3AALL%20OR%20inventory_stores_3p%3ASVRR%20OR%20inventory_stores_3p%3ASK8Q%20OR%20inventory_stores_3p%3ASANS%20OR%20inventory_stores_3p%3ASURR%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3ASANQ%20OR%20inventory_stores_3p%3ASANS%20OR%20inventory_stores_3p%3ASURR%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3ASANQ%20OR%20inventory_stores_3p%3Aelectronics_zone%20OR%20inventory_stores_3p%3AS236%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3AR696%20OR%20inventory_stores_3p%3ASE40%20OR%20inventory_stores_3p%3AR804%20OR%20inventory_stores_3p%3AY331%20OR%20inventory_stores_3p%3ASK1M%20OR%20inventory_stores_3p%3ASD78%20OR%20inventory_stores_3p%3ASJ93%20OR%20inventory_stores_3p%3AR396%20OR%20inventory_stores_3p%3ASLKO%20OR%20inventory_stores_3p%3AY350%20OR%20inventory_stores_3p%3Afashion_zone%20OR%20inventory_stores_3p%3AS535%20OR%20inventory_stores_3p%3ASLG2%20OR%20inventory_stores_3p%3ASXJJ%20OR%20inventory_stores_3p%3AR741%20OR%20inventory_stores_3p%3ASURR%20OR%20inventory_stores_3p%3AR300%20OR%20inventory_stores_3p%3ASAFY%20OR%20inventory_stores_3p%3ASLI1%20OR%20inventory_stores_3p%3AV050%20OR%20inventory_stores_3p%3ASH50%20OR%20inventory_stores_3p%3AY524%20OR%20inventory_stores_3p%3AR351%20OR%20inventory_stores_3p%3ASJ14%20OR%20inventory_stores_3p%3AV012%20OR%20inventory_stores_3p%3ASL9F%20OR%20inventory_stores_3p%3AR975%20OR%20inventory_stores_3p%3AS402%20OR%20inventory_stores_3p%3AS223%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3AV017%20OR%20inventory_stores_3p%3ASL7L%20OR%20inventory_stores_3p%3ASB41%20OR%20inventory_stores_3p%3ASQ9C%20OR%20inventory_stores_3p%3ASLTP%20OR%20inventory_stores_3p%3ASANS%20OR%20inventory_stores_3p%3ASL7Q%20OR%20inventory_stores_3p%3ASG84%20OR%20inventory_stores_3p%3ASANQ%20OR%20inventory_stores_3p%3ASE87%20OR%20inventory_stores_3p%3ASH62%20OR%20inventory_stores_3p%3ASL7O%20OR%20inventory_stores_3p%3ASH09%20OR%20inventory_stores_3p%3ASK3Y%20OR%20inventory_stores_3p%3AV027%20OR%20inventory_stores_3p%3AS352%20OR%20inventory_stores_3p%3A254%20OR%20inventory_stores_3p%3A420%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3A60%20OR%20inventory_stores_3p%3A270%20OR%20inventory_stores_3p%3Agroceries_zone_non-essential_services%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3ATNJ0%20OR%20inventory_stores_3p%3Agroceries_zone_essential_services%20OR%20inventory_stores_3p%3ASANS%20OR%20inventory_stores_3p%3ASURR%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3ASANQ%20OR%20inventory_stores_3p%3ASANS%20OR%20inventory_stores_3p%3ASURR%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3ASANQ%20OR%20inventory_stores_3p%3ATMX5%20OR%20inventory_stores_3p%3ASK36%20OR%20inventory_stores_3p%3ASANS%20OR%20inventory_stores_3p%3ASURR%20OR%20inventory_stores_3p%3Ageneral_zone%20OR%20inventory_stores_3p%3ASANQ))&highlightPostTag=__%2Fais-highlight__&highlightPreTag=__ais-highlight__&hitsPerPage=12&maxValuesPerFacet=100&page=0&query=")

(defmacro with-normalized-var-json (vars keys data &rest body)
  "Bind each VAR to its associated KEYed value in DATA and execute BODY."
  ;; (declare (indent 1) (debug t))
  (let* ((b `(mapcar (lambda (v) (json-resolve v ,data t)) ,keys)))
    `(cl-destructuring-bind ,vars ,b ,@body)))

(defmacro with-normalized-var (vars values &rest body)
  "Bind each VAR to its associated KEYed value in DATA and execute BODY."
  `(cl-destructuring-bind ,vars ,values ,@body))

(defun shop--fn-call (service fn &rest args)
  (apply (intern-soft
	  (format "shop--api-%s-%s" fn service))
	 args))

(defun shop--api-request (url &optional method data header-lines)
  (let* ((headers (mapcar (lambda (a)
			    (let ((v (split-string a ": ")))
			      (cons (car v) (cadr v))))
			  header-lines))
	 (url-request-method method)
         (url-request-data data)
         (url-request-extra-headers headers)
	 (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (tumblesocks-api-process-response)
      )))

(defun shop--api-request-bb (url &optional method data header-lines)
  (let* ((channel "X-Channel: BB-WEB")
	 (lines (append (cdr (assoc 'bb shop--cookies)) header-lines)))
    (push channel lines)
    (shop--api-request url method data lines)
    ))

(defun shop--api-search-bb (item)
  (let* ((url (concat "https://www.bigbasket.com/listing-svc/v1/product/term-completion?term=" item))
	 (res (shop--api-request-bb url)))
    ;; (json-serialize res)
    res))

(defun shop--api-add-bb (item)
  (let* ((url "https://www.bigbasket.com/mapi/v3.5.2/c-incr-i/")
	 (data (format "{\"prod_id\":%s,\"qty\":1,\"_bb_client_type\":\"web\",\"first_atb\":1}"
		       item))
	 (res (shop--api-request-bb url "POST" data
				    `("Content-Type: application/json"))))
    (if (not (string= (json-resolve "status" res t) "OK"))
	(error (json-resolve "message" res t)))
    ;; (json-serialize res)
    res))

(defun shop--api-remove-bb (item)
  (let* ((url "https://www.bigbasket.com/mapi/v3.5.2/c-decr-i/")
	 (data (format "{\"prod_id\":%s,\"qty\":1,\"_bb_client_type\":\"web\",\"first_atb\":1}"
		       item))
	 (res (shop--api-request-bb url "POST" data
				    `("Content-Type: application/json"))))
    (if (not (string= (json-resolve "status" res t) "OK"))
	(error (json-resolve "message" res t)))
    ;; (json-serialize res)
    res))


(defun shop--api-request-jio (url &optional method data header-lines)
  (let* ((lines (append (cdr (assoc 'jio shop--cookies)) header-lines)))
    (shop--api-request url method data lines)
    ))

(defun shop--api-search-jio (item)
  (let* ((url "https://3yp0hp3wsh-2.algolianet.com/1/indexes/*/queries")
	 (data (concat shop--jio-query-string item "\"}]}"))
	 (res (shop--api-request-jio url "POST" data
				     `("Content-Type: application/json"))))
    ;; (json-serialize res)
    res))

(defvar shop--jio-cart "421051364")
(defun shop--api-add-jio (item)
  (let* ((url (format "https://www.jiomart.com/mst/rest/v1/5/cart/add_item?product_code=%s&qty=1&seller_id=1&n=1703240024017&cart_id=%s"
		      item shop--jio-cart))
	 (res (shop--api-request-jio url)))
    (if (not (string= (json-resolve "status" res t) "success"))
	(error (json-resolve "message" res t)))
    ;; (json-serialize res)
    res))

(defun shop--api-remove-jio (item)
  (let* ((url (format "https://www.jiomart.com/mst/rest/v1/5/cart/remove_item?product_code=%s&qty=1&seller_id=1&n=1703240024017&cart_id=%s"
		      item shop--jio-cart))
	 (res (shop--api-request-jio url)))
    (if (not (string= (json-resolve "status" res t) "success"))
	(error (json-resolve "message" res t)))
    ;; (json-serialize res)
    res))

(defun shop--items-display ()
  (let* ((buf (get-buffer-create "Search Results")))
    (with-current-buffer buf
      (erase-buffer)
      (jit-image-mode)
      (dolist (item shop--items)
	(with-normalized-var
	 (service id desc brand w img unit-price unit price)
	 item

	 (jit-insert-image-from-url img nil 50)
	 ;; (shr-insert-document `(img ((src . ,img)
	 ;; 			     (height . "50")
	 ;; 			     (width  . "50")
	 ;; 			     )))
	 (insert (format " %-8s %-25s %10s %8.2f %13s "
			 (substring brand 0 (min 8 (length brand)))
			 (substring desc 0 (min 25 (length desc)))
			 w (if (stringp price)
			       (string-to-number price)
			     (or price 0))
			 (cond ((string-prefix-p "unit" unit t)
				(format "(%.2f/%s)" price unit))
			       ((string-prefix-p "pcs" unit t)
				(format "(%.2f/%s)" unit-price "unit"))
			       (t
				(format "(%.2f/100%s)" unit-price unit)))))
	 (widget-create 'group
			:format "%v"
			:notify (lambda (widget _ev source)
				  (let* ((all (widget-get widget :children))
					 (plus (nth 0 all))
					 (label (nth 1 all))
					 (minus (nth 2 all))
					 (i (string-to-number (widget-value label))))
				    (when (eq source plus)
				      (shop--fn-call service "add" id)
				      (message "Adding %s to %s" id service)
				      (setq i (1+ i)))
				    (when (and (> i 0) (eq source minus))
				      (shop--fn-call service "remove" id)
				      (message "Removing %s from %s" id service)
				      (setq i (1- i)))
				    (widget-value-set label
						      (number-to-string i))
				    ))
			(widget-convert 'push-button :tag-glyph "plus")
			(widget-convert 'item :format "%v" "0")
			(widget-convert 'push-button :tag-glyph "minus"))
	 (insert service)
	 (newline)
	 ))
      (goto-char (point-min)))
    (display-buffer buf)
    ))

(defun shop--item-search-for-service (service item)
  (let* ((buf (get-buffer-create "Search Results"))
	 (res (shop--fn-call service "search" item))
	 (re-num "\\([0-9]+\\(\.[0-9]\\)?\\) ?\\(K*g\\|ml\\|L\\|pcs\\)")
	 (keys-result
	  (pcase service
	    ('bb  '("products"))
	    ('jio '("results[0].hits"))
	    ))
	 (keys-item
	  (pcase service
	    ('bb  '("id" "desc" "brand.name" "w" "images[0].s"
		    "pricing.discount.prim_price.sp"))
	    ('jio '("product_code" "display_name" "brand"
		    "uom_for_price_compare_value" "image_url"
		    "buybox_mrp.TNJ0.price")) ;; TNJ0 TLI7
	    ))
	 prod s v unit-price unit mult)

    (with-current-buffer buf
      (with-normalized-var-json
       (products) keys-result res
       (dotimes (i (length products))
	 (setq prod (aref products i)
	       mult 1.0)

	 (with-normalized-var-json
	  (id desc brand w img price)
	  keys-item
	  prod
	  (when (eq service 'bb)
	    (when (string-match (concat "\\([0-9]+\\) ?x ?" re-num) w)
	      (setq mult (* mult (string-to-number (match-string 1 w)))
		    w (format "%s %s" (match-string 2 w) (match-string 4 w))))
	    ;; (message "test1 %s %s %s" desc w mult)
	    )
	  (when (eq service 'jio)
	    ;; (message "test1 %s %s %s" desc w price)
	    (setq desc (replace-regexp-in-string " per " " 1 " desc))
	    (if (zerop w) (setq w "1 unit"))
	    (when (setq i (string-match re-num desc))
	      (setq w (format "%s %s"
			      (match-string 1 desc)
			      (match-string 3 desc))
		    desc (substring desc 0 i)))
	    (unless price
	      (let* ((mrp (json-resolve "buybox_mrp" prod t))
		     (key (car (hash-table-keys mrp))))
		(setq price (json-resolve (format "%s.price" key) mrp t))))
	    ;; (message "test2 %s %s %s" desc w price)
	    (setq img (concat "https://www.jiomart.com/" img)))
	  ;; (message "test %s %s" price img)

	  (setq price (if (stringp price)
			  (string-to-number price)
			(or price 0)))
	  ;; Calculate unit price
	  ;; (pp (list id desc brand w img unit-price unit price))
	  (when (setq s (split-string w))
	    (setq v (string-to-number (nth 0 s))
		  w (concat (if (> mult 1) (format "%dx" mult)) w)
		  unit (nth 1 s))
	    (when (string-prefix-p "k" unit t)
	      (setq mult (* mult 1000.0)
		    unit (substring unit 1)))
	    (when (string-prefix-p "l" unit t)
	      (setq mult (* mult 1000)
		    unit "ml"))
	    (when (string-prefix-p "pcs" unit t)
	      (setq mult (* mult 100)))
	    (setq unit-price (* (/ price v mult) 100)))
	  (push (list (symbol-name service) id desc brand w img unit-price unit price)
		shop--items)
	  )))
      )
    ))

(defun shop--item-search (item)
  (let* ()
    (setq shop--items nil)
    (dolist (s shop--services)
      (shop--item-search-for-service s item))
    (setq shop--items (sort shop--items 'shop--by-unit-price))
    (shop--items-display)
    ))

(defun shop--get-item ()
  (buffer-substring-no-properties
   (save-excursion (beginning-of-line) (point))
   (save-excursion (end-of-line) (point))
   ))


(defun shop-search-line ()
  (interactive)
  (shop--item-search (shop--get-item)))

(defun shop-cart ()
  (interactive)
  (let* ();(buf (get-buffer-create "Search Results")))
    ;; (setq shop--items nil)
    ;; (dolist (s shop--services)
    ;;   (shop--item-search-for-service s item))
    ))

;; (service id desc brand w img unit-price unit price)
(defun shop--by-price (a b)
  (< (nth 8 a) (nth 8 b)))

(defun shop--by-unit-price (a b)
  (< (nth 6 a) (nth 6 b)))

(defun shop--by-brand (a b)
  (string< (nth 3 a) (nth 3 b)))

(defun shop--by-service (a b)
  (string< (nth 0 a) (nth 0 b)))

(defun shop--by-desc (a b)
  (string< (nth 2 a) (nth 2 b)))

(defun shop-sort ()
  (interactive)
  (let* ((fields '(service desc brand price unit-price))
	 (field (completing-read "Field: " fields nil t))
	 ;; (field "service")
	 )
    (setq field (if (string-empty-p field) "price" field)
	  shop--items (sort shop--items
			    (intern-soft (concat "shop--by-" field))
			    ))
    (shop--items-display)
    ))

(defvar shop-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "c" 'shop-cart)
    (define-key map "o" 'shop-sort)
    (define-key map "q" 'shop-mode)
    (define-key map "s" 'shop-search-line)
    map))

;;;###autoload
(define-minor-mode shop-mode
  "Minor mode for shopping."
  :lighter " Shop"
  :keymap shop-mode-map
  )


(provide 'shop)
