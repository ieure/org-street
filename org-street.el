
(require 'nominatim)

(defvar org-street--target nil
  "The target buffer and position to set the chosen location.")
(make-variable-buffer-local 'org-street--target)

(defun org-street-org-set-location (place)
  "Set the LOCATION property of this Org entry."
  (interactive (list (read-string "Location: ")))
  (let* ((matches (nominatim-geocode place))
         (n (length matches)))
    (cond
     ;; No matches, error
     ((= n 0) (error "No matches for `%s'" place))

     ;; One match, put it in.
     ((= n 1) (org-set-property "LOCATION" (org-street--address (aref matches 0))))

     ;; Multiple matches, prompt
     (t (org-street--choose place (cons (current-buffer) (point)) matches)))))

(defun org-street--printable (loc)
  (let ((cc (thread-last (assoc 'address loc)
              (cdr)
              (assoc 'country_code)
              (cdr))))
    (cond ((string= cc "us") (org-street--printable-us loc))
          (t (error "Don't know how to handle addresses in `%s'" cc)))))

(defun org-street--printable-us (loc)
  (let ((elements)
        (addr (cdr (assoc 'address loc)))
        (type-sym (intern (cdr (assoc 'type loc)))))

    ;; Business name
    (if-let ((biz-name (cdr (assoc type-sym addr))))
        (progn
          (push biz-name elements)
          (push :break elements)))

    ;; House number
    (if-let ((house-number (cdr (assoc 'house_number addr))))
        (push house-number elements))

    ;; Road
    (if-let ((road (cdr (assoc 'road addr))))
        (progn
          (push road elements)
          (push :break elements)))

    ;; City
    (if-let ((city-or-county (or (cdr (assoc 'city addr))
                                 (cdr (assoc 'county addr)))))
        (progn
          (push city-or-county elements)
          (push :soft-break elements)))

    ;; State
    (if-let ((state (cdr (assoc 'state addr))))
        (progn
          (push state elements)))

    ;; Postcode
    (if-let ((postcode (cdr (assoc 'postcode addr))))
        (progn
          (push postcode elements)
          (push :break elements)))

    ;; Country
    (if-let ((country (cdr (assoc 'country addr))))
        (push country elements))

    (reverse elements)))

(defun org-street--printable->oneline (printable-loc)
  (thread-first
      (lambda (s elt)
        (cond ((keywordp elt) (concat s ","))
              (t (concat s " " elt))))
    (reduce printable-loc)))

(defun org-street--printable->nline (printable-loc)
  (thread-first
      (lambda (s elt)
        (cond ((eq :soft-break elt) (concat s ","))
              ((eq :break elt) (concat s "\n"))
              (t (concat s " " elt))))
    (reduce printable-loc)))

(defun org-street-choose-select-location ()
  "Choose a location."
  (interactive)
  (org-mark-element)
  (let ((location (thread-last
                      (buffer-substring-no-properties (+ 2 (region-beginning))
                                                      (region-end))
                    (s-trim)
                    (replace-regexp-in-string ":[^:]+:$" "")))
        (target-buf (car org-street--target))
        (target-buf-loc (cdr org-street--target)))
    (with-current-buffer target-buf
      (goto-char target-buf-loc)
      (org-set-property "LOCATION" location))
    (kill-buffer-and-window)))

(defvar org-street-choose-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "n" 'org-next-visible-heading)
    (define-key kmap (kbd "<down>") 'org-next-visible-heading)
    (define-key kmap "p" 'org-previous-visible-heading)
    (define-key kmap (kbd "<up>") 'org-previous-visible-heading)
    (define-key kmap "q" 'kill-buffer-and-window)
    (define-key kmap (kbd "RET") 'org-street-choose-select-location)
    kmap))

(define-derived-mode org-street-choose-mode org-mode
  "Org Street Choose"
  "Major mode to choose an Org Street result.")

(defun org-street--insert-locations (locations)
  (cl-loop for loc across locations
           do
           (insert (format "* %s :%s:\n"
                           (thread-first loc
                             (org-street--printable)
                             (org-street--printable->oneline))
                           (cdr (assoc 'type loc))))))

(defun org-street--choose (search-text target locations)
  (with-current-buffer (get-buffer-create (format "*Org Street[%s]*" search-text))
    (widen)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-street-choose-mode)
    (org-street--insert-locations locations)
    (indent-region (point-min) (point-max))
    (org-street-choose-mode)
    (setq buffer-read-only t)
    (setq org-street--target target)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(provide 'org-street)
