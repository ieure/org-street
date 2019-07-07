
(require 'dbus)
(require 's)

(defconst org-street--nominatim-url "https://nominatim.openstreetmap.org"
  "Base URL of Nominatim.")

(defun org-street-query (text)
  "Look up TEXT with Nominatim.
   Returns an array of results."
  (with-current-buffer
      (thread-first
          (format "%s/search?format=json&addressdetails=1&q=%s"
                  org-street--nominatim-url text)
        (url-retrieve-synchronously t))
    (goto-char (point-min))
    (search-forward "\n\n")
    (json-read)))

(defun org-street--current-location ()
  
  )

(defun org-street--fields->str (entry fields)
  "Return a printable version of fields FIELDS in ENTRY."
  (let* ((type (assoc 'type entry))
         (entry-type-key (intern (cdr type)))
         (address (cons type
                        (cdr (assoc 'address entry))))
         (address (cons
                   `(type-value . ,(cdr (assoc entry-type-key address)))
                   address))
         (commas '(type-value road city)))
    (thread-last
        (cl-loop for field in fields
                 for raw-val = (cdr (assoc field address))
                 for val = (if (memq field commas)
                               (concat raw-val ",")
                             raw-val)
                 when val collect val)
      (s-join " "))))

(defun org-street--address (entry)
  "Return a printable address for a Nominatim result."
  (org-street--fields->str
   entry
   '(type-value house_number road city state postcode country)))

(defun org-street-org-set-location (place)
  (interactive (list (read-string "Location: ")))

  (let* ((matches (org-street-query place))
         (n (length matches)))
    (cond
     ;; No matches, error
     ((= n 0) (error "No matches for `%s'" place))

     ;; One match, put it in.
     ((= n 1) (org-set-property "LOCATION" (org-street--address (aref matches 0))))

     ;; Multiple matches, prompt
     (t ))))

(defun org-street-next-location (&optional arg)
  (interactive "P"))

(defun org-street-previous-location (&optional arg)
  (interactive "P"))

(defun org-street-choose-location ()
  (interactive))

(defvar org-street-choose-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "q" 'kill-buffer-and-window)
    (define-key kmap "n" 'next-line)
    (define-key kmap "p" 'previous-line)
    (define-key kmap "M-n" 'org-street-next-location)
    (define-key kmap "M-P" 'org-street-previous-location)
    (define-key kmap (kbd "RET") 'org-street-choose-location)
    kmap))

(define-derived-mode org-street-choose-mode fundamental-mode "Org Street"
  "Major mode to choose an Org Street location."
  (setq buffer-read-only t
        buffer-modified-p nil))

(defun org-street--insert-locations (locations)
  (cl-loop for loc across locations
           for type = (intern (cdr (assoc 'type loc)))
           do
           (insert (format "%s (%s)\n" (cdr (assoc type loc)) type))
           (insert (concat "  " (org-street--fields->str loc '(house_number road))
                           "\n"))
           (insert (concat "  " (org-street--fields->str loc '(city state portcode))
                           "\n"))
           (insert (concat "  " (org-street--fields->str loc '(country))
                           "\n\n"))))

(defun org-street--choose (search-text locations)
  (with-current-buffer (get-buffer-create (format "*Org Street[%s]*" search-text))
    (widen)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-street--insert-locations locations)
    (org-street-choose-mode)
    (pop-to-buffer (current-buffer))))

(provide 'org-street)
