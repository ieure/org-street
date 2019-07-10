
(require 'nominatim)

(defun org-street-org-set-location (place)
  (interactive (list (read-string "Location: ")))
  (let* ((matches (nominatim-geocode place))
         (n (length matches)))
    (cond
     ;; No matches, error
     ((= n 0) (error "No matches for `%s'" place))

     ;; One match, put it in.
     ((= n 1) (org-set-property "LOCATION" (org-street--address (aref matches 0))))

     ;; Multiple matches, prompt
     (t (error "Not yet implemented.")))))

(defun org-street-next-location (&optional arg)
  (interactive "P")
  (goto-char (field-end))
  (forward-line 1))

(defun org-street-previous-location (&optional arg)
  (interactive "P")
  (goto-char (field-beginning))
  (forward-line -1))

(defvar org-street-choose-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "q" 'kill-buffer-and-window)
    (define-key kmap "n" 'org-street-next-location)
    (define-key kmap "p" 'org-street-previous-location)
    (define-key kmap (kbd "RET") 'org-street-choose-location)
    (define-key kmap "mouse-1" 'org-street-choose-location)
    kmap))

(define-derived-mode org-street-choose-mode fundamental-mode "Org Street"
  "Major mode to choose an Org Street location."
  (setq buffer-read-only t
        buffer-modified-p nil))

(defconst org-street-choose-mode-font-lock-keywords
  `(
    (,(rx bol
          (group (minimal-match (1+ anything)))
          "(" (group (1+ anything)) ")" eol)
     (1 bold)
     (2 hi-yellow)))

  "Font-locking defintions for ‘org-street-choose-mode’.")

(defun org-street--insert-locations (locations)
  (cl-loop for loc across locations
           for address = (cdr (assoc 'address loc))
           for type = (intern (cdr (assoc 'type loc)))
           do
           (insert
            (thread-first
                (concat
                 (propertize (cdr (assoc type address)) 'face 'bold)
                 " (" (propertize (cdr (assoc 'type loc)) 'face 'hi-yellow) ")\n"

                 "  " (org-street--fields->str loc '(house_number road)) "\n"
                 "  " (org-street--fields->str loc '(city state portcode)) "\n"
                 "  " (org-street--fields->str loc '(country)) "\n")
              (propertize 'field t 'mouse-face 'highlight)
              (concat "\n")))))

(defun org-street--choose (search-text locations)
  (with-current-buffer (get-buffer-create (format "*Org Street[%s]*" search-text))
    (widen)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-street--insert-locations locations)
    (org-street-choose-mode)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(provide 'org-street)
