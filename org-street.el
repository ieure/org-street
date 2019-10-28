;;; org-street.el --- Physical addresses for Org   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; Version: 0.7.0
;; URL: https://github.com/ieure/org-street
;; Package-Requires: ((emacs "25") (nominatum "0.8.0"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Street is an extension for Org Mode for turning the names of
;; places into a `LOCATION' property containing their address.  Given
;; some freeform text approximately describing a location, it geocodes it
;; with OpenStreetMap’s Nominatum API to determine a canonical location.
;; If Nominatum returns multiple locations, a list is displayed to choose
;; from.
;;
;; Having a `LOCATION' is helpful; if you export your Org Agenda to iCal
;; files, the location becomes the address of the calendar entry.
;;
;; Usage
;; -----
;;
;; Place point inside an Org entry, then run:
;;
;; `M-x org-street-set-location RET'
;;
;; Then enter some text and press `RET' again.
;;

;;; Code:

(require 'nominatim)

;;; Code:

(defvar org-street--target nil
  "The target buffer and position to set the chosen location.")
(make-variable-buffer-local 'org-street--target)

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
    kmap)
  "Keymap for `org-street-choose' major mode.")

(define-derived-mode org-street-choose-mode org-mode
  "Org Street Choose"
  "Major mode to choose an Org Street result.")

(defun org-street--insert-locations (locations)
  "Place LOCATIONS into the `org-street-choose' buffer."
  (cl-loop for loc across locations
           do
           (insert (format "* %s :%s:\n"
                           (thread-first loc
                             (nominatum--printable)
                             (nominatum--printable->oneline))
                           (cdr (assoc 'type loc))))))

(defun org-street--choose (search-text target locations)
  "Bring up the org-street choose buffer.

   SEARCH-TEXT is the text that was used for the nominatim
   search.  TARGET is a cons cell of (BUFFER . LOCATION), the
   place the LOCATION property should be set.  LOCATIONS is the
   list of possible choices."
  (with-current-buffer (get-buffer-create (format "*Org Street[%s]*" search-text))
    (widen)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-street-choose-mode)
    (org-street--insert-locations locations)
    (indent-region (point-min) (point-max))
    (setq buffer-read-only t
          org-street--target target)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))


 ;; User-serviceable parts

(defun org-street-set-location (text)
  "Set the LOCATION property of the current entry to the address of TEXT.

   Nominatum is used to look up TEXT.  If a single location is
   returned, its address is used for the LOCATION property.  If
   multiple locations are returned, a list is displayed to choose
   from."
  (interactive (list (read-string "Location: ")))
  (let* ((matches (nominatim-geocode text))
         (n (length matches)))
    (cond
     ;; No matches, error
     ((= n 0) (error "No matches for `%s'" text))

     ;; One match, put it in.
     ((= n 1) (org-set-property "LOCATION" (nominatim--printable->oneline (nominatim--printable (aref matches 0)))))

     ;; Multiple matches, prompt
     (t (org-street--choose text (cons (current-buffer) (point)) matches)))))

(provide 'org-street)

;;; org-street.el ends here
