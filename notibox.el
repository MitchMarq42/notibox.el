;;; notibox.el --- PosFrame-based notification UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  mitch

;; Author: mitch <mitch@mitchmarq42.xyz>
;; Keywords:frames,convenience,help
;; Package-Requires: ((posframe) (alert))

;; This file is NOT part of GNU Emacs.

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

;; This package shows a temporary child frame which mirrors the contents of the
;; echo area.  It was designed to be used with configurations that hide the echo
;; area by splitting it into a `minibuffer-frame'.

;;; Code:

(require 'posframe)
(require 'alert)

(defvar notibox-width 40) ; characters
(defvar notibox-height 4) ; characters
(defvar notibox-border-color "#808080")
(defcustom notibox-corner 'bottomright
  "Corner in which to show the NotiBox."
  :type '(choice
	  (const :tag "Top Left" 'topleft)
	  (const :tag "Bottom Left" 'bottomleft)
	  (const :tag "Top Right" 'topright)
	  (const :tag "Bottom Right" 'bottomright)))
(defvar notibox-padding 30)
(defvar alert-fade-time 5); seconds, also provided by `alert' package
(defvar notibox--refresh-delay 0.1) ; seconds. Probably don't change this one.
(defun notibox--get-position ()
  "Return the starting coordinate at which to place the notibox, as cons."
  (let* ((parent-width (frame-pixel-width))
	 (child-width (* notibox-width (string-pixel-width " ")))
	 (parent-height (frame-pixel-height))
	 (child-height (* notibox-height (line-pixel-height))))
    (pcase notibox-corner
      ('topleft (cons notibox-padding notibox-padding))
      ('topright (cons (- parent-width (+ child-width notibox-padding))
		       notibox-padding))
      ('bottomleft (cons notibox-padding
			 (- parent-height (+ child-height notibox-padding))))
      ('bottomright (cons
		     (- parent-width (+ child-width notibox-padding))
		     (- parent-height (+ child-height notibox-padding)))))))
;; (notibox--get-position)

(defun notibox--prepare-buffer (title body)
  "Populate the `*notibox*' buffer with TITLE and BODY properly formatted."
  (with-current-buffer (get-buffer-create "*notibox*")
    (erase-buffer)
    (insert (format "%s\n%s\n%s" ;; (buttonize title #'view-echo-area-messages)
		    title
		    (propertize (make-string notibox-width ?─)
				'face `((:foreground ,notibox-border-color)))
		    body))))
;; (notibox--prepare-buffer "test" "this better work gadangit")

(defvar notibox-current-posframes nil)
(defun notibox--show (&optional timeout)
  "Show the notibox currently prepared, with optional TIMEOUT."
  (add-to-list 'notibox-current-posframes
	       (posframe-show (get-buffer-create "*notibox*")
			      :position (notibox--get-position)
			      :left-fringe 0
			      :right-fringe 0
			      :max-width notibox-width
			      :max-height notibox-height
			      :min-width notibox-width
			      :min-height notibox-height
			      :border-width 2
			      :border-color notibox-border-color
			      :timeout timeout))
  nil)

(defun notibox-alert (info)
  (let* ((message (plist-get info :message))
	 (title   (plist-get info :title))
	 (timeout (plist-get info :persistent)))
    (notibox--prepare-buffer title message)
    (notibox--show (unless timeout alert-fade-time))))

(defun notibox--hide (frame)
  (posframe-hide "*notibox*"))

(defun notibox-delete (frame)
  ;; TODO: if parent, hide all. Otherwise just the one.
  (notibox--hide (car notibox-current-posframes))
  (pop notibox-current-posframes))

(defun notibox--tail-echoarea ()
  (if (current-message)
      (notibox-alert `(
		       :title ,(format "%s" (current-buffer))
		       :message ,(current-message)))
    (if notibox-current-posframes
	(notibox-delete 'current))))

(defun notibox/setup-timer ()
  (interactive)
  (run-with-timer notibox--refresh-delay notibox--refresh-delay #'notibox--tail-echoarea))

;; (notibox--hide 'anything)
(defun notibox-test-alert ()
  (interactive)
  (notibox-alert '(:title "five" :message "six")))

(provide 'notibox)
;;; notibox.el ends here
