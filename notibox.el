;;; notibox.el --- PosFrame-based notification UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  mitch

;; Author: mitch <mitch@mitchmarq42.xyz>
;; Keywords:frames,convenience,help
;; Package-Requires: ((alert) (posframe))

;; This file is NOT part of GNU Emacs

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

;; 

;;; Code:

(require 'posframe)
(require 'alert)

(defvar notibox-width 40) ; characters
(defvar notibox-height 4) ; characters
(defvar notibox-border-color "green")
(defvar notibox-corner 'bottomright)
(defvar notibox-padding 30)
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
    (insert (format "%s\n%s\n%s" title
		    (propertize (make-string notibox-width ?─)
				'face `((:foreground ,notibox-border-color)))
		    body))))
;; (notibox--prepare-buffer "test" "this better work gadangit")

(defvar notibox-current-posframes nil)
(defun notibox--show ()
  (add-to-list 'notibox-current-posframes
	       (posframe-show (get-buffer-create "*notibox*")
			      :position (notibox--get-position)
			      :max-width notibox-width
			      :max-height notibox-height
			      :min-width notibox-width
			      :min-height notibox-height
			      :border-width 2
			      :border-color notibox-border-color))
  )

(defun notibox-alert (info)
  (let* ((message (plist-get info :message))
	 (title   (plist-get info :title)))
    (notibox--prepare-buffer title message)
    (notibox--show)))

(defun notibox--hide (frame)
  (posframe-hide "*notibox*"))

(defun notibox-alert-clear (info)
  (notibox--hide (car notibox-current-posframes))
  (pop notibox-current-posframes))

(alert-define-style 'notibox
		    :title "NotiBox"
		    :notifier #'notibox-alert
		    :remover #'notibox-alert-clear)
(setq alert-default-style 'notibox)

;; (alert "messaig" :title "tital")
;; (notibox-alert '(:title "five" :message "six"))
;; (notibox--hide 'anything)

(provide 'notibox)
;;; notibox.el ends here
