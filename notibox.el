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
(require 'shr) ;for `shr-string-pixel-width'

(defvar notibox-width 40) ; characters
(defvar notibox-height 4) ; characters
(defvar notibox-border-color "#808080")
(defgroup notibox nil
  "The notification box.")
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
(defun notibox--get-position (&optional stackdepth)
  "Return the starting coordinate at which to place the next notibox, as cons.

If STACKDEPTH is non-nil and nonzero, return a position that far down."
  (let* (
	 (stackdepth (or stackdepth 0))
	 (parent-width (frame-pixel-width))
	 (child-width (* notibox-width (shr-string-pixel-width " ")))
	 (parent-height (frame-pixel-height))
	 (child-height (* notibox-height (line-pixel-height)))
	 (padded-height (+ child-height notibox-padding))
	 (x-justify (pcase notibox-corner
			     ((or 'topleft 'bottomleft) notibox-padding)
			     ((or 'topright 'bottomright)
			      (- parent-width (+ child-width notibox-padding)))))
	 (y-justify (pcase notibox-corner
		      ((or 'topleft 'topright) notibox-padding)
		      ((or 'bottomleft 'bottomright)
		       (- parent-height (+ child-height notibox-padding)))))
	 (y-direction (pcase notibox-corner
			((or 'topleft 'topright) #'+)
			((or 'bottomleft 'bottomright) #'-)))
	 (y-coord (funcall y-direction y-justify (* stackdepth child-height)))
	 )
    (cons x-justify y-coord)
    ))
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
(cl-defun notibox--show (&key timeout &key depth)
  "Show the notibox currently prepared, with optional TIMEOUT, at DEPTH."
  (add-to-list 'notibox-current-posframes
	       (posframe-show (get-buffer-create "*notibox*")
			      :position (notibox--get-position depth)
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
  "Show a notibox corresponding with INFO.  Can* be used as a backend for `alert'."
  (let* ((message (plist-get info :message))
	 (title   (plist-get info :title))
	 (timeout (plist-get info :persistent))
	 (depth   (plist-get info :depth)))
    (notibox--prepare-buffer title message)
    (notibox--show :timeout (unless timeout alert-fade-time)
		   :depth (or depth (- (length notibox-current-posframes) 1)))))

(defun notibox--resolve-frame (object)
  "Return the /frame reference/ signified by OBJECT, whatever it may be."
  (cond
   ((framep object) object) ;this is the easy one
   ((windowp object) (window-frame object))
   ((bufferp object) (window-frame (get-window-buffer object)))
   ((stringp object)
    (window-frame (get-buffer-window (get-buffer object))))
   ((symbolp object)
    (window-frame (get-buffer-window (get-buffer (format "%s" object)))))
   ))
;; (notibox--resolve-frame "*notibox*")

(defun notibox--hide (frame)
  "Stop showing FRAME."
  (posframe-hide (window-buffer (frame-selected-window frame))))

(defun notibox-delete (frame)
  "Delete the notibox FRAME.

If FRAME is the root Emacs window, or some other symbol, hide all notiboxes."
  (let ((frame (notibox--resolve-frame frame)))
    (if frame 
	(notibox--hide frame)
      (notibox--hide (car notibox-current-posframes))
      ))
  
  (pop notibox-current-posframes))
;; (frame-parent (selected-frame)) ;=> nil
;; (frame-live-p (car notibox-current-posframes))
;; (notibox-delete (car notibox-current-posframes))

(defun notibox--tail-echoarea ()
  "Show `current-message' in the notibox.  If that does not exist, probably hide it."
  (if (current-message)
      (let ((notibox-border-color "#0faa0f"))
	(notibox-alert `(
			 :title ,(format "%s" (current-buffer))
				:message ,(current-message)
				:depth 0)))
    (if notibox-current-posframes
	(notibox-delete (car notibox-current-posframes))))
)

(defun notibox/setup-timer ()
  "Start running notibox."
  (interactive)
  (run-with-timer notibox--refresh-delay notibox--refresh-delay #'notibox--tail-echoarea))

;; (notibox-delete 'anything)
(defun notibox-test-alert ()
  "Show a sample notibox to prove we can."
  (interactive)
  (notibox-alert '(:title "five" :message "six")))
;; (notibox-alert '(:title "一" :message "二" :timeout 5 :depth 0))

(provide 'notibox)
;;; notibox.el ends here
