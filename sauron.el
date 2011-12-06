;;; sauron -- enhanced tracking of the world inside and outside your emacs
;;; buffers
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: 
;; Version: 0.0

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(add-to-list 'load-path "/home/djcb/Sources/sauron") ;; FIXME

(eval-when-compile (require 'cl))

(defvar sauron-modules
  '(sauron-erc
    sauron-dbus)
  "List of sauron modules to use. Currently supported are:
sauron-erc and sauron-dbus.")

;; user settable variables
(defvar sauron-separate-frame t
  "Whether the sauron should use a separate frame.")

(defvar sauron-event-format "%t %p %o %e %m"
  "Format of a sauron event line. The following format parameters are available:
%t: the timestamp (see `sauron-timestamp-format' for its format)
%p: priority of the event (an integer [1..5])
%o: the origin of the event
%e: the type of event
%m: the message for this event.")

(defvar sauron-timestamp-format "%Y-%m-%d %H:%M:%S"  
  "Format for the timestamps, as per `format-time-string'.")

(defvar sauron-max-line-length 80
  "Maximum length of messages in the log (longer messages will be
  truncated. If set to nil, there is no maximum.")

;;  faces; re-using the font-lock stuff...

(defface sauron-timestamp-face
  '((t :inherit font-lock-type-face))
  "Face for a sauron time stamp.")

(defface sauron-message-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for a sauron event message.")

(defface sauron-event-type-face
  '((t :inherit font-lock-doc-face))
  "Face for a sauron event-type.")

(defface sauron-origin-face
  '((t :inherit font-lock-pseudo-keyword-face))
  "Face for a sauron event origin.")

;;(setq sauron-mode-map nil)
(defvar sauron-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c"         'sauron-clear)
    (define-key map (kbd "RET") 'sauron-activate-event)
    map)
  "Keymap for the sauron buffer.")
(fset 'sauron-mode-map sauron-mode-map)

(defun sauron-mode ()
  "Major mode for the sauron."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sauron-mode-map)
  (setq
    major-mode 'sauron-mode
    mode-name "Sauron"
    truncate-lines t
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defun sauron-start ()
  "Start sauron."
  (interactive)
  (dolist (module sauron-modules)
    (require module)
    (let* ((start-func-name (concat (symbol-name module) "-start"))
	    (start-func (intern-soft start-func-name)))
      (if start-func
	(funcall start-func)
	(error "%s not defined" start-func-name))))
  (message "Sauron has started")
  (sauron-show)
  (sauron-add-event "sauron" "start" 1 nil "sauron has started"))

(defun sauron-stop ()
  "Stop sauron."
  (interactive)
  (dolist (module sauron-modules)
    (let* ((stop-func-name (concat (symbol-name module) "-stop"))
	    (stop-func (intern-soft stop-func-name)))
      (if stop-func
	(funcall stop-func)
	(error "%s not defined" stop-func-name))))
  (message "Sauron has stopped")
  (sauron-add-event "sauron" "stop" 1 nil "sauron has stopped")
  (sauron-hide))


(defun sauron-add-event (origin event-type priority callback
			  frm &rest params)
  "Add a new event to the Sauron log with ORIGIN (e.g., \"erc\")
  being a string describing the origin of the event, EVENT-TYPE the
  type of event (e.g.,\"ping\" when someone pinged you, PRIORITY
  gives the priority (an integer in the range [1..5]), CALLBACK, if
  non-nil a function to be called when user activates the event in
  the log, and FRM a format parameter, with optional PARAMS with
  the usual meaning."
  (let* ((msg (apply 'format frm params))
	  (line (format-spec sauron-event-format
		(format-spec-make
		  ?o (propertize origin 'face 'sauron-origin-face)
		  ?p (propertize (format "%d" priority))
		  ?e (propertize event-type 'face 'sauron-event-type-face)
		  ?t (propertize (format-time-string sauron-timestamp-format
				   (current-time)) 'face 'sauron-timestamp-face)
		  ?m (propertize msg 'face 'sauron-message-face))))
	  ;; add the callback as a text property, remove any embedded newlines,
	  ;; truncate if necessary append a newline
	  (line (concat 
		  (truncate-string-to-width 
		    (propertize (replace-regexp-in-string "\n" " " line)
		      'callback callback)
		    sauron-max-line-length 0 nil t)
		  "\n"))
	  (inhibit-read-only t))
    (sr-create-buffer-maybe) ;; create log if it did not exist yet
    (with-current-buffer sr-buffer
      (goto-char (point-max))
      (insert line))))

(defun sauron-activate-event ()
  "Activate the callback for the current sauron line, and remove
any special faces from the line."
  (interactive)
  (unless (eq major-mode 'sauron-mode)
    (error "Not in sauron mode"))
  (let* ((callback (get-text-property (point) 'callback))
  	  (inhibit-read-only t))
    ;; remove the funky faces
    (put-text-property (line-beginning-position)
      (line-end-position) 'face 'default) 
    (if callback
      (funcall callback)
      (message "No callback defined for this line."))))


(defun sauron-switch-to-buffer (buffer-or-name)
  "Switch to BUFFER-OR-NAME in another frame/window."
  (let* ((buf (if (buffer-live-p buffer-or-name)
		buffer-or-name
		(get-buffer buffer-or-name)))
	  (win (and buf (get-buffer-window buf 'visible))))
    (unless (and buf (buffer-live-p buf))
      (error "Buffer %s not found" channel))
    (let* ( ;; don't re-use the Sauron window
	    (display-buffer-reuse-frames t)
	    ;; if we're using a separate Sauron frame, don't split it
	    (pop-up-windows (not sauron-separate-frame))
	   ;; don't create new frames
	    (pop-up-frames nil)
	    ;; find a window for our buffer
	    (win  (display-buffer buf t)))
      (select-frame-set-input-focus (window-frame win)))))

(defun sauron-show ()
  "Show the sauron buffer. Depending on
`sauron-separate-frame', it will use the current frame or a new
one."
  (interactive)
  (setq sr-buffer (sr-create-buffer-maybe))
  (let* ((win (get-buffer-window sr-buffer))
	 (frame (and win (window-frame frame))))
    (if (and frame win)
      (progn
	(select-window win)
	(make-frame-visible frame))
      (if sauron-separate-frame
	(switch-to-buffer-other-frame sr-buffer)
	(switch-to-buffer sr-buffer))
      (set-window-dedicated-p (selected-window) t))))

(defun sauron-hide ()
  "Hide the sauron buffer and/of frame."
  (interactive)
  (unless (buffer-live-p sr-buffer)
    (error "No sauron buffer found"))
  (let* ((win (get-buffer-window sr-buffer t))
	  (frame (and win (window-frame win))))
    (if (and sauron-separate-frame (frame-live-p frame)) 
      (make-frame-invisible frame))))
;; TODO: handle the non-separate frame case


(defun sauron-clear ()
  "Clear the sauron buffer."
  (interactive)
  (when
    (and sr-buffer (buffer-live-p sr-buffer)
      (yes-or-no-p "Are you sure you want to clear the log? "))
    (with-current-buffer sr-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)))
    (message nil)))


(defun sauron-switch-to-target ()
  "Switch to the target buffer for the current line."
  (interactive)
  (let* ((target (get-text-property (point) 'target-buffer))
	  (buf (and target (get-buffer target)))
  	  (inhibit-read-only t))
    (unless target
      (error "No target specified for this line"))
    (unless buf
      (error "Target buffer not found"))
    ;; remove the funky faces
    (put-text-property (line-beginning-position)
      (line-end-position) 'face 'default) 
    (switch-to-buffer-other-windown target)))


;; internal settings
(defvar sr-buffer nil
  "*internal* The sauron buffer")

(defconst sr-buffer-name "*Sauron*"
  "*internal* Name of the sauron buffer.")

(defun sr-create-buffer-maybe ()
  "Create the sauron buffer, if it does not yet exist. Return the
sauron buffer."
  (unless (and sr-buffer (buffer-live-p sr-buffer))
    (setq sr-buffer (get-buffer-create sr-buffer-name))
    (with-current-buffer sr-buffer
      (sauron-mode)))
  sr-buffer)

(provide 'sauron)
