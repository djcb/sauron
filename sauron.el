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
(eval-when-compile (require 'cl))

(defvar sauron-modules
  '(sauron-erc sauron-dbus sauron-org)
  "List of sauron modules to use. Currently supported are:
sauron-erc and sauron-dbus.")

;; user settable variables
(defvar sauron-separate-frame t
  "Whether the sauron should use a separate frame.")

(defvar sauron-frame-geometry "8x100+0-0"
  "Geometry (size, position) of the the sauron frame, in X geometry
  notation.")

(defvar sauron-min-priority 3
  "The minimum priority range for sauron to consider -- ie. all
events with priority < MIN will be ignored, and not passed to the
hook functions.")

(defvar sauron-watch-patterns nil
  "The list of regular expressions to watch for - when an event has
one of these words, the priority of the event will raised to at
least `sauron-min-priority'.")

(defvar sauron-event-format "%t %o %m"
  "Format of a sauron event line. The following format parameters are available:
%t: the timestamp (see `sauron-timestamp-format' for its format)
%p: priority of the event (an integer [1..5])
%o: the origin of the event
%m: the message for this event.")

(defvar sauron-timestamp-format "%Y-%m-%d %H:%M:%S"  
  "Format for the timestamps, as per `format-time-string'.")

(defvar sauron-max-line-length 80
  "Maximum length of messages in the log (longer messages will be
  truncated. If set to nil, there is no maximum.")

(defvar sauron-event-block-functions nil
  "Hook to be called *before* an event is added. If all of the
hooked functions return nil, the event is allowed to be added. The
function takes the following arguments:
  (ORIGIN MSG PROPS), where:
ORIGIN is a symbol denoting the source of the event (ie.,'erc or 'dbus)
MSG is the message for this event
PROPS is a backend-specific plist.
If the hook is not set, all events are allowed.")

(defvar sauron-event-added-functions nil
  "Hook to be called *after* an event is added. If any of the hook
functions return non-nil, the event is blocked from being
added. The hook function takes the following arguments:
  (ORIGIN MSG PROPS), where:
ORIGIN is a symbol denoting the source of the event (ie.,'erc or 'dbus)
MSG is the message for this event
PROPS is a backend-specific plist.")


;;  faces; re-using the font-lock stuff...
(defface sauron-timestamp-face
  '((t :inherit font-lock-type-face))
  "Face for a sauron time stamp.")

(defface sauron-message-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for a sauron event message.")

(defface sauron-origin-face
  '((t :inherit font-lock-pseudo-keyword-face))
  "Face for a sauron event origin.")

;; these highlight faces are for use in backends
(defface sauron-highlight1-face
  '((t :inherit font-lock-warning-face))
  "Face to highlight certain things (1).")

(defface sauron-highlight2-face
  '((t :inherit font-lock-string-face))
  "Face to highlight certain things (2).")

(defface sauron-highlight3-face
  '((t :inherit font-lock-constant-face))
  "Face to highlight certain things (3).")

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
  (sauron-add-event 'sauron 1 "sauron has started"))

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
  (sauron-add-event 'sauron 1 "sauron has stopped")
  (sauron-hide))


(defun sr-pattern-matches (msg ptrnlist)
  "Return t if any regexp in the list PTRNLIST matches MSG,
otherwise return nil."
  ;; (when ptrnlist
  ;;   (message "MATCH %S %S => %S" (car ptrnlist) msg
  ;;     (string-match (car ptrnlist) msg)))
  (cond
    ((null ptrnlist) nil)                ;; no match
    ((string-match (car ptrnlist) msg) t) ;; match   
    (t (sr-pattern-matches msg (cdr ptrnlist))))) ;; continue searching

    
;; the main work horse function
(defun sauron-add-event (origin prio msg &optional func props)
  "Add a new event to the Sauron log with:
ORIGIN the source of the event (e.g., 'erc or 'dbus or 'org)
PRIO the priority of the event, integer [0..5]
MSG a string describing the event.
Then, optionally:
FUNC if non-nil, a function called when user activates the event in the log.
PROPS an origin-specific property list that will be passed to the hook funcs."
  ;; since this is called by possibly external modules, scrutinize the params
  ;; a bit more, to make debugging easier
  (unless (symbolp origin)
    (error "sauron-add-event: ORIGIN must be a symbol, not %S" origin))
  (unless (and (integerp prio) (>= prio 0) (<= prio 5))
    (error "sauron-add-event: PRIO  ∈ [0..5], not %S" prio))
  (unless (stringp msg) 
    (error "sauron-add-event: MSG must be a string, not %S" msg))
  (unless (or (null func) (functionp func))
    (error "sauron-add-event: FUNC must be nil or a function, not %S"
      func))
  (unless (or (null props) (listp props))
    (error "sauron-add-event: PROPS must be nil or a plist, not %S" props))

  ;; if priority is below `sauron-min-priority, but it does match one of our
  ;; `sauron-watch-patterns, raise its priority to `sauron-min-priority'.
  (let ((prio (cond
		((>= prio sauron-min-priority) prio) ;; don't change
		((sr-pattern-matches msg sauron-watch-patterns)
		  sauron-min-priority) ;; raise priority
		(t prio)))) ;; otherwise, dont change
    
    ;; we allow this event only if it's prio >= `sauron-min-priority' and
    ;; running the `sauron-event-block-functions' hook evaluates to nil.
    (when (and (>= prio sauron-min-priority)
	    (null (run-hook-with-args-until-success
		     'sauron-event-block-function origin prio msg props)))
      (let* ((line (format-spec sauron-event-format
		     (format-spec-make
		       ?t (propertize (format-time-string sauron-timestamp-format
					(current-time)) 'face 'sauron-timestamp-face)
		       ?p (format "%d" prio)
		       ?o (propertize (symbol-name origin) 'face 'sauron-origin-face)
		       ?m msg)))
	      ;; add the callback as a text property, remove any embedded newlines,
	      ;; truncate if necessary append a newline
	      (line (concat 
		      (truncate-string-to-width 
			(propertize (replace-regexp-in-string "\n" " " line)
			  'callback func)
			sauron-max-line-length 0 nil t)
		      "\n"))
	      (inhibit-read-only t))
	(sr-create-buffer-maybe) ;; create buffer if it did not exist yet
	(with-current-buffer sr-buffer
	  (goto-char (point-max))
	  (insert line))
	(run-hook-with-args
	  'sauron-event-added-functions origin prio msg props)))))
  
  
(defun sauron-activate-event ()
  "Activate the callback for the current sauron line, and remove
any special faces from the line."
  (interactive)
  (unless (eq major-mode 'sauron-mode)
    (error "Not in sauron mode"))
  (let* ((callback (get-text-property (point) 'callback))
  	  (inhibit-read-only t))
    ;; remove the funky faces
    (put-text-property (line-beginning-position) (line-end-position)
      'face 'default) 
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
	(progn
	  (switch-to-buffer-other-frame sr-buffer)
	  (let ((frame-params
		  (append
		    '((tool-bar-lines . 0) (menu-bar-lines . 0))
		    (x-parse-geometry sauron-frame-geometry))))		  
	    (modify-frame-parameters nil frame-params)))
	(switch-to-buffer sr-buffer)))
      (set-window-dedicated-p (selected-window) t)))

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


;; some helper function for writing event hooks
(defun sauron-aplay (path)
  "Play a wav-file at PATH using program aplay."
  (unless (and (file-readable-p path) (file-regular-p path))
    (error "%s is not a playable file" path))
  (unless (executable-find "aplay")
    (error "aplay not found"))
  (call-process "aplay" nil 0 nil "-q" "-N" path))

(defun sauron-sox (path)
  "Play a wav-file at PATH using program sox."
  (unless (and (file-readable-p path) (file-regular-p path))
    (error "%s is not a playable file" path))
  (unless (executable-find "sox")
    (error "sox not found"))
  (call-process "sox" nil 0 nil "--volume=9" "-V0" "-q" path "-d"))

(defun sauron-gnome-osd (msg secs)
  "Display MSG on your screen for SECS second... for really important stuff."
  (unless (executable-find "gnome-osd-client")
    (error "gnome-osd-client not found"))
  (let ((xmlmsg
	  (concat ;; weird XML... but this should work
	    "<message id=\"sauron\" osd_vposition=\"center\" osd_halignment=\"left\" "
	    "osd_fake_translucent_bg=\"on\" "
	    "hide_timeout=\"" (format "%d" (* 1000 secs)) "\">"
	    msg
	    "</message>")))
    (call-process "gnome-osd-client" nil 0 nil "-f" "--dbus" xmlmsg)))

(defun sauron-zenity (msg)
  "Pop-up a zenity window with MSG."
  (unless (executable-find "zenity")
    (error "zenity not found"))
  (call-process "zenity" nil 0 nil "--info" "--title=Sauron"
    (concat "--text=" msg)))
 
  
(provide 'sauron)
