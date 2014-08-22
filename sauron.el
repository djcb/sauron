;;; sauron.el --- a frame tracking events inside and outside your emacs buffers
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Created: 06 Dec 2011
;; Version: 0.10
;; Keywords:comm,frames

;; NOTE: odd minor version numbers (0.3, 0.5, ...) are for development, even
;; numbers (0.2, 0.4, ...) are for releases (ELPA etc.). Also note scripts use
;; the ";; Version" line.

;; This file is not part of GNU Emacs.
;;
;; Sauron is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Sauron is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  For documentation, please see:
;;  https://github.com/djcb/sauron/blob/master/README.org

;; note - 'public' functions/variables are prefixed with 'sauron-', while
;; internal stuff starts with 'sr-'

;;; Code:
(require 'cl)

(defvar sauron-modules
  '(sauron-erc sauron-dbus sauron-org sauron-notifications
     sauron-twittering sauron-jabber sauron-identica)
  "List of sauron modules to use. Currently supported are:
sauron-erc, sauron-org and sauron-dbus, sauron-twittering,
sauron-jabber, sauron-identica.")

(defvar sauron-separate-frame t
  "Show sauron in a separate frame; if set to nil (*experimental*),
show sauron embedded in the current frame.")

(defvar sauron-frame-geometry "100x8+0-0"
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

(defvar sauron-watch-nicks nil
  "The list of nicks (as in, IRC-nicks) to watch for
joining/leaving IRC channels - when such an event comes the
priority of the event will raised to at least
`sauron-min-priority'.")

(defvar sauron-nick-insensitivity 60
  "The time (in seconds) we give lower priority to the events
coming from some nick after something has come in. This is to
prevent large numbers of beeps, light effects when dealing with
nick. Must be < 65536")

(defvar sauron-column-alist
  '( ( timestamp  .  20)
     ( origin     .   7)
     ( priority   .   4)
     ( message    . nil))
"An alist with elements (FIELD . WIDTH) which describes the columns to
  show. The fields are truncated to fit in WIDTH characters, with
  'nil' meaning 'no limit', so that one's should be reserverd for
  the last field. Also, the width should be >= 3.")

(defvar sauron-timestamp-format "%Y-%m-%d %H:%M:%S"
  "Format for the timestamps, as per `format-time-string'.")

(defvar sauron-max-line-length 80
  "Maximum length of messages in the log (longer messages will be
  truncated. If set to nil, there is no maximum.")

(defvar sauron-log-events nil
  "Whether to show write all Sauron events (even the filtered ones)
to the sauron log buffer.")

(defvar sauron-log-buffer-max-lines 1000
  "Maximum number of messages to store in the sauron log buffer.
Messages are removed from the buffer when the total number
exceeds this number.")

(defvar sauron-sticky-frame nil
  "If t, show the sauron frame on every (virtual) desktop.")

(defvar sauron-hide-mode-line nil
  "If t, hide the modeline in the sauron frame.")

(defvar sauron-scroll-to-bottom t
  "Wether to automatically scroll the sauron window to the bottom
when new events arrive. Set to nil to prevent this.")

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

(defvar sauron-prio-sauron-started 3
  "Sauron started event priority.")

(defvar sauron-prio-sauron-stopped 1
  "Sauron stopped event priority.")


;;  faces; re-using the font-lock stuff...
(defgroup sauron-faces nil
  "Faces for sauron."
  :group 'local
  :group 'faces)

(defface sauron-timestamp-face
  '((t :inherit font-lock-type-face))
  "Face for a sauron time stamp."
:group 'sauron-faces)

(defface sauron-message-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for a sauron event message."
  :group 'sauron-faces)

(defface sauron-origin-face
  '((t :inherit font-lock-variable-name-face))
  "Face for a sauron event origin."
  :group 'sauron-faces)

(defface sauron-priority-face
  '((t :inherit font-lock-operator))
  "Face for a sauron event priority."
  :group 'sauron-faces)

;; these highlight faces are for use in backends
(defface sauron-highlight1-face
  '((t :inherit font-lock-pseudo-keyword-face))
  "Face to highlight certain things (1) - for use in backends."
  :group 'sauron-faces)

(defface sauron-highlight2-face
  '((t :inherit font-lock-string-face :italic t))
  "Face to highlight certain things (2) - for use in backends."
  :group 'sauron-faces)

(defface sauron-highlight3-face
  '((t :inherit font-lock-constant-face))
  "Face to highlight certain things (3) - for use in backends.."
  :group 'sauron-faces)

(defface sauron-header-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for the header line."
  :group 'sauron-faces)

(defface sauron-event-handled-face
  '((t :strike-through t))
  "Face for a handled event."
:group 'sauron-faces)

;;(setq sauron-mode-map nil)
(defvar sauron-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c"               'sauron-clear)
    (define-key map (kbd "RET")       'sauron-activate-event)
    (define-key map (kbd "<mouse-2>") 'sauron-activate-event)
    (define-key map (kbd "<M-up>")    'sauron-activate-event-prev)
    (define-key map (kbd "<M-down>")  'sauron-activate-event-next)
    (define-key map "n"               'next-line)
    (define-key map "p"               'previous-line)
    (define-key map "q"               'bury-buffer)
    map)
  "Keymap for the sauron buffer.")
(fset 'sauron-mode-map sauron-mode-map)

(defvar sauron-debug nil
  "Whether to show errors. Unless you're actually debugging, it's
good to leave this to nil, since when there's some error happening
in your hook function, this may interfere with normal operation,
e.g. when using ERC.")

(defconst sr-column-name-alist
  '( ( timestamp    . "Time"   )
     ( origin       . "Orig"   )
     ( priority     . "Prio"   )
     ( message      . "Message"))
  "Alist of the column names.")

(defvar sr-buffer nil
  "*internal* The sauron buffer")

(defconst sr-buffer-name "*Sauron*"
  "*internal* Name of the sauron buffer.")

(defvar sr-log-buffer nil
  "*internal* The sauron log buffer.")

(defconst sr-log-buffer-name "*Sauron Log*"
  "*internal* Name of the sauron log buffer.")

(defvar sr-nick-event-hash nil
  "*internal* hash of nicks and the last time we raised an 'event'
  for that at >= `sauron-min-priority'.")

;; not all versions of emacs define 'fringe-columns
(unless (fboundp 'fringe-columns)
  (defun fringe-columns (dummy1 &optional dummy2) 0))

(defun sr-set-header-line ()
  "Set the header line for the sauron buffer."
  (setq header-line-format
    (cons
      (make-string (floor (fringe-columns 'left t)) ?\s)
      (map 'list
	(lambda (elm)
	  (let ((field (cdr (assoc (car elm) sr-column-name-alist)))
		 (width (cdr elm)))
	    (concat
	      (propertize
		(if width
		  (truncate-string-to-width field width 0 ?\s t)
		  field)
		'face 'sauron-header-face)
	      " ")))
	sauron-column-alist))))


(define-derived-mode sauron-mode nil "Sauron"
  "Major mode for sauron.

\\{sauron-mode-map}."
  (setq
   truncate-lines t
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary)
  (sr-set-header-line))

(defvar sr-running-p nil
  "*internal* Whether sauron is running.")

;;;###autoload
(defun sauron-start (&optional hidden)
  "Start sauron. If the optional parameter HIDDEN is non-nil,
don't show the sauron window."
  (interactive)
  (unless sr-running-p
    (setq sr-running-p t
      sr-nick-event-hash (make-hash-table :size 100 :test 'equal))
    (let ((started))
      (dolist (module sauron-modules)
	(require module)
	(let* ( (name (symbol-name module))
		(start-func-name (concat name "-start"))
		(start-func (intern-soft start-func-name)))
	  (if start-func
	    (when (funcall start-func)
	      (add-to-list 'started name t))
	    (error "%s not defined" start-func-name))))
      (message "Sauron has started")
      (unless hidden
	(sr-show))
      (sauron-add-event 'sauron sauron-prio-sauron-started
	(concat "sauron started: " (mapconcat 'identity started ", "))))))

;;;###autoload
(defun sauron-start-hidden ()
  "Start sauron, but don't show the window."
  (interactive)
  (sauron-start t))


(defun sauron-stop ()
  "Stop sauron."
  (interactive)
  (when sr-running-p
    (dolist (module sauron-modules)
      (let* ((stop-func-name (concat (symbol-name module) "-stop"))
	      (stop-func (intern-soft stop-func-name)))
	(if stop-func
	  (funcall stop-func)
	  (error "%s not defined" stop-func-name))))
    (message "Sauron has stopped")
    (setq sr-running-p nil)
    (sauron-add-event 'sauron sauron-prio-sauron-stopped "sauron has stopped")
    (sr-hide)))

(defun sr-pattern-matches (str ptrnlist cmpfunc)
  "Return t if any regexp in the list PTRNLIST matches STR,
otherwise return nil. CMPFUNC is the comparison function."
  (cond
    ((or (null ptrnlist) (null str)) nil) ;; no match
    ((funcall cmpfunc (car ptrnlist) str) t) ;; match
    (t (sr-pattern-matches str (cdr ptrnlist) cmpfunc)))) ;; continue searching


(defun sr-fresh-nick-event (nick)
  "Whether we have triggered an 'event' for NICK in the last
`sauron-nick-insensitivity' SECS. If so, return t and do nothing;
otherwise, return nil, and update the table with the NICK and a
timestamp."
   ;; we only store the lsb, which is good enough for 2^16 seconds.
  (let* ((now-lsb (float-time))
	 (tstamp (gethash nick sr-nick-event-hash))
	  (diff (when tstamp (- now-lsb tstamp))))
    (when (or (not diff) (> diff sauron-nick-insensitivity))
      (puthash nick now-lsb sr-nick-event-hash))))


(defun sr-calibrated-prio (msg props prio)
  "Re-calibrate the PRIO for MSG with PROPS:
1) if we already saw something from this nick in the last
`sauron-nick-insensitity' seconds, set priority to 2 (see `sr-fresh-nick-event')
2) otherwise:
   if msg matches `sauron-watch-patterns', prio = prio + 1
   if nick matches `sauron-watch-nicks', prio = prio + 1
3) if prio > 5, prio = 5
Returns the new priority."
  (let ((prio prio) (nick (plist-get props :sender)))
    (if (and nick (not (sr-fresh-nick-event nick))) ;;
      (setq prio 2) ;; set prio to 2 for nicks we got events for recently
      (progn
	(when (sr-pattern-matches msg sauron-watch-patterns 'string-match)
	  (incf prio))
	(when (sr-pattern-matches nick sauron-watch-nicks 'string=)
	  (incf prio))
	(when (> prio 5)
	  (setq prio 5))))
      prio))


(defmacro sr-ignore-errors-maybe (&rest body)
  "Execute BODY; if sauron-debug is nil, do so in a
`ignore-errors'-block, otherwise run with without such a block.
For debugging purposes."
  `(if sauron-debug
     (progn ,@body)
     (declare (debug t) (indent 0))
     (condition-case nil (progn ,@body) (error nil))))


(defun sr-event-line (origin prio msg)
  "Get a one-line string describing the event."
  (mapconcat
    (lambda (f-w)
      (let* ((field (car f-w)) (width (cdr f-w))
	      (str
		(case field
		  ('timestamp
		    (propertize (format-time-string sauron-timestamp-format
				  (current-time)) 'face 'sauron-timestamp-face))
		  ('priority (propertize (format "%d" prio)
			       'face 'sauron-priority-face))
		  ('origin   (propertize (symbol-name origin)
			       'face 'sauron-origin-face))
		  ('message  msg)
		  (otherwise (error "Unknown field %S" field)))))
	(if width
	  (truncate-string-to-width str width 0 ?\s t)
	  str)))
    sauron-column-alist " "))

(defvar sr-buffer nil
  "*internal* The sauron buffer.")

(defun sr-scroll-to-bottom ()
  "Scroll to the bottom of the sauron frame."
  (dolist (win (get-buffer-window-list sr-buffer nil t))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -1))))

(defun sr-add-to-log (line)
  "Add LINE to the Sauron log buffer."
  (unless (buffer-live-p sr-log-buffer)
    (setq sr-log-buffer (sr-create-buffer-maybe sr-log-buffer-name)))
  (with-current-buffer sr-log-buffer
    (goto-char (point-max))
    (insert line)
    (sr-clear-log-buffer-maybe)))


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

  ;; recalculate the prio, based on watchwords, nicks involved, and recent
  ;; history.
  ;;  (message "old prio: %d" prio)
  (setq prio (sr-calibrated-prio msg props prio))
  ;;  (message "new prio:%S msg:%S" prio msg)
  ;; we allow this event only if it's prio >= `sauron-min-priority' and
  ;; running the `sauron-event-block-functions' hook evaluates to nil.
  (let* ((line (sr-event-line origin prio msg))
	 ;; add the callback as a text property, remove any embedded newlines,
	 ;; truncate if necessary append a newline
	 (line (replace-regexp-in-string "\n" " " line))
	 (line (if sauron-max-line-length
		   (truncate-string-to-width
		    line sauron-max-line-length 0 nil t)
		 line))
	 (line (concat (propertize line 'callback func) "\n"))
	 (inhibit-read-only t))

    ;; when logging is enabled, write the line to the sauron log as well
    (when sauron-log-events (sr-add-to-log line))

    (when (and (>= prio sauron-min-priority)
	       (null (sr-ignore-errors-maybe
		      ;; ignore errors unless we're debugging
		      (run-hook-with-args-until-success
		       'sauron-event-block-functions origin prio msg props))))
      ;; create buffer if it did not exist yet
      (setq sr-buffer (sr-create-buffer-maybe sr-buffer-name))
      (with-current-buffer sr-buffer
	(goto-char (point-max))
	(insert line)
	(when sauron-scroll-to-bottom
	  (sr-scroll-to-bottom)))
      (sr-ignore-errors-maybe ;; ignore errors unless we're debugging
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
      'face 'sauron-event-handled-face)
    (if callback
      (funcall callback)
      (message "No callback defined for this line."))))


(defun sauron-activate-event-prev (&optional n)
  "Move to the previous line, and then activate the event for that
line. Optionally, takes an integer N (prefix argument), to go to
the Nth previous line."
  (interactive "P")
  (forward-line (- (or n 1)))
  (sauron-activate-event))

(defun sauron-activate-event-next (&optional n)
  "Move to the previous line, and then activate the event for that
line. Optionally, takes an integer N (prefix argument), to go to
the Nth previous line."
  (interactive "P")
  (forward-line (or n 1))
  (sauron-activate-event))

(defun sauron-switch-to-marker-or-buffer (mbn)
  "Switch to MBN (marker-or-buffer-or-name) in another
frame/window."
  (if (not mbn)
    (message "No target buffer defined")
    (let* ((buf) (pos))
      (if (markerp mbn)
	(setq buf (marker-buffer mbn) pos (marker-position mbn))
	(setq buf (or (buffer-live-p mbn) (get-buffer mbn))))
      (unless (buffer-live-p buf)
	(error "Buffer not found"))
      (let* ( ;; don't re-use the Sauron window
	      (display-buffer-reuse-frames t)
	      (pop-up-windows nil)
	      ;; don't create new frames
	      (pop-up-frames nil)
	      ;; find a window for our buffer
	      (win (display-buffer buf t)))
	(select-frame-set-input-focus (window-frame win))
	(goto-char (if pos pos (point-max)))))))


(defun sr-show ()
  "Show the sauron buffer; if `sauron-separate-frame' is non-nil,
show it in a separate frame, otherwise, show it embedded in the
current frame."
  (if sauron-separate-frame
    (sr-show-in-separate-frame)
    (sr-show-embedded)))

(defun sr-show-in-separate-frame ()
  "Show the sauron buffer in a separate frame."
  (setq sr-buffer (sr-create-buffer-maybe sr-buffer-name))
  (let* ((win (get-buffer-window sr-buffer))
	  (frame (and win (window-frame win))))
    (if (and frame win)
      (progn
	(select-window win)
	(make-frame-visible frame))
      (progn
	  (switch-to-buffer-other-frame sr-buffer)
	  (let ((frame-params
		  (append
		    `((tool-bar-lines . 0) (menu-bar-lines . 0)
		       (unsplittable . t) (sticky . ,sauron-sticky-frame))
		    (x-parse-geometry sauron-frame-geometry))))
	    (modify-frame-parameters nil frame-params))))
	(if sauron-hide-mode-line
	  (setq mode-line-format nil))
    (set-window-dedicated-p (selected-window) t)))

(defun sr-split-window-below (new-win-size)
  "Split the window, return the new window below. We need this
function because emacs 23 does not support the negative size
argument to split-window."
  (split-window
    (frame-root-window)
    (- (window-height (frame-root-window)) new-win-size)))


(defun sr-show-embedded ()
  "Show the sauron buffer embedded in the current frame."
  (setq sr-buffer (sr-create-buffer-maybe sr-buffer-name))
  (let* ((win (or (get-buffer-window sr-buffer)
		(sr-split-window-below 8))))
    (with-selected-window win
      (switch-to-buffer sr-buffer)
      (if sauron-hide-mode-line
	(setq mode-line-format nil))
      (set-window-dedicated-p (selected-window) t))))


(defun sr-hide ()
  "Hide the sauron buffer, window and/or frame."
  (unless (buffer-live-p sr-buffer)
    (error "No sauron buffer found"))
  (let* ((win (get-buffer-window sr-buffer t))
	  (frame (and win (window-frame win))))
    ;; depending on whether we showing sauron in a window or in a separate frame
    (if (and (frame-live-p frame)
	  (eq win (frame-root-window frame)))
      (make-frame-invisible frame)
      (delete-window win))))

(defun sauron-toggle-hide-show ()
  "Toggle between showing/hiding the Sauron window or frame, and
start sauron if it weren't so already."
  (interactive)
  ;; sr-sauron-visible may be wrong, let's double-check
  (if (and (buffer-live-p sr-buffer)
	(window-live-p (get-buffer-window sr-buffer)))
    (sr-hide)
    (progn
      (sauron-start)
      (sr-show))))

(defun sauron-pop-to-buffer ()
  "Popup sauron buffer."
  (interactive)
  (unless (buffer-live-p sr-buffer)
    (error "No sauron buffer found.  Please start sauron by `sauron-start'."))
  (pop-to-buffer sr-buffer))

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


(defun sr-create-buffer-maybe (name)
  "Create the sauron buffer of NAME, if it does not yet exist. Return the
sauron buffer."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (equal major-mode 'sauron-mode)
	(sauron-mode)))
    buffer))


(defun sr-clear-log-buffer-maybe ()
  "Clear the sauon log "
  (when sr-log-buffer
    (with-current-buffer sr-log-buffer
      (save-excursion
        (let ((lines (count-lines (point-min) (point-max)))
	       (inhibit-read-only t))
          (when (> lines sauron-log-buffer-max-lines)
            (forward-line (- sauron-log-buffer-max-lines lines))
            (delete-region (point-min) (point))))))))



;; adapters

(defun sauron-alert-el-adapter (origin prio msg &optional props)
  "A handler function to feed sauron events through John Wiegley's
alert.el (https://github.com/jwiegley/alert). You can use it like:
  (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)
Obviously, 'alert.el' must be loaded for this to work."
  ;; sauron priorities [0..5] mapping alert severities
  (when (fboundp 'alert)
    (let ((sev (nth prio '(trivial trivial low normal moderate high urgent)))
	   (cat origin)   ;; origins map to alert categories
	   (title (format "Alert from %S" origin)))
      (alert msg :severity sev :title title :category cat))))



;; some convenience function sound/light fx in event hooks
(defun sauron-fx-aplay (path)
  "Play a wav-file at PATH using program aplay."
  (unless (and (file-readable-p path))
    (error "%s is not a playable file" path))
  (unless (executable-find "aplay")
    (error "aplay not found"))
  (call-process "aplay" nil 0 nil "-q" "-N" path))

(defun sauron-fx-gnome-osd (msg secs)
  "Display MSG on your screen for SECS second... for really important stuff."
  (unless (executable-find "gnome-osd-client")
    (error "gnome-osd-client not found"))
  (let ((xmlmsg
	  (concat ;; weird XML... but this should work
	    "<message id=\"sauron\" osd_vposition=\"center\" "
	    "osd_halignment=\"left\" "
	    "osd_fake_translucent_bg=\"on\" "
	    "hide_timeout=\"" (format "%d" (* 1000 secs)) "\">"
	    msg
	    "</message>")))
    (call-process "gnome-osd-client" nil 0 nil "-f" "--dbus" xmlmsg)))

(defun sauron-fx-mplayer (path)
  "Play a wav-file at PATH using program mplayer."
  (unless (and (file-readable-p path))
     (error "%s is not a playable file" path))
  (unless (executable-find "mplayer")
     (error "mplayer not found"))
  (call-process "mplayer" nil 0 nil "-really-quiet" path))

(defun sauron-fx-notify (title msg secs)
  "Send a notification with TITLE and MSG to the notification
daemon of D-bus, and show the message for SECS seconds. Return the
id for the notification."
  (when (require 'dbus nil 'noerror)
    (let ((note-id (random 65535)))
      (dbus-call-method
	:session "org.freedesktop.Notifications"
	"/org/freedesktop/Notifications"
	"org.freedesktop.Notifications" "Notify"
	"Sauron"
	note-id
	"emacs" title msg
	'(:array) '(:array :signature "{sv}") ':int32 secs)
      note-id)))

(defun sauron-fx-sox (path)
  "Play a wav-file at PATH using program sox."
  (unless (and (file-readable-p path) (file-regular-p path))
    (error "%s is not a playable file" path))
  (unless (executable-find "sox")
    (error "sox not found"))
  (call-process "sox" nil 0 nil "--volume=9" "-V0" "-q" path "-d"))

(defun sauron-fx-zenity (msg)
  "Pop-up a zenity window with MSG."
  (unless (executable-find "zenity")
    (error "zenity not found"))
  (call-process "zenity" nil 0 nil "--info" "--title=Sauron"
    (concat "--text=" msg)))

(provide 'sauron)

;;; sauron.el ends here
