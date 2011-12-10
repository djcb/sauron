;;; sauron-org -- enhanced tracking of the world inside and outside your emacs
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
(require 'appt nil 'noerror)
(eval-when-compile (require 'cl))

;; this is really about 'appt', not 'org', but anyway...

(defvar sr-org-old-appt-func nil
  "*internal* The old org appt function.")

(defvar sr-org-running nil
  "*internal* Whether the org-backend is active.")


(defun sauron-org-start ()
  "Start watching org (appt)."
  (if (not (boundp 'appt-disp-window))
    (message "sauron-org not available")
    (unless sr-org-running
      (setq ;; save the old one, set the new one
	sr-org-old-appt-func appt-disp-window-function
	appt-disp-window-function (function sr-org-handler-func)
	sr-org-running t))))

(defun sauron-org-stop ()
  "Stop checking appointments; restore the old function."
  (when sr-org-running
    (setq appt-disp-window-function
      (function sr-org-old-appt-func))))

(defun sr-org-handler-func (minutes-to-app new-time msg)
  "Handle appointment reminders. FIXME: apparently these params
could be lists, too."
  (let* ((left (string-to-number minutes-to-app))
	  (prio ;; priorities, hard-coded....
	    (cond
	      ((> left 15) 3)
	      ((> left 10) 4)
	      ((< left 10) 5))))
    (sauron-add-event 'org prio
      (format "%s minutes left before %s" minutes-to-app msg)
      'org-agenda-list
      `(:minutes-left ,left :msg ,msg))
    ;; call the old function as well, if defined
    (when sauron-org-old-appt-func
      (funcall sauron-org-old-appt-func minutes-to-app new-time msg))))
  
(provide 'sauron-org)  
