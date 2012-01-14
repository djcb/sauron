;;; sauron-org.el --- an org-mode (appt) tracking module, part of sauron
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

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
  (if (not (boundp 'appt-disp-window-function))
    (message "sauron-org not available")
    (unless sr-org-running
      (setq ;; save the old one, set the new one
	sr-org-old-appt-func appt-disp-window-function
	appt-disp-window-function (function sr-org-handler-func)
	sr-org-running t))))

(defun sauron-org-stop ()
  "Stop checking appointments; restore the old function."
  (when sr-org-running
    (setq
      appt-disp-window-function (function sr-org-old-appt-func)
      sr-org-running nil)))


(defun sr-org-handler-func (minutes-to-app new-time msg)
  "Handle appointment reminders. FIXME: apparently these params
could be lists, too."
  (let* ((left (string-to-number minutes-to-app))
	  (prio ;; priorities, hard-coded....
	    (cond
	      ((> left 15) 2)
	      ((> left 10) 3)
	      ((> left 5)  3)
	      ((> left 2)  4)
	      (t 5))))
    (sauron-add-event 'org prio
      (format "%s minutes left before %s" minutes-to-app msg)
      'org-agenda-list
      `(:minutes-left ,left :msg ,msg))
    ;; call the old function as well, if defined
    (when sr-org-old-appt-func
      (funcall sr-org-old-appt-func minutes-to-app new-time msg))))
  
(provide 'sauron-org)  

;;; sauron-org ends here
