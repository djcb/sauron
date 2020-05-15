;;; sauron-appt.el --- appointment tracking module, part of sauron
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

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
;; along with GNU Emacs.  If not, see <http://www.gnu.appt/licenses/>.

;;; Commentary:
;;  For documentation, please see:
;;  https://github.com/djcb/sauron/blob/master/README.appt

;;; Code:
(require 'appt nil 'noerror)
(eval-when-compile (require 'cl))

(defvar sauron-prio-appt-default 5
  "Appt event default priority.")

(defvar sauron-prio-appt-minutes-left-list
  '((15 2)
    (10 3)
    (5 3)
    (2 4))
  "A list of pairs, where the first element of each pair is the
number of minutes left before the appointment and the last
element is an Appt event priority.")

(defvar sr-appt-old-appt-func nil
  "*internal* The old appt appt function.")

(defvar sr-appt-running nil
  "*internal* Whether the appt-backend is active.")

(defun sauron-appt-start ()
  "Start watching appt (appt)."
  (if (not (boundp 'appt-disp-window-function))
    (progn
      (message "sauron-appt not available")
      nil)
    (unless sr-appt-running
      (setq ;; save the old one, set the new one
	sr-appt-old-appt-func appt-disp-window-function
	appt-disp-window-function (function sr-appt-handler-func)
	sr-appt-running t))
    t))

(defun sauron-appt-stop ()
  "Stop checking appointments; restore the old function."
  (when sr-appt-running
    (setq
      appt-disp-window-function (function sr-appt-old-appt-func)
      sr-appt-running nil)))

(defun sr-appt-handler-func (minutes-to-app new-time msg)
  "Handle appointment reminders - the actual work is done in
`sr-appt-handler-func-real', but this function deals with the
possibility of getting lists for the `minutes-to-app' and `msg'
arguments rather than single values."
   (when minutes-to-app
    (if (listp minutes-to-app)
      (progn
	(sr-appt-handler-func-real (car minutes-to-app) new-time (car msg))
	(sr-appt-handler-func (cdr minutes-to-app) new-time (cdr msg)))
      (sr-appt-handler-func-real minutes-to-app new-time msg))))

(defun sr-appt-handler-func-real (minutes-to-app new-time msg)
  "Handle appointment reminders. Also see: `sr-appt-handler-func.'"
  (let* ((left (string-to-number minutes-to-app))
         (prio (catch 'prio-found
                 (dolist (pair sauron-prio-appt-minutes-left-list)
                   (when (> left (car pair))
                     (throw 'prio-found (car (last pair)))))
                 sauron-prio-appt-default)))
    (sauron-add-event 'appt prio
      (format "%s minutes left before %s" minutes-to-app msg)
      'appt-agenda-list
      `(:minutes-left ,left :msg ,msg))
    ;; call the old function as well, if defined
    (when sr-appt-old-appt-func
      (funcall sr-appt-old-appt-func minutes-to-app new-time msg))))

(provide 'sauron-appt)

;;; sauron-appt ends here
