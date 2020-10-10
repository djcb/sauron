;;; sauron-mu4e.el -- a ZEROCONF traking module, part of sauron

;; Copyright (C) 2016 Zachary Allison <zack@zackallison.com>

;; This file is not part of GNU Emacs.
;;
;; Sauron is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Sauron is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MELFEEDHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  For documentation, please see:
;;  https://github.com/djcb/sauron/blob/master/README.org

;;; Code:

(require 'mu4e nil 'noerror) ;; keep errors out if mu4e is not there

(defvar sauron-mu4e-priority 3
  "MU4E default-priority.")

(defcustom sauron-mu4e-formatter-function
  'sauron-mu4e-formatter
  "Formatter function.  Takes a MAIL object and returns a string to log.")

(defun sauron-mu4e-uread-messages (mails)
  "Add each email to the log.  Takes a list of mu4e mails as MAILS."
  (dolist (mail mails)
    (let ((log-message (funcall sauron-mu4e-formatter-function mail))
	  (callback (sauron-mu4e-make-link mail)))
      (sauron-add-event
       'mu4e
       sauron-mu4e-priority
       log-message
       callback))))

(defun sauron-mu4e-formatter (mail)
  "Format the mail for the log.  Takes a single mu4e mail as MAIL."
  (let
      ((subject (plist-get mail :subject))
       (from (car (plist-get mail :from))))
    (let*
       ((name (car from))
	(email (cdr from))
	(display (cond ((stringp name) name)
		       ((stringp email) email)
		       (t "?"))))

      (concat
       (propertize (format "%s" display) 'face 'sauron-highlight1-face)
       ": "
       (format "%s" subject)))))

(defun sauron-mu4e-make-link (mail)
  "Make the callback link.  Takes a single mu4e mail as MAIL."
  (lexical-let
      ((message-id (plist-get mail :message-id)))
    (lambda ()
      (switch-to-buffer "*mu4e-headers*")
      (mu4e-view-message-with-msgid message-id))))

(defun sauron-mu4e-start ()
  "Start the sauron mu4e service."
  (advice-add 'mu4e-alert-notify-unread-messages :before
	      #'sauron-mu4e-uread-messages))

(defun sauron-mu4e-stop ()
  "Stop the sauron mu4e service."
  (advice-remove 'mu4e-alert-notify-unread-messages
		 #'sauron-mu4e-uread-messages))

(provide 'sauron-mu4e)

;;; sauron-mu4e.el ends here
