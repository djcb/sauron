;;; sauron-jabber.el --- Jabber notifications for sauron

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

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
(require 'jabber nil 'noerror)

(defvar sauron-prio-jabber-alert-message 3
  "Jabber alert message event priority.")

(defvar sauron-prio-jabber-alert-info-message 2
  "Jabber alert info message event priority.")

(defvar sauron-prio-jabber-alert-muc 3
  "Jabber alert MUC event priority.")

(defvar sauron-prio-jabber-alert-presence 2
  "Jabber alert presence event priority.")

(defvar sauron-prio-jabber-connected 2
  "Jabber connected event priority.")

(defvar sauron-prio-jabber-lost-connection 2
  "Jabber lost connection event priority.")

(defvar sr-jabber-running nil
  "*internal* whether sauron jabber is running.")

(defun sauron-jabber-start ()
  "Start watching jabber."
  (if (not (boundp 'jabber-version))
      (progn
        (message "sauron-jabber not available")
        nil)
    (unless sr-jabber-running
      (add-hook 'jabber-alert-message-hooks
                'sr-jabber-alert-message-func)
      (add-hook 'jabber-alert-muc-hooks
                'sr-jabber-alert-muc-func)
      (add-hook 'jabber-alert-info-message-hooks
                'sr-jabber-alert-info-message-func)
      (add-hook 'jabber-alert-presence-hooks
                'sr-jabber-alert-presence-func)
      (add-hook 'jabber-post-connect-hooks
                'sr-jabber-post-connect-func)
      (add-hook 'jabber-lost-connection-hooks
                'sr-jabber-lost-connection-func)
      (setq sr-jabber-running t))
    t))

(defun sauron-jabber-stop ()
  "Stop watching jabber."
  (when sr-jabber-running
    (remove-hook 'jabber-alert-message-hooks
                 'sr-jabber-alert-message-func)
    (remove-hook 'jabber-alert-info-message-hooks
                 'sr-jabber-alert-info-message-func)
    (remove-hook 'jabber-alert-muc-hooks
                 'sr-jabber-alert-muc-func)
    (remove-hook 'jabber-alert-presence-hooks
                 'sr-jabber-alert-presence-func)
    (remove-hook 'jabber-post-connect-hooks
                 'sr-jabber-post-connect-func)
    (remove-hook 'jabber-lost-connection-hooks
                 'sr-jabber-lost-connection-func)
    (setq sr-jabber-running nil)))

(defun sr-jabber-alert-message-func (from buffer text
                                                proposed-alert)
  (let ((name (jabber-jid-displayname from)))
    (sauron-add-event 'jabber sauron-prio-jabber-alert-message
                      proposed-alert
                      `(lambda ()
                         (sauron-switch-to-marker-or-buffer
                          ,(buffer-name buffer))))))

(defun sr-jabber-alert-info-message-func (what buffer
                                                     proposed-alert)
  (sauron-add-event 'jabber sauron-prio-jabber-alert-info-message
                    proposed-alert
                    `(lambda ()
                       (sauron-switch-to-marker-or-buffer
                        ,(buffer-name buffer)))))

(defun sr-jabber-alert-muc-func (nick group buffer text
                                            proposed-alert)
  (sauron-add-event 'jabber sauron-prio-jabber-alert-muc
                    proposed-alert
                    `(lambda ()
                       (sauron-switch-to-marker-or-buffer
                        ,(buffer-name buffer)))))

(defun sr-jabber-alert-presence-func (who oldstatus newstatus
                                                statustext
                                                proposed-alert)
  (if (not (or (string-equal proposed-alert "")
               (eq proposed-alert nil)))
      (sauron-add-event 'jabber sauron-prio-jabber-alert-presence
                        proposed-alert)))

(defun sr-jabber-post-connect-func (conn)
  (sauron-add-event 'jabber sauron-prio-jabber-connected
                    (format "%s connected"
                            (jabber-connection-jid conn))))

(defun sr-jabber-lost-connection-func (conn)
  (sauron-add-event 'jabber sauron-prio-jabber-lost-connection
                    (format "Connection for %s lost"
                            (jabber-jid-username
                             (jabber-connection-jid conn)))))

(provide 'sauron-jabber)

;;; sauron-jabber.el ends here
