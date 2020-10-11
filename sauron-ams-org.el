;;; sauron-ams-org.el --- Parse org-agenda for sauron.
;; -*- lexical-binding: t; -*-

;; Adam Simpson <adam@adamsimpson.net>
;; Version: 0.2.1
;; Package-Requires: (sauron org)
;; Keywords: sauron, org
;; URL: https://github.com/asimpson/dotfiles/

;;; Commentary:

;; sauron-ams-org is meant to replace the built-in sauron-org module.
;; I found syncing up org with the built in calendar and diary functionality frustrating and redundant.
;; sauron-ams-org uses built-in org functions to parse the agenda and generate events for items with :DEADLINES.
;; It runs every 5 minutes by default and warns about :DEADLINEs that are 15 minutes out and sooner.

;;; Code:

(defun sauron-ams-org-parse-org()
  "Fire Sauron event if deadline is within 15 minutes.
  Also schedule event for when deadline is due."
  (let ((deadline (org-entry-get nil "DEADLINE"))
        (task (nth 4 (org-heading-components))) diff interval msg)
    (setq diff (org-time-stamp-to-now deadline t))
    (setq interval (number-to-string (round (/ diff 60))))
    (when (and (< diff 300)
               (> diff 0))
      (setq msg (concat "DUE! " task))
      (run-at-time diff nil
        (lambda(msg)
          (sauron-add-event 'ams-org 5 (propertize msg 'face
            '(:foreground "firebrick2")) #'org-agenda-list)) msg))
    (when (and (< diff 900)
               (> diff 0))
      (setq msg (concat interval " minutes left before " task))
      (sauron-add-event 'ams-org 3 msg #'org-agenda-list))))

(defun sauron-ams-org-check-org()
  "Map over org agenda entries that have a DEADLINE and aren't done."
  ; API information available here: http://orgmode.org/manual/Matching-tags-and-properties.html.
  (org-map-entries #'sauron-ams-org-parse-org "DEADLINE<>\"\"-TODO=\"DONE\"-TODO=\"CANCELED\""
                   'agenda))

(defun sauron-ams-org-start()
  "schedules org polling"
  (setq sauron-ams-org-timer (run-at-time 0 300 #'sauron-ams-org-check-org)))

(defun sauron-ams-org-stop()
  "cleans up by canceling the timer"
  (cancel-timer sauron-ams-org-timer))

(provide 'sauron-ams-org)

;;; sauron-ams-org.el ends here
