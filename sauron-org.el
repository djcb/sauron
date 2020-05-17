;;; sauron-org.el --- org-mode tracking module, part of sauron -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jake Coble
;;
;; Author: Jake Coble <http://github/jakecoble>
;; Maintainer: Jake Coble <j@kecoble.com>
;; Created: May 14, 2020
;; Modified: May 16, 2020
;; Version: 0.0.2
;; Keywords:
;; Homepage: https://github.com/jakecoble/sauron-org
;; Package-Requires: ((emacs "26.3") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  org-mode tracking module, part of sauron
;;
;;; Code:

(require 'org)
(require 'cl)

(defvar sauron-prio-org-default 5
  "Default event priority for org-mode headings.")

(defvar sauron-prio-org-minutes-left-list
  '((15 2)
    (10 3)
    (5 3)
    (2 4))
  "A list of pairs, where the first element of each pair is the
number of minutes left before the deadline and the last
element is an org-mode heading priority.")

(defvar sauron-org-refresh-interval 5
  "Length of time between rebuilding the heading list specified in minutes.")

(defvar sauron-org-print-todo-keyword t
  "Include the todo keyword when sending the heading to sauron.")

(defvar sauron-org-print-tags nil
  "Include the tags when sending the heading to sauron.")

(defvar sauron-org-print-priority t
  "Include the priority cookie when sending the heading to sauron.")

(defvar sauron-org-exclude-tags (list org-archive-tag)
  "Headings with any of these tags will be excluded from tracking.")

(defvar sauron-org-heading-formatting-function #'sauron-org-default-heading-formatter
  "Function to apply to a heading before it's sent to a sauron event.")

(defvar sauron-org--heading-list '()
  "List of headings that sauron-org is currently tracking.")

(defvar sauron-org--refresh-timer nil
  "Timer that rebuilds the list of org headings we're tracking.")

(defvar sauron-org--time-hour-regexp "<\\([^>]+[0-9]\\{1,2\\}:[0-9]\\{2\\}[0-9+:hdwmy/ 	.-]*\\)>"
  "Matches timestamps with an explicitly set hour. Extracted from org-deadline-time-hour-regexp.")

(defun sauron-org-default-heading-formatter (heading type time)
  "Default formatter for tracked headings."
  (let ((plan-phrase (cond ((eq type 'scheduled) "is scheduled")
                           ((eq type 'deadline) "has a deadline")
                           (t "@")))
        (minutes-from-now (round
                            (/ (- (time-to-seconds time)
                                  (time-to-seconds))
                              60))))
    (format "%s %s %d minutes from now" heading plan-phrase minutes-from-now)))

(defun sauron-org-add-timers (heading time props)
  "Add a timer for every time interval."
  (remove-if #'null
             (mapcar
              (lambda (interval)
                (let ((priority (cadr interval))
                      (target-time (seconds-to-time
                                      (- (time-to-seconds time)
                                        (* (car interval) 60)))))
                  (if (time-less-p nil target-time)
                      (run-at-time target-time nil
                        (lambda ()
                          (sauron-add-event 'org priority (funcall #'sauron-org-default-heading-formatter
                                                                   heading
                                                                   (plist-get props :type)
                                                                   time)
                                            nil props))))))
              sauron-prio-org-minutes-left-list)))

(defun sauron-org-maybe-string-to-time (str)
  "If STR is a timestamp with an hour component, return the parsed string. Return NIL otherwise."
  (if (and str (string-match sauron-org--time-hour-regexp str))
      (org-time-string-to-time str)))

(defun sauron-org-maybe-add-heading ()
  "Add heading at point if it is scheduled, has a deadline, and isn't done."
  (unless (or (org-entry-is-done-p)
              (cl-some (lambda (tag) (member tag sauron-org-exclude-tags))
                       (org-get-tags nil t)))
    (let* ((heading (org-get-heading
                     (not sauron-org-print-tags)
                     (not sauron-org-print-todo-keyword)
                     (not sauron-org-print-priority)))
           (scheduled-string (org-entry-get (point) "SCHEDULED"))
           (deadline-string (org-entry-get (point) "DEADLINE"))
           (scheduled (sauron-org-maybe-string-to-time scheduled-string))
           (deadline (sauron-org-maybe-string-to-time deadline-string)))
      (if (and scheduled
               (time-less-p nil scheduled))
          (cl-pushnew
            `(:heading ,heading
              :time ,scheduled
              :type 'scheduled
              :timers ,(sauron-org-add-timers heading scheduled
                                              (list :type 'scheduled :time scheduled)))
            sauron-org--heading-list))

      (if (and deadline
               (time-less-p nil deadline))
          (cl-pushnew
            `(:heading ,heading
              :time ,deadline
              :type 'deadline
              :timers ,(sauron-org-add-timers heading deadline
                                              (list :type 'deadline :time deadline)))
            sauron-org--heading-list)))))

(defun sauron-org--clear-heading-list ()
  "Cancel all the timers in the heading list and set it to nil."
  (dolist (heading sauron-org--heading-list)
    (mapc #'cancel-timer (plist-get heading :timers)))

  (setq sauron-org--heading-list '()))

(defun sauron-org-rebuild-heading-list ()
  "Scan all agenda files to build the heading list."
  (sauron-org--clear-heading-list)

  (org-map-entries
   #'sauron-org-maybe-add-heading t 'agenda))

(defun sauron-org-start ()
  "Start watching org-mode."
  (setq sauron-org--refresh-timer
        (run-at-time nil (* sauron-org-refresh-interval 60) #'sauron-org-rebuild-heading-list)))

(defun sauron-org-stop ()
  "Stop watching org-mode."
  (cancel-timer sauron-org--refresh-timer)
  (setq sauron-org--refresh-timer nil)
  (sauron-org--clear-heading-list))

(provide 'sauron-org)
;;; sauron-org.el ends here
