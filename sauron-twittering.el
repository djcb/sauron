;;; sauron-twittering.el --- a twittering-mode tracking module, part of sauron
;;
;; Copyright (C) 2012 Dirk-Jan C. Binnema, Joel McCracken

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

(require 'twittering-mode nil 'noerror)

(defvar sauron-prio-twittering-new-tweets 3
  "Twittering new tweets event priority.")

(defvar sauron-twittering-running nil
  "when non-nil, sauron-twittering is running")

(defun sauron-twittering-start ()
  "Starts sauron-twittering."
  (if (and (boundp 'twittering-mode-version)
	twittering-mode-version)
    (progn
      (when sauron-twittering-running
	(error "sauron-twittering is already running. Call
          sauron-twittering-stop first."))

      (add-hook 'twittering-new-tweets-hook 'sauron-twittering-new-tweets-func)
      (setq sauron-twittering-running t))
    (message "No twittering, so sauron-twittering could not
    start")))


(defun sauron-twittering-stop ()
  "Stops and cleans up sauron-twittering."
  (when sauron-twittering-running
    (remove-hook 'twittering-new-tweets-hook 'sauron-twittering-new-tweets-func)
    (setq sauron-twittering-running nil)))


(defun sauron-twittering-new-tweets-func ()
  "Hook which handles the arrival of new tweets. Main entry point and interface
to twittering."
  (sr-twit-add-event sauron-prio-twittering-new-tweets
                     (format "%d new tweets"  twittering-new-tweets-count)
    (lexical-let
      ((tweets-spec twittering-new-tweets-spec)
	(tweets-data twittering-new-tweets-statuses)
	(tweets-count twittering-new-tweets-count))
      (lambda ()
	(sr-twit-activate-event tweets-data tweets-count tweets-spec)
	))))

(defun sr-twit-add-event (priority message callback)
  (sauron-add-event 'twittering priority message callback))

(defun sr-twit-activate-event (tweets-data tweets-count tweets-spec)
  "Handles event activation."
  (switch-to-buffer-other-window
    (twittering-get-managed-buffer tweets-spec))
  (sr-twit-goto-tweet (car tweets-data)))

(defun sr-twit-goto-tweet (tweet-data)
  "Shows tweet in current window.
TWEET-DATA is the twittering data for a tweet."
  (let ((tweet-point (sr-twit-find-tweet tweet-data)))
    (when tweet-point
      (goto-char tweet-point)
      (sr-twit-point-to-top-current-window ))))

(defun sr-twit-point-to-top-current-window ()
  "Move window to show point at top"
  (set-window-start (selected-window) (point)))

(defun sr-twit-find-tweet (tweet-data)
  "Find the point for the rendered tweet for TWEET-DATA."
  (let (found-tweet-point)
    (save-excursion
      (twittering-goto-first-status)
      (let ((looking-for-tweet-id (cdr (assoc 'id tweet-data)))
	     (keep-looking t))

        (while keep-looking
          (if (string= looking-for-tweet-id (twittering-get-id-at))
	    (setq found-tweet-point (point)
	      keep-looking nil)
            (if (twittering-get-next-status-head)
	      (twittering-goto-next-status)
              (setq keep-looking nil)
              )))))
    found-tweet-point))

(provide 'sauron-twittering)

;; End of sauron-twittering.el
