;;; sauron-elfeed.el --- an ELFEED tracking module, part of sauron
;;
;; Copyright (C) 2015 Sébastien Le Maguer

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
(require 'elfeed nil 'noerror)

(eval-when-compile
  (require 'cl))


;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sauron-prio-elfeed-default 2
  "ELFEED event default priority.")

(defvar sauron-elfeed-prio-hash (make-hash-table :test 'equal)
  "Hashtable to associate a priority for each feed.  
You can add a specific priority using `puthash' using the url of
the seed as a key and the priority as the value.")

(defvar sr-elfeed-running nil
  "*internal* Whether sauron elfeed is running.")


;; Sauron commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sauron-elfeed-start ()
  "Start watching ELFEED."
  (if (not (boundp 'elfeed-version-string))
    (progn
      (message "sauron-elfeed not available")
      nil)
    (unless sr-elfeed-running
      (add-hook 'elfeed-update-hooks 'sr-elfeed-update-hook-func)
      (setq sr-elfeed-running t))
    t))

(defun sauron-elfeed-stop ()
  "Stop watching ELFEED."
  (when sr-elfeed-running
    (remove-hook 'elfeed-update-hooks 'sr-elfeed-update-hook-func)
    (setq sr-elfeed-running nil)))


;; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun counting-unread (url)
  "Getting the unread entries of a specific feed identified by URL."
  (let (read-list)
    (dolist (entry (elfeed-feed-entries url))
      (when (member 'unread (elfeed-entry-tags entry))
        (push (elfeed-entry-title entry) read-list)))
    (length read-list)))


;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sr-elfeed-update-hook-func (url)
  "Hook function,  to be called for elfeed-matched-hook.  URL is the url of the feed."
  (let* ((nb-unread (counting-unread url)))
    (when (> nb-unread 0)
      (sauron-add-event
       'elfeed
       (if (gethash url sauron-elfeed-prio-hash)
           (gethash url sauron-elfeed-prio-hash)
         sauron-prio-elfeed-default)
       
       (concat (propertize (elfeed-feed-title (elfeed-db-get-feed url)) 'face 'sauron-highlight1-face)
               " has "
               (format "%d" nb-unread)
               " new entries")))))


(provide 'sauron-elfeed)

;;; sauron-elfeed ends here
