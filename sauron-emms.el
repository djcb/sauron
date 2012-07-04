;;; sauron-emms.el --- EMMS notifications for sauron
;;
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
(require 'emms nil 'noerror)

(defvar sr-emms-running nil
  "*internal* whether sauron emms is running.")

(defun sauron-emms-start ()
  "Start listening to EMMS."
  (if (not (boundp 'emms-version))
      (progn
        (message "sauron-emms not available")
        nil)
    (unless sr-emms-running
      (add-hook 'emms-player-finished-hook 'sr-emms-player-finished-func)
      (add-hook 'emms-playlist-selection-changed-hook
                'sr-emms-playlist-selection-changed-func)
      (setq sr-emms-running t))
    t))

(defun sauron-emms-stop ()
  "Stop listening to EMMS."
  (when sr-emms-running
    (remove-hook 'emms-player-finished-hook 'sr-emms-player-finished-func)
    (remove-hook 'emms-playlist-selection-changed-hook
                 'sr-emms-playlist-selection-changed-func)
    (setq sr-emms-running nil)))

(defun sr-emms-player-finished-func ()
  "Notify when EMMS is finished playing."
  (sauron-add-event 'emms 3 "Playlist finished" 'emms))

(defun sr-emms-playlist-selection-changed-func ()
  "Notify of song change."
  (sauron-add-event
   'emms 3
   (format emms-show-format
           (emms-track-description
            (emms-playlist-current-selected-track)))
   'emms))

(provide 'sauron-emms)

;;; sauron-emms.el ends here
