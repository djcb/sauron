;;; sauron-identica.el --- Identica notifications for sauron
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
(require 'identica-mode nil 'noerror)

(defvar sauron-prio-identica-new-dents 3
  "Identica new dents event priority.")

(defvar sr-identica-running nil
  "*internal* whether sauron identica is running.")

(defun sauron-identica-start ()
  "Start watching identica."
  (if (not (boundp 'identica-mode-version))
      (progn
        (message "sauron-identica not available")
        nil)
    (unless sr-identica-running
      (add-hook 'identica-new-dents-hook
                'sr-identica-new-dents-func)
      (setq sr-identica-running t))
    t))

(defun sauron-identica-stop ()
  "Stop watching identica."
  (when sr-identica-running
    (remove-hook 'identica-new-dents-hook
                 'sr-identica-new-dents-func)
    (setq sr-identica-running nil)))

(defun sr-identica-new-dents-func ()
  "Print the # of new dents."
  (sauron-add-event
   'identica sauron-prio-identica-new-dents
   (if (= identica-new-dents-count 1)
       "There is 1 new dent."
     (format "There are %i new dents." identica-new-dents-count))
   'identica))

(provide 'sauron-identica)

;;; sauron-identica.el ends here
