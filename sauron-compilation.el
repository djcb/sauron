;;; sauron-compilation.el --- a compilation tracking module, part of sauron
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

(require 'compile nil 'noerror)

(defvar sauron-prio-compilation 3
  "Compilation event priority.")

(defvar sauron-compilation-running nil
  "when non-nil, sauron-compilation is running")

(defun sauron-compilation-start ()
  "Starts sauron-compilation."
  (when sauron-compilation-running
    (error "sauron-compilation is already running. Call
          sauron-compilation-stop first."))
  (add-to-list 'compilation-finish-functions #'sauron-compilation-complete-func)
  (setq sauron-compilation-running t))


(defun sauron-compilation-stop ()
  "Stops and cleans up sauron-compilation."
  (when sauron-compilation-running
    (setq compilation-finish-functions
          (remove 'sauron-compilation-complete-func
                  compilation-finish-functions))
    (setq sauron-compilation-running nil)))


(defun sauron-compilation-complete-func (buffer msg)
  "Hook which handles the compilation completion. Main entry
point and interface to compilation.

`compilation-finish-functions' passes in the compilation buffer
name and message to this function."
  (sauron-add-event 'compilation
                    sauron-prio-compilation
                    (format "[%s]: %s" buffer msg)
                    '(lambda () (switch-to-buffer-other-window "*compilation*"))
                    nil))

(provide 'sauron-compilation)

;; End of sauron-compilation.el
