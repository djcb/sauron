;;; sauron-erc -- tracking your ERC irc-channels
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: 
;; Version: 0.0

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'erc)
(eval-when-compile (require 'cl))

(defun sauron-erc-start ()
  "Start watching ERC."
  (add-hook 'erc-text-matched-hook
    (lambda (match-type nick msg)
      (when (memq match-type '(current-nick keyword pal))
	(sr-erc-handler match-type nick msg (buffer-name))))))

(defun sauron-erc-stop ()
  "Stop watching ERC."
  (remove-hook 'erc-text-matched-hook 'sr-erc-handler))

(defun sr-erc-handler (match-type nick msg channel)
  "Handler function for ERC messages."
  (sauron-add-event
    "erc"
    (format "%S" match-type)
    3 ;; priority
    (lexical-let ((channel channel))
      (lambda() (sauron-switch-to-buffer channel)))
    msg))


(provide 'sauron-erc)
