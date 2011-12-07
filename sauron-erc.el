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
  ;; the match hook
  (add-hook 'erc-text-matched-hook 'sr-erc-text-matched-hook-func)
  (add-hook 'erc-server-PRIVMSG-functions 'sr-erc-PRIVMSG-hook-func))
 

(defun sauron-erc-stop ()
  "Stop watching ERC."
  (remove-hook 'erc-text-matched-hook 'sr-erc-text-matched-hook-func)
  (remove-hook 'erc-server-PRIVMSG-functions 'sr-erc-PRIVMSG-hook-func))

(defun sr-erc-text-matched-hook-func (match-type nick msg)
  "Hook function, to be called for erc-matched-hook."
  (when (memq match-type '(current-nick keyword pal))
    (sr-erc-handler match-type nick msg (buffer-name))))

(defun sr-erc-PRIVMSG-hook-func (proc parsed)
  "Hook function, to be called for erc-matched-hook."
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (erc-current-nick-p target)
      (sr-erc-handler "privmsg" nick msg nick))))

(defun sr-erc-handler (match-type nick msg channel)
  "Handler function for ERC messages."
  (let* ((msg (format "%s:%s" nick msg)))
    ;; remove all the text properties, it seems there are a lot
    (set-text-properties 0 (length msg) nil msg)
    (sauron-add-event
      "erc"
      (format "%S" match-type)
      3 ;; priority
      (lexical-let ((channel channel))
	(lambda() (sauron-switch-to-buffer channel)))
      msg)))
  
(provide 'sauron-erc)


