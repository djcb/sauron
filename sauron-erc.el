;;; sauron-erc -- tracking your ERC irc-channels
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords:
;; Version: 0.0

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

;;; Code:
(require 'erc nil 'noerror)
(eval-when-compile (require 'cl))

(defvar sauron-erc-interesting-events
  '(privmsg current-nick keyword)
  "The list of ERC events we are interested in. Available are:
- privmsg:       a PRIVMSG message received
- join           a JOIN message received
- quit           a QUIT message received
The following events are erc-track
- current-nick:  current nick mentioned in ERC
- keyword:       some keyword mentioned in ERC.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sr-erc-running nil
  "*internal* Whether sauron erc is running.")

(defun sauron-erc-start ()
  "Start watching ERC."
  (if (not (boundp 'erc-version-string))
    (message "sauron-erc not available")
    (unless sr-erc-running
      (add-hook 'erc-server-PRIVMSG-functions 'sr-erc-PRIVMSG-hook-func)
      (add-hook 'erc-server-JOIN-functions 'sr-erc-JOIN-hook-func)
      (add-hook 'erc-server-PART-functions 'sr-erc-PART-hook-func)
      (add-hook 'erc-server-QUIT-functions 'sr-erc-QUIT-hook-func)
      (setq sr-erc-running t))))

(defun sauron-erc-stop ()
  "Stop watching ERC."
  (when sr-erc-running
    (remove-hook 'erc-server-PRIVMSG-functions 'sr-erc-PRIVMSG-hook-func)
    (remove-hook 'erc-server-JOIN-functions 'sr-erc-JOIN-hook-func)
    (remove-hook 'erc-server-PART-functions 'sr-erc-PART-hook-func)
    (remove-hook 'erc-server-QUIT-functions 'sr-erc-QUIT-hook-func)
    (setq sr-erc-running nil)))

(defun sr-erc-hook-func (proc parsed event)
  "Hook function, to be called for erc-matched-hook."
  (let* ( (me     (erc-current-nick))
	  (sender (car (erc-parse-user (erc-response.sender parsed))))
	  (target (car (erc-response.command-args parsed)))
	  (msg (erc-response.contents parsed)))
    (sauron-add-event
      'erc
      2
      (concat (propertize sender 'face 'sauron-highlight1-face) " has "
	(case event
	  ('quit (concat "quit (" msg ")"))
	  ('part (concat "left "
		   (propertize target 'face 'sauron-highlight2-face)
		   " (" msg ")"))
	  ('join (concat "joined "
		   (propertize target 'face 'sauron-highlight2-face)))))
      ;; FIXME: assumes we open separate window
      (when (eq event 'join)
	(lexical-let ((target target))
	  (lambda()  (sauron-switch-to-buffer target))))
      `( :event  ,event
	 :sender ,sender
	 :me     ,me
	 :target ,target
	 :msg    ,msg))))


(defun sr-erc-JOIN-hook-func (proc parsed)
  "JOIN hook function."
  (sr-erc-hook-func proc parsed 'join))

(defun sr-erc-QUIT-hook-func (proc parsed)
  "QUIT hook function."
  (sr-erc-hook-func proc parsed 'quit))

(defun sr-erc-PART-hook-func (proc parsed)
  "PART hook function."
  (sr-erc-hook-func proc parsed 'part))

(defun sr-erc-msg-clean (msg)
  "Clean IRC escaped stuff from messages."
  (when msg ;; remove the IRC meta crap
    (replace-regexp-in-string ".*[]" "" msg)))

(defun sr-erc-PRIVMSG-hook-func (proc parsed)
  "Hook function, to be called for erc-matched-hook."
    (let* ( (me     (erc-current-nick))
	    (sender (car (erc-parse-user (erc-response.sender parsed))))
	    (target (car (erc-response.command-args parsed)))
	    (msg (sr-erc-msg-clean (erc-response.contents parsed)))
	    (prio
	      (cond
		((string= sender "root") 2)    ;; bitlbee stuff; low-prio
		((string= me target)	 3)    ;; private msg for me => prio 4
		((string-match me msg)	 3)    ;; I'm mentioned => prio 3
		(t			 2)))) ;; default
      (sauron-add-event
	'erc
	prio
	(concat
	  (propertize sender 'face 'sauron-highlight1-face) "@"
	  (propertize target 'face 'sauron-highlight2-face)
	  (propertize " says " 'face 'sauron-highlight1-face)
	  msg)
	;; FIXME: assumes we open separate window
	(lexical-let* ((bufname (if (string= target me) sender target)))
	  (lambda()
	    (let ((buf (get-buffer bufname)))
	      (if (buffer-live-p buf)
		(sauron-switch-to-buffer buf)
		(message "Buffer %S not available" bufname)))))
	`( :event   privmsg
	   :sender ,sender
	   :me     ,me
	   :target ,target
	   :msg    ,msg))))

(provide 'sauron-erc)
