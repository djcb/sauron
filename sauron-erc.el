;;; sauron-erc.el --- an ERC tracking module, part of sauron
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

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

(defvar sauron-prio-erc-default 2
  "ERC event default priority.")

(defvar sauron-prio-erc-privmsg-root 2
  "ERC message from root event priority.")

(defvar sauron-prio-erc-privmsg-for-me 3
  "ERC message for me event priority.")

(defvar sauron-prio-erc-privmsg-mentioned 3
  "ERC mention event priority.")

(defvar sauron-prio-erc-privmsg-default 2
  "ERC private message event default priority.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sr-erc-running nil
  "*internal* Whether sauron erc is running.")

(defun sauron-erc-start ()
  "Start watching ERC."
  (if (not (boundp 'erc-version-string))
    (progn
      (message "sauron-erc not available")
      nil)
    (unless sr-erc-running
      (add-hook 'erc-server-PRIVMSG-functions 'sr-erc-PRIVMSG-hook-func)
      (add-hook 'erc-server-JOIN-functions 'sr-erc-JOIN-hook-func)
      (add-hook 'erc-server-PART-functions 'sr-erc-PART-hook-func)
      (add-hook 'erc-server-QUIT-functions 'sr-erc-QUIT-hook-func)
      (setq sr-erc-running t))
    t))

(defun sauron-erc-stop ()
  "Stop watching ERC."
  (when sr-erc-running
    (remove-hook 'erc-server-PRIVMSG-functions 'sr-erc-PRIVMSG-hook-func)
    (remove-hook 'erc-server-JOIN-functions 'sr-erc-JOIN-hook-func)
    (remove-hook 'erc-server-PART-functions 'sr-erc-PART-hook-func)
    (remove-hook 'erc-server-QUIT-functions 'sr-erc-QUIT-hook-func)
    (setq sr-erc-running nil)))

;; note, this function is called from the server-buffer, not from the channel it
;; refers to
(defun sr-erc-hook-func (proc parsed event)
  "Hook function, to be called for erc-matched-hook."
  (let* ( (me      (erc-current-nick))
	  (sender  (car (erc-parse-user (erc-response.sender parsed))))
	  (channel (car (erc-response.command-args parsed)))
	  (msg     (erc-response.contents parsed)))
    (sauron-add-event
      'erc
      sauron-prio-erc-default
      (concat (propertize sender 'face 'sauron-highlight1-face) " has "
	(case event
	  ('quit (concat "quit (" msg ")"))
	  ('part (concat "left "
		   (propertize channel 'face 'sauron-highlight2-face)
		   " (" msg ")"))
	  ('join (concat "joined "
		   (propertize channel 'face 'sauron-highlight2-face)))))
      (lexical-let ((target channel))
	(lambda()  (sauron-switch-to-marker-or-buffer target)))
      `( :event    ,event
	 :sender   ,sender
	 :me       ,me
	 :channel  ,channel
	 :msg      ,msg))))


(defun sr-erc-JOIN-hook-func (proc parsed)
  "JOIN hook function."
  (sr-erc-hook-func proc parsed 'join) nil)

(defun sr-erc-QUIT-hook-func (proc parsed)
  "QUIT hook function."
  (sr-erc-hook-func proc parsed 'quit) nil)

(defun sr-erc-PART-hook-func (proc parsed)
  "PART hook function."
  (sr-erc-hook-func proc parsed 'part) nil)

(defun sr-erc-msg-clean (msg)
  "Clean IRC escaped stuff from messages."
  (when msg ;; remove the IRC meta crap
    (replace-regexp-in-string ".*[]" "" msg)))

(defun sr-erc-PRIVMSG-hook-func (proc parsed)
  "Hook function, to be called for erc-matched-hook."
    (let* ( (me      (erc-current-nick))
	    (sender  (car (erc-parse-user (erc-response.sender parsed))))
	    (channel (car (erc-response.command-args parsed)))
	    (msg     (sr-erc-msg-clean (erc-response.contents parsed)))
	    (for-me  (string= me channel))
	    (prio
	      (cond
                ;; e.g. bitlbee stuff; low-prio
		((string= sender "root") sauron-prio-erc-privmsg-root)
                ;; private msg for me => prio 4
		(for-me sauron-prio-erc-privmsg-for-me)
                ;; I'm mentioned => prio 3
		((string-match me msg) sauron-prio-erc-privmsg-mentioned)
                ;; default
		(t sauron-prio-erc-privmsg-default)))
	    (target (if (buffer-live-p (get-buffer channel))
	     	      (with-current-buffer (get-buffer channel)
			(point-marker)))))
      (sauron-add-event
	'erc
	prio
	(concat
	  (propertize sender 'face 'sauron-highlight1-face) "@"
	  (propertize channel 'face 'sauron-highlight2-face)
	  (propertize ": " 'face 'sauron-highlight1-face)
	  msg)
	(lexical-let* ((target-mark target)
			(target-buf (if for-me sender channel)))
	  (lambda ()
	    (sauron-switch-to-marker-or-buffer (or target-mark target-buf))))
	`( :event   privmsg
	   :sender ,sender
	   :me     ,me
	   :channel ,channel
	   :msg    ,msg)))
  nil)

(provide 'sauron-erc)

;;; sauron-erc ends here
