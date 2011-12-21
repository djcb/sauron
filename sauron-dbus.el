;;; sauron -- enhanced tracking of the world inside and outside your emacs
;;; buffers. 
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

;; listen for dbus notifications.
;; to call from shell, some convenience functions (for zsh, bash, *not* sh)
;; # send URL to sauron...

;; function sauron-url () {
;;     # --print-reply
;;     dbus-send --session          	   \
;;     --dest="org.gnu.Emacs"		   \
;;      "/org/gnu/Emacs/Sauron"            \
;;     "org.gnu.Emacs.Sauron.AddUrlEvent"  \
;;     string:shell uint32:3 string:url string:"$1"
;; }

;; # send message to sauron...
;; function sauron-msg () {
;;     # --print-reply
;;     dbus-send --session          	       \
;;     --dest="org.gnu.Emacs"		       \
;;      "/org/gnu/Emacs/Sauron"                \
;;     "org.gnu.Emacs.Sauron.AddMsgEvent"      \
;;     string:shell uint32:3 string:$1
;; }

;;; Code:

(require 'dbus nil 'noerror)

(defconst sr-dbus-service dbus-service-emacs
  "*internal* the D-bus service name for sauron.")

(defconst sr-dbus-path
  (concat dbus-path-emacs "/Sauron")
  "*internal* the D-bus interface for sauron.")

(defconst sr-dbus-interface 
  (concat sr-dbus-service ".Sauron")
  "*internal* the D-bus interface for sauron.")

(defvar sr-dbus-running nil
  "*internal* Whether the dbus backend is running.")

(defun sauron-dbus-start ()
  "Start listening for sauron dbus messages."
  (if (not (boundp 'dbus-path-emacs))
    (message "sauron-dbus not available")
    (unless sr-dbus-running
      (dbus-register-method
	:session               ;; use the session bus
	sr-dbus-service        ;; ie. org.gnu.Emacs
	sr-dbus-path           ;; ie. /org/gnu/Emacs/Sauron
	sr-dbus-interface      ;; ie. org.gnu.Emacs.Sauron
	"AddUrlEvent"          ;; method name
	'sr-dbus-add-url-event ;; handler function
	))
    (dbus-register-method
      :session               ;; use the session bus
      sr-dbus-service        ;; ie. org.gnu.Emacs
      sr-dbus-path           ;; ie. /org/gnu/Emacs/Sauron
      sr-dbus-interface      ;; ie. org.gnu.Emacs.Sauron
      "AddMsgEvent"          ;; method name
      'sr-dbus-add-msg-event ;; handler function
      )
    ;; make it introspectable ==> FIXME: doesn't work; hmmm...
    (dbus-register-method
      :session
      sr-dbus-service
      sr-dbus-path
      dbus-interface-introspectable
      "Introspect"
      (lambda () ;; return the introspection XML for our object" 
	(concat  ;; 
	  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
         \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
        <node name=\"" sr-dbus-path "\"> 
          <interface name=\"" sr-dbus-interface "\">
            <method name=\"AddUrlEvent\">
              <arg name=\"origin\"   type=\"s\" direction=\"in\"/>
              <arg name=\"priority\" type=\"u\" direction=\"in\"/>
              <arg name=\"message\"  type=\"s\" direction=\"in\"/>
              <arg name=\"url\"      type=\"s\" direction=\"in\"/>
             </method>
            <method name=\"AddMsgEvent\">
              <arg name=\"origin\"   type=\"s\" direction=\"in\"/>
              <arg name=\"priority\" type=\"u\" direction=\"in\"/>
              <arg name=\"message\"  type=\"s\" direction=\"in\"/>
             </method>
          </interface>
       </node>")))))

(defun sauron-dbus-stop ()
  "Stop listening for dbus messages."
  (when sr-dbus-running
    (dbus-unregister-service :session sr-dbus-service)
    (setq sr-dbus-running nil)))


(defun sr-dbus-add-url-event (origin prio message url)
  "Add a dbus-originated event."
  (sauron-add-event
    'dbus
    prio
    (concat (propertize origin 'face 'sauron-highlight2-face)
      ":" message)
    ;; pseudo closure...
    (lexical-let ((url url))
      (lambda() (browse-url url)))
    `(:url ,url :origin: ,origin))
  '(:boolean t))

(defun sr-dbus-add-msg-event (origin prio message)
  "Add a dbus-originated event."
  (sauron-add-event
    'dbus
    prio
    (concat (propertize origin 'face 'sauron-highlight1-face)
      ": " message)
    ;; pseudo closure...
    (lexical-let ((msg message))
      (lambda() (message "%s" msg)))
    `(:origin origin))
  '(:boolean t))

(provide 'sauron-dbus)


