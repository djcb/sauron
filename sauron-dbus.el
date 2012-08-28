;;; sauron-dbus.el --- a dbus tracking module, part of sauron
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
(require 'cl)
(require 'dbus nil 'noerror) ;; keep errors out if dbus is not there

;; keep the byte-compiler happy, even if dbus isn't there
(defvar dbus-service-emacs nil)
(defvar dbus-path-emacs nil)
(defvar dbus-interface-introspectable nil)
(when (not (fboundp 'dbus-unregister-service))
  (defun dbus-unregister-service (&rest args) nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sauron-dbus-cookie nil
  "If non-nil, write the dbus-address for this session to a file
~/.sauron-dbus, and will contain something like:
\"unix:abstract=/tmp/dbus-BRQFYEwZz1,guid=...\"
you can source this in e.g. a shell script:
   DBUS_SESSION_BUS_ADDRESS=\"`cat ~/.sauron-dbus`\"
and thus send messages to sauron, even when not in the session.")

(defconst sr-dbus-service dbus-service-emacs
  "*internal* the D-bus service name for sauron.")

(defconst sr-dbus-path
  (concat (or dbus-path-emacs "" ) "/Sauron")
  "*internal* the D-bus interface for sauron.")

(defconst sr-dbus-interface
  (concat sr-dbus-service ".Sauron")
  "*internal* the D-bus interface for sauron.")

(defvar sr-dbus-running nil
  "*internal* Whether the dbus backend is running.")


(defun sr-register-methods ()
  "Register our functions on BUS (either :session or :system)."
    (dbus-register-method
      :session               ;; bus to use (:session or :system)
      sr-dbus-service        ;; ie. org.gnu.Emacs or org.gnu.Emacs.<username>
      sr-dbus-path           ;; ie. /org/gnu/Emacs/Sauron
      sr-dbus-interface      ;; ie. org.gnu.Emacs.Sauron
      "AddUrlEvent"          ;; method name
      'sr-dbus-add-url-event);; handler function
    (dbus-register-method
      :session                    ;; bus to use (:session or :system)
      sr-dbus-service                ;; ie. org.gnu.Emacs or org.gnu.Emacs.<username>
      sr-dbus-path           ;; ie. /org/gnu/Emacs/Sauron
      sr-dbus-interface      ;; ie. org.gnu.Emacs.Sauron
      "AddMsgEvent"          ;; method name
      'sr-dbus-add-msg-event) ;; handler function
    ;; make it introspectable ==> FIXME: doesn't work; hmmm...
    (dbus-register-method
      :session              ;; bus to use (:session or :system)
      sr-dbus-service       ;; ie. org.gnu.Emacs or org.gnu.Emacs.<username>
      sr-dbus-path          ;; ie. org.gnu.Emacs.Sauron
      dbus-interface-introspectable
      "Introspect"
      (lambda () ;; return the introspection XML for our object"
	(concat  ;;
	  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object "
	  "Introspection 1.0//EN\"
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
       </node>"))))

(defun sr-dbus-drop-cookie ()
  "Write the DBUS_SESSION_BUS_ADDRESS to ~/.sauron-dbus."
  (with-temp-file "~/.sauron-dbus"
    (insert (getenv "DBUS_SESSION_BUS_ADDRESS"))))

(defun sauron-dbus-start ()
  "Start listening for sauron dbus message; if this is succesful
return t, otherwise, return nil."
  (if (not (boundp 'dbus-path-emacs))
    (progn
      (message "sauron-dbus: not available")
      nil)
    (ignore-errors
      (when (not sr-dbus-running)
	(sr-register-methods)
	(when sauron-dbus-cookie
	  (sr-dbus-drop-cookie))
	(setq sr-dbus-running t))
      t)))

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

;;; sauron-dbus ends here
