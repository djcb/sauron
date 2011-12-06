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

;; listen for dbus notifications.
;; to call from shell, some convenience functions (for zsh, bash, *not* sh)
;; # send URL to sauron...

;; function sauron-url () {
;;     # --print-reply
;;     dbus-send --session          	\
;;     --dest="org.gnu.Emacs.Sauron"	\
;;      "/org/gnu/Emacs/Sauron"            \
;;     "org.gnu.Emacs.Sauron.AddUrlEvent"  \
;;     string:shell string:url string:"$1" string:"$1"
;; }

;; # send message to sauron...
;; function sauron-msg () {
;;     # --print-reply
;;     dbus-send --session          	    \
;;     --dest="org.gnu.Emacs.Sauron"	    \
;;      "/org/gnu/Emacs/Sauron"                \
;;     "org.gnu.Emacs.Sauron.AddMsgEvent"      \
;;     string:shell string:msg string:"$1"
;; }

;;; Code:

(require 'dbus)

(defvar sr-dbus-handler nil
  "*internal* The D-bus handler function.")

(defconst sr-dbus-service dbus-service-emacs
  "*internal* the D-bus service name for sauron.")

(defconst sr-dbus-path
  (concat dbus-path-emacs "/Sauron")
  "*internal* the D-bus interface for sauron.")

(defconst sr-dbus-interface 
  (concat sr-dbus-service ".Sauron")
  "*internal* the D-bus interface for sauron.")

(defun sauron-dbus-start ()
  "Start listening for sauron dbus messages."
  (interactive)
  (when sr-dbus-handler
    (error "d-bus handler already defined"))
  (setq sr-dbus-handler
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
              <arg name=\"origin\"  type=\"s\" direction=\"in\"/>
              <arg name=\"type\"    type=\"s\" direction=\"in\"/>
              <arg name=\"message\" type=\"s\" direction=\"in\"/>
              <arg name=\"url\"     type=\"s\" direction=\"in\"/>
             </method>
            <method name=\"AddMsgEvent\">
              <arg name=\"origin\"  type=\"s\" direction=\"in\"/>
              <arg name=\"type\"    type=\"s\" direction=\"in\"/>
              <arg name=\"message\" type=\"s\" direction=\"in\"/>
             </method>
          </interface>
       </node>"))))

(defun sr-url-handler (url)
  (lexical-let ((url url))
    (lambda() (browse-url url))))

(defun sr-dbus-add-url-event (origin type message url)
  "Add a dbus-originated event."
  (sauron-add-event
    origin
    type
    3
    ;; pseudo closure...
    (lexical-let ((url url))
      (lambda() (browse-url url)))
    message)
  '(:boolean t))

(defun sr-dbus-add-msg-event (origin type message)
  "Add a dbus-originated event."
  (sauron-add-event
    origin
    type
    3
    ;; pseudo closure...
    (lexical-let ((msg message))
      (lambda() (message "%s" msg)))
    message)
  '(:boolean t))


(defun sauron-dbus-stop ()
  "Stop listening for dbus messages."
  (interactive)
  (unless sr-dbus-handler
    (error "no dbus-handler defined"))
  (dbus-unregister-object sr-dbus-handler)
  (dbus-unregister-service :session sr-dbus-service)
  (setq sr-dbus-handler nil))

(provide 'sauron-dbus)


