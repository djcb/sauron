;;; sauron-zeroconf.el -- a ZEROCONF tracking module, part of sauron

;; Copyright (C) 2016 Zachary Allison <zack@zackallison.com>

;; This file is not part of GNU Emacs.
;;
;; Sauron is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Sauron is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MELFEEDHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  For documentation, please see:
;;  https://github.com/djcb/sauron/blob/master/README.org

;;; Code:

(require 'zeroconf)

(eval-when-compile
  (require 'cl))

;; Variables
;;;;;;;;;;;;;
(defvar sauron-zeroconf-priority 3
  "ZEROCONF default priority.")

(defvar sauron-zeroconf-service-file "/usr/share/avahi/service-types"
  "Location of avahi service-types file to add to the service map.")

(defvar sauron-zeroconf-service-map
  #s(hash-table size 100 test equal rehash-size 1.5 rehash-threshold 0.8
                data ("_MacOSXDupSuppress._tcp" "MacOS X Duplicate Machine Suppression"
                      " _wd-2go._tcp" "Western Digital 2Go"
                      "_acrobatSRV._tcp" "Adobe Acrobat"
                      "_adb._tcp.local" "Android TV"
                      "_adisk._tcp" "Apple TimeMachine"
                      "_adobe-vc._tcp" "Adobe Version Cue"
                      "_afpovertcp._tcp" "Apple File Sharing"
                      "_airplay._tcp" "Apple AirPlay"
                      "_airport._tcp" "Apple AirPort"
                      "_amzn-wplay._tcp" "Amazon Fire TV"
                      "_androidtvremote._tcp" "Android TV Remote"
                      "_appletv-v2._tcp" "Apple TV (V2)"
                      "_apt._tcp" "APT Package Repository"
                      "_apt_proxy._tcp" "APT Proxy"
                      "_arlo-video._tcp" "Arlo Video"
                      "_bzr._tcp" "Bazaar"
                      "_canon-bjnp1._tcp" "Canon Printer"
                      "_daap._tcp" "iTunes Audio Access"
                      "_dacp._tcp" "iTunes Remote Control"
                      "_device-info._tcp" "Device Info"
                      "_distcc._tcp" "Distributed Compiler"
                      "_dkapi._tcp" "Daikin"
                      "_domain._udp" "DNS Server"
                      "_dpap._tcp" "Digital Photo Sharing"
                      "_dyson_mqtt._tcp" "Dyson Air Filter"
                      "_ecobee._tcp" "Ecobee Smart Thermostat"
                      "_esphomelib._tcp" "Sonoff ESP Home"
                      "_ftp._tcp" "FTP File Transfer"
                      "_googlecast._tcp" "Google Cast"
                      "_googlerpc._tcp" "Google Cast"
                      "_googlezone._tcp" "Google Cast"
                      "_h323._tcp" "H.323 Telephony"
                      "_hap._tcp" "Home Automation"
                      "_home-sharing._tcp" "Apple Home Sharing"
                      "_http-alt._tcp" "Alternative Web Site"
                      "_http._tcp" "Web Site"
                      "_https._tcp" "Secure Web Site"
                      "_hue._tcp" "Phillips Hue"
                      "_iax._udp" "Asterisk Exchange"
                      "_imap._tcp" "IMAP Mail Access"
                      "_ipp._tcp" "Internet Printer"
                      "_ipps._tcp" "Secure Internet Printer"
                      "_ksysguard._tcp" "KDE System Guard"
                      "_ldap._tcp" "LDAP Directory Server"
                      "_libvirt._tcp" "Virtual Machine Manager"
                      "_lobby._tcp" "Gobby Collaborative Editor Session"
                      "_lutron._tcp" "Caseta Smart Bridge"
                      "_miio._udp" "Xiaomi Mi Product Line"
                      "_mpd._tcp" "Music Player Daemon"
                      "_mumble._tcp" "Mumble Server"
                      "_nanoleafapi._tcp" "Nanoleaf"
                      "_nanoleafms._tcp" "Nanoleaf"
                      "_net-assistant._udp" "Apple Net Assistant"
                      "_nfs._tcp" "Network File System"
                      "_ntp._udp" "NTP Time Server"
                      "_nut._tcp" "Network UPS Tools"
                      "_nv_shield_remote._tcp" "NVidia Shield"
                      "_octoprint._tcp" "Octoprint"
                      "_odisk._tcp" "DVD or CD Sharing"
                      "_omni-bookmark._tcp" "OmniWeb Bookmark Sharing"
                      "_pdl-datastream._tcp" "PDL Printer"
                      "_pgpkey-hkp._tcp" "GnuPG/PGP HKP Key Server"
                      "_philipstv_rpc._tcp" "Phillips TV"
                      "_pop3._tcp" "Posta - POP3"
                      "_postgresql._tcp" "PostgreSQL Server"
                      "_presence._tcp" "iChat Presence"
                      "_presence_olpc._tcp" "OLPC Presence"
                      "_print-caps._tcp" "Printer Capabilities"
                      "_printer._tcp" "UNIX Printer"
                      "_pulse-server._tcp" "PulseAudio Sound Server"
                      "_pulse-sink._tcp" "PulseAudio Sound Sink"
                      "_pulse-source._tcp" "PulseAudio Sound Source"
                      "_qdiscover._tcp" "QNAP Discovery"
                      "_qmobile._tcp" "QNAP Mobile"
                      "_raop._tcp" "AirTunes Remote Audio"
                      "_realplayfavs._tcp" "RealPlayer Shared Favorites"
                      "_remote-jukebox._tcp" "Remote Jukebox"
                      "_rfb._tcp" "VNC Remote Access"
                      "_rss._tcp" "Web Syndication RSS"
                      "_rtp._udp" "RTP Realtime Streaming Server"
                      "_rtsp._tcp" "RTSP Realtime Streaming Server"
                      "_scanner._tcp" "Scanner"
                      "_see._tcp" "SubEthaEdit Collaborative Text Editor"
                      "_sftp-ssh._tcp" "SFTP File Transfer"
                      "_shifter._tcp" "Window Shifter"
                      "_sip._udp" "SIP Telephony"
                      "_skype._tcp" "Skype VoIP"
                      "_smb._tcp" "Microsoft Windows Network"
                      "_sonos._tcp" "Sonos"
                      "_soundtouch._tcp" "Bose"
                      "_spotify-connect._tcp" "Spotify Connect"
                      "_ssh._tcp" "SSH Remote Terminal"
                      "_svn._tcp" "Subversion Revision Control"
                      "_telnet._tcp" "Telnet Remote Terminal"
                      "_tftp._udp" "TFTP Trivial File Transfer"
                      "_timbuktu._tcp" "Timbuktu Remote Desktop Control"
                      "_touch-able._tcp" "iPod Touch Music Library"
                      "_tp-http._tcp" "Thousand Parsec Server (HTTP Tunnel)"
                      "_tp-https._tcp" "Thousand Parsec Server (Secure HTTP Tunnel)"
                      "_tp._tcp" "Thousand Parsec Server"
                      "_tps._tcp" "Thousand Parsec Server (Secure)"
                      "_udisks-ssh._tcp" "Remote Disk Management"
                      "_viziocast._tcp" "Visio"
                      "_vlc-http._tcp" "VLC Streaming"
                      "_webdav._tcp" "WebDAV File Share"
                      "_webdavs._tcp" "Secure WebDAV File Share"
                      "_wled._tcp" "WLED"
                      "_xbmc-events._udp" "Kodi / XBMC"
                      "_xbmc-jsonrpc-h._tcp" "Kodi / XBMC"
                      "_xbmc-jsonrpc._tcp"  "Kodi / XBMC"
                      "_workstation._tcp" "Workstation"))
  "Mapping to friendly names of services.")

(defvar sauron-zeroconf-filter-function nil
  "Filter receives a SERVICE object and returns t to pass it or nil to filter it.")

(defun sauron-zeroconf-read-service-map ()
  "Read the avahi service list and add to hashmap."
  (if (and (file-exists-p sauron-zeroconf-service-file)
           (file-readable-p sauron-zeroconf-service-file))
      (let  ((lines (delq nil
                          (mapcar (lambda (x)
                                    (and (string-match "^\\(_.*\\..*p\\):\\(.*\\)" x)
                                         x))
                                  (with-temp-buffer
                                    (insert-file-contents sauron-zeroconf-service-file)
                                    (split-string (buffer-string)  "\n\r?"))))))
        (dolist (line lines)
          (let* ((split-line (split-string line ":"))
                 (type (car split-line))
                 (readable (car (cdr split-line))))
            (puthash type readable sauron-zeroconf-service-map))))))

(defun sauron-zeroconf-processor (service)
  "Maybe process the zeroconf service as SERVICE and log to sauron."
  (if (or (not (fboundp sauron-zeroconf-filter-function))
          (funcall sauron-zeroconf-filter-function service))
      (sauron-add-event
       'zeroconf
       sauron-zeroconf-priority
       (concat (cond
                ((string-equal (ignore-errors (dbus-event-member-name last-input-event)) "ItemNew")
                 "New: ")
                ((string-equal (ignore-errors (dbus-event-member-name last-input-event)) "ItemRemove")
                 "Removed: ")
                (t "Info: "))
               (if (gethash (format "%s" (zeroconf-service-type service)) sauron-zeroconf-service-map)
                   (gethash (format "%s" (zeroconf-service-type service)) sauron-zeroconf-service-map)
                 (zeroconf-service-type service))
               " \"" (zeroconf-service-name service) "\""
               )
       (lexical-let
           ((type (zeroconf-service-type service))
            (txt (zeroconf-service-txt service))
            (address (zeroconf-service-address service))
            (port (zeroconf-service-port service))
            (name (zeroconf-service-name service))
            (flags (zeroconf-service-flags service))
            (proto (zeroconf-service-protocol service))
            (aproto (zeroconf-service-aprotocol service))
            (interface (zeroconf-service-interface service))
            (host (zeroconf-service-host service))
            (link (sauron-zeroconf-make-link service)))
         (lambda ()
           (sauron-zeroconf-details type host txt address
                                    port name flags proto aproto
                                    interface link))))))


(defun sauron-zeroconf-make-link (service)
  "Attempt to make an URL for the advertised service"
  (cond
   ;; HTTP links
   ((or (string-equal (zeroconf-service-type service) "_http._tcp")
        (string-equal (zeroconf-service-type service) "_http-alt._tcp"))
    (concat "http://" (zeroconf-service-host service) ":" (format "%s" (zeroconf-service-port service))))
   ;; HTTPS links
   ((or (string-equal (zeroconf-service-type service) "_https._tcp"))
    (concat "https://" (zeroconf-service-host service) ":" (format "%s" (zeroconf-service-port service))))))


(defun sauron-zeroconf-details (type host txt address
                                     port name flags proto aproto
                                     interface link)
  "Create a buffer showing the details."
  (let
      ((msg
        (concat
         (concat "name:\t" (propertize (or name "") 'face 'font-lock-variable-name-face)
                 "\n")
         (concat "host:\t" (propertize (or host "") 'face 'font-lock-keyword-face)
                 " [" address "]" "\n")
         (if link (concat "link:\t" (propertize (or link "") 'face 'font-lock-variable-name-face) "\n" ) "")
         (concat "type:\t"
                 (propertize
                  (if (gethash (format "%s" type)
                               sauron-zeroconf-service-map)
                      (gethash (format "%s" type) sauron-zeroconf-service-map)
                    "??")
                  'face 'font-lock-string-face)
                 "\n")
         (concat "port:\t" (format "%s" port) "\n")
         (concat "type:\t" type "\n")
         (concat "flags:\t" (format "%s" flags) "\n")
         (concat "proto:\t" (format "%s" proto) "\n")
         (concat "aproto:\t" (format "%s" aproto) "\n")
         (concat "iface:\t" (zeroconf-get-interface-name interface)
                 " - " (format "%s" interface)
                 "\n")
         (concat "txt:\t" (format "%s" txt)))))
    (switch-to-buffer
     (get-buffer-create "*sauron-zeroconf-details*"))
    (read-only-mode -1)
    (erase-buffer)
    (insert msg)
    (goto-address-mode t)
    (read-only-mode t)))

(defun sauron-zeroconf-log-all-services ()
  "Write all known services to sauron."
  (interactive)
  (dolist (services (zeroconf-list-service-types))
    (dolist (service (zeroconf-list-services services))
      (sauron-zeroconf-processor service))))

(defun sauron-zeroconf-handler (&rest service)
  "Pack the list as SERVICE and pass it allong."
  (sauron-zeroconf-processor service))

;; Sauron interface requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sauron-zeroconf-start ()
  "Initialize zeroconf."
  (zeroconf-init)
  (sauron-zeroconf-read-service-map)
  (advice-add 'zeroconf-service-browser-handler :before
              #'sauron-zeroconf-handler))

(defun sauron-zeroconf-stop ()
  "Stop zeroconf."
  (advice-remove 'zeroconf-service-browser-handler #'sauron-zeroconf-handler))

(provide 'sauron-zeroconf)
;;; sauron-zeroconf ends here
