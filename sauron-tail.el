;;; sauron-tail.el --- tail commands with filters, logging through sauron

;;; Commentary:

;; Tail a process and filter it, sending the results to sauron.
;;
;; The macros st-filter-highlight and st-filter-match make writing new rules
;; much easier.
;;
;; Here is an example use of sauron-tail where we watch the output of
;; NetworkManager (via journalctl) and log any ip changes, router announcements,
;; or disconnection, highlighting the important information.
;;
;; The second part watches the kernel messages (again from journalctl) and sends
;; alerts to sauron when a new usb mass storage device is plugged in.  The
;; result is an alert with the device, size, and product name highlighted

  ;; (use-package sauron-tail
  ;;   :after sauron
  ;;   :custom
  ;;   (sauron-tail-faces '((:foreground "cornflower blue")
  ;;                        (:foreground "light green")
  ;;                        (:foreground "yellow")))
  ;;   :config
  ;;   (defun example/journalctl-nm-filter (line)
  ;;     (let ((message
  ;;            (cond
  ;;             ((st-filter-highlight "option \\(\\(ip_address\\|routers\\).*'\\(.*\\)'\\)"))    ;; IP and Router
  ;;             ((st-filter-highlight "\\(state change: .*-> disconnected\\)"))             ;; Disconnected
  ;;             ((st-filter-highlight "policy: set \\('\\(.*\\)' (\\(.*\\)) as default .*\\)"))  ;; New primary route
  ;;             )))
  ;;       (if message `(:msg ,message))))

  ;;   (sauron-tail-command "/usr/bin/journalctl --unit NetworkManager -b --since \"5 minutes ago\" -f"
  ;;                        :filter 'example/journalctl-nm-filter
  ;;                        :prefix "nm")

  ;;   (defun example/journalctl-kernel-filter (line)
  ;;     (let ((message
  ;;            (cond
  ;;             ((st-filter-highlight "\\(\\[\\(sd[a-z]\\).* logical blocks: \\(.*\\)\\)"))  ;; New disk
  ;;             ((st-filter-highlight "\\(\\[\\(sd[a-z]\\).*Attached.*disk\\)"))  ;; New disk
  ;;             ((st-filter-highlight ": \\(usb \\([^ ]*\\): USB disconnect, device number \\(.*\\)\\)"))  ;; Disconnected
  ;;             ((st-filter-highlight ": \\(usb \\([^ ]*\\): Product: \\(.*\\)\\)"))  ;; Human readable device name
  ;;             )))
  ;;       (if message `(:msg ,message :orig "test"))))

  ;;   (sauron-tail-command "/usr/bin/journalctl -k -b -f"
  ;;                        :filter 'example/journalctl-kernel-filter
  ;;                        :prefix "kernel"))

;;; Code:


(defvar sauron-tail-default-prio 3 "Default priority for sauron-tail-cmd.")

(defvar sauron-tail-filter-function #'sauron-tail-check-line "Default filter: No empty lines.")

(defvar sauron-tail-process-list (list) "A list of running processes.")

(defvar sauron-tail-process-to-prefix-map (make-hash-table :test 'equal))

(defvar sauron-tail-process-to-filter-map (make-hash-table :test 'equal))

(defvar sauron-tail-faces '(font-lock-keyword-face font-lock-builtin-face font-lock-constant-face))

(defun sauron-tail-command (command &rest options)
  "Tail a COMMAND, with OPTIONS (:filter :prefix)."
  (let ((filter (or (plist-get options :filter)
                    sauron-tail-filter-function))
        (process (start-process-shell-command "sauron-tail" nil command))
        (prefix (or (plist-get options :prefix) command)))
    (if process
        (progn (set-process-filter process 'sauron-tail-handler)
               (puthash process prefix sauron-tail-process-to-prefix-map)
               (puthash process filter sauron-tail-process-to-filter-map)
               (add-to-list 'sauron-tail-process-list process)))))

(defun sauron-tail-handler (process lines)
  "If we pass the filter for the PROCESS for each of the LINES,  then alert."
  (dolist (line (split-string lines "\n"))
    (let ((filter (gethash process sauron-tail-process-to-filter-map))
          (prefix (gethash process sauron-tail-process-to-prefix-map)))
      (let ((result (if filter (funcall filter line))))
        (when result
          (let*
              (
               (msg (or (plist-get result :msg) line))
               (prio (or (plist-get result :prio) sauron-tail-default-prio))
               (func (plist-get result :func))
               (props (plist-get result :props))
               (prefix (or (plist-get result :prefix) prefix))
               (disp-msg (concat (if prefix (concat prefix ": ") "") msg))
               )
            (sauron-add-event 'tail
                              prio
                              disp-msg
                              func
                              props)))))))

(defun sauron-tail-start () "Dummy Function." t)

(defun sauron-tail-stop ()
  "Stop existing tail processes."
  (dolist (process sauron-tail-process-list)
    (delete-process process))
  (setq sauron-tail-process-to-prefix-map (make-hash-table :test 'equal)
        sauron-tail-process-to-filter-map (make-hash-table :test 'equal)
        sauron-tail-process-list (list)))

;; Filters
(defun sauron-tail-check-line (line)
  "Filter out an empty LINE."
  (< 0 (length line)))

;;; Helper functions for creating filters
(defun sauron-tail-match-p (funcitem line)
  "Is FUNCITEM found in LINE?"
  (string-match-p funcitem line))

(defun sauron-tail-match (item line)
  "If ITEM is found in LINE return the whole line."
  (if (sauron-tail-match-p item line)
      (match-string 0 line)))

(defun sauron-tail-highlight (item line &optional faces &rest options)
  "Apply highlight to each match from ITEM in LINE.  Optionally adding FACES or OPTIONS."
  (let ((faces (or faces sauron-tail-faces)))
    (if (string-match item line)
        (let*
            ((full-match (substring line (match-beginning 1) (match-end 1)))
             (face (car faces))
             (match 2))
          (while (match-beginning match)
            (setq face (car faces))
            (set-text-properties (- (match-beginning match) (match-beginning 1))
                                 (- (match-end match) (match-beginning 1))
                                 `(face ,face) full-match)
            (setq match (1+ match)
                  face (add-to-list 'faces (pop faces) t)))
          full-match))))

;; Macros to make matching easier for the end user.
(defmacro st-filter-match (item &optional rest)
  "Check if ITEM match LINE, pass the REST along."
  `(sauron-tail-match ,item line ,rest))

(defmacro st-filter-highlight (item &optional rest)
  "Check if ITEM match LINE, pass the REST along."
  `(sauron-tail-highlight ,item line ,rest))

(provide 'sauron-tail)
;;; sauron-tail.el ends here
