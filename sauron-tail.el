;;; sauron-tail.el --- tail files

;;; Commentary:

;;; ;; Tail /var/log/messages
;;; (sauron-tail-file "/var/log/messages")

;;; ;; Tail /var/log/messages, only return lines that contain the word "error"
;;; (sauron-tail-file "/var/log/messages" :filter (lambda (line) (string-match-p "error" line)))

;;; ;; If the line contains "error" set the priority to 4 and change the message
;;; (sauron-tail-file "/var/log/messages"
;;;               :filter (lambda (line)
;;;                         (if (string-match-p "error" line)
;;;                             `(:msg "I saw an error!" :prio 4))))


;;; Code:

(require 'tail)

(defvar sauron-tail-default-prio 3 "Default priority for tail.")

(defvar sauron-tail-filter-function #'sauron-tail-check-line)

(defvar sauron-tail-process-list (list))

(defcustom sauron-tail-tail-command "tail -F" "Command to run to tail files.")

(defvar sauron-tail-process-to-prefix-map (make-hash-table :test 'equal))

(defvar sauron-tail-process-to-filter-map (make-hash-table :test 'equal))

(defun sauron-tail-file (file &rest options)
  "Tail a FILE, with OPTIONS (:filter :prefix)."
  (sauron-tail-command (format "%s %s" sauron-tail-tail-command file) options))

(defun sauron-tail-command (command &rest options)
  "Tail a COMMAND, with OPTIONS (:filter :prefix)."
  (let ((filter (or (plist-get options :filter)
		    sauron-tail-filter-function))
	(process (start-process-shell-command "tail" nil command))
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
	      ((msg (or (plist-get result :msg) line))
	       (prio (or (plist-get result :prio) sauron-tail-default-prio))
	       (func (plist-get result :func))
	       (props (plist-get result :props))
	       (prefix (or (plist-get result :prefix) prefix))
	       (disp-msg (concat (if (and prefix (< 0 (length prefix)))
					     (concat prefix ":") "")
					 msg)))
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
  (setq sauron-tail-process-to-prefix-map (make-hash-table :test 'equal))
  (setq sauron-tail-process-to-filter-map (make-hash-table :test 'equal))
  (setq sauron-tail-process-list (list)))

(defun sauron-tail-check-line (line)
  "Filter out an empty LINE."
  (< 0 (length line)))

;;; Helper functions for creating filters
(defun sauron-tail-match-p (item)
  "Return a lambda where line match ITEM."
  (lexical-let ((funcitem item))
    (lambda (line) (string-match-p funcitem line))))

(defun sauron-tail-match (item)
  "Return a lambda where line match ITEM."
  (lexical-let ((funcitem item))
    (when (not (string-match (regexp-quote "\\(") funcitem))
      (setq funcitem (concat "\\(" funcitem "\\)")))    
    (lambda (line) (if (string-match funcitem line)
		       `(:msg ,(or (match-string 1 line) (match-string 0 line)))))))

(defun sauron-tail-highlight (item &rest options)
  "Return a lambda where line match ITEM."
  (lexical-let ((funcitem item)
		(face (or (plist-get options :face) 'sauron-highlight1-face))
		(func (plist-get options :func))
		(all (plist-get options :all)))
    (lambda (line)
      (if (string-match funcitem line)
	  (progn
	    (set-text-properties (or (match-beginning 1) (match-beginning 0))
				 (or (match-end 1) (match-end 0))
				 `(face ,face)
				 line)
	    `(:msg ,line :func ,func))
	(if all line nil)))))


(provide 'sauron-tail)
;;; sauron-tail.el ends here
