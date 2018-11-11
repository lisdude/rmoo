;;
;; MUD Client Protocol 2.1
;; (http://www.moo.mud.org/mcp2/mcp2.html)
;;
;; Adapted from the original MCP 1.0 implementation by lisdude <lisdude@lisdude.com>
;;
;; Original Authors: Ron Tapia, Erik, mattcamp
;;
(require 'rmoo)
(provide 'rmoo-mcp)
(provide 'mcp)

(defvar rmoo-mcp-regexp (concat "^#\\$#"
			       "\\([^ :*\\\"]+\\)"                          ; request-name
			       "\\( +\\([^ :\\\"]+\\)\\( \\|$\\)\\)?"       ; authentication-key
			       "\\( *\\(.*\\)\\)$"))                        ; key-value pairs

(defvar rmoo-mcp-quoting-prefix "@@@"
  "Prepended to all data sent after an mcp command.
Must not contain any special regexp characters.")

(defvar rmoo-mcp-incomplete nil)

(defvar rmoo-status-text "Status"
"The text currently appended to the status line.")

(defvar rmoo-mcp-quoting-end "#$#END"
  "Signals end of mcp data.")

(defvar rmoo-mcp-cleanup-function nil)

(defun rmoo-mcp-init-connection ()
  "MCP negotiation. Send an authentication key, enumerate packages, and send a negotiation end message."
  (interactive)
  (make-local-variable 'rmoo-mcp-auth-key)
  (setq rmoo-mcp-auth-key (prin1-to-string (random 99999999999)))
  (let ((proc (get-buffer-process (current-buffer))))
    (rmoo-send-string (concat "#$#mcp authentication-key: " rmoo-mcp-auth-key " version: 2.1 to: 2.1") proc)))
  ;;  (rmoo-send-string (concat "#$#mcp-negotiate-can " rmoo-mcp-auth-key " package: \"mcp-negotiate\" min-version: \"1.0\" max-version: \"2.0\"") proc)
   ;; (loop for x in rmoo-mcp-request-table
  ;;        do (rmoo-send-string (concat "#$#mcp-negotiate-can " rmoo-mcp-auth-key " package: \"" (nth 0 x) "\" min-version: \"1.0\" max-version: \"2.0\"") proc))
;;    (rmoo-send-string (concat "#$#mcp-negotiate-end " rmoo-mcp-auth-key) proc))
;;    (loop for x in rmoo-mcp-request-table
;;          do (progn
 ;;              (if (rmoo-mcp-init-function x)
   ;;              (funcall (rmoo-mcp-init-function x) proc)))))

(defun rmoo-mcp-dispatch (request-name auth-key keyval-string)
  "Figure out if we know what to do with the given request;
check the auth-key if it's important;
check that we have all the args we want;
if data-follows, start gathering it."
(message (concat "<< MCP Dispatch >> request: " request-name " auth-key: " auth-key " keyval-string: " keyval-string))
  (setq fooo (list request-name auth-key keyval-string))
  (let ((entry (assoc request-name rmoo-mcp-request-table)))
    (if entry
	(if (not (equal auth-key rmoo-mcp-auth-key))
	    (error "Illegal authentication key in %s" (buffer-name))
	  (let ((arglist (rmoo-mcp-arglist entry keyval-string)))
	    (if (listp arglist)
		(apply (rmoo-mcp-setup-function entry) arglist)
	      (rmoo-mcp-handle-unknown keyval-string))))
      (rmoo-mcp-handle-unknown keyval-string))))

(defun rmoo-mcp-setup-function (entry)
  "What function do we call to deal with this entry in the table?"
  (nth 2 (cdr entry)))

(defun rmoo-mcp-init-function (entry)
  "What function do we call to initialize this package after confirming negotiation?"
  (nth 6 entry))

(defun rmoo-mcp-handle-unknown (data-follows)
  (if data-follows
      (let* ((start (point))
	     (line (progn
		     (beginning-of-line 2)
		     (buffer-substring start (point)))))
	(let ((buf (current-buffer)))
	  (set-buffer (rmoo-mcp-setup-data (generate-new-buffer
					   "Unknown data")))
	  (insert line)
	  (set-buffer buf)))
    (rmoo-mcp-remove-line)))

(defun rmoo-mcp-arglist (entry keyval-string)
  (let ((alist (rmoo-mcp-parse-keyvals keyval-string)))
    (if (listp alist)
	(catch 'rmoo-mcp-missing-arg
	  (mapcar
	   (function
	    (lambda (template)
	      (let ((a (assoc (car template) alist)))
		(cond (a
		       (cdr a))
		      ((not (eq 'required (cdr template)))
		       (cdr template))
		      (t
		       (throw 'rmoo-mcp-missing-arg 'rmoo-mcp-missing-arg))))))
	   (nth 1 (cdr (setq barr entry))))))))

(defvar rmoo-mcp-keyval-regexp (concat "\\([^ \\\":]+\\): +"
				      "\\([^ \\\"]+\\|"
				      "\"\\([^\\\"]\\|\\\\.\\)*\"\\)"
				      "\\( +\\|$\\)")
  "Recognize one keyword: value pair.")

(defun rmoo-mcp-parse-keyvals (keyval-string)
  (catch 'rmoo-mcp-failed-parse
    (let ((arglist nil)
	  (start 0))
      (while (< start (length keyval-string))
	(if (string-match rmoo-mcp-keyval-regexp keyval-string start)
	    (setq start (match-end 0)
		  arglist (cons
			   (cons
			    (rmoo-match-string 1 keyval-string)
			    (if (eq (elt keyval-string
					 (match-beginning 2))
				    ?\")
				(car (read-from-string
				      (rmoo-match-string 2 keyval-string)))
			      (rmoo-match-string 2 keyval-string)))
			   arglist))
	  (throw 'rmoo-mcp-failed-parse 'rmoo-mcp-failed-parse)))
      arglist)))

(defvar rmoo-mcp-request-table '()
  "Alist of information about known request types, keyed by string.")

(defun rmoo-mcp-register (package-name auth-key keys
				      setup-function min-version max-version init-function)
  "Register a new mcp request type.
PACKAGE-NAME is the full, official name of the MCP package.
AUTH-KEY is t if this request type needs an authentication key to work.
KEYS is an alist of pairs (key . default-value).
  The key must be a string.
  The default-value must be a string, or the symbol 'required, which means
that the request must supply a value.
SETUP-FUNCTION is a symbol for the function that gets called to set up
request-specific details.
MIN-VERSION is the minimum version of the MCP package supported.
MAX-VERSION is the maximum version of the MCP package supported.
INIT-FUNCTION is a symbol for the function that gets called immediately after negotiation is complete."
(message (concat "<< MCP >> Registering " package-name))
  (let* ((key package-name)
	 (value (list auth-key keys setup-function min-version max-version init-function))
	 (entry (assoc key rmoo-mcp-request-table)))
    (if entry
	(setcdr entry value)
      (progn
	(setq rmoo-mcp-request-table (cons (cons key value)
					  rmoo-mcp-request-table))))))

(defun rmoo-mcp-need-auth-key (entry)
  "Does this entry in the table need an authentication key?"
  (car (cdr entry)))

(defun rmoo-mcp-remove-line ()
  (let ((start (progn (rmoo-beginning-of-line) (point))))
    (beginning-of-line 2)
    (delete-region start (point))))

(defun rmoo-mcp-setup-data (buffer)
  (rmoo-mcp-remove-line)
  (setq rmoo-state 'unquoting
	rmoo-current-process (get-buffer-process (current-buffer))
	rmoo-buffer buffer))

(rmoo-mcp-register "dns-org-mud-moo-simpleedit" nil
		  '(("type" . "text")
		    ("name" . 'required)
		    ("upload" . 'required))
		  'rmoo-mcp-start-edit
		  "1.0"
          "1.0"
          nil)

;;
;; What have I done here? Basically, I've told rmoo-mcp-start-edit
;; about rmoo-worlds.
;;
(defun rmoo-mcp-start-edit (type name upload)
  (let ((buf (current-buffer))
	(world rmoo-world-here))
    (set-buffer (rmoo-mcp-setup-data (get-buffer-create name)))
    (insert (concat "Buffer: " (prin1-to-string (current-buffer))))
    (setq rmoo-world-here world)
    (put world 'output-buffer (current-buffer))
    (put world 'last_output_buffer buf)
    (put world 'last-output-function (get world 'output-function))
    (put world 'output-function 'rmoo-mcp-output-function)
    (erase-buffer)
    (setq rmoo-mcp-cleanup-function
	  (cond
	   ((equal type "program")
	    'rmoo-mcp-cleanup-edit-program)
	   ((equal type "list")
	    'rmoo-mcp-cleanup-edit-list)
	   ((equal type "mail")
	    'rmoo-mcp-cleanup-edit-mail)
	   ((equal type "jtext")
	    'rmoo-mcp-cleanup-edit-jtext)
	   (t
	    'rmoo-mcp-cleanup-edit-text)))
    (insert  upload "\n")
    (set-buffer buf)))

;;
;; I've told rmoo-mcp-cleanup-edit-* about rmoo-worlds. I've also given them
;; the responsibility of displaying the buffer. It might be better
;; to have this responsibility lie elsewhere.
;;
(defun rmoo-mcp-cleanup-edit-program ()
  (let ((world rmoo-world-here))
    (moocode-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-text ()
  (let ((world rmoo-world-here))
    (rmoo-text-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-mail ()
  (let ((world rmoo-world-here))
    (rmoo-mail-mode)
    (goto-char (point-max))
    (insert "\n.\n")
    (backward-char 3)
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-list ()
  (let ((world rmoo-world-here))
    (rmoo-list-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-jtext ()
  (let ((world rmoo-world-here))
    (rmoo-list-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (switch-to-buffer-other-window (current-buffer))
    (setq rmoo-world-here world)))

;;
;; The functions that don't have data following are easily handled, they
;; didn't require any modification. But see the mods to the general
;; mcp stuff...
;;
;; Kind of cheat a little with negotiation and register each message as an individual package.
;; That way they get appropriately detected and handled without a separate regex or effort.
;;
(rmoo-mcp-register "mcp-negotiate" t '() nil "1.0" "2.0" nil)

(rmoo-mcp-register "mcp-negotiate-can" t
                   `(("package" . 'required)
                     ("min-version" . 'required)
                     ("max-version" . 'required))
                   'rmoo-mcp-do-negotiation
                   "1.0"
                   "2.0"
                   nil)

(defun rmoo-mcp-do-negotiation (package-name min-version max-version)
  "When the server tells us it supports a package, check if we also support it and send along a negotiation response."
  (let ((entry (assoc package-name rmoo-mcp-request-table)))
    (if entry (progn
        (rmoo-send-string (concat "#$#mcp-negotiate-can " rmoo-mcp-auth-key " package: \"" (nth 0 entry) "\" min-version: \"" (nth 4 entry) "\" max-version: \"" (nth 5 entry) "\"") proc)
        (if (rmoo-mcp-init-function entry)
           (funcall (rmoo-mcp-init-function entry) proc))))))

(rmoo-mcp-register "mcp-negotiate-end" t '() 'rmoo-mcp-end-negotiation "1.0" "2.0" nil)

(defun rmoo-mcp-end-negotiation ()
  "When the server is done negotiating, confirm that we are too."
    (rmoo-send-string (concat "#$#mcp-negotiate-end " rmoo-mcp-auth-key) proc))

(rmoo-mcp-register "dns-com-awns-status" t
		  '(("text" . 'required))
		  'rmoo-mcp-do-status
          "1.0"
          "1.0"
          nil)

(defun rmoo-mcp-do-status (text)
  (rmoo-mcp-remove-line)
  (delete rmoo-status-text mode-line-misc-info)
  (add-to-list 'mode-line-misc-info text 'APPEND)
  (setq rmoo-status-text text))

(rmoo-mcp-register "dns-com-vmoo-client" t '() 'rmoo-mcp-do-client "1.0" "1.0" 'rmoo-mcp-initialize-client)

(defun rmoo-mcp-initialize-client (proc)
  (rmoo-send-string (concat "#$#dns-com-vmoo-client-info " rmoo-mcp-auth-key " name: \"RMOO (Emacs)\" text-version: \"1.2\" internal-version: \"1.2\"") proc))

(defun rmoo-mcp-redirect-function (line)
  (when (string-match "^#$#mcp version: [0-9]\.[0-9] to: [0-9]\.[0-9]$" line)
    (rmoo-mcp-init-connection))
  (cond ((eq (string-match rmoo-mcp-regexp line) 0)
	 (rmoo-mcp-dispatch (rmoo-match-string 1 line)
			   (if (match-beginning 3)
			       (rmoo-match-string 3 line)
			     nil)
			   (rmoo-match-string 6 line))
	 'rmoo-mcp-nil-function)
	    (t nil)))

(defun rmoo-mcp-nil-function (line) "Okay, this is a kludge")

(defun rmoo-mcp-output-function-hooks ())

(defun rmoo-mcp-output-function (line)
  (cond ((string= rmoo-mcp-quoting-end line)
	 (progn
	   (set-buffer (get rmoo-world-here 'output-buffer))
	   (funcall rmoo-mcp-cleanup-function)
	   (rmoo-output-function-return-control-to-last)
	   (rmoo-set-output-buffer-to-last)
	   'rmoo-mcp-nil-function))
	((eq (string-match (concat "^" rmoo-mcp-quoting-prefix) line) 0)
	 (setq line (substring line (match-end 0)))
	 (set-buffer (get rmoo-world-here 'output-buffer))
	 (let ((start (point))
	       end)
	   (goto-char (point-max))
	   (insert-before-markers (concat line "\n"))
	   (save-restriction
	     (narrow-to-region start (point))
	     (goto-char start)
	     (run-hooks (rmoo-mcp-output-function-hooks)))))
	(t
	 ;;Aieee! Run away
	 (rmoo-output-function-return-control-to-last)
	 (rmoo-set-output-buffer-to-last)
	 (message (concat "Garbled MCP Data: " line)))))

(defun rmoo-mcp-output-function-discard (line)
  (message "Discarding line: %s" line))

;;
;; Interface to moo.el
;;
;; (add-hook 'rmoo-interactive-mode-hooks 'rmoo-mcp-init-connection)
(add-hook 'rmoo-handle-text-redirect-functions 'rmoo-mcp-redirect-function)

;;
;; $Log: rmoo-mcp.el,v $
;; Revision 1.3  1999/11/24 19:20:22  mattcamp
;; Added "(provide 'rmoo-mcp)" so that other modules requiring MCP can require rmoo-mcp and byte-compile successfully (because one of my users insists on having all of RMOO compiled).
;;
;; Revision 1.2  1999/06/07 14:34:51  mattcamp
;; Disabled automatic calling of rmoo-mcp-init-connection on connection; now it is only called if the server sends "#$#mcp version: 1.0".  This is to prevent error messages from servers that don't handle MCP/1.0 messages well.
;;
;; Revision 1.1  1999/03/02 00:14:38  mattcamp
;; Initial revision
;;
