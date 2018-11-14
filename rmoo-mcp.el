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

(defvar rmoo-mcp-keyval-regexp (concat "\\([^ \\\":]+\\): +"
                                       "\\([^ \\\"]+\\|"
                                       "\"\\([^\\\"]\\|\\\\.\\)*\"\\)"
                                       "\\( +\\|$\\)")
  "Recognize one keyword: value pair.")

(defvar rmoo-status-text "Status"
  "The text currently appended to the status line.")

(defvar rmoo-mcp-intermediaries nil
  "An intermediary associated list used to temporarily store information needed in editor buffers.")

(defvar rmoo-mcp-cleanup-function nil)

(defun rmoo-mcp-init-connection ()
  "MCP negotiation. Send an authentication key, enumerate packages, and send a negotiation end message."
  (interactive)
  (make-local-variable 'rmoo-mcp-auth-key)
  (setq rmoo-mcp-auth-key (prin1-to-string (random 99999999999)))
  (let ((proc (get-buffer-process (current-buffer))))
    (rmoo-send-string (concat "#$#mcp authentication-key: " rmoo-mcp-auth-key " version: 2.1 to: 2.1") proc)))

(defun rmoo-mcp-dispatch (request-name auth-key keyval-string)
  "Figure out if we know what to do with the given request;
  check the auth-key;
  check that we have all the args we want;
  if data-follows, start gathering it."
  (setq fooo (list request-name auth-key keyval-string))
  (let ((entry (assoc request-name rmoo-mcp-request-table)))
    (if entry
      (if (not (equal auth-key rmoo-mcp-auth-key))
        (error "Illegal authentication key in %s" (buffer-name))
        (let ((arglist (rmoo-mcp-arglist entry keyval-string)))
          (if (listp arglist)
            (apply (rmoo-mcp-setup-function entry) arglist)
            (rmoo-mcp-handle-unknown request-name keyval-string))))
      (rmoo-mcp-handle-unknown request-name keyval-string))))

(defun rmoo-mcp-setup-function (entry)
  "What function do we call to deal with this entry in the table?"
  (nth 2 entry))

(defun rmoo-mcp-init-function (entry)
  "What function do we call to initialize this package after confirming negotiation?"
  (nth 5 entry))

(defun rmoo-mcp-handle-unknown (request data-follows)
  (if data-follows
    (let* ((start (point))
           (line (progn
                   (beginning-of-line 2)
                   (buffer-substring start (point)))))
      (let ((buf (current-buffer)))
        (set-buffer (rmoo-mcp-setup-data (generate-new-buffer
                                           "Unknown data")))
        (insert (concat "Request name: " request))
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
               (nth 0 (cdr (setq barr entry))))))))


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

(defun rmoo-mcp-register (package-name keys setup-function min-version max-version init-function)
  "Register a new mcp request type.
  PACKAGE-NAME is the full, official name of the MCP package.
  KEYS is an alist of pairs (key . default-value).
  The key must be a string.
  The default-value must be a string, or the symbol 'required, which means
  that the request must supply a value.
  SETUP-FUNCTION is a symbol for the function that gets called to set up
  request-specific details.
  MIN-VERSION is the minimum version of the MCP package supported.
  MAX-VERSION is the maximum version of the MCP package supported.
  INIT-FUNCTION is a symbol for the function that gets called immediately after negotiation is complete."
  (let* ((key package-name)
         (value (list keys setup-function min-version max-version init-function))
         (entry (assoc key rmoo-mcp-request-table)))
    (if entry
      (setcdr entry value)
      (progn
        (setq rmoo-mcp-request-table (cons (cons key value)
                                           rmoo-mcp-request-table))))))

(defun rmoo-mcp-remove-line ()
  (let ((start (progn (rmoo-beginning-of-line) (point))))
    (beginning-of-line 2)
    (delete-region start (point))))

(defun rmoo-mcp-setup-data (buffer)
  (rmoo-mcp-remove-line)
  (setq rmoo-state 'unquoting
        rmoo-current-process (get-buffer-process (current-buffer))
        rmoo-buffer buffer))

;; Simply register the package to begin with.
(rmoo-mcp-register "dns-org-mud-moo-simpleedit" '() nil "1.0" "1.0" nil)

;; Now go for the trigger
(rmoo-mcp-register "dns-org-mud-moo-simpleedit-content"
                   '(("reference" . 'required)
                     ("name" . 'required)
                     ("type" . 'required)
                     ("content*" . 'required)
                     ("_data-tag" . 'required))
                   'rmoo-mcp-start-edit
                   "1.0"
                   "1.0"
                   nil)

(defun rmoo-mcp-start-edit (reference name type content _data-tag)
  (setq rmoo-mcp-intermediaries (cons (cons "auth-key" rmoo-mcp-auth-key) rmoo-mcp-intermediaries))
  (let ((buf (current-buffer))
        (world rmoo-world-here))
    (set-buffer (rmoo-mcp-setup-data (get-buffer-create name)))
    (insert (concat "Buffer: " (prin1-to-string (current-buffer))))
    (setq rmoo-world-here world)
    (put world 'output-buffer (current-buffer))
    (put world 'last_output_buffer buf)
    (put world 'last-output-function (get world 'output-function))
    (put world 'output-function 'rmoo-mcp-output-function)
    (setq rmoo-mcp-intermediaries (cons (cons "_data-tag" _data-tag) rmoo-mcp-intermediaries))
    (setq rmoo-mcp-intermediaries (cons (cons "reference" (concat "reference: \"" reference "\" type: \"" type "\" content*: \"\" _data-tag: " _data-tag)) rmoo-mcp-intermediaries))
    (erase-buffer)
    (setq rmoo-mcp-cleanup-function
          (cond
            ((equal type "moo-code")
             'rmoo-mcp-cleanup-edit-program)
            ((equal type "string-list")
             'rmoo-mcp-cleanup-edit-list)
            ((equal type "mail")
             'rmoo-mcp-cleanup-edit-mail)
            ((equal type "jtext")
             'rmoo-mcp-cleanup-edit-jtext)
            (t
              'rmoo-mcp-cleanup-edit-list)))
    (set-buffer buf)))

;;
;; I've told rmoo-mcp-cleanup-edit-* about rmoo-worlds. I've also given them
;; the responsibility of displaying the buffer. It might be better
;; to have this responsibility lie elsewhere.
;;
(defun rmoo-mcp-common-editor-functions ()
  (goto-char (point-max))
  (goto-char (point-min))
  (setq rmoo-select-buffer (current-buffer))
  (display-buffer (current-buffer) t)
  (setq rmoo-world-here world)
  (put rmoo-world-here 'goto-function 'switch-to-buffer-other-window)
  (put rmoo-world-here 'goto-buffer (current-buffer))
  (setq-local rmoo-mcp-data rmoo-mcp-intermediaries)
  (setq rmoo-mcp-intermediaries nil))

(defun rmoo-mcp-cleanup-edit-program ()
  (let ((world rmoo-world-here))
    (moocode-mode)
    (rmoo-mcp-common-editor-functions)))

(defun rmoo-mcp-cleanup-edit-list ()
  (let ((world rmoo-world-here))
    (rmoo-list-mode)
    (rmoo-mcp-common-editor-functions)))

(defun rmoo-mcp-cleanup-edit-mail ()
  (let ((world rmoo-world-here))
    (rmoo-mail-mode)
    (goto-char (point-max))
    (insert "\n.\n")
    (backward-char 3)
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
;; Kind of cheat a little with negotiation and register each message as an individual package.
;; That way they get appropriately detected and handled without a separate regex or effort.
;;
(rmoo-mcp-register "mcp-negotiate" '() nil "1.0" "2.0" nil)

(rmoo-mcp-register "mcp-negotiate-can"
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
                (rmoo-send-string (concat "#$#mcp-negotiate-can " rmoo-mcp-auth-key " package: \"" (nth 0 entry) "\" min-version: \"" (nth 3 entry) "\" max-version: \"" (nth 4 entry) "\"") proc)
                (if (rmoo-mcp-init-function entry)
                  (funcall (rmoo-mcp-init-function entry) proc))))))

(rmoo-mcp-register "mcp-negotiate-end" '() 'rmoo-mcp-end-negotiation "1.0" "2.0" nil)

(defun rmoo-mcp-end-negotiation ()
  "When the server is done negotiating, confirm that we are too."
  (rmoo-send-string (concat "#$#mcp-negotiate-end " rmoo-mcp-auth-key) proc))

(rmoo-mcp-register "dns-com-awns-status"
                   '(("text" . 'required))
                   'rmoo-mcp-do-status
                   "1.0"
                   "1.0"
                   nil)

(defun rmoo-mcp-do-status (text)
  (delete rmoo-status-text mode-line-misc-info)
  (add-to-list 'mode-line-misc-info text 'APPEND)
  (setq rmoo-status-text text))

(rmoo-mcp-register "dns-com-vmoo-client" '() 'rmoo-mcp-do-client "1.0" "1.0" 'rmoo-mcp-initialize-client)

(defun rmoo-mcp-initialize-client (proc)
  (rmoo-send-string (concat "#$#dns-com-vmoo-client-info " rmoo-mcp-auth-key " name: \"RMOO (Emacs)\" text-version: \"" rmoo-version "\" internal-version: \"0\"") proc)
  (rmoo-send-string (concat "#$#dns-com-vmoo-client-screensize " rmoo-mcp-auth-key " Cols: " (number-to-string (- (window-total-width) 5)) " Rows: " (number-to-string (window-total-height))) proc))

(defun rmoo-mcp-redirect-function (line)
  (if (string-match "^#$#mcp version: [0-9]\.[0-9] to: [0-9]\.[0-9]$" line)
    (rmoo-mcp-init-connection)
    (cond ((eq (string-match rmoo-mcp-regexp line) 0)
           (rmoo-mcp-dispatch (rmoo-match-string 1 line)
                              (if (match-beginning 3)
                                (rmoo-match-string 3 line)
                                nil)
                              (rmoo-match-string 6 line))
           'rmoo-mcp-nil-function)
          (t nil))))

(defun rmoo-mcp-nil-function (line) "Okay, this is a kludge")

(defun rmoo-mcp-output-function-hooks ())

(defun rmoo-mcp-output-function (line)
  ;; Ignore MCP_snoop
  (if (not (string-match "^S->C.*" line))
    (progn
      (cond ((string= (concat "#$#: " (cdr (assoc "_data-tag" rmoo-mcp-intermediaries))) line)
             (progn
               (set-buffer (get rmoo-world-here 'output-buffer))
               (funcall rmoo-mcp-cleanup-function)
               (rmoo-output-function-return-control-to-last)
               (rmoo-set-output-buffer-to-last)
               'rmoo-mcp-nil-function))
            ((eq (string-match (concat "#$#\\* " (cdr (assoc "_data-tag" rmoo-mcp-intermediaries)) " content: ") line) 0)
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
              (message (concat "Garbled MCP Data: " line)))))))

;;
;; Interface to moo.el
;;
;; (add-hook 'rmoo-interactive-mode-hooks 'rmoo-mcp-init-connection)
(add-hook 'rmoo-handle-text-redirect-functions 'rmoo-mcp-redirect-function)
