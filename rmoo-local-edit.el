;; rmoo-local-edit.el
;;
;; Some code to interact with Lambdastyle local editing.
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; Revised by: mattcamp

(require 'rmoo)
(require 'rmoo-mcp)
(provide 'rmoo-local-edit)

(defvar rmoo-local-edit-regexp (concat "^#\\$# "
				      "edit"
				      " name: "
				      "\\(.*\\)"
				      " upload: "
				      "\\(.*\\)$"))

(add-hook 'rmoo-handle-text-redirect-functions 'rmoo-local-edit-redirect-function)

(defun rmoo-local-edit-redirect-function (line)
  (cond ((eq (string-match rmoo-local-edit-regexp line) 0)
	 (let ((buf (get-buffer-create (generate-new-buffer-name
					(substring line
						   (match-beginning 1)
						   (match-end 1)))))
	       (world rmoo-world-here))
	   (set-buffer buf)
	   (setq rmoo-world-here world)
	   (put world 'last-output-function (get world 'output-function))
	   (put world 'output-function 'rmoo-local-edit-output-function)
	   (put world 'last-output-buffer (get world 'output-buffer))
	   (put world 'output-buffer (current-buffer))
	   (insert (substring line (match-beginning 2) (match-end 2)))
	   (insert "\n"))
	 'rmoo-mcp-nil-function)
	(t
	 nil)))

(defun rmoo-local-edit-output-function (line)
  (cond ((eq (string-match "^\\.$" line) 0)
	 (set-buffer (get rmoo-world-here 'output-buffer))
	 (insert ".\n")
	 (funcall 'rmoo-local-edit-cleanup-function)
	 (rmoo-output-function-return-control-to-last)
	 (rmoo-set-output-buffer-to-last)
	 'rmoo-mcp-nil-function)
	(t
	 (set-buffer (get rmoo-world-here 'output-buffer))
	 (insert (concat line "\n")))))

(defun rmoo-local-edit-cleanup-function ()
  (let ((world rmoo-world-here))
    (if 'rmoo-coldc
        (coldc-mode)
        (moocode-mode))
    (setq rmoo-world-here world)
    (goto-char (point-min))
    (put rmoo-world-here 'goto-function 'switch-to-buffer-other-window)
    (put rmoo-world-here 'goto-buffer (current-buffer))))
