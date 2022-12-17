;;rmoo-scratch.el
;;
;; A scratch buffer will prompt you for an upload command and let you do whatever.
;; (shamelessly stolen from rmoo-mail.el)

(require 'rmoo)
(provide 'rmoo-scratch)

(defvar rmoo-scratch-use-new-frame nil
  "If non-nil, create a new frame for editing.")

(defvar rmoo-scratch-mode-hooks nil "Hooks to run whenever MOO Scratch mode starts up in a buffer.")

(defvar rmoo-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'rmoo-upload-and-destroy)
    (define-key map (kbd "C-c s") 'rmoo-upload-buffer-directly)
    (define-key map (kbd "C-c ]") 'rmoo-destroy)
    (define-key map (kbd "C-c C-c") 'rmoo-code-commentify)
    (define-key map (kbd "C-c C-u") 'rmoo-code-uncommentify)
    map)
  "Keymap for MOO scratch major mode.")


(define-key rmoo-interactive-mode-map "\^c\^s" 'rmoo-scratch)

(provide 'rmoo-scratch)
(defun rmoo-scratch (command)
  (interactive "sUpload Command: ")

  (let ((buf (get-buffer-create (generate-new-buffer-name
                                  (concat "Scratch Buffer (" command ")"))))
        (world rmoo-world-here)
        frame)
    (set-buffer buf)
    (setq rmoo-world-here world)
    (rmoo-scratch-mode)
    (goto-char (point-min))
    (if (not (equal "" command))
        (progn
        (insert-before-markers (concat command "\n\n."))
        (backward-char 2)))
    (if rmoo-scratch-use-new-frame
      (make-frame)
      (switch-to-buffer-other-window (current-buffer)))))

(defun rmoo-scratch-send-buffer ()
  (interactive)
  (rmoo-send-here (buffer-string)))

(defun rmoo-scratch-mode ()
  (interactive)
  (setq mode-name (concat "RMOOScratch@" (symbol-name rmoo-world-here)))
  (setq major-mode 'rmoo-scratch-mode)
  (use-local-map rmoo-scratch-mode-map)
  (run-hooks 'rmoo-scratch-mode-hooks))
