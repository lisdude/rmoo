;;rmoo-list
;;
;; Random functions for editing moo list properties
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; Revised by: mattcamp
;;
(require 'rmoo)
(provide 'rmoo-list)

(defvar rmoo-list-mode-hooks nil "List of functions run everytime emacs enters MOO List Mode")

(defvar rmoo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'rmoo-upload-and-destroy)
    (define-key map (kbd "C-c s") 'rmoo-upload-buffer-directly)
    (define-key map (kbd "<f7>") 'rmoo-upload-buffer-directly)
    (define-key map (kbd "C-c ]") 'rmoo-destroy)
  map))

(defun rmoo-list-mode ()
  "A major mode for editing MOO lists and arbitrary text.

Commands:
\\{rmoo-list-mode-map}"

  (interactive)
  (let ((world rmoo-world-here))
    (setq mode-name (concat "RMOO-List@" (symbol-name world)))
    (setq major-mode 'rmoo-list-mode)
    (use-local-map (copy-keymap rmoo-list-mode-map))
    (run-hooks rmoo-list-mode-hooks)))
