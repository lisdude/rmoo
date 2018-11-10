;;; moocode-mode.el - Major mode for editing LambdaMOO code files
;;
;; Copyright (C) 2012-2013 Rob Myers <rob@robmyers.org>
;; moocode-font-lock-(maybe)-notedit adapted from ruby-mode.el
;; Copyright (C) 1994-2008 Free Software Foundation, Inc.
;; smie code adapted from octave-mode.el
;; Copyright (C) 1997, 2001-2013 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation
;;
;; To install, add this file to your Emacs load path.
;; You can then load it using M-x moocode-mode
;; Alternatively you can have Emacs load it automatically for files with
;; a .moo extension by adding the following to your .emacs file:
;;
;;    (require 'moocode-mode)
;;    (add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))

;;; TODO:
;;
;; strings as object descriptors in declarations
;; string comments
;; Short form error handing
;; templates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rmoo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moocode-font-lock-notedit (limit)
  (when (re-search-forward "^@notedit\\>" limit t)
    (end-of-line)
    (let ((from (point)))
      (forward-line 1)
      (when (re-search-forward "^\\.$" limit t)
        (beginning-of-line)
        (set-match-data (list from (point)))
        t))))

(defun moocode-font-lock-maybe-notedit (limit)
  (let (from)
    (save-excursion
      (when (and (re-search-backward "^\\(@notedit\\>\\|\\.$\\)" nil t)
                 (string= (match-string 1) "@notedit"))
        (end-of-line)
        (setq from (point))))
    (if (and from (and (re-search-forward "^\\(@notedit\\>\\|\\.$\\)" nil t)
                       (string= (match-string 1) ".")))
        (save-excursion
          (beginning-of-line)
          (set-match-data (list beg (point)))
          t)
      nil)))

(defconst moocode-font-lock-keywords
  '(
    ;; @commands: @create @edit etc, although see below for @verb and @prop
    ("^\\s-*@\\w+\\b"
     . font-lock-preprocessor-face)
    ;; Types
    ("\\<\\(ERR\\|FLOAT\\|INT\\|LIST\\|MAP\\|NUM\\|OBJ\\|STR\\)\\>"
     . font-lock-constant-face)
    ;; Keywords
    ("^\\s-*\\(?:break\\|continue\\|e\\(?:lse\\(?:if\\)?\\|nd\\(?:fork?\\|if\\|try\\|while\\)\\|xcept\\)\\|f\\(?:inally\\|ork?\\)\\|if\\|return\\|try\\|while\\)\\>"
     ;; Yes, zero. Weird
     . (0 font-lock-keyword-face))
    ;; Single local variable declarations
    ("^\\s-*\\(\\w+\\)\\s-*=[^=]"
     (1 font-lock-variable-name-face))
    ;; Multiple local variable declarations
    ;; Note the empty \\(\\) to ensure there's always a 2, even if empty
    ("^\\s-*{" "\\(\\(\\<\\w+\\>\\)[^,]*\\|\\(\\)\\s-*}\\)" nil nil
     (2 font-lock-variable-name-face))
    ;; Verb declarations
    ("^\\S-*@verb\\s-+\\(\\(\".+\"\\|\\w+\\):\\(\\w+\\)\\)\\(.*\\)$"
     ;; the verb name
     (3 font-lock-function-name-face)
     ;; The verb spec and permissions
     (4 font-lock-constant-face))
    ;; Property declarations
    ("@\\(prop\\|property\\)\\s-+\\(\\w+\\.\\w+\\)\\(.+\\s-+\\([rwc]+\\)\\)?"
     ;; The property name
     (2 font-lock-variable-name-face)
     ;; The property permission flags
     (4 font-lock-constant-face))
    ;; Automatically provided variables in verbs
    ("\\<\\(args\\|argstr\\|caller\\|dobj\\|dobjstr\\|iobj\\|iobjstr\\|player\\|prepstr\\|this\\|verb\\)\\>"
     . font-lock-variable-name-face)
    ;; Built-in functions
    ("\\<\\(\\(?:a\\(?:bs\\|cos\\|dd_\\(?:property\\|verb\\)\\|\\(?:si\\|ta\\)n\\)\\|b\\(?:inary_hash\\|oot_player\\|uffered_output_length\\)\\|c\\(?:all\\(?:_function\\|er\\(?:\\(?:_perm\\)?s\\)\\)\\|eil\\|h\\(?:ildren\\|parent\\)\\|lear_property\\|o\\(?:nnect\\(?:ed_\\(?:\\(?:player\\|second\\)s\\)\\|ion_\\(?:name\\|options?\\)\\)\\|sh?\\)\\|r\\(?:eate\\|ypt\\)\\|time\\)\\|d\\(?:b_disk_size\\|e\\(?:code_binary\\|lete_\\(?:property\\|verb\\)\\)\\|\\(?:isassembl\\|ump_databas\\)e\\)\\|e\\(?:ncode_binary\\|qual\\|val\\|xp\\)\\|f\\(?:l\\(?:o\\(?:\\(?:atst\\|o\\)r\\)\\|ush_input\\)\\|orce_input\\|unction_info\\)\\|i\\(?:dle_seconds\\|ndex\\|s_\\(?:clear_property\\|\\(?:memb\\|play\\)er\\)\\)\\|kill_task\\|l\\(?:ength\\|ist\\(?:append\\|delete\\|en\\(?:ers\\)?\\|\\(?:inser\\|se\\)t\\)\\|og\\(?:10\\)?\\)\\|m\\(?:a\\(?:tch\\|x\\(?:_object\\)?\\)\\|emory_usage\\|in\\|ove\\)\\|notify\\|o\\(?:bject_bytes\\|pen_network_connection\\|utput_delimiters\\)\\|p\\(?:a\\(?:rent\\|ss\\)\\|layers\\|ropert\\(?:ies\\|y_info\\)\\)\\|queue\\(?:_info\\|d_tasks\\)\\|r\\(?:a\\(?:ise\\|ndom\\)\\|e\\(?:ad\\|cycle\\|number\\|s\\(?:et_max_object\\|ume\\)\\)\\|index\\|match\\)\\|s\\(?:e\\(?:conds_left\\|rver_\\(?:log\\|version\\)\\|t\\(?:_\\(?:connection_option\\|p\\(?:layer_flag\\|roperty_info\\)\\|task_perms\\|verb_\\(?:args\\|code\\|info\\)\\)\\|add\\|remove\\)\\)\\|hutdown\\|inh?\\|qrt\\|tr\\(?:cmp\\|ing_hash\\|sub\\)\\|u\\(?:bstitute\\|spend\\)\\)\\|t\\(?:a\\(?:nh?\\|sk_\\(?:id\\|stack\\)\\)\\|i\\(?:cks_left\\|me\\)\\|o\\(?:float\\|int\\|literal\\|num\\|obj\\|str\\)\\|runc\\|ypeof\\)\\|unlisten\\|v\\(?:al\\(?:id\\|ue_\\(?:bytes\\|hash\\)\\)\\|erb\\(?:_\\(?:args\\|code\\|info\\)\\|s\\)\\)\\)\\)\\s-*("
     . (1 font-lock-builtin-face))
    ;; Objects on #1 such as $thing and $string_utils
    ("\\<$\\w+\\>"
     . font-lock-constant-face)
    ;; Don't format the contents of @notedit blocks as code
    ;; (In fact, overwrite any highlighting with the default font)
    (moocode-font-lock-notedit
     0 'default t)
    (moocode-font-lock-maybe-notedit
     0 'default t)
    ;; Warn of bare object number references
    ("#[0-9]+" . font-lock-warning-face)
  )
  "Highlighting for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst rmoo-code-reserved-words
  '(("if[ (]" "for[ (]" "while[ (]" "fork[ (]" "try"    "else" "except"
     "finally")
    ("endif"  "endfor"  "endwhile"  "endfork"  "endtry" "else" "except"
     "finally")))

(defun rmoo-code-indent-line ()
  (interactive)
  (let* ((pos (- (point-max) (point)))
	 (orig (point-marker))
	 (gotoindent (progn (back-to-indentation)
			    (>= (point) orig))))
    (if (not (looking-at "^\\.$"))
	(indent-to
	 (let ((offset 0))
	   (delete-horizontal-space)
	   (if (memq t (mapcar 'looking-at (nth 1 rmoo-code-reserved-words)))
	       (setq offset -2))
	   (save-excursion
	     (if (not (eq (forward-line -1) -1))
		 (progn
		   (while (and (looking-at "^\\s-*$")
			       (not (eq (forward-line -1) -1))))
		   (back-to-indentation)
		   (if (memq t (mapcar 'looking-at
				       (car rmoo-code-reserved-words)))
		       (setq offset (+ 2 offset)))
		   (+ (current-indentation) offset))
	       0)))))
    (if gotoindent
	(back-to-indentation)
      (goto-char orig))))

(setq indent-line-function 'rmoo-code-indent-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for missing semicolons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moocode-line-needs-semicolonp ()
  "Guess whether the current line should end with a semicolon."
  (not (looking-at "^\\(^\\s-*$\\|^\\s-*\\<@\\w+\\>\\|^.$\\|^\\s-*;\\|^\\w+$\\|\\s-*\\<\\(?:break\\|continue\\|e\\(?:lse\\(?:if\\)?\\|nd\\(?:fork?\\|if\\|while\\)\\)\\|fork?\\|if\\|while\\)\\>\\)")))

(defun moocode-line-ends-with-semicolonp ()
  "Check whether the current line ends with a semicolon."
  (looking-at ".+;\\s-*$"))

(defun moocode-skip-edit-forward ()
  "Skip most of the body of an @edit (assuming it contains one entry block)."
  (while (and (not (eobp))
              (not (looking-at "^\\.$")))
    (forward-line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moocode-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Allow some extra characters in words
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?$ "w" st)
    ;; Both /* ... */ and // style comments
    (modify-syntax-entry ?\/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n "> " st)
    st)
  "Syntax table for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moocode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map (kbd "C-c c") 'rmoo-upload-and-destroy)
    (define-key map (kbd "C-c s") 'rmoo-upload-buffer-directly)
    (define-key map (kbd "<f7>") 'rmoo-upload-buffer-directly)
    (define-key map (kbd "<f8>") 'rmoo-resize-editor-frame)
    (define-key map (kbd "C-c ]") 'rmoo-destroy)
    (define-key map (kbd "C-c C-c") 'rmoo-code-commentify)
    (define-key map (kbd "C-c C-u") 'rmoo-code-uncommentify)
    map)
  "Keymap for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful MOO editing utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rmoo-code-commentify (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (save-restriction
      (narrow-to-region start end)
      (rmoo-perform-replace "\\" "\\\\")
      (rmoo-perform-replace "\"" "\\\"")
      (while (re-search-forward "^.*$" nil t)
	(back-to-indentation)
;	(beginning-of-line)
	(insert "\"")
	(end-of-line)
	(insert "\";")))))

(defun rmoo-code-uncommentify (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "^.*$" nil t)
	(back-to-indentation)
	(delete-char 1)
	(end-of-line)
	(delete-char -2))
      (goto-char start)
      (rmoo-perform-replace "\\\\" "\1")
      (rmoo-perform-replace "\\\"" "\"")
      (rmoo-perform-replace "\1"   "\\"))))

(defmacro rmoo-perform-replace (from to)
  "Replace one string with another."
  (list 'save-excursion
	(list 'while (list 'search-forward from nil t)
	      (cond ((not (equal to ""))
		     (list 'replace-match to t t))
		    (t
		     (list 'delete-char
			   (if (stringp from)
			       (- (length from))
			     (list '- (list 'length from)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode moocode-mode prog-mode "MOO"
  "Major mode for editing LambdaMOO programming language files.
\\{moocode-mode-map}"
  :group 'moocode-mode
  (use-local-map moocode-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(moocode-font-lock-keywords))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'moocode-mode)
