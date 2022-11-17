;;; coldc-mode.el by Jeremy Weatherford, no rights reserved
;; basically stolen from php-mode.el by Turadg Aleahmad
;; with very minor mods

(defgroup coldc nil
  "Major mode for editing ColdC code."
  :prefix "coldc-"
  :group 'languages)

(defcustom coldc-file-patterns (list "\\.coldc\\'" "textdump\\'" "\\.cdc\\'")
  "*List of file patterns for which to automatically invoke coldc-mode."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'coldc)

(defcustom coldc-mode-user-hook nil
  "List of functions to be executed on entry to coldc-mode"
  :type 'hook
  :group 'coldc)

;; Note whether we're in XEmacs
(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version)
  "Non nil if using XEmacs.")

;;;###autoload
(define-derived-mode coldc-mode c-mode "ColdC"
  "Major mode for editing ColdC code.\n\n{coldc-mode-map}"

  (setq comment-start "// "
	comment-end   "")
;	comment-start-skip "// *")

  (defvar coldc-mode-syntax-table coldc-mode-syntax-table)
  ;; single-quote considered part of word
  (modify-syntax-entry ?' "w" coldc-mode-syntax-table)
  (modify-syntax-entry ?_ "w" coldc-mode-syntax-table)

  ;; dollar-sign considered punctuation, not part of word
  ;(modify-syntax-entry ?$ "." coldc-mode-syntax-table)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
 	'((coldc-font-lock-keywords-1
 	   coldc-font-lock-keywords-2
 	   ;; Comment-out the next line if the font-coloring is too
 	   ;; extreme/ugly for you.
 	   coldc-font-lock-keywords-3
 	   )
 	  nil				; KEYWORDS-ONLY
 	  T				; CASE-FOLD
 	  nil				; SYNTAX-ALIST
 	  nil				; SYNTAX-BEGIN
 	  (font-lock-syntactic-keywords . coldc-font-lock-syntactic-keywords)))

  (setq font-lock-maximum-decoration t
	case-fold-search t)		; ColdC vars are case-sensitive
;	imenu-generic-expression cc-imenu-coldc-generic-expression)

  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the ColdC file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)

  ;; personal preference
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'c-basic-offset) 4)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (run-hooks 'coldc-mode-user-hook)
  )

;; Make coldc-mode the default mode for ColdC source code buffers.
;;;###autoload
(let ((coldc-file-patterns-temp coldc-file-patterns))
  (while coldc-file-patterns-temp
    (setq auto-mode-alist
	  (cons
	   (cons (car coldc-file-patterns-temp) 'coldc-mode)
	   auto-mode-alist)
	  )
    (setq coldc-file-patterns-temp (cdr coldc-file-patterns-temp))))

(defconst coldc-constants
  (eval-when-compile
    (regexp-opt
     '(;; core constants

	   ; umm... none in ColdC?

       ) t))
  "ColdC constants.")

(defconst coldc-keywords
  (eval-when-compile
    (regexp-opt
     '(

	   ; straight from token.c
	   "any"
	   "arg"
	   "break"
	   "case"
	   "catch"
	   "continue"
	   "default"
	   "disallow_overrides"
	   "else"
	   "filter"
	   "find"
	   "for"
	   "fork"
	   "handler"
	   "hash"
	   "if"
	   "in"
	   "map"
	   "pass"
	   "return"
	   "switch"
	   "to"
	   "var"
	   "where"
	   "while"
	   "with"

	   ; hackish, but eh
	   "public method"
	   "protected method"
	   "private method"
	   "root method"
	   "driver method"
	   "frob method"

	   ) t))

  "ColdC keywords.")

(defconst coldc-identifier
  (eval-when-compile
    '"[a-zA-Z\_][a-zA-Z0-9\_]*")
  "Characters in a ColdC identifier.")

; not sure what we'll do with these
(defconst coldc-types
  (eval-when-compile
    (regexp-opt '("integer" "float" "string" "objnum" "list" "symbol"
				  "error" "frob" "dictionary" "buffer")
				t))
  "ColdC types.")

;; Set up font locking
(defconst coldc-font-lock-keywords-1
  (list
   ;; symbols
   (cons
	"'[a-zA-Z0-9\_]+"
	'font-lock-type-face)

   ;; errors
   (cons
	"~[a-zA-Z0-9\_]+"
	'font-lock-warning-face)

   ;; object refs
   (cons
	"\\$[a-zA-Z][a-zA-Z0-9\_]*"
	'font-lock-constant-face)

   ;; Fontify constants
   (cons
    (concat "\\<\\(" coldc-constants "\\)\\>")
    'font-lock-constant-face)

   ;; Fontify keywords
   (cons
    (concat "\\<\\(" coldc-keywords "\\)\\>")
    'font-lock-keyword-face)

   ;; Fontify keywords and targets, and case default/goto tags.
   (list "\\<\\(break\\|case\\|continue\\)\\>[ \t]*\\(-?\\sw+\\)?"
	 '(1 font-lock-keyword-face) '(2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	  (beginning-of-line) (end-of-line)
	  (1 font-lock-constant-face))))

  "Subdued level highlighting for ColdC mode.")

(defconst coldc-font-lock-keywords-2
  (append
   coldc-font-lock-keywords-1
   (list
    ))
	; could be expanded

  "Medium level highlighting for ColdC mode.")

(defconst coldc-font-lock-keywords-3
  (append
   coldc-font-lock-keywords-2
   (list
	))
    ; could also use some expansion

  "Gauchy level highlighting for ColdC mode.")

; todo?
(defconst coldc-font-lock-syntactic-keywords nil)

;; Create "default" symbol for GNU Emacs so that both Xemacs and GNU
;; emacs can refer to the default face by a variable named "default".
(unless (boundp 'default)
  (defvar default 'default))

;; Create faces for XEmacs
(unless (boundp 'font-lock-keyword-face)
  (copy-face 'bold 'font-lock-keyword-face))
(unless (boundp 'font-lock-constant-face)
  (copy-face 'font-lock-keyword-face 'font-lock-constant-face))

(provide 'coldc-mode)

;;; coldc-mode.el ends here
