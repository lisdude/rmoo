;; emacspeak-rmoo.el: Speech-enabling extensions to RMOO
;; Copyright 1999-2000 by Matthew Campbell
;; Some code adapted from an extension written by Amit Patel
;; <amitp@cs.stanford.edu>.  However, please contact Matt regarding
;; any questions or problems you have with this software.
;; This is free software, covered by the GNU General Public License.
;;
;; Original Author: Matthew Campbell <mattcampbell@pobox.com>
;;

(require 'rmoo)
(condition-case () (progn
		     (require 'emacspeak-speak)
		     (require 'emacspeak-fix-interactive))
  (error nil))
(require 'advice)
(provide 'emacspeak-rmoo)

(emacspeak-define-sound 'moo-login "cow.wav")
(emacspeak-define-sound 'moo-activity "cow5.au")

(defvar emacspeak-rmoo-already-did-activity-notify nil)
(make-variable-buffer-local 'emacspeak-rmoo-already-did-activity-notify)
(defvar emacspeak-rmoo-unheard-output-start nil)
(make-variable-buffer-local 'emacspeak-rmoo-unheard-output-start)
(defvar emacspeak-rmoo-unheard-output-end nil)
(make-variable-buffer-local 'emacspeak-rmoo-unheard-output-end)

(defun emacspeak-rmoo-handle-text ()
"This function is intended to be added to the rmoo-handle-text-hooks
variable in order to make RMOO speak incoming text and notify the user
of activity in RMOO buffers other than the current one."
  (declare (special emacspeak-comint-autospeak))
  (let ((dtk-stop-immediately nil))
    (when (and
     (eq (window-buffer) (current-buffer))
     emacspeak-comint-autospeak)
      (emacspeak-speak-region (point-min) (point-max)))
      (unless (or
     (eq (window-buffer) (current-buffer))
     emacspeak-rmoo-already-did-activity-notify)
      (setq emacspeak-rmoo-already-did-activity-notify t)
      (emacspeak-auditory-icon 'moo-activity)
      (message (concat "Activity in " (buffer-name))))
    (unless (eq (window-buffer) (current-buffer))
      (when (eq emacspeak-rmoo-unheard-output-start nil)
        (setq emacspeak-rmoo-unheard-output-start (point-min)))
      (setq emacspeak-rmoo-unheard-output-end (point-max)))))

(add-hook 'rmoo-handle-text-hooks 'emacspeak-rmoo-handle-text)

(defadvice emacspeak-speak-mode-line (after emacspeak-rmoo last pre act)
  (when (eq major-mode 'rmoo-interactive-mode)
    (emacspeak-rmoo-catchup)))

(defun emacspeak-rmoo-catchup ()
"This function is intended to be called by the advice form for
emacspeak-speak-mode-line in the emacspeak-rmoo module.  It speaks any activity in the
current buffer that the user may have missed while he was off in
another buffer, and updates the variables used internally by
the emacspeak-rmoo module."
  (declare (special emacspeak-comint-autospeak))
  (when (and
   (not (eq emacspeak-rmoo-unheard-output-start nil))
   (not (eq emacspeak-rmoo-unheard-output-end nil)))
    (when emacspeak-comint-autospeak
      (let ((dtk-stop-immediately nil))
        (emacspeak-speak-region emacspeak-rmoo-unheard-output-start emacspeak-rmoo-unheard-output-end)))
    (setq emacspeak-rmoo-unheard-output-start nil)
    (setq emacspeak-rmoo-unheard-output-end nil))
  (setq emacspeak-rmoo-already-did-activity-notify nil))

(emacspeak-fix-interactive-command-if-necessary 'rmoo-worlds-add-new-moo)

;; The following code was contribued by T. V. Raman <raman@adobe.com>
(defadvice rmoo-send (around emacspeak pre act comp)
  "Speak what is displayed"
  (cond 
   ((interactive-p)
    (let ((orig (point)))
      (set-mark (point))
      ad-do-it
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice rmoo-interactive-mode (after emacspeak-rmoo pre act)
  "Play an appropriate auditory icon and sync Emacspeak and the synthesizer."
  (emacspeak-auditory-icon 'moo-login)
  (emacspeak-dtk-sync))
