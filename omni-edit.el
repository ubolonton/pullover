(require 'omni-edit-sys)

;;; TODO: Consider allowing multiple omni-edit sessions at the same time.
(defvar omni-edit--buffer nil)

(defvar-local omni-edit--app nil)

(defcustom omni-edit-clipboard-timeout 100
  "Number of milliseconds to wait for the external app to copy
text into the clipboard.")

(defcustom omni-edit-clear-clipboard-when-done nil
  "")

(defcustom omni-edit-major-mode 'text-mode
  "Major mode to use for omni-edit sessions.")

(defun omni-edit--get-current-app ()
  (do-applescript "
tell application \"System Events\"
     name of first application process whose frontmost is true
end tell
"))

(defun omni-edit--copy-text (app)
  (do-applescript (format "
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Copy\"
     end tell
end tell
" app)))

(defun omni-edit--paste-text (app)
  (do-applescript (format "
activate application %s
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Paste\"
     end tell
end tell
" app app)))

(defun omni-edit--activate-app (app)
  (do-applescript (format "activate application %s" app)))

(defun omni-edit--activate-emacs ()
  (omni-edit--activate-app "\"Emacs\""))

(defmacro omni-edit-with-clipboard-wait (timeout &rest body)
  "Wait for a new item to appear in the clipboard, while evaluating BODY.

This is typically used to check whether the evaluation of BODY copied a new item
to the clipboard, directly or asynchronously.

Return (RESULT . CHANGE-COUNT), where CHANGE-COUNT represents the latest state
of the clipboard.

If there's no new item in the clipboard after TIMEOUT milliseconds,
return (RESULT . nil). The time taken to evaluate BODY is ignored.

This will block the current thread. Therefore it's recommended to use a small
value for TIMEOUT."
  (declare (indent 1))
  `(let* ((start (omni-edit-sys--change-count))
          (result (progn ,@body)))
     (cons result (omni-edit-sys--wait-for-clipboard ,timeout start))))

(defun omni-edit-start ()
  (let ((app (omni-edit--get-current-app)))
    (if (string-match-p (regexp-quote "emacs") (downcase app))
        (progn
          (message "Trying to finish on-going omni-edit session")
          (omni-edit-finish))
      (pcase-let
          ((`(,_ . ,change-count)
            ;; TODO: Avoid blocking the main thread like this. One way to do it is making a
            ;; background thread that signals the main thread upon completion, with `thread-yield'.
            ;; However, that currently results in Emacs being aborted (gc_in_progress ||
            ;; waiting_for_input)'.
            (omni-edit-with-clipboard-wait omni-edit-clipboard-timeout
              (message "copy-text %s" (benchmark-run (omni-edit--copy-text app))))))
        ;; TODO: If there's already another on-going, ask user what to do.
        (setq omni-edit--buffer (generate-new-buffer app))
        (omni-edit--activate-emacs)
        (switch-to-buffer omni-edit--buffer)
        ;; XXX: Hmm
        (when (fboundp omni-edit-major-mode)
          (funcall omni-edit-major-mode))
        (setq omni-edit--app app)
        (omni-edit-mode +1)
        (when change-count              ; External app didn't copy, or is taking too long.
          (clipboard-yank))))))

(defun omni-edit-finish ()
  (interactive)
  (unless (buffer-live-p omni-edit--buffer)
    (error "No omni-edit session"))
  (with-current-buffer omni-edit--buffer
    (clipboard-kill-ring-save (point-min) (point-max))
    (unwind-protect
        (omni-edit--paste-text omni-edit--app)
      (kill-buffer)
      (setq omni-edit--buffer nil))))

(defun omni-edit-cancel ()
  (interactive)
  (unless (buffer-live-p omni-edit--buffer)
    (error "No omni-edit session"))
  (with-current-buffer omni-edit--buffer
    (unwind-protect
        (omni-edit--activate-app omni-edit--app)
      (kill-buffer)
      (setq omni-edit--buffer nil))))

;;; TODO: I'm not sure these bindings make sense.
(defvar omni-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap server-edit] 'omni-edit-cancel)
    (define-key map [remap save-buffer] 'omni-edit-finish)
    map)
  "Keymap of `omni-edit-mode'.")

(define-minor-mode omni-edit-mode
  "Minor mode for editing text grabbed from another app, then sending it back to
it."
  :init-value nil
  :lighter "omni-edit"
  :keymap omni-edit-mode-map
  ())

(provide 'omni-edit)
