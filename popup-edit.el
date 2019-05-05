(require 'popup-edit-sys)

;;; TODO: Consider allowing multiple popup-edit sessions at the same time.
(defvar popup-edit--buffer nil)

(defvar-local popup-edit--app nil)

(defcustom popup-edit-clipboard-timeout 100
  "Number of milliseconds to wait for the external app to copy
text into the clipboard.")

(defcustom popup-edit-clear-clipboard-when-done nil
  "")

(defcustom popup-edit-major-mode 'text-mode
  "Major mode to use for popup-edit sessions.")

(defun popup-edit--get-current-app ()
  (do-applescript "
tell application \"System Events\"
     name of first application process whose frontmost is true
end tell
"))

(defun popup-edit--copy-text (app)
  (do-applescript (format "
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Copy\"
     end tell
end tell
" app)))

(defun popup-edit--paste-text (app)
  (do-applescript (format "
activate application %s
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Paste\"
     end tell
end tell
" app app)))

(defun popup-edit--activate-app (app)
  (do-applescript (format "activate application %s" app)))

(defun popup-edit--activate-emacs ()
  (popup-edit--activate-app "\"Emacs\""))

(defmacro popup-edit-with-clipboard-wait (timeout &rest body)
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
  `(let* ((start (popup-edit-sys--change-count))
          (result (progn ,@body)))
     (cons result (popup-edit-sys--wait-for-clipboard ,timeout start))))

(defun popup-edit-start ()
  (let ((app (popup-edit--get-current-app)))
    (if (string-match-p (regexp-quote "emacs") (downcase app))
        (progn
          (message "Trying to finish on-going popup-edit session")
          (popup-edit-finish))
      (pcase-let
          ((`(,_ . ,change-count)
            ;; TODO: Avoid blocking the main thread like this. One way to do it is making a
            ;; background thread that signals the main thread upon completion, with `thread-yield'.
            ;; However, that currently results in Emacs being aborted (gc_in_progress ||
            ;; waiting_for_input)'.
            (popup-edit-with-clipboard-wait popup-edit-clipboard-timeout
              (message "copy-text %s" (benchmark-run (popup-edit--copy-text app))))))
        ;; TODO: If there's already another on-going, ask user what to do.
        (setq popup-edit--buffer (generate-new-buffer app))
        (popup-edit--activate-emacs)
        (switch-to-buffer popup-edit--buffer)
        ;; XXX: Hmm
        (when (fboundp popup-edit-major-mode)
          (funcall popup-edit-major-mode))
        (setq popup-edit--app app)
        (popup-edit-mode +1)
        (when change-count              ; External app didn't copy, or is taking too long.
          (clipboard-yank))))))

(defun popup-edit-finish ()
  (interactive)
  (unless (buffer-live-p popup-edit--buffer)
    (error "No popup-edit session"))
  (with-current-buffer popup-edit--buffer
    (clipboard-kill-ring-save (point-min) (point-max))
    (unwind-protect
        (popup-edit--paste-text popup-edit--app)
      (kill-buffer)
      (setq popup-edit--buffer nil))))

(defun popup-edit-cancel ()
  (interactive)
  (unless (buffer-live-p popup-edit--buffer)
    (error "No popup-edit session"))
  (with-current-buffer popup-edit--buffer
    (unwind-protect
        (popup-edit--activate-app popup-edit--app)
      (kill-buffer)
      (setq popup-edit--buffer nil))))

;;; TODO: I'm not sure these bindings make sense.
(defvar popup-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap server-edit] 'popup-edit-cancel)
    (define-key map [remap save-buffer] 'popup-edit-finish)
    map)
  "Keymap of `popup-edit-mode'.")

(define-minor-mode popup-edit-mode
  "Minor mode for editing text grabbed another app, then sending it back to it."
  :init-value nil
  :lighter "popup-edit"
  :keymap popup-edit-mode-map
  ())

(provide 'popup-edit)
