(require 'pullover-sys)

;;; TODO: Consider allowing multiple pullover sessions at the same time.
(defvar pullover--buffer nil)

(defvar-local pullover--app nil)
(put 'pullover--app 'permanent-local t)

(defcustom pullover-clipboard-timeout 100
  "Number of milliseconds to wait for the external app to copy
text into the clipboard.")

(defcustom pullover-clear-clipboard-when-done nil
  "")

(defcustom pullover-major-mode 'text-mode
  "Major mode to use for pullover sessions.")

(defun pullover--get-current-app ()
  (do-applescript "
tell application \"System Events\"
     name of first application process whose frontmost is true
end tell
"))

(defun pullover--copy-text (app)
  (do-applescript (format "
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Copy\"
     end tell
end tell
" app)))

(defun pullover--paste-text (app)
  (do-applescript (format "
activate application %s
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Paste\"
     end tell
end tell
" app app)))

(defun pullover--activate-app (app)
  (do-applescript (format "activate application %s" app)))

(defun pullover--activate-emacs ()
  (pullover--activate-app "\"Emacs\""))

(defmacro pullover-with-clipboard-wait (timeout &rest body)
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
  `(let* ((start (pullover-sys--change-count))
          (result (progn ,@body)))
     (cons result (pullover-sys--wait-for-clipboard ,timeout start))))

(defun pullover-start ()
  (let ((app (pullover--get-current-app)))
    (if (string-match-p (regexp-quote "emacs") (downcase app))
        (progn
          (message "Trying to finish on-going pullover session")
          (pullover-finish))
      (pcase-let
          ((`(,_ . ,change-count)
            ;; TODO: Avoid blocking the main thread like this. One way to do it is making a
            ;; background thread that signals the main thread upon completion, with `thread-yield'.
            ;; However, that currently results in Emacs being aborted (gc_in_progress ||
            ;; waiting_for_input)'.
            (pullover-with-clipboard-wait pullover-clipboard-timeout
              (message "copy-text %s" (benchmark-run (pullover--copy-text app))))))
        ;; TODO: If there's already another on-going, ask user what to do.
        (setq pullover--buffer (generate-new-buffer app))
        (pullover--activate-emacs)
        (switch-to-buffer pullover--buffer)
        ;; XXX: Hmm
        (when (fboundp pullover-major-mode)
          (funcall pullover-major-mode))
        (setq pullover--app app)
        (pullover-mode +1)
        (when change-count              ; External app didn't copy, or is taking too long.
          (clipboard-yank))))))

(defun pullover-finish ()
  (interactive)
  (unless (buffer-live-p pullover--buffer)
    (error "No pullover session"))
  (with-current-buffer pullover--buffer
    (clipboard-kill-ring-save (point-min) (point-max))
    (unwind-protect
        (pullover--paste-text pullover--app)
      (kill-buffer)
      (setq pullover--buffer nil))))

(defun pullover-cancel ()
  (interactive)
  (unless (buffer-live-p pullover--buffer)
    (error "No pullover session"))
  (with-current-buffer pullover--buffer
    (unwind-protect
        (pullover--activate-app pullover--app)
      (kill-buffer)
      (setq pullover--buffer nil))))

;;; TODO: I'm not sure these bindings make sense.
(defvar pullover-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap server-edit] 'pullover-cancel)
    (define-key map [remap save-buffer] 'pullover-finish)
    map)
  "Keymap of `pullover-mode'.")

(define-minor-mode pullover-mode
  "Minor mode for editing text grabbed from another app, then sending it back to
it."
  :init-value nil
  :lighter "pullover"
  :keymap pullover-mode-map
  ())

(provide 'pullover)
