(require 'pullover-osa)
(require 'pullover-dyn)

;;; TODO: Clear clipboard when done.

;;; TODO: Consider allowing multiple pullover sessions at the same time.
(defvar pullover--buffer nil)

(defvar pullover--debug t)

(defvar-local pullover--app nil)
(put 'pullover--app 'permanent-local t)

(defcustom pullover-clipboard-timeout 100
  "Number of milliseconds to wait for the external app to copy
text into the clipboard.")

(defcustom pullover-major-mode 'text-mode
  "Major mode to use for pullover sessions.")

(defcustom pullover-get-current-app-function #'pullover-dyn--get-current-app
  "Function used to get currently active app.")

(defcustom pullover-copy-text-function #'pullover-dyn--copy-text
  "Function used to copy text from the specified app into the clipboard.")

(defcustom pullover-paste-text-function #'pullover-dyn--paste-text
  "Function used to paste text from the clipboard into the specified app.")

(defcustom pullover-activate-app-function #'pullover-dyn--activate-app
  "Function used to activate the specified app.")

(defmacro pullover--bench (text &rest body)
  (declare (indent 1))
  (if pullover--debug
      `(let ((pullover--result))
         (message "%s %s" ,text
                  (benchmark-run (setq pullover--result ,@body)))
         pullover--result)
    `(progn ,@body)))

(defun pullover--get-current-app ()
  (funcall pullover-get-current-app-function))

(defun pullover--copy-text (app)
  (pullover--bench "copy-text "
    (funcall pullover-copy-text-function app)))

(defun pullover--paste-text (app)
  (funcall pullover-paste-text-function app))

(defun pullover--activate-app (app)
  (funcall pullover-activate-app-function app))

(defun pullover--activate-emacs ()
  (pullover--activate-app "org.gnu.Emacs"))

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
  `(let* ((start (pullover-dyn--change-count))
          (result (progn ,@body)))
     (cons result (pullover-dyn--wait-for-clipboard ,timeout start))))

(defun pullover-start ()
  (pcase-let
      ((`(,app . ,change-count)
        ;; TODO: Avoid blocking the main thread like this. One way to do it is making a
        ;; background thread that signals the main thread upon completion, with `thread-yield'.
        ;; However, that currently results in Emacs being aborted (gc_in_progress ||
        ;; waiting_for_input)'.
        (pullover-with-clipboard-wait pullover-clipboard-timeout
          ;; TODO: Terminate the clipboard wait if `app' is `nil'.
          (pullover--copy-text nil))))
    (if (null app)
        (progn
          (message "Invoked while inside Emacs. Trying to finish a pullover session ...")
          (pullover-finish))
      ;; TODO: If there's already another on-going, ask user what to do.
      (when (buffer-live-p pullover--buffer)
        (kill-buffer pullover--buffer))
      (setq pullover--buffer (generate-new-buffer app))
      (pullover--activate-emacs)
      (switch-to-buffer pullover--buffer)
      ;; XXX: Hmm
      (when (fboundp pullover-major-mode)
        (funcall pullover-major-mode))
      (setq pullover--app app)
      (pullover-mode +1)
      (when change-count                  ; External app didn't copy, or is taking too long.
        (clipboard-yank)))))

(defun pullover-finish ()
  (interactive)
  (unless (buffer-live-p pullover--buffer)
    (error "No pullover session"))
  (with-current-buffer pullover--buffer
    (clipboard-kill-ring-save (point-min) (point-max))
    (unwind-protect
        (pullover--bench "paste-text"
          (pullover--paste-text pullover--app))
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
