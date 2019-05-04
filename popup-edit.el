(require 'popup-edit-sys)

;;; TODO: Consider allowing multiple popup-edit sessions at the same time.
(defvar popup-edit--buffer nil)

(defvar-local popup-edit--app nil)

(defcustom popup-edit-clipboard-timeout 100
  "Number of milliseconds to wait for the external app to copy
text into the clipboard.")

(defcustom popup-edit-clear-clipboard-when-done nil
  "")

;; (defcustom popup-edit-major-mode 'gfm-mode)

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

;;; TODO: If there's already another on-going, ask user what to do.
(defun popup-edit-start ()
  (let ((app (popup-edit--get-current-app)))
    (if (string-match-p (regexp-quote "emacs") (downcase app))
        (progn
          (message "Trying to finish on-going popup-edit session")
          (popup-edit-finish))
      ;; TODO: For Emacs 25: Add a separate API to get initial change count, then poll from that,
      ;; since there's no threading support.
      (let ((watcher (make-thread (lambda () (popup-edit-sys--wait-for-clipboard popup-edit-clipboard-timeout nil)))))
        (message "copy-text %s" (benchmark-run (popup-edit--copy-text app)))
        ;; TODO: Find a way not to block the main thread.
        (let* ((change-count (thread-join watcher)))
          (setq popup-edit--buffer (generate-new-buffer "popup-edit.md"))
          (popup-edit--activate-emacs)
          (switch-to-buffer popup-edit--buffer)
          (setq popup-edit--app app)
          ;; TODO: Skip this if the external app didn't paste. Some apps don't if the input is
          ;; empty.
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

(provide 'popup-edit)
