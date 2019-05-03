(require 'popup-edit-sys)

(defvar popup-edit--buffer)

(defcustom popup-edit-major-mode 'gfm-mode)

(defun popup-edit--get-current-app ()
  (do-applescript "
tell application \"System Events\"
     name of first application process whose frontmost is true
end tell
"))

(defun popup-edit--copy-text (app)
  (message "copy-text %s" (benchmark-run (do-applescript (format "
tell application \"System Events\" to tell process %s
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Copy\"
     end tell
end tell
" app)))))

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

(defvar popup-edit--file)

(defun popup-edit-start ()
  (let ((app (popup-edit--get-current-app))
        (watcher (make-thread (lambda () (popup-edit-sys--watch-clipboard 2000)))))
    (popup-edit--copy-text app)
    (let* ((change-count (thread-join watcher))
           (buffer (generate-new-buffer "popup-edit.md")))
      (do-applescript "activate application \"emacs\"")
      (switch-to-buffer buffer)
      (clipboard-yank))))

(defun popup-edit-save ())

(defun popup-edit-cancel ())

(defun popup-edit-start-1 ()
  (let ((app (popup-edit--get-current-app))
        (watcher (make-thread (lambda () (popup-edit-sys--watch-clipboard 2000)))))
    (popup-edit--copy-text app)
    (let* ((change-count (thread-join watcher))
           (file (make-temp-file "popup-edit." nil ".md")))
      (message "popup-edit-start %s" file)
      (with-temp-file file
        (clipboard-yank))
      ;; XXX
      (setq popup-edit--file file)
      (make-thread
       (lambda ()
         (message "Calling emacsclient on %s" popup-edit--file)
         (let ((e (shell-command (format "emacsclient -a '' %s" popup-edit--file))))
           (message "----")
           (print e)
           (when (zerop e)
             (shell-command (format "pbcopy < %s" popup-edit--file))
             (popup-edit--paste-text app))))))))

(provide 'popup-edit)
