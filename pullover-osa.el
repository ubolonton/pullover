(defun pullover-osa--unquote (osa-string)
  (replace-regexp-in-string (regexp-quote "\"") "" osa-string))

(defun pullover-osa--get-current-app ()
  (pullover-osa--unquote
   (do-applescript "
tell application \"System Events\"
     bundle identifier of first application process whose frontmost is true
end tell
")))

(defun pullover-osa--copy-text (app)
  ;; TODO: Compare PIDs instead of bundle identifiers.
  (let ((app (if app
                 (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
    set i to bundle identifier
    if i is \"org.gnu.Emacs\" then return null
    tell menu 1 of menu bar item \"Edit\" of menu bar 1
        click menu item \"Select All\"
        click menu item \"Copy\"
    end tell
    i
end tell
" app))
               (do-applescript "
tell application \"System Events\" to tell first process whose frontmost is true
    set i to bundle identifier
    if i is \"org.gnu.Emacs\" then return null
    tell menu 1 of menu bar item \"Edit\" of menu bar 1
        click menu item \"Select All\"
        click menu item \"Copy\"
    end tell
    i
end tell
"))))
    (if (equal app "null")
        ;; XXX: Unquote more reliably.
        nil (pullover-osa--unquote app))))

;;; Can be faster, but less reliable (delay must be big enough).
(defun pullover-osa--copy-text-using-keys (app)
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
     keystroke \"a\" using command down
     keystroke \"c\" using command down
end tell
delay 0.3
" app)))

(defun pullover-osa--paste-text (app)
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
     set frontmost to true
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Paste\"
     end tell
end tell
" app)))

(defun pullover-osa--activate-app (app)
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
    set frontmost to true
end tell" app)))

(provide 'pullover-osa)
