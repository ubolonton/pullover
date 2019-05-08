(defun pullover-osa--get-current-app ()
  (do-applescript "
tell application \"System Events\"
     name of first application process whose frontmost is true
end tell
"))

(defun pullover-osa--copy-text (app)
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Copy\"
     end tell
end tell
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
" app app)))

(defun pullover-osa--activate-app (app)
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
    set frontmost to true
end tell" app)))

(provide 'pullover-osa)
