;;; pullover-osa.el --- Applescript-based functionalities -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This library implements functionalities that use Applescript.

;;; Code:

(require 'frame)

;; XXX: Unquote more reliably.
(defun pullover-osa--unquote (string)
  "Unquote a STRING returned by `do-applescript'."
  (replace-regexp-in-string (regexp-quote "\"") "" string))

(defun pullover-osa--get-current-app ()
  "Return the current (frontmost) app."
  (pullover-osa--unquote
   (do-applescript "
tell application \"System Events\"
     bundle identifier of first application process whose frontmost is true
end tell
")))

(defun pullover-osa--copy-text (bundle-id)
  "Copy text from the app identified by BUNDLE-ID.

The copied text is put into the clipboard.

If BUNDLE-ID is nil, copy from the current (frontmost) app instead.

Return the bundle ID of the affected app. If the app is Emacs itself, return nil
without trying to copy.

Unlike `pullover-dyn--copy-text', this does not display a notification message if
the app takes too long to copy the text."
  ;; TODO: Compare PIDs instead of bundle identifiers.
  (let ((bundle-id (if bundle-id
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
" bundle-id))
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
    (if (equal bundle-id "null")
        nil (pullover-osa--unquote bundle-id))))

(defun pullover-osa--paste-text (bundle-id)
  "Paste text from the clipboard into the app identified by BUNDLE-ID."
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
     set frontmost to true
     tell menu 1 of menu bar item \"Edit\" of menu bar 1
          click menu item \"Select All\"
          click menu item \"Paste\"
     end tell
end tell
" bundle-id)))

(defun pullover-osa--activate-app (bundle-id)
  "Switch to the app identified by BUNDLE-ID (making it frontmost)."
  (do-applescript (format "
tell application \"System Events\" to tell first process whose bundle identifier is \"%s\"
    set frontmost to true
end tell" bundle-id)))

(provide 'pullover-osa)
;;; pullover-osa.el ends here
