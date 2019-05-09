;;; pullover.el --- Pulling other apps over to do things -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Keywords:
;; Homepage: https://github.com/ubolonton/pullover
;; Package-Requires: ((emacs "25.1") (frame))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package allows extracting text from other apps, editing it in Emacs, then sending it back.
;; It is useful when you have to work with apps (or web pages) that have poor editing support, like
;; bad key bindings, small edit areas, or little syntax highlighting.

;;; Code:

(eval-and-compile
  (unless (equal system-type 'darwin)
    (error "Package pullover currently only works on macOS"))
  (unless (member window-system '(mac ns))
    (error "Package pullover requires a GUI")))

(require 'pullover-osa)
(require 'pullover-dyn)

;;; TODO: Clear clipboard when done.

;;; TODO: Consider allowing multiple pullover sessions at the same time.
(defvar pullover--buffer nil)

(defvar pullover--debug t)

(defvar-local pullover--app nil)
(put 'pullover--app 'permanent-local t)

(defcustom pullover-clipboard-timeout 50
  "Number of milliseconds to wait for the external app to copy the text.")

(defcustom pullover-major-mode 'text-mode
  "Major mode to use for pullover sessions.")

(defcustom pullover-copy-text-function #'pullover-dyn--copy-text
  "Function used to copy text from the specified app into the clipboard.

Possible values are:
- `pullover-dyn--copy-text': Shows a notification message if it takes more than
  1 second to copy text.
- `pullover-osa--copy-text': Is a little bit faster, but doesn't show any
  notification message.")

(defcustom pullover-paste-text-function #'pullover-dyn--paste-text
  "Function used to paste text from the clipboard into the specified app.")

(defcustom pullover-activate-app-function #'pullover-dyn--activate-app
  "Function used to activate the specified app.")

(defmacro pullover--bench (text &rest body)
  "Eval BODY, printing the run time, prefixed with TEXT."
  (declare (indent 1))
  (if pullover--debug
      `(let ((pullover--result))
         (message "%s %s" ,text
                  (benchmark-run (setq pullover--result ,@body)))
         pullover--result)
    `(progn ,@body)))

;;;###autoload
(defun pullover-activate-emacs ()
  "Switch to Emacs, making in the frontmost app."
  (funcall pullover-activate-app-function "org.gnu.Emacs"))

(defmacro pullover-with-clipboard-wait (timeout &rest body)
  "Wait for a new item to appear in the clipboard, while evaluating BODY.

This is typically used to check whether the evaluation of BODY copied a new item
to the clipboard, directly or asynchronously.

Return (RESULT . CHANGE-COUNT), where CHANGE-COUNT represents the latest state
of the clipboard. If there's no new item in the clipboard after TIMEOUT
milliseconds, return (RESULT . nil). The time taken to evaluate BODY is ignored.

This will block the current thread. Therefore it's recommended to use a small
value for TIMEOUT."
  (declare (indent 1))
  `(let* ((start (pullover-dyn--change-count))
          (result (progn ,@body)))
     (cons result (pullover-dyn--wait-for-clipboard ,timeout start))))

;;;###autoload
(defun pullover-start ()
  "Start an editing session by opening a new buffer.
If Emacs is the current app, this function calls `pullover-finish'."
  (pcase-let
      ((`(,app . ,change-count)
        ;; TODO: Avoid blocking the main thread like this. One way to do it is making a background
        ;; thread that signals the main thread upon completion, with `thread-yield'. However, that
        ;; currently results in Emacs being aborted (gc_in_progress || waiting_for_input)'.
        (pullover-with-clipboard-wait pullover-clipboard-timeout
          ;; TODO: Terminate the clipboard wait if `app' is nil.
          (pullover--bench "copy-text "
            (funcall pullover-copy-text-function nil)))))
    (if (null app)
        (progn
          (message "Invoked while inside Emacs. Trying to finish a pullover session ...")
          (pullover-finish))
      ;; TODO: If there's already another on-going, ask user what to do.
      (when (buffer-live-p pullover--buffer)
        (kill-buffer pullover--buffer))
      (setq pullover--buffer (generate-new-buffer app))
      (pullover-activate-emacs)
      (switch-to-buffer pullover--buffer)
      (setq pullover--app app)
      ;; Paste only if the item in the clipboard is new (from the other app).
      (when change-count
        (clipboard-yank))
      ;; TODO: Use an app-to-major-mode mapping.
      (when (fboundp pullover-major-mode)
        (funcall pullover-major-mode))
      (pullover-mode +1))))

(defun pullover-finish ()
  "Finish an editing session, sending the edited text back to app it originated from."
  (interactive)
  (unless (buffer-live-p pullover--buffer)
    (error "No pullover session"))
  (with-current-buffer pullover--buffer
    (clipboard-kill-ring-save (point-min) (point-max))
    (unwind-protect
        (pullover--bench "paste-text"
          (funcall pullover-paste-text-function pullover--app))
      (kill-buffer)
      (setq pullover--buffer nil))))

(defun pullover-cancel ()
  "Cancel an editing session, switching back to the original app.
The text being edited is discarded."
  (interactive)
  (unless (buffer-live-p pullover--buffer)
    (error "No pullover session"))
  (with-current-buffer pullover--buffer
    (unwind-protect
        (funcall pullover-activate-app-function pullover--app)
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
  "Minor mode for editing text grabbed from another app, then sending it back ."
  :init-value nil
  :lighter "pullover"
  :keymap pullover-mode-map
  ())

(provide 'pullover)
;;; pullover.el ends here
