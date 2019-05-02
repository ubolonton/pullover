(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'popup-edit)

(ert-deftest basic ()
  (message "%s" (popup-edit-mac-watch-clipboard 5000)))
