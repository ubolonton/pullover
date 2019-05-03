(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'popup-edit-sys)

(ert-deftest basic ()
  (message "%s" (popup-edit-sys--watch-clipboard 5000)))
