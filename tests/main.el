(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'omni-edit-sys)

(ert-deftest basic ()
  (message "%s" (omni-edit-sys--wait-for-clipboard 5000 (omni-edit-sys--change-count))))
