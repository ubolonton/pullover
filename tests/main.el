(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'pullover-dyn)

(ert-deftest basic ()
  (message "%s" (pullover-dyn--wait-for-clipboard 5000 (pullover-dyn--change-count))))

(ert-deftest frontmost ()
  (message "benchmark:frontmost %s" (benchmark-run 10 (pullover-dyn--frontmost-bundle-id))))

(ert-deftest copy-text ()
  (message "benchmark:copy-text %s" (benchmark-run (pullover-dyn--copy-text "com.jetbrains.intellij.ce"))))
