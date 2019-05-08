(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'pullover-dyn)

(ert-deftest basic ()
  (message "%s" (pullover-dyn--wait-for-clipboard 500 (pullover-dyn--change-count))))

(ert-deftest get-current-app ()
  (message "benchmark:current-app  %s" (benchmark-run 10 (pullover-dyn--get-current-app))))

(ert-deftest copy-text ()
  (message "benchmark:copy-text    %s" (benchmark-run (pullover-dyn--copy-text "com.jetbrains.intellij.ce"))))

(ert-deftest activate-app ()
  (message "benchmark:activate-app %s" (benchmark-run (pullover-dyn--activate-app "com.googlecode.iterm2"))))
