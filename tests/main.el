(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'pullover-dyn)

(ert-deftest basic ()
  (message "%s" (pullover-dyn--wait-for-clipboard 500 (pullover-dyn--change-count))))

(ert-deftest get-current-app ()
  (message ":current-app  %s" (benchmark-run 10 (pullover-dyn--get-current-app))))

(ert-deftest copy-text ()
  (message ":copy-text    SE %s" (benchmark-run (pullover-dyn--copy-text "com.apple.ScriptEditor2"))))

(ert-deftest activate-app-and-copy ()
  (message ":activate-app Notes  %s"
           (benchmark-run (pullover-dyn--activate-app "com.apple.Notes")))
  (message ":copy-text    curr   %s"
           (benchmark-run (pullover-dyn--copy-text nil)))
  (message ":activate-app iTerm2 %s"
           (benchmark-run (pullover-dyn--activate-app "com.googlecode.iterm2"))))
