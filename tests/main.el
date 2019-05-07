(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'pullover-sys)

;; (ert-deftest basic ()
;;   (message "%s" (pullover-sys--wait-for-clipboard 5000 (pullover-sys--change-count))))

(ert-deftest frontmost ()
  (dotimes (_ 2)
    (message "frontmost -> %s" (pullover-sys--frontmost-bundle-id))))

(ert-deftest copy-text ()
  (message "copied -> %s" (pullover-sys--copy-text "com.jetbrains.intellij.ce")))
