(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path packages-dir)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq package-native-compile t)

;; Set CC compiler path only on macOS
(when (eq system-type 'darwin)
  (setenv "CC" "/opt/homebrew/bin/gcc-15"))

(provide 'packages)
