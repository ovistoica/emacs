;; JS utility to install/remove & run package.json scrips

(use-package js-pkg-mode
  :diminish js-pkg-mode
  :load-path "~/workspace/js-pkg-mode"
  :config
  (js-pkg-global-mode 1))

(provide 'setup-js-pkg-mode)
