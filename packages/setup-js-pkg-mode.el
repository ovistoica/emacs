;;; setup-js-pkg-mode.el --- JS package.json script runner configuration -*- lexical-binding: t; -*-

(use-package js-pkg-mode
  :diminish js-pkg-mode
  :load-path "~/workspace/js-pkg-mode"
  :config
  (js-pkg-global-mode 1))

(provide 'setup-js-pkg-mode)
