;; JS utility to install/remove & run package.json scrips

(use-package js-pkg-mode
  :diminish js-pkg-mode
  :straight (:type git :local-repo "~/workspace/js-pkg-mode")
  :config (js-pkg-global-mode 1))
