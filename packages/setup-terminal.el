;;; setup-terminal.el --- Terminal emulator configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; For most usecases ghostel should be chosen as it is more performant

;;; Code:

(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name (or (getenv "SHELL") "/bin/bash")
        vterm-max-scrollback 500000)
  (unbind-key "M-`" vterm-mode-map)
  (unbind-key "M-s" vterm-mode-map)
  (unbind-key "C-M-b" vterm-mode-map)
  (unbind-key "C-M-f" vterm-mode-map)
  (unbind-key "M-w" vterm-mode-map)
  (unbind-key "C-M-@" vterm-mode-map)
  (unbind-key "M-&" vterm-mode-map))

(use-package ghostel
  :ensure t
  :config
  (unbind-key "S-<up>"    ghostel-mode-map)
  (unbind-key "S-<down>"  ghostel-mode-map)
  (unbind-key "S-<left>"  ghostel-mode-map)
  (unbind-key "S-<right>" ghostel-mode-map))


(provide 'setup-terminal)
;;; setup-terminal.el ends here
