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
  ;; S-<arrows> (windmove) and M-o (other-window) are NOT in
  ;; `ghostel-mode-map' — `ghostel--define-terminal-keys' binds nearly
  ;; every modified key to `ghostel--send-event' in the semi-char/char
  ;; input maps, forwarding them to the terminal.  So `unbind-key' on
  ;; `ghostel-mode-map' is a no-op.  The supported way to let keys pass
  ;; through to Emacs is `ghostel-keymap-exceptions'; its setter rebuilds
  ;; the semi-char keymap.  Append our window-management keys to the
  ;; package defaults.
  (setopt ghostel-keymap-exceptions
          (append ghostel-keymap-exceptions
                  '("M-o" "S-<up>" "S-<down>" "S-<left>" "S-<right>"))))


(provide 'setup-terminal)
;;; setup-terminal.el ends here
