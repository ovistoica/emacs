;; vterm is a performant terminal emulator, which is better suited for long
;; running processes like claude code

(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 500000)
  (unbind-key "M-`" vterm-mode-map)
  (unbind-key "M-s" vterm-mode-map)
  (unbind-key "C-M-b" vterm-mode-map)
  (unbind-key "C-M-f" vterm-mode-map)
  (unbind-key "M-w" vterm-mode-map)
  (unbind-key "C-M-@" vterm-mode-map)
  (unbind-key "M-&" vterm-mode-map))


(provide 'setup-vterm)
