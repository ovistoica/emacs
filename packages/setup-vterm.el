;; vterm is a performant terminal emulator, which is better suited for long
;; running processes like claude code

(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 500000))


(provide 'setup-vterm)
