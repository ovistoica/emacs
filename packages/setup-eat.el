;; setup-eat.el

;; Setup terminal emulator

(use-package eat
  :ensure t

  :config
  ;; Unbind M-o from eat-mode keymaps
  (unbind-key "M-o" eat-char-mode-map)
  (unbind-key "M-o" eat-semi-char-mode-map)
  (unbind-key "M-o" eat-eshell-char-mode-map)
  (unbind-key "M-o" eat-eshell-semi-char-mode-map))


(provide 'setup-eat)
