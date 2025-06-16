;; setup-eat.el

;; Setup terminal emulator

(use-package eat
  :straight '(:type git :host codeberg :repo "akib/emacs-eat" :files ("*.el" ("term" "term/*.el") "*.texi"
                                                                      "*.ti" ("terminfo/e" "terminfo/e/*")
                                                                      ("terminfo/65" "terminfo/65/*")
                                                                      ("integration" "integration/*")
                                                                      (:exclude ".dir-locals.el" "*-tests.el")))

  :config
  ;; Unbind M-o from eat-mode keymaps
  (unbind-key "M-o" eat-char-mode-map)
  (unbind-key "M-o" eat-semi-char-mode-map)
  (unbind-key "M-o" eat-eshell-char-mode-map)
  (unbind-key "M-o" eat-eshell-semi-char-mode-map))


(provide 'setup-eat)
