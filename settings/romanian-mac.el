;; change command to meta, and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
(setq mac-right-option-modifier nil)

;; Fix for emacs password prompt for gpg:
;; https://bpanthi977.com/braindump/configuring_gpg_for_emacs_in_macos.html
(setq epa-pinentry-mode 'loopback)

(defun my/setup-romanian-diacritics ()
  "Set up Romanian diacritics input"
  (interactive)
  (local-set-key (kbd "s-a") (λ (insert "ă")))
  (local-set-key (kbd "s-q") (λ (insert "â")))
  (local-set-key (kbd "s-t") (λ (insert "ț")))
  (local-set-key (kbd "s-i") (λ (insert "î")))
  (local-set-key (kbd "s-s") (λ (insert "ș")))
  (local-set-key (kbd "s-A") (λ (insert "Ă")))
  (local-set-key (kbd "s-Q") (λ (insert "Â")))
  (local-set-key (kbd "s-T") (λ (insert "Ț")))
  (local-set-key (kbd "s-I") (λ (insert "Î")))
  (local-set-key (kbd "s-S") (λ (insert "Ș"))))

(defun insert-backslash ()
  (interactive)
  (insert "\\"))

;; Insert backslash, no questions asked
(global-set-key (kbd "H-7") 'insert-backslash)

;; Move to OSX trash folder when deleting stuff
(setq trash-directory "~/.Trash/emacs")

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/opt/homebrew/bin/aspell")

;; Use GNU ls - install with: brew install xz coreutils
(setq insert-directory-program "gls")

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-shell-name "/bin/zsh") ; Explicitly set shell
  (setq exec-path-from-shell-variables
        '(
          "ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "MANPATH"
          "NVM_DIR"
          "PATH"
          "SSH_AGENT_PID"
          "SSH_AUTH_SOCK"
          ))
  (exec-path-from-shell-initialize))

(provide 'romanian-mac)
