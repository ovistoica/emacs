;; change command to meta, and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
(setq mac-right-option-modifier nil)

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
  (exec-path-from-shell-initialize))

(provide 'setup-mac)
