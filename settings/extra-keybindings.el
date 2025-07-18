;; No need to kill emacs that easily
;; The mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Search for more than just commands 
(global-set-key (kbd "<f1> a") 'apropos)

;; Open url at point in a browser
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Make suspend-frame less convenient
;; Do nothing on C-z, since I apparently repeatedly mistakenly type it
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-z") 'shell)

;; A convenient alternative to C-u as universal argument
(global-set-key (kbd "s-u") 'universal-argument)

(global-set-key (kbd "s-r") 'revert-buffer)

;; Org organisation keybinds
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'extra-keybindings)


