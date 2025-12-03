;; Romanians understand

defun my/setup-romanian-diacritics ()
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
(local-set-key (kbd "s-S") (λ (insert "Ș")))
