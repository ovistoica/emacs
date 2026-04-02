;;; setup-chez-moi.el --- Chezmoi configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Chezmoi support for fast edit config

;;; Code:

(defun chezmoi-add-current-file ()
  "Run `chezmoi add' on the current file or the file at point in Dired."
  (interactive)
  (if-let ((file (if (derived-mode-p 'dired-mode)
                     (dired-get-filename nil t)
                   (buffer-file-name))))
      (let ((result (shell-command-to-string (format "chezmoi add %s" (shell-quote-argument file)))))
        (message "chezmoi add %s: %s" (file-name-nondirectory file)
                 (if (string-empty-p result) "done" result)))
    (user-error "No file found at point")))

(use-package chezmoi
  :vc (:url "https://github.com/tuh8888/chezmoi.el")
  :commands (chezmoi-find chezmoi-write chezmoi-sync-files chezmoi-diff chezmoi-ediff)
  :bind (("C-c C-f" . chezmoi-find)
         ("C-c C-s" . chezmoi-write)
         ("C-c C-a" . chezmoi-add-current-file)
         (:map dired-mode-map
               ("C-c C-a" . chezmoi-add-current-file))))

(provide 'setup-chez-moi)
;;; setup-chez-moi.el ends here
