(use-package envrc
  :diminish nil
  :vc (:url "https://github.com/purcell/envrc")
  :init (envrc-global-mode))

(defun my/project-file-exists-p (filename)
  "Checks if FILENAME exists in the project root"
  (let ((project-file (projectile-expand-root filename)))
    (projectile-file-exists-p project-file)))

(defun my/initialize-python-venv ()
  "Initiate python venv and load it with specified Python version"
  (interactive)
  (let* ((python-version (or (completing-read "Python version: " '("python3" "python3.9" "python3.10" "python3.11" "python3.12" "python3.13") nil nil "python3")))
         (envrc-file (projectile-expand-root ".envrc")))
    (projectile-run-shell-command-in-root (format "%s -m venv .venv" python-version))
    (with-temp-file envrc-file (insert "export VIRTUAL_ENV=.venv\nlayout python3"))
    (envrc-allow)
    (envrc-reload)
    (when (my/project-file-exists-p "requirements.txt")
      (projectile-run-shell-command-in-root "pip install -r requirements.txt"))
    (message "Python venv initialization finished!")))

(provide 'setup-envrc)
