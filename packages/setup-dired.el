(with-eval-after-load 'dired
  (require 'dired-x)

  ;; Open markdown files with Typora via dired-do-open
  (setq dired-guess-shell-alist-user
        '(("\\.md\\'" "typora")
          ("\\.markdown\\'" "typora")))

  ;; Advise dired-do-open to use typora for markdown files
  (defun dired-do-open--markdown-typora (orig-fn &rest args)
    "Open markdown files with Typora instead of xdg-open."
    (let ((files (dired-get-marked-files)))
      (if (and files
               (cl-every (lambda (f)
                           (string-match-p "\\.\\(md\\|markdown\\)\\'" f))
                         files))
          (apply #'start-process "typora" nil "typora" files)
        (apply orig-fn args))))
  (advice-add 'dired-do-open :around #'dired-do-open--markdown-typora)

  ;; Make dired less verbose, toggle with (
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; Move files between split panes
  (setq dired-dwim-target t)

  ;; Sort directories first
  ;; (setq dired-use-ls-dired t)
  ;; (setq dired-listing-switches "-lAXGh --group-directories-first --sort=name")

  ;; Don't ask me for recursive copies
  (setq dired-recursive-copies 'always)

  ;; C-a is nicer in dired if it moves back to start of files
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; Mirror OSX folder navigation
  (define-key dired-mode-map (kbd "M-<up>") 'dired-jump)
  (define-key dired-mode-map (kbd "M-<down>") 'dired-find-file)

  ;; Delete files with k
  (define-key dired-mode-map (kbd "k") 'dired-do-delete)
  
  ;; Easier up directory
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

;; Nicer navigation also in writeable dired
(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-quick-sort
  :after dired
  :init
  (dired-quick-sort-setup))

;; Diminish dired in modeline
(with-eval-after-load 'dired
  (when (fboundp 'diminish)
    (diminish 'dired-mode)))

(use-package dired-toggle-sudo
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("C-c C-s" . dired-toggle-sudo))
  :config
  ;; Allow to use: /sudo:user@host:/path/to/file
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-default-proxies-alist
                 '(".*" "\\`.+\\'" "/ssh:%h:"))))

(provide 'setup-dired)
