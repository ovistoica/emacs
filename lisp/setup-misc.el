;;; setup-misc.el --- Misc configuration -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:


(use-package envrc
  :straight '(envrc :type git :host github :repo "purcell/envrc")
  :init (envrc-global-mode))

(use-package elfeed
  :straight '(elfeed :type git :host github :repo "skeeto/elfeed")
  :config
  (setq elfeed-feeds
        '(("https://www.daily.co/blog/rss/" blog ai voice-ai)
          ("https://jackrusher.com/feed.xml" blog clojure))))

(use-package wakatime-mode
  :ensure t
  :diminish ""
  :custom (setq wakatime-api-key os-secret-wakatime-api-key
                wakatime-cli-path "~/.wakatime/wakatime-cli")
  :init (global-wakatime-mode))

(use-package keycast
  :ensure t)

(use-package package-lint
  :straight '(package-lint :type git :host github :repo "purcell/package-lint"))

(use-package water-reminder
  :ensure nil
  :hook (after-init . water-reminder-mode))

;; TODO Document on this further, possibly disable LSP on very long files
(use-package so-long
  :init (global-so-long-mode 1))

(provide 'setup-misc)
;;; setup-misc.el ends here
