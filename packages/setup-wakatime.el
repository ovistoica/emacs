;; Waka time is a tracker of how much time you spend coding.
;; It's great to keep you honest

(require 'private)

(use-package wakatime-mode
  :ensure t
  :diminish ""
  :custom (setq wakatime-api-key my/secret-wakatime-api-key
                wakatime-cli-path "~/.wakatime/wakatime-cli")
  :init (global-wakatime-mode))


(provide 'setup-wakatime)
