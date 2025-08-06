;; Eca - Editor Code Assistant

(use-package eca
  :straight '(:type git :host github :repo "editor-code-assistant/eca-emacs" :branch "master"
                    :files ("*.el" (:exclude "demo.gif")))

  ;;:config
  ;;(setq eca-extra-args '("--verbose"))
  ;;(setq eca-chat-auto-add-repomap nil)

  )


(provide 'setup-eca)
