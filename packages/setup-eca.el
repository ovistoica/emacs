;; Eca - Editor Code Assistant

;;; Code:
(use-package eca
  :straight  '(:type git :local-repo "~/workspace/eca-emacs"
                     ;;:repo "editor-code-assistant/eca-emacs" :branch "master"
                     :files ("*.el" (:exclude "demo.gif")))

  ;;:config
  ;;(setq eca-extra-args '("--verbose"))
  ;;(setq eca-chat-auto-add-repomap nil)

  )


(provide 'setup-eca)
