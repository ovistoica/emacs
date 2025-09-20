;;; Code:

(use-package significant-other
  :straight '(significant-other :type git :local-repo "~/workspace/significant-other")
  :bind ("s-j" . significant-other-jump))

(provide 'significant-other)

