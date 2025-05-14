;;; setup-clojure.el --- Clojure setup -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(load (expand-file-name "plugins/clj-functions.el" user-emacs-directory))

(use-package clojure-mode
  :ensure t
  :defines put-clojure-indent
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands (clojure-project-dir)
  :bind ( :map clojure-mode-map
          ("C-:" . clj-functions-hiccup-classes-to-string))
  :config
  (defun clojure-set-compile-command ()
    (let ((project-dir (clojure-project-dir)))
      (cond ((and (file-exists-p (expand-file-name "project.clj" project-dir))
                  (executable-find "lein"))
             (setq-local compile-command "lein "))
            ((and (file-exists-p (expand-file-name "deps.edn" project-dir))
                  (executable-find "clojure"))
             (setq-local compile-command "clojure ")))))
  (defun clojure-mode-setup ()
    "Setup Clojure buffer."
    (common-lisp-modes-mode 1)
    (clojure-set-compile-command))

  ;; Custom indentation for Midje's fact and facts
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'facts 1)
  (put-clojure-indent 'vthread-loop 1))

(use-package cider
  :ensure t
  :after clojure-mode
  :defines
  cider-current-repl
  cider-interractive-eval
  :preface
  (defun cider-reset ()
    (interactive)
    (with-current-buffer (cider-current-repl)
      (cider-interactive-eval "(reset)")))
  :delight " CIDER"
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          ("M-:" . cider-reset)
          :map cider-mode-map
          ("M-¬" . cider-format-buffer)
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("M-:" . cider-reset)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))

  :config
  (setq cider-show-eval-spinner t
        cider-dynamic-indentation nil
        cider-allow-jack-in-without-project t
        cider-preferred-build-tool 'clojure-cli
        ;; ~make sure we can always debug nrepl issues~
        ;; Turning this off again, seems it may really blow up memory usage
        nrepl-log-messages nil)
  (setq cider-download-java-sources t))

(use-package ob-clojure
  :after (org clojure-mode)
  :custom
  (org-babel-clojure-backend 'cider)
  :init
  (require 'cider))

(use-package clj-refactor
  :ensure t
  :delight clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package clj-decompiler
  :ensure t
  :hook (cider-mode . clj-decompiler-setup))

(use-package babashka
  :ensure t)


(provide 'setup-clojure)
;;; setup-clojure.el ends here
