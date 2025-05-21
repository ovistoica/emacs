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
    (clojure-set-compile-command)
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c r"))

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
  :ensure t
  :bind (("C-c b t" . babashka-tasks)))

(setq mac-right-command-modifier 'hyper) ; Set right command key as Hyper


(use-package html-to-hiccup
  :ensure t
  :bind (:map clojure-mode-map
              ("H-h" . html-to-hiccup-convert-region)
              ("H-y" . html-to-hiccup-yank))
  :config
  (setq html-to-hiccup-use-shorthand-p t))


(use-package jdecomp
  :ensure t
  :mode ("\\.class\\'" . jdecomp-mode)
  :preface
  (defvar cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")
  (defvar fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library.")
  :when (or (file-exists-p cfr-path)
            (file-exists-p fernflower-path))
  :custom
  (jdecomp-decompiler-type
   (cond ((file-exists-p cfr-path) 'cfr)
         ((file-exists-p fernflower-path) 'fernflower)))
  (jdecomp-decompiler-paths
   `((cfr . ,cfr-path)
     (fernflower . ,fernflower-path))))

(require 'transient)

(use-package context-transient
  :ensure t
  :bind ("<f6>" . context-transient)
  :custom
  (context-transient-require-defclj)

  (defclj my-sync-deps (sync-deps) "Sync project deps")
  (defclj my-portal (user/portal))
  (defclj my-portal-clear (user/portal-clear))
  (defclj my-require-snitch (require '[snitch.core :refer [defn* defmethod* *fn *let]]))
  (defclj my-restart-sync (user/restart-sync))
  (defclj my-stop-sync (user/stop-sync))

  (context-transient-define
      my-clj-transient
    :doc "Transient for clojure envs"
    :repo "shipclojure-datom"
    :menu
    [["REPL"
      ("c" "Connect REPL" (lambda () (interactive) (cider-connect-clj '(:host "localhost" :port 63000))) :transient nil)
      ("d" "Sync deps" my-sync-deps)]
     ["Debug"
      ("p" "Start portal" my-portal)
      ("P" "Clear portal" my-portal-clear)
      ("S" "Require snitch" my-require-snitch)]
     ["Systems"
      ("a" "(Re)start main system" my-restart-sync)
      ("A" "Stop main system" my-stop-sync)]])
  )

(provide 'setup-clojure)
;;; setup-clojure.el ends here
