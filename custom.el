(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("05692bda554c178fafe15cc3e6ab09539e7db4846eb9bb6272b97068c055a903" default))
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((eval progn (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-clojure-cli-parameters . "--port 7888")
     (cider-clojure-cli-aliases . ":dev:test:snitch")
     (cider-clojure-cli-aliases . "-A:dev")
     (cider-figwheel-main-default-options . ":ui")
     (cider-preferred-build-tool . clojure-cli)
     (cider-default-cljs-repl . figwheel-main)
     (cider-clojure-cli-aliases . "-A:ui")
     (flycheck-disabled-checkers emacs-lisp-checkdoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "dark magenta")))))
