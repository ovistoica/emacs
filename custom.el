(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("df39cc8ecf022613fc2515bccde55df40cb604d7568cb96cd7fe1eff806b863b"
     "aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09"
     "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
     "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729"
     "ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239"
     "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac"
     "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
     "05692bda554c178fafe15cc3e6ab09539e7db4846eb9bb6272b97068c055a903" default))
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
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))
 )
