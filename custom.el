(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f9d423fcd4581f368b08c720f04d206ee80b37bfb314fa37e279f554b6f415e9"
     "a0e9bc5696ce581f09f7f3e7228b949988d76da5a8376e1f2da39d1d026af386"
     "cd5f8f91cc2560c017cc9ec24a9ab637451e36afd22e00a03e08d7b1b87c29ca"
     "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac"
     "a64d8fd62fd5259244a9dd7c87b8af7277e153b8556e4e06f2c66f0781ec7e5a"
     "b7d9d334170c9d14ef3b88d49812b1d637ff48cf787d585d2ce8aa99f5d78ccf"
     "5b7ea19f12774b21bc6482aa041ac3fbde1374e31a928cd492453e4bb802e377"
     "d6b369a3f09f34cdbaed93eeefcc6a0e05e135d187252e01b0031559b1671e97"
     "ffa78fc746f85d1c88a2d1691b1e37d21832e9a44a0eeee114a00816eabcdaf9"
     "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
     "df39cc8ecf022613fc2515bccde55df40cb604d7568cb96cd7fe1eff806b863b"
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
     (flycheck-disabled-checkers emacs-lisp-checkdoc))))
