;; work.el

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docker/build"))

(prodigy-define-service
  :name "Datomic Transactor"
  :tags '(shipclojure-datom datomic)
  :command "transactor"
  :args '("config/samples/dev-transactor-template.properties")
  :cwd "~/"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Shadow-CLJS Watch"
  :tags '(shipclojure-datom frontend)
  :command "npx"
  :args '("shadow-cljs" "watch" "app" "portfolio")
  :cwd "~/workspace/shipclojure-datom"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "CSS Watch"
  :tags '(shipclojure-datom css)
  :command "npm"
  :args '("run" "styles:watch")
  :cwd "~/workspace/shipclojure-datom"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'work)
