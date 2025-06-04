(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docker/build"))

(prodigy-define-service
  :name "Datomic transactor"
  :tags '(shipclojure datomic)
  :command "transactor"
  :args '("config/samples/dev-transactor-template.properties")
  :cwd "~/"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


(provide 'work)
