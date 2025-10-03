;; work.el

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docker/build"))

(prodigy-define-service
  :name "Datomic Transactor"
  :tags '(wavekit-ai shipclojure-datom shipclojure-website datomic)
  :command "/Users/ovistoica/datomic-pro-1.0.7364/bin/transactor"
  :args '("config/samples/dev-transactor-template.properties")
  :cwd "~/"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

;; (prodigy-define-service
;;   :name "ShipClojure Datom Shadow-CLJS Watch"
;;   :tags '(shipclojure-datom frontend)
;;   :command "bb"
;;   :args '("watch:frontend")
;;   :cwd "~/workspace/shipclojure-datom"
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ShipCLojure Datom CSS Watch"
  :tags '(shipclojure-datom css)
  :command "bb"
  :args '("watch:css")
  :cwd "~/workspace/shipclojure-datom"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Wavekit CSS Watch"
  :tags '(wavekit-ai css)
  :command "bb"
  :args '("watch:css")
  :cwd "~/workspace/wavekit-ai"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Shipclojure Website CSS Watch"
  :tags '(shipclojure-website css)
  :command "bb"
  :args '("watch:css")
  :cwd "~/workspace/shipclojure-website"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

;; (prodigy-define-service
;;   :name "Wavekit Shadow-CLJS Watch"
;;   :tags '(wavekit-ai frontend)
;;   :command "bb"
;;   :args '("watch:frontend")
;;   :cwd "~/workspace/wavekit-ai"
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "replicant-daisyui CSS Watch"
  :tags '(replicant-daisyui css)
  :command "npm"
  :args '("run" "css:watch")
  :cwd "~/workspace/replicant-daisyui"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "replicant-daisyui Shadow Watch"
  :tags '(replicant-daisyui shadow-cljs)
  :command "npm"
  :args '("run" "shadow:watch")
  :cwd "~/workspace/replicant-daisyui"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


(setq world-clock-list
      '(("America/Chicago" "Houston/Texas")
        ("America/New_York" "New York")
        ("Europe/Bucharest" "Bucharest")
        ("Europe/Brussels" "Belgium")
        ("Europe/London" "UK")
        ("Europe/Madrid" "Spain")
        ("Europe/Stockholm" "Stockholm")
        ("Asia/Jerusalem" "Israel")
        ("Europe/Athens" "Greece")
        ("Asia/Kolkata" "India/Bangalore")
        ("Etc/UTC" "UTC")))

(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

(provide 'work)
