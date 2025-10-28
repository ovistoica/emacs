(use-package dashboard
  :demand t
  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome to Emacs Dashboard")

  ;; Set the banner
  (dashboard-startup-banner 'logo)

  ;; Content is not centered by default. To center, set
  (dashboard-center-content t)

  ;; vertically center content
  (dashboard-vertically-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (dashboard-show-shortcuts nil)

  ;; Set the items to show on the dashboard
  (dashboard-items '((recents   . 5)
                     (projects  . 5)
                     (agenda    . 5)
                     (bookmarks . 5)))

  ;; Show agenda for the next 7 days
  (dashboard-week-agenda t)

  ;; Filter out done tasks from agenda
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  ;; Show item counts
  (dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                          ("Agenda for the coming week:" . "Org Agenda:")
                          ("Projects:" . "Recent projects:")
                          ("Bookmarks:" . "Bookmarks:")))

  ;; Icons
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)

  ;; Navigator below the banner
  (dashboard-set-navigator t)

  :config
  ;; Set up navigator buttons after all-the-icons is available
  (when (and (fboundp 'all-the-icons-octicon)
             (fboundp 'all-the-icons-faicon))
    (setq dashboard-navigator-buttons
          `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Homepage"
              "Browse homepage"
              (lambda (&rest _) (browse-url "https://github.com/emacs-dashboard/emacs-dashboard")))
             (,(all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0)
              "Update"
              "Update packages"
              (lambda (&rest _) (package-refresh-contents)
                (package-upgrade-all)))
             (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
              "Config"
              "Open config"
              (lambda (&rest _) (find-file user-init-file)))))))

  ;; Set initial buffer
  (dashboard-setup-startup-hook)

  ;; Refresh dashboard when switching to it
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (setq-local revert-buffer-function
                          (lambda (&rest _) (dashboard-refresh-buffer))))))

(provide 'setup-dashboard)
