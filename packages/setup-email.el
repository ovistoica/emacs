;; Setup email
;; Tools and utils to read email from emacs

(require 'prot-common)

(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources '("~/.authinfo.gpg")
        user-full-name "Ovidiu Stoica"
        user-mail-address "ovidiu.stoica1094@gmail.com")

  ;; This is needed so Emacs doesn't keep old cached data
  (auth-source-forget-all-cached))

(use-package mm-encode
  :ensure nil
  :defer t
  :config
  (setq mm-encrypt-option nil ; use 'guided for both if you need more control
        mm-sign-option nil))

(use-package mml-sec
  :ensure nil
  :defer t
  :config
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t))

(use-package message
  :ensure nil
  :defer t
  :hook
  (message-setup . message-sort-headers)
  :config
  (setq mail-user-agent 'message-user-agent
        message-mail-user-agent t) ; use `mail-user-agent'
  (setq mail-header-separator "--text follows this line--")
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)
  (setq message-signature "Ovidiu Stoica\nhttps://ovistoica.com\n"
        mail-signature message-signature)
  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  (setq message-citation-line-format (concat "> From: %f\n"
                                             "> Date: %a, %e %b %Y %T %z\n"
                                             ">")
        message-ignored-cited-headers "") ; default is "." for all headers
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  ;; (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))
  (setq message-wide-reply-confirm-recipients nil))

(use-package gnus-dired
  :ensure nil
  :after message
  :hook
  (dired-mode . turn-on-gnus-dired-mode))

;; SMTP settings for Gmail
(use-package smtpmail
  :ensure nil
  :after message
  :config

  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-stream-type 'starttls))

(use-package smtpmail-multi
  :straight '(:type git :repo "vapniks/smtpmail-multi" :host github)
  :config
  (setq smtpmail-multi-accounts
        `((gmail-primary . (,(prot-common-auth-get-field "gmail-primary-smtp" :user)
                            "smtp.gmail.com"
                            ,(string-to-number (prot-common-auth-get-field "gmail-primary-smtp" :port))
                            ,(prot-common-auth-get-field "gmail-primary-smtp" :user)
                            starttls nil nil nil))
          (gmail-secondary . (,(prot-common-auth-get-field "gmail-secondary-smtp" :user)
                              "smtp.gmail.com"
                              ,(string-to-number (prot-common-auth-get-field "gmail-secondary-smtp" :port))
                              ,(prot-common-auth-get-field "gmail-secondary-smtp" :user)
                              starttls nil nil nil)
                           )
          (gmail-repsmate . (,(prot-common-auth-get-field "gmail-repsmate-smtp" :user)
                             "smtp.gmail.com"
                             ,(string-to-number (prot-common-auth-get-field "gmail-repsmate-smtp" :port))
                             ,(prot-common-auth-get-field "gmail-repsmate-smtp" :user)
                             starttls nil nil nil))))

  (setq smtpmail-multi-associations
        `((,(prot-common-auth-get-field "gmail-primary-smtp" :user) gmail-primary)
          (,(prot-common-auth-get-field "gmail-secondary-smtp" :user) gmail-secondary)
          (,(prot-common-auth-get-field "gmail-repsmate-smtp" :user) gmail-repsmate)))

  (setq smtpmail-servers-requiring-authorization "\\.com")
  (setq send-mail-function #'smtpmail-multi-send-it))

(use-package notmuch-indicator
  :ensure t
  :after notmuch
  :config
  (setq notmuch-indicator-args
        '(( :terms "tag:unread and tag:inbox"
            :label "[U] ")
          ( :terms "tag:unread and tag:package"
            :label "[P] "))

        notmuch-indicator-refresh-count (* 60 3)
        notmuch-indicator-hide-empty-counters t
        notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))

  ;; I control its placement myself.  See prot-emacs-modeline.el where
  ;; I set the `mode-line-format'.
  (setq notmuch-indicator-add-to-mode-line-misc-info nil)

  (notmuch-indicator-mode 1))

(defun sync-email ()
  "Runs a script for syncing email"
  (interactive)
  (async-shell-command "/Users/ovistoica/.local/bin/sync-mail"))


(defun show-email-sync-logs ()
  "Show the logs for email"
  (interactive)
  (let ((prev-config (current-window-configuration)))
    (find-file "~/.local/logs/mbsync.log")
    (setq-local my/previous-window-configuration prev-config)
    (local-set-key (kbd "q")
      (lambda ()
        (interactive)
        (kill-buffer (current-buffer))
        (set-window-configuration my/previous-window-configuration)))))

(provide 'setup-email)
