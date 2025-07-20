;; Setup email
;; Tools and utils to read email from emacs

(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources '("~/.authinfo.gpg")
        user-full-name "Ovidiu Stoica"
        user-mail-address "ovidiu.stoica1094@gmail.com"))

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

;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs. Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
(use-package notmuch
  :load-path "/opt/homebrew/Cellar/notmuch/0.39/share/emacs/site-lisp/notmuch/"
  :defer t
  :commands (notmuch notmuch-mua-new-mail))


(provide 'setup-email)
