;; Consult 
;; Provides search and navigation commands based on the Emacs completion
;; function completion-read. Completion allows you to quickly select an item
;; from a list of candidates.

(use-package consult
  :bind (("C-x f" . consult-recent-file)
         ("C-x C-i" . consult-imenu)
         ("M-g i" . consult-imenu)
         ("C-x M-i" . consult-imenu-multi)
         ("C-x i" . consult-outline)
         ("C-x C-y" . consult-yank-from-kill-ring)
         ("C-v" . consult-line)
         ("M-v" . consult-line-multi)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-y" . consult-yank-pop)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s k" . consult-keep-lines)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;;("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )
  :after (perspective)

  :config 
  ;; Show only perspective-buffers with consult-buffer
  
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; (when (featurep 'perspective)
  ;;     ;; Remove any existing perspective sources
  ;;     (setq consult-buffer-sources
  ;;           (cl-remove-if (lambda (source)
  ;;                           (string= (plist-get source :name) "Perspective"))
  ;;                         consult-buffer-sources))
  ;;     ;; Add single fast perspective source
  ;;     (push `(:name "Perspective"
  ;;             :narrow ?s
  ;;             :category buffer
  ;;             :state ,#'consult--buffer-state
  ;;             :history buffer-name-history
  ;;             :default t
  ;;             :items ,(lambda ()
  ;;                       (let ((buffers (persp-current-buffer-names t)))
  ;;                         (sort buffers
  ;;                               (lambda (a b)
  ;;                                 (< (or (cl-position (get-buffer a) (buffer-list))
  ;; 999)
  ;;                                    (or (cl-position (get-buffer b) (buffer-list))
  ;; 999)))))))
  ;;           consult-buffer-sources))
  )

(use-package consult-flycheck
  :bind (("M-g f" . consult-flycheck)))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(provide 'setup-consult)
