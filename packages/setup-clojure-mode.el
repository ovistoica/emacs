;;; setup-clojure-mode.el --- Clojure mode configuration -*- lexical-binding: t; -*-

(declare-function paredit-forward "paredit")
(declare-function paredit-forward-down "paredit")
(declare-function cider-interactive-eval "cider-eval")
(declare-function cider-pprint-eval-last-sexp "cider-eval")

(use-package clojure-mode
  :after (significant-other)
  :hook (((clojure-mode clojurescript-mode clojurec-mode) . setup-clojure-mode-so))

  :custom
  (clojure-toplevel-inside-comment-form t)

  :config
  ;; don't steal hippie-expand-lines keybinding
  (unbind-key (kbd "C-:") clojure-mode-map)

  (require 'i18n-edn)

  ;; After threading all forms, check if we should maybe unwind once
  ;; according to my tastes
  (advice-add 'clojure--thread-all :after
              (lambda (&rest _)
                (when (my/clojure-should-unwind-once?)
                  (clojure-unwind))))

  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)
              ("C-\"" . clojure-toggle-keyword-string)
              ("C-x M-e" . my/cider-eval-including-lets)
              ("C-c C-M-s" . my/cider-eval-def-symbol)
              ("C-." . clj-hippie-expand-no-case-fold)
              ;; ("C-c i 1 8 n" . i18n-edn-edit-in-multifile)
              ("<f7>" . cider-eval-last-sexp)
              ("<f6>" . cider-pprint-eval-last-sexp)
              ("<f5>" . delete-other-windows)
              ("s-<return>" . clerk-show)))


(use-package zprint-mode
  :defer 2)

;; Set up jumping to other file (src/test, component/scene)

(require 's)
(require 'significant-other)

(defun setup-clojure-mode-so ()
  (with-significant-others file-name
    ("/src/cljc/.+/ui/" (list (s-with file-name
                                (s-replace "/src/cljc/" "/portfolio/")
                                (s-replace ".cljc" "_scenes.cljs"))
                              (s-with file-name
                                (s-replace "/src/" "/test/")
                                (s-replace ".cljc" "_test.clj"))
                              (s-with file-name
                                (s-replace "/src/" "/test/")
                                (s-replace ".cljc" "_test.cljc"))))
    ("/portfolio/.+/ui/" (list (s-with file-name
                                 (s-replace "/portfolio/" "/src/cljc/")
                                 (s-replace "_scenes.cljs" ".cljc"))))
    ("/src/.+\.cljc" (list (s-with file-name
                             (s-replace "/src/" "/test/")
                             (s-replace ".cljc" "_test.clj"))
                           (s-with file-name
                             (s-replace "/src/" "/test/")
                             (s-replace ".cljc" "_test.cljc"))))
    ("/src/.+\.clj" (list (s-with file-name
                            (s-replace "/src/" "/test/")
                            (s-replace ".clj" "_test.clj"))))

    ("/test/.+\.clj" (list
                      (s-with file-name
                        (s-replace "/test/" "/src/")
                        (s-replace "_test.clj" ".clj"))
                      (s-with file-name
                        (s-replace "/test/" "/src/")
                        (s-replace "_test.clj" ".cljc"))))))

;; Set up Clojure CSS completions

(use-package css-completions
  :after (cider projectile)
  :ensure nil
  :defer t
  :commands (cssc/enable-for-clojure)
  :init
  (add-hook 'clojure-mode-hook 'cssc/enable-for-clojure)
  (add-hook 'clojurescript-mode-hook 'cssc/enable-for-clojure)
  (add-hook 'clojurec-mode-hook 'cssc/enable-for-clojure)
  (add-hook 'mhtml-mode-hook 'cssc/enable-for-html))

;; Don't fully unthread always

(defun my/clojure-should-unwind-once? ()
  (save-excursion
    (ignore-errors
      (when (looking-at "(")
        (forward-char 1)
        (forward-sexp 1)))
    (let ((forms nil))
      (while (not (looking-at ")"))
        (clojure-forward-logical-sexp)
        (clojure-backward-logical-sexp)
        (setq forms (cons (buffer-substring-no-properties (point) (+ 1 (point))) forms))
        (clojure-forward-logical-sexp))
      (and (--any? (s-equals? it "(") forms)
           (< 2 (length forms))))))

;; eval-current-sexp while also including surrounding lets

(defun my/cider-looking-at-lets? ()
  (or (looking-at "(let ")
      (looking-at "(letfn ")
      (looking-at "(when-let ")
      (looking-at "(if-let ")))

(defun my/cider-collect-lets (&optional max-point)
  (let* ((beg-of-defun (save-excursion (beginning-of-defun) (point)))
         (lets nil))
    (save-excursion
      (while (not (= (point) beg-of-defun))
        (paredit-backward-up 1)
        (when (my/cider-looking-at-lets?)
          (save-excursion
            (let ((beg (point)))
              (paredit-forward-down 1)
              (paredit-forward 2)
              (when (and max-point (< max-point (point)))
                (goto-char max-point))
              (setq lets (cons (concat (buffer-substring-no-properties beg (point))
                                       (if max-point "]" ""))
                               lets))))))
      lets)))

(defun my/inside-let-block? ()
  (save-excursion
    (paredit-backward-up 2)
    (my/cider-looking-at-lets?)))

(defun my/cider-eval-including-lets (&optional output-to-current-buffer)
  "Evaluates the current sexp form, wrapped in all parent lets."
  (interactive "P")
  (let* ((beg-of-sexp (save-excursion (paredit-backward 1) (point)))
         (code (buffer-substring-no-properties beg-of-sexp (point)))
         (lets (my/cider-collect-lets (when (my/inside-let-block?)
                                        (save-excursion (paredit-backward 2) (point)))))
         (code (concat (s-join " " lets)
                       " " code
                       (s-repeat (length lets) ")"))))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler)))))

(defun my/cider-eval-def-symbol ()
  "Pretty-print the value of the symbol defined by the top-level def at point.
Works from anywhere inside a `(def...' form, e.g. `def', `defn' or
`defonce'.  Does nothing when the top-level form is not a def."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (paredit-forward-down)
    (when (looking-at "def\\(\\w*\\)")
      (paredit-forward 2)
      (cider-pprint-eval-last-sexp))))

(defun clj-hippie-expand-no-case-fold ()
  "Consider / as whitespace when doing hippie-expand i clojure-mode"
  (interactive)
  (let ((old-syntax (char-to-string (char-syntax ?/))))
    (modify-syntax-entry ?/ " ")
    (hippie-expand-no-case-fold)
    (modify-syntax-entry ?/ old-syntax)))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(defun my/cider-unload-current-namespace-aliases ()
  ;; A clojure.tools.namespace.refresh can leave namespaces with aliases that
  ;; point to nothingness! Those namespaces pointing to nothingness prevent the
  ;; (ns ,,,) form from evaluating. A workaround is to unload the aliases for
  ;; your currently open namespace.
  (interactive)
  (cider-interactive-eval "(doseq [a (keys (ns-aliases *ns*))] (ns-unalias (symbol (str *ns*)) a))"))

(defun my/clojure-sync-deps ()
  "Load deps newly added to deps.edn into the running REPL.
Requires Clojure 1.12+ (`clojure.repl.deps/sync-deps')."
  (interactive)
  (cider-interactive-eval "((requiring-resolve 'clojure.repl.deps/sync-deps))"))

(use-package neil :defer t) ;; M-x neil-find-clojure-package

(provide 'setup-clojure-mode)
