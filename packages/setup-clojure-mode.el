(use-package clojure-mode
  :hook ((clojure-mode . setup-clojure-mode-so)
         (clojurescript-mode-hook . setup-clojure-mode-so)
         (clojurec-mode-hook . setup-clojure-mode-so))
  
  :custom 
  (clojure-toplevel-inside-comment-form t)
  :config
  (unbind-key (kbd "C-:")  clojure-mode-map)
  (require 'i18n-edn)

  ;; After threading all forms, check if we should maybe unwind once according
  ;; to my tastes
  (defadvice clojure--thread-all (after possibly-unwind-once activate)
    (when (my/clojure-should-unwind-once?)
      (clojure-unwind)))

  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)
              ("C-\"" . clojure-toggle-keyword-string)
              ("C-x M-e" . my/cider-eval-including-lets)
              ("C-." . cqlj-hippie-expand-no-case-fold)
              ("C-c i 1 8 n" . i18n-edn-edit-in-multifile)
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
                                (s-replace ".cljc" "_scenes.cljs"))))
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
        (forward-sexp-1)))
    (let ((forms nil))
      (while (not (looking-at ")"))
        (clojure-forward-logical-sexp)
        (clojure-backward-logical-sexp)
        (setq forms (cons (buffer-substring-no-properties (point) (+ 1 (point))) forms))
        (clojure-forward-logical-sexp))
      (and (--any? (s-equals? it "(") forms)
           (< 2 (length forms))))))

;; eval-current-sexp while also including lets
(defun my/cider-looking-at-lets? ()
  (or (looking-at "(let ")
      (looking-at "(letfn ")
      (looking-at "(when-let ")
      (looking-at "(if-let ")))

(defun my/cider-collect-lets (&optional max-point)
  (let* ((beg-of-defun (save-excursion)))))
