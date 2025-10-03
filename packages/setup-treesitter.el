;;; setup-treesitter.el --- Treesitter configuration -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;; * TREE-SITTER MODES
(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.py\\'" . python-ts-mode)
         ("\\.cmake\\'" . cmake-ts-mode)
         ("\\.java\\'" . java-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.php\\'" . php-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mm\\'" . objc-mode)
         ("\\.mdx\\'" . markdown-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.25.0" "src"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java"))
               (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.25.0"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (objc "https://github.com/tree-sitter-grammars/tree-sitter-objc")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (php "https://github.com/tree-sitter/tree-sitter-php" "v0.22.8" "php/src" )
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
               (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.1"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.24.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package combobulate
  :after (treesit)
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  :bind (:map combobulate-key-map
              ("M-a" . nil)
              ("M-n" . nil)
              ("M-p" . nil))
  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook
  ((python-ts-mode . combobulate-mode)
   (java-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (go-mode . go-ts-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("~/workspace/combobulate"))

(provide 'setup-treesitter)
;;; setup-treesitter.el ends here
