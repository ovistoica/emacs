;;; flexoki-light.el --- Flexoki light theme for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Generated with Claude Code
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme
;; URL: https://github.com/kepano/flexoki

;;; Commentary:

;; Flexoki is an inky color scheme for prose and code.
;; Designed by Steph Ango.
;; This is the light variant.

;;; Code:

(deftheme flexoki-light
  "Flexoki light theme - an inky color scheme for prose and code.")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors
      (paper      "#FFFCF0")
      (base-50    "#F2F0E5")
      (base-100   "#E6E4D9")
      (base-150   "#DAD8CE")
      (base-200   "#CECDC3")
      (base-300   "#B7B5AC")
      (base-500   "#878580")
      (base-600   "#6F6E69")
      (base-700   "#575653")
      (base-800   "#403E3C")
      (base-850   "#343331")
      (base-900   "#282726")
      (base-950   "#1C1B1A")
      (black      "#100F0F")

      ;; Accent colors (light mode uses 600 values)
      (red        "#AF3029")
      (orange     "#BC5215")
      (yellow     "#AD8301")
      (green      "#66800B")
      (cyan       "#24837B")
      (blue       "#205EA6")
      (purple     "#5E409D")
      (magenta    "#A02F6F")

      ;; Semantic mappings for light mode
      (bg         "#FFFCF0")  ; paper
      (bg-2       "#F2F0E5")  ; base-50
      (ui         "#E6E4D9")  ; base-100
      (ui-2       "#DAD8CE")  ; base-150
      (ui-3       "#CECDC3")  ; base-200
      (tx         "#100F0F")  ; black
      (tx-2       "#878580")  ; base-500
      (tx-3       "#B7B5AC")) ; base-300

  (custom-theme-set-faces
   'flexoki-light

   ;; Basic faces
   `(default ((,class (:foreground ,tx :background ,bg))))
   `(cursor ((,class (:background ,tx))))
   `(region ((,class (:background ,ui-2))))
   `(highlight ((,class (:background ,ui))))
   `(hl-line ((,class (:background ,bg-2))))
   `(fringe ((,class (:background ,bg :foreground ,tx-3))))
   `(vertical-border ((,class (:foreground ,ui-2))))
   `(window-divider ((,class (:foreground ,ui-2))))
   `(window-divider-first-pixel ((,class (:foreground ,ui-2))))
   `(window-divider-last-pixel ((,class (:foreground ,ui-2))))

   ;; Mode line
   `(mode-line ((,class (:background ,ui :foreground ,tx :box (:line-width 1 :color ,ui-3)))))
   `(mode-line-inactive ((,class (:background ,bg-2 :foreground ,tx-2 :box (:line-width 1 :color ,ui-2)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,tx))))
   `(mode-line-emphasis ((,class (:weight bold))))

   ;; Header line
   `(header-line ((,class (:background ,bg-2 :foreground ,tx-2))))

   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))

   ;; Font lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,purple))))
   `(font-lock-comment-face ((,class (:foreground ,tx-3 :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,tx-3 :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,purple))))
   `(font-lock-doc-face ((,class (:foreground ,tx-3 :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,orange :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,green :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
   `(font-lock-string-face ((,class (:foreground ,cyan))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,purple))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,tx-3 :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,tx :background ,bg-2 :weight bold))))

   ;; Search
   `(isearch ((,class (:foreground ,bg :background ,yellow :weight bold))))
   `(isearch-fail ((,class (:foreground ,red :background ,bg-2))))
   `(lazy-highlight ((,class (:foreground ,tx :background ,ui-2))))

   ;; Links
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))

   ;; Buttons
   `(button ((,class (:foreground ,blue :underline t))))

   ;; Parenthesis matching
   `(show-paren-match ((,class (:foreground ,bg :background ,green :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,bg :background ,red :weight bold))))

   ;; Error/Warning/Success
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,yellow :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; Compilation
   `(compilation-info ((,class (:foreground ,green))))
   `(compilation-warning ((,class (:foreground ,yellow))))
   `(compilation-error ((,class (:foreground ,red))))
   `(compilation-line-number ((,class (:foreground ,tx-2))))
   `(compilation-column-number ((,class (:foreground ,tx-2))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,blue :weight bold))))
   `(dired-symlink ((,class (:foreground ,cyan))))
   `(dired-flagged ((,class (:foreground ,red :weight bold))))
   `(dired-marked ((,class (:foreground ,yellow :weight bold))))
   `(dired-header ((,class (:foreground ,orange :weight bold))))

   ;; Org mode
   `(org-level-1 ((,class (:foreground ,orange :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,blue :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,green :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,purple :weight bold))))
   `(org-level-5 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-6 ((,class (:foreground ,magenta :weight bold))))
   `(org-level-7 ((,class (:foreground ,yellow :weight bold))))
   `(org-level-8 ((,class (:foreground ,red :weight bold))))
   `(org-document-title ((,class (:foreground ,tx :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,tx-2))))
   `(org-document-info-keyword ((,class (:foreground ,tx-3))))
   `(org-meta-line ((,class (:foreground ,tx-3))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-date ((,class (:foreground ,purple))))
   `(org-tag ((,class (:foreground ,tx-2 :weight normal))))
   `(org-block ((,class (:background ,bg-2 :foreground ,tx :extend t))))
   `(org-block-begin-line ((,class (:background ,ui :foreground ,tx-3 :extend t))))
   `(org-block-end-line ((,class (:background ,ui :foreground ,tx-3 :extend t))))
   `(org-code ((,class (:foreground ,orange :background ,bg-2))))
   `(org-verbatim ((,class (:foreground ,cyan :background ,bg-2))))
   `(org-special-keyword ((,class (:foreground ,tx-3))))
   `(org-table ((,class (:foreground ,blue))))

   ;; Markdown
   `(markdown-header-face-1 ((,class (:foreground ,orange :weight bold :height 1.3))))
   `(markdown-header-face-2 ((,class (:foreground ,blue :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground ,green :weight bold :height 1.1))))
   `(markdown-header-face-4 ((,class (:foreground ,purple :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,cyan :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,magenta :weight bold))))
   `(markdown-code-face ((,class (:foreground ,orange :background ,bg-2))))
   `(markdown-inline-code-face ((,class (:foreground ,orange :background ,bg-2))))
   `(markdown-pre-face ((,class (:foreground ,tx :background ,bg-2))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))
   `(markdown-url-face ((,class (:foreground ,cyan :underline t))))
   `(markdown-list-face ((,class (:foreground ,green))))

   ;; Company
   `(company-tooltip ((,class (:background ,ui :foreground ,tx))))
   `(company-tooltip-selection ((,class (:background ,ui-2 :foreground ,tx :weight bold))))
   `(company-tooltip-common ((,class (:foreground ,blue :weight bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,blue :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,tx-2))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,tx-2))))
   `(company-scrollbar-bg ((,class (:background ,ui))))
   `(company-scrollbar-fg ((,class (:background ,ui-3))))
   `(company-preview ((,class (:foreground ,tx-2 :background ,bg-2))))
   `(company-preview-common ((,class (:foreground ,blue :background ,bg-2))))

   ;; Ivy/Counsel
   `(ivy-current-match ((,class (:background ,ui-2 :foreground ,tx :weight bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,tx-2))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,blue :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,yellow :weight bold))))
   `(ivy-confirm-face ((,class (:foreground ,green))))
   `(ivy-match-required-face ((,class (:foreground ,red))))

   ;; Helm
   `(helm-header ((,class (:background ,bg-2 :foreground ,tx))))
   `(helm-source-header ((,class (:background ,ui :foreground ,orange :weight bold :height 1.1))))
   `(helm-selection ((,class (:background ,ui-2 :foreground ,tx :weight bold))))
   `(helm-match ((,class (:foreground ,blue :weight bold))))
   `(helm-candidate-number ((,class (:foreground ,tx-2))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,orange :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-2))))
   `(magit-branch-local ((,class (:foreground ,blue :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,green :weight bold))))
   `(magit-tag ((,class (:foreground ,yellow))))
   `(magit-hash ((,class (:foreground ,tx-2))))
   `(magit-diff-file-heading ((,class (:foreground ,tx :weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-2 :foreground ,tx :weight bold))))
   `(magit-diff-hunk-heading ((,class (:background ,ui :foreground ,tx-2))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,ui-2 :foreground ,tx))))
   `(magit-diff-context ((,class (:foreground ,tx-2))))
   `(magit-diff-context-highlight ((,class (:background ,bg-2 :foreground ,tx))))
   `(magit-diff-added ((,class (:foreground ,green :background ,bg))))
   `(magit-diff-added-highlight ((,class (:foreground ,green :background ,bg-2))))
   `(magit-diff-removed ((,class (:foreground ,red :background ,bg))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background ,bg-2))))
   `(magit-diffstat-added ((,class (:foreground ,green))))
   `(magit-diffstat-removed ((,class (:foreground ,red))))

   ;; Diff mode
   `(diff-added ((,class (:foreground ,green :background ,bg))))
   `(diff-removed ((,class (:foreground ,red :background ,bg))))
   `(diff-changed ((,class (:foreground ,yellow :background ,bg))))
   `(diff-header ((,class (:background ,ui :foreground ,tx))))
   `(diff-file-header ((,class (:background ,ui-2 :foreground ,tx :weight bold))))
   `(diff-hunk-header ((,class (:background ,ui :foreground ,tx-2))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,blue)))))

   ;; Flymake
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flymake-note ((,class (:underline (:style wave :color ,blue)))))

   ;; LSP
   `(lsp-face-highlight-textual ((,class (:background ,ui-2))))
   `(lsp-face-highlight-read ((,class (:background ,ui-2))))
   `(lsp-face-highlight-write ((,class (:background ,ui-2 :weight bold))))

   ;; Tree-sitter
   `(tree-sitter-hl-face:function ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,blue))))
   `(tree-sitter-hl-face:variable.parameter ((,class (:foreground ,blue))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,blue))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,green :weight bold))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,cyan))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,purple))))
   `(tree-sitter-hl-face:constant ((,class (:foreground ,purple))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,blue))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,tx-3 :slant italic))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,blue :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,tx-3))))
   `(which-key-command-description-face ((,class (:foreground ,tx))))
   `(which-key-group-description-face ((,class (:foreground ,orange))))

   ;; Vertico
   `(vertico-current ((,class (:background ,ui-2 :foreground ,tx :weight bold))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,blue :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,green :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,yellow :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,orange :weight bold))))

   ;; Corfu
   `(corfu-default ((,class (:background ,ui :foreground ,tx))))
   `(corfu-current ((,class (:background ,ui-2 :foreground ,tx :weight bold))))
   `(corfu-bar ((,class (:background ,ui-3))))
   `(corfu-border ((,class (:background ,ui-2))))

   ;; Marginalia
   `(marginalia-key ((,class (:foreground ,blue))))
   `(marginalia-documentation ((,class (:foreground ,tx-2))))
   `(marginalia-file-name ((,class (:foreground ,tx))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,purple))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,magenta))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,red))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))

   ;; Clojure specific
   `(clojure-keyword-face ((,class (:foreground ,cyan))))

   ;; CIDER
   `(cider-result-overlay-face ((,class (:background ,ui :foreground ,tx))))
   `(cider-repl-prompt-face ((,class (:foreground ,blue :weight bold))))
   `(cider-repl-stdout-face ((,class (:foreground ,tx))))
   `(cider-repl-stderr-face ((,class (:foreground ,red))))
   `(cider-test-failure-face ((,class (:foreground ,red :weight bold))))
   `(cider-test-success-face ((,class (:foreground ,green :weight bold))))

   ;; Terminal
   `(term-color-black ((,class (:foreground ,black :background ,black))))
   `(term-color-red ((,class (:foreground ,red :background ,red))))
   `(term-color-green ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((,class (:foreground ,base-200 :background ,base-200))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'flexoki-light)

;;; flexoki-light.el ends here
