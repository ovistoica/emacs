;;; osaka-jade-theme.el --- Osaka Jade theme for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Generated with Claude Code
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme
;; URL: https://github.com/ovistoica/omarchy

;;; Commentary:

;; Osaka Jade is a dark, soothing theme with jade and green tones.
;; Based on the osaka-jade color scheme.

;;; Code:

(deftheme osaka-jade
  "Osaka Jade theme - a dark theme with jade and green tones.")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors
      (bg         "#111C18")
      (bg-alt     "#1a2822")
      (bg-hl      "#23372B")
      (fg         "#C1C497")
      (fg-alt     "#9ba082")
      (fg-dim     "#6d7a65")

      ;; UI colors
      (cursor     "#D7C995")
      (selection  "#C1C497")
      (line       "#1f2e28")
      (border     "#2a3f35")
      (ui         "#23372B")
      (ui-alt     "#53685B")

      ;; Accent colors
      (red        "#FF5345")
      (red-dim    "#DB9F9C")
      (green      "#549E6A")
      (green-br   "#63b07a")
      (yellow     "#E5C736")
      (yellow-dim "#459451")
      (blue       "#509475")
      (blue-br    "#ACD4CF")
      (magenta    "#D2689C")
      (cyan       "#2DD5B7")
      (cyan-dim   "#8CD3CB")
      (teal       "#75BBB3")
      (white      "#F6F5DD")
      (white-br   "#9EEBB3"))

  (custom-theme-set-faces
   'osaka-jade

   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,ui :foreground ,selection))))
   `(highlight ((,class (:background ,bg-hl))))
   `(hl-line ((,class (:background ,line))))
   `(fringe ((,class (:background ,bg :foreground ,ui-alt))))
   `(vertical-border ((,class (:foreground ,border))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:foreground ,border))))
   `(window-divider-last-pixel ((,class (:foreground ,border))))

   ;; Mode line
   `(mode-line ((,class (:background ,ui :foreground ,fg :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,bg-alt :foreground ,fg-dim :box (:line-width 1 :color ,bg-hl)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,cyan))))
   `(mode-line-emphasis ((,class (:weight bold :foreground ,green-br))))

   ;; Header line
   `(header-line ((,class (:background ,bg-alt :foreground ,fg-alt))))

   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,cyan :weight bold))))

   ;; Font lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,blue-br))))
   `(font-lock-comment-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,magenta))))
   `(font-lock-doc-face ((,class (:foreground ,cyan-dim :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,green-br :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,teal))))
   `(font-lock-string-face ((,class (:foreground ,cyan-dim))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue-br))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,ui-alt :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,line :weight bold))))

   ;; Search
   `(isearch ((,class (:foreground ,bg :background ,yellow :weight bold))))
   `(isearch-fail ((,class (:foreground ,red :background ,bg-alt))))
   `(lazy-highlight ((,class (:foreground ,bg :background ,yellow-dim))))

   ;; Links
   `(link ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))

   ;; Buttons
   `(button ((,class (:foreground ,cyan :underline t))))

   ;; Parenthesis matching
   `(show-paren-match ((,class (:foreground ,bg :background ,green-br :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,bg :background ,red :weight bold))))

   ;; Error/Warning/Success
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,yellow :weight bold))))
   `(success ((,class (:foreground ,green-br :weight bold))))

   ;; Compilation
   `(compilation-info ((,class (:foreground ,green-br))))
   `(compilation-warning ((,class (:foreground ,yellow))))
   `(compilation-error ((,class (:foreground ,red))))
   `(compilation-line-number ((,class (:foreground ,fg-alt))))
   `(compilation-column-number ((,class (:foreground ,fg-alt))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,blue-br :weight bold))))
   `(dired-symlink ((,class (:foreground ,cyan))))
   `(dired-flagged ((,class (:foreground ,red :weight bold))))
   `(dired-marked ((,class (:foreground ,yellow :weight bold))))
   `(dired-header ((,class (:foreground ,green-br :weight bold))))

   ;; Org mode
   `(org-level-1 ((,class (:foreground ,cyan :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,green-br :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,yellow :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,blue-br :weight bold))))
   `(org-level-5 ((,class (:foreground ,magenta :weight bold))))
   `(org-level-6 ((,class (:foreground ,teal :weight bold))))
   `(org-level-7 ((,class (:foreground ,cyan-dim :weight bold))))
   `(org-level-8 ((,class (:foreground ,green :weight bold))))
   `(org-document-title ((,class (:foreground ,cyan :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,fg-alt))))
   `(org-document-info-keyword ((,class (:foreground ,fg-dim))))
   `(org-meta-line ((,class (:foreground ,fg-dim))))
   `(org-link ((,class (:foreground ,cyan :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green-br :weight bold))))
   `(org-date ((,class (:foreground ,blue-br))))
   `(org-tag ((,class (:foreground ,fg-alt :weight normal))))
   `(org-block ((,class (:background ,bg-alt :foreground ,fg :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(org-block-end-line ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(org-code ((,class (:foreground ,yellow :background ,bg-alt))))
   `(org-verbatim ((,class (:foreground ,cyan :background ,bg-alt))))
   `(org-special-keyword ((,class (:foreground ,fg-dim))))
   `(org-table ((,class (:foreground ,blue-br))))

   ;; Markdown
   `(markdown-header-face-1 ((,class (:foreground ,cyan :weight bold :height 1.3))))
   `(markdown-header-face-2 ((,class (:foreground ,green-br :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground ,yellow :weight bold :height 1.1))))
   `(markdown-header-face-4 ((,class (:foreground ,blue-br :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,magenta :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,teal :weight bold))))
   `(markdown-code-face ((,class (:foreground ,yellow :background ,bg-alt))))
   `(markdown-inline-code-face ((,class (:foreground ,yellow :background ,bg-alt))))
   `(markdown-pre-face ((,class (:foreground ,fg :background ,bg-alt))))
   `(markdown-link-face ((,class (:foreground ,cyan :underline t))))
   `(markdown-url-face ((,class (:foreground ,blue-br :underline t))))
   `(markdown-list-face ((,class (:foreground ,green-br))))

   ;; Company
   `(company-tooltip ((,class (:background ,bg-hl :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,ui :foreground ,selection :weight bold))))
   `(company-tooltip-common ((,class (:foreground ,cyan :weight bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,cyan :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-alt))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,fg-alt))))
   `(company-scrollbar-bg ((,class (:background ,bg-hl))))
   `(company-scrollbar-fg ((,class (:background ,ui))))
   `(company-preview ((,class (:foreground ,fg-dim :background ,bg-alt))))
   `(company-preview-common ((,class (:foreground ,cyan :background ,bg-alt))))

   ;; Ivy/Counsel
   `(ivy-current-match ((,class (:background ,ui :foreground ,selection :weight bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,fg-alt))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,cyan :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green-br :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,yellow :weight bold))))
   `(ivy-confirm-face ((,class (:foreground ,green-br))))
   `(ivy-match-required-face ((,class (:foreground ,red))))

   ;; Helm
   `(helm-header ((,class (:background ,bg-alt :foreground ,fg))))
   `(helm-source-header ((,class (:background ,bg-hl :foreground ,cyan :weight bold :height 1.1))))
   `(helm-selection ((,class (:background ,ui :foreground ,selection :weight bold))))
   `(helm-match ((,class (:foreground ,cyan :weight bold))))
   `(helm-candidate-number ((,class (:foreground ,fg-alt))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,cyan :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-alt))))
   `(magit-branch-local ((,class (:foreground ,green-br :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,blue-br :weight bold))))
   `(magit-tag ((,class (:foreground ,yellow))))
   `(magit-hash ((,class (:foreground ,fg-alt))))
   `(magit-diff-file-heading ((,class (:foreground ,fg :weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-alt :foreground ,fg :weight bold))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-hl :foreground ,fg-alt))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,ui :foreground ,fg))))
   `(magit-diff-context ((,class (:foreground ,fg-alt))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt :foreground ,fg))))
   `(magit-diff-added ((,class (:foreground ,green-br :background ,bg))))
   `(magit-diff-added-highlight ((,class (:foreground ,green-br :background ,bg-alt))))
   `(magit-diff-removed ((,class (:foreground ,red :background ,bg))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background ,bg-alt))))
   `(magit-diffstat-added ((,class (:foreground ,green-br))))
   `(magit-diffstat-removed ((,class (:foreground ,red))))

   ;; Diff mode
   `(diff-added ((,class (:foreground ,green-br :background ,bg))))
   `(diff-removed ((,class (:foreground ,red :background ,bg))))
   `(diff-changed ((,class (:foreground ,yellow :background ,bg))))
   `(diff-header ((,class (:background ,bg-hl :foreground ,fg))))
   `(diff-file-header ((,class (:background ,ui :foreground ,fg :weight bold))))
   `(diff-hunk-header ((,class (:background ,bg-hl :foreground ,fg-alt))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,cyan)))))

   ;; Flymake
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flymake-note ((,class (:underline (:style wave :color ,cyan)))))

   ;; LSP
   `(lsp-face-highlight-textual ((,class (:background ,bg-hl))))
   `(lsp-face-highlight-read ((,class (:background ,bg-hl))))
   `(lsp-face-highlight-write ((,class (:background ,ui :weight bold))))

   ;; Tree-sitter
   `(tree-sitter-hl-face:function ((,class (:foreground ,yellow))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,yellow))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,yellow))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,yellow))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,blue-br))))
   `(tree-sitter-hl-face:variable.parameter ((,class (:foreground ,blue-br))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,blue))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,green-br :weight bold))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,cyan))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,magenta))))
   `(tree-sitter-hl-face:constant ((,class (:foreground ,magenta))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,blue))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,fg-dim :slant italic))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,cyan :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,fg-dim))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-group-description-face ((,class (:foreground ,green-br))))

   ;; Vertico
   `(vertico-current ((,class (:background ,ui :foreground ,selection :weight bold))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,cyan :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,green-br :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,yellow :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,magenta :weight bold))))

   ;; Corfu
   `(corfu-default ((,class (:background ,bg-hl :foreground ,fg))))
   `(corfu-current ((,class (:background ,ui :foreground ,selection :weight bold))))
   `(corfu-bar ((,class (:background ,ui-alt))))
   `(corfu-border ((,class (:background ,border))))

   ;; Marginalia
   `(marginalia-key ((,class (:foreground ,cyan))))
   `(marginalia-documentation ((,class (:foreground ,fg-alt))))
   `(marginalia-file-name ((,class (:foreground ,fg))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green-br))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,blue-br))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,magenta))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,teal))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,cyan-dim))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))

   ;; Clojure specific
   `(clojure-keyword-face ((,class (:foreground ,magenta))))

   ;; CIDER
   `(cider-result-overlay-face ((,class (:background ,bg-hl :foreground ,fg))))
   `(cider-repl-prompt-face ((,class (:foreground ,cyan :weight bold))))
   `(cider-repl-stdout-face ((,class (:foreground ,fg))))
   `(cider-repl-stderr-face ((,class (:foreground ,red))))
   `(cider-test-failure-face ((,class (:foreground ,red :weight bold))))
   `(cider-test-success-face ((,class (:foreground ,green-br :weight bold))))

   ;; Terminal
   `(term-color-black ((,class (:foreground ,bg-hl :background ,bg-hl))))
   `(term-color-red ((,class (:foreground ,red :background ,red))))
   `(term-color-green ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((,class (:foreground ,white :background ,white))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'osaka-jade)

;;; osaka-jade-theme.el ends here
