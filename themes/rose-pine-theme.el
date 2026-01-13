;;; rose-pine-theme.el --- Rose Pine (Dawn) theme for Emacs -*- lexical-binding: t; -*-

;; Author: Adapted from Omarchy Rose Pine theme
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme
;; URL: https://github.com/ovistoica/omarchy

;;; Commentary:
;;
;; Rose Pine Dawn-inspired light theme to match the Omarchy "rose-pine" theme.
;; Colors are taken from `themes/rose-pine/colors.toml` in the Omarchy repo so
;; that Emacs visually matches the rest of the system.
;;
;;; Code:

(deftheme rose-pine
  "Rose Pine Dawn theme, matching Omarchy's rose-pine colors.")

(let* ((class '((class color) (min-colors 89)))
       ;; Base palette from colors.toml
       (bg         "#faf4ed")   ; background
       (bg-alt     "#f2e9e1")   ; color0
       (bg-hl      "#dfdad9")   ; selection_background
       (fg         "#575279")   ; foreground
       (fg-alt     "#9893a5")   ; color8

       ;; Accents from colors.toml
       (rose       "#b4637a")   ; color1
       (pine       "#286983")   ; color2
       (gold       "#ea9d34")   ; color3
       (foam       "#56949f")   ; accent / color4
       (iris       "#907aa9")   ; color5
       (love       "#d7827e")   ; color6

       ;; Convenience mappings
       (cursor     "#cecacd")
       (border     "#e4ded7"))

  (custom-theme-set-faces
   'rose-pine

   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,bg-hl :foreground ,fg))))
   `(highlight ((,class (:background ,bg-alt))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(fringe ((,class (:background ,bg :foreground ,fg-alt))))
   `(vertical-border ((,class (:foreground ,border))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:foreground ,border))))
   `(window-divider-last-pixel ((,class (:foreground ,border))))

   ;; Mode line
   `(mode-line ((,class (:background ,bg-alt :foreground ,fg
                                     :box (:line-width 1 :color ,border))))
               )
   `(mode-line-inactive
     ((,class (:background ,bg :foreground ,fg-alt
                           :box (:line-width 1 :color ,bg-hl)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,foam))))
   `(mode-line-emphasis ((,class (:weight bold :foreground ,pine))))

   ;; Header line
   `(header-line ((,class (:background ,bg-alt :foreground ,fg-alt))))

   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,foam :weight bold))))

   ;; Font lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,iris))))
   `(font-lock-comment-face ((,class (:foreground ,fg-alt :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-alt :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,iris))))
   `(font-lock-doc-face ((,class (:foreground ,fg-alt :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,gold :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,pine :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,love))))
   `(font-lock-string-face ((,class (:foreground ,foam))))
   `(font-lock-type-face ((,class (:foreground ,foam))))
   `(font-lock-variable-name-face ((,class (:foreground ,iris))))
   `(font-lock-warning-face ((,class (:foreground ,rose :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,rose))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,iris))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,iris))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,fg-alt :background ,bg))))
   `(line-number-current-line
     ((,class (:foreground ,fg :background ,bg-alt :weight bold))))

   ;; Search
   `(isearch ((,class (:foreground ,bg :background ,gold :weight bold))))
   `(isearch-fail ((,class (:foreground ,rose :background ,bg-alt))))
   `(lazy-highlight ((,class (:foreground ,fg :background ,bg-hl))))

   ;; Links
   `(link ((,class (:foreground ,foam :underline t))))
   `(link-visited ((,class (:foreground ,iris :underline t))))

   ;; Buttons
   `(button ((,class (:foreground ,foam :underline t))))

   ;; Parenthesis matching
   `(show-paren-match ((,class (:foreground ,bg :background ,pine :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,bg :background ,rose :weight bold))))

   ;; Error/Warning/Success
   `(error ((,class (:foreground ,rose :weight bold))))
   `(warning ((,class (:foreground ,gold :weight bold))))
   `(success ((,class (:foreground ,pine :weight bold))))

   ;; Org mode (minimal but pleasant)
   `(org-level-1 ((,class (:foreground ,gold :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,foam :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,pine :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,iris :weight bold))))
   `(org-level-5 ((,class (:foreground ,love :weight bold))))
   `(org-level-6 ((,class (:foreground ,rose :weight bold))))
   `(org-level-7 ((,class (:foreground ,fg-alt :weight bold))))
   `(org-level-8 ((,class (:foreground ,fg :weight bold))))
   `(org-document-title ((,class (:foreground ,fg :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,fg-alt))))
   `(org-document-info-keyword ((,class (:foreground ,fg-alt))))
   `(org-block ((,class (:background ,bg-alt :foreground ,fg :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-hl :foreground ,fg-alt :extend t))))
   `(org-block-end-line ((,class (:background ,bg-hl :foreground ,fg-alt :extend t))))

   ;; Terminal 16-color palette
   `(term-color-black   ((,class (:foreground ,bg-alt   :background ,bg-alt))))
   `(term-color-red     ((,class (:foreground "#b4637a" :background "#b4637a")))) ; color1
   `(term-color-green   ((,class (:foreground "#286983" :background "#286983")))) ; color2
   `(term-color-yellow  ((,class (:foreground "#ea9d34" :background "#ea9d34")))) ; color3
   `(term-color-blue    ((,class (:foreground "#56949f" :background "#56949f")))) ; color4
   `(term-color-magenta ((,class (:foreground "#907aa9" :background "#907aa9")))) ; color5
   `(term-color-cyan    ((,class (:foreground "#d7827e" :background "#d7827e")))) ; color6
   `(term-color-white   ((,class (:foreground ,fg       :background ,fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rose-pine)

;;; rose-pine-theme.el ends here

