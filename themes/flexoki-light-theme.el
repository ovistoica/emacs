;;; flexoki-light-theme.el --- Flexoki light, derived from Modus  -*- lexical-binding: t; -*-

;; Author: Ovidiu Stoica <ovidiu.stoica1094@gmail.com>
;; URL: https://github.com/ovistoica/omarchy
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (modus-themes "4.4"))
;; Keywords: faces, theme

;;; Commentary:
;;
;; Flexoki Light for Emacs, derived from Modus Operandi Tinted via
;; `modus-themes-theme'.  Mirrors the canonical flexoki-emacs-theme
;; (https://codeberg.org/crmsnbleyd/flexoki-emacs-theme) face mapping
;; so Emacs renders code the same way as the reference implementation.
;;
;; Flexoki palette (from Steph Ango's design, https://stephango.com/flexoki):
;;   paper    #fffcf0   bg-main
;;   base-50  #f2f0e5   bg-dim
;;   base-100 #e6e4d9   ui / bg-alt
;;   base-150 #dad8ce   ui-2
;;   base-200 #cecdc3   ui-3 / region / show-paren bg
;;   base-300 #b7b5ac   hl-line / tx-3
;;   base-500 #878580   comments, muted text
;;   base-700 #575653   fg-alt
;;   base-800 #403e3c   fg-faint
;;   base-900 #232726   deep text
;;   black    #100f0f   default fg
;;   red      #af3029   errors, diff remove
;;   orange   #bc5215   function names
;;   yellow   #ad8301   types, warnings, paren match
;;   green    #66800b   builtins, diff add, success
;;   cyan     #24837b   strings
;;   blue     #205ea6   variables, types, diff changed
;;   purple   #5e409d   constants, minibuffer prompt
;;   magenta  #a02f6f   keywords
;;
;; Uses modus-operandi-tinted as the base since flexoki shares its paper
;; tinted background philosophy.  Because this theme uses the Modus
;; face-generation machinery, it covers the full Modus face catalog
;; (magit, org, eglot, tree-sitter, corfu, vertico, etc.) without
;; enumerating faces by hand.

;;; Code:

(eval-and-compile
  (require 'modus-themes))

(defconst flexoki-light-palette-partial
  '(;; Core surfaces
    (bg-main       "#fffcf0")  ; paper
    (bg-dim        "#f2f0e5")  ; base-50
    (bg-alt        "#e6e4d9")  ; base-100 / ui
    (bg-active     "#cecdc3")  ; base-200 / region
    (bg-inactive   "#f2f0e5")  ; base-50
    (border        "#dad8ce")  ; base-150

    ;; Foregrounds
    (fg-main       "#100f0f")  ; black
    (fg-dim        "#878580")  ; base-500
    (fg-alt        "#575653")  ; base-700
    (cursor        "#100f0f")  ; black

    ;; Named Flexoki slots
    (flex-red      "#af3029")
    (flex-orange   "#bc5215")
    (flex-yellow   "#ad8301")
    (flex-green    "#66800b")
    (flex-cyan     "#24837b")
    (flex-blue     "#205ea6")
    (flex-purple   "#5e409d")
    (flex-magenta  "#a02f6f")
    (flex-meek     "#878580")  ; base-500
    (flex-tx-3     "#b7b5ac")  ; base-300
    (flex-ui-2     "#dad8ce")  ; base-150

    ;; Modus primary color slots — mapped to Flexoki equivalents
    (red           "#af3029")
    (red-warmer    "#bc5215")  ; orange
    (red-cooler    "#af3029")
    (red-faint     "#af3029")
    (red-intense   "#af3029")
    (green         "#66800b")
    (green-warmer  "#66800b")
    (green-cooler  "#24837b")  ; cyan
    (green-faint   "#66800b")
    (green-intense "#66800b")
    (yellow        "#ad8301")
    (yellow-warmer "#bc5215")  ; orange
    (yellow-cooler "#ad8301")
    (yellow-faint  "#ad8301")
    (yellow-intense "#ad8301")
    (blue          "#205ea6")
    (blue-warmer   "#5e409d")  ; purple
    (blue-cooler   "#205ea6")
    (blue-faint    "#205ea6")
    (blue-intense  "#205ea6")
    (magenta       "#a02f6f")
    (magenta-warmer "#a02f6f")
    (magenta-cooler "#5e409d")  ; purple
    (magenta-faint "#a02f6f")
    (magenta-intense "#a02f6f")
    (cyan          "#24837b")
    (cyan-warmer   "#24837b")
    (cyan-cooler   "#205ea6")  ; blue
    (cyan-faint    "#24837b")
    (cyan-intense  "#24837b")

    ;; Diff backgrounds — tuned to read on paper bg
    (bg-added            "#dce6c4")
    (bg-added-faint      "#e8eed6")
    (bg-added-refine     "#c8d6a8")
    (bg-added-intense    "#b0c48c")
    (fg-added            "#4a5f09")
    (fg-added-intense    "#36470a")

    (bg-removed          "#efcfcc")
    (bg-removed-faint    "#f5dedb")
    (bg-removed-refine   "#e5b5b0")
    (bg-removed-intense  "#d59890")
    (fg-removed          "#8a241f")
    (fg-removed-intense  "#6b1915")

    (bg-changed          "#e6dfc0")
    (bg-changed-faint    "#eee9d3")
    (bg-changed-refine   "#d6cb94")
    (bg-changed-intense  "#c2b26a")
    (fg-changed          "#6d5300")
    (fg-changed-intense  "#523e00"))
  "Flexoki Light base colors, in Modus palette format.
Unspecified entries are filled in by `modus-themes-generate-palette'
from `modus-themes-operandi-tinted-palette'.")

(defconst flexoki-light-palette-mappings-partial
  '(;; ---- Syntax (matches crmsnbleyd/flexoki-emacs-theme) ----
    (keyword         flex-magenta)   ; Keyword, Conditional, Include
    (builtin         flex-green)     ; Builtin
    (constant        flex-purple)    ; Constant, Character, Number
    (fnname          flex-orange)    ; Function definitions
    (fnname-call     flex-orange)    ; Function calls
    (name            flex-orange)
    (type            flex-yellow)    ; Type, TypeDef, Structure
    (variable        flex-blue)      ; @variable
    (variable-use    flex-blue)      ; usages
    (identifier      flex-blue)      ; Identifier
    (property        flex-blue)      ; @property, @field
    (property-use    flex-blue)
    (string          flex-cyan)      ; String, @string
    (docstring       flex-cyan)      ; docstrings — muted + italic
    (comment         flex-meek)      ; Comment — base-500, italic
    (preprocessor    fg-main)        ; PreProc — strong black
    (operator        fg-main)        ; Operator
    (punctuation     fg-alt)         ; Delimiter
    (rx-construct    flex-purple)    ; regex constructs
    (rx-backslash    flex-purple)    ; escapes

    ;; ---- Status / diagnostics ----
    (err             flex-red)
    (warning         flex-yellow)
    (info            flex-blue)
    (note            flex-purple)
    (success         flex-green)

    ;; ---- Mode line ----
    (bg-mode-line-active       bg-alt)
    (fg-mode-line-active       fg-main)
    (border-mode-line-active   border)
    (bg-mode-line-inactive     bg-main)
    (fg-mode-line-inactive     fg-dim)
    (border-mode-line-inactive bg-active)
    (modeline-err              flex-red)
    (modeline-warning          flex-yellow)
    (modeline-info             flex-blue)

    ;; ---- Line numbers ----
    (fg-line-number-inactive   flex-tx-3)
    (fg-line-number-active     flex-purple)
    (bg-line-number-inactive   bg-main)
    (bg-line-number-active     bg-alt)

    ;; ---- Region / highlight / search ----
    (bg-region                 bg-active)
    (fg-region                 fg-main)
    (bg-hl-line                bg-dim)
    (bg-paren-match            bg-active)
    (fg-paren-match            flex-yellow)
    (bg-search-current         flex-yellow)
    (bg-search-lazy            bg-active)

    ;; ---- Completion / popups ----
    (bg-completion             bg-alt)
    (bg-hover                  bg-active)
    (bg-hover-secondary        bg-alt)

    ;; ---- Links / prompts ----
    (link                      flex-blue)
    (link-symbolic             flex-cyan)
    (cursor                    fg-main)
    (prompt                    flex-purple)

    ;; ---- Headings (match flexoki-emacs-theme outline-N) ----
    (fg-heading-0              flex-blue)
    (fg-heading-1              flex-blue)
    (fg-heading-2              flex-purple)
    (fg-heading-3              flex-orange)
    (fg-heading-4              flex-magenta)
    (fg-heading-5              flex-cyan)
    (fg-heading-6              flex-green)
    (fg-heading-7              flex-yellow)
    (fg-heading-8              flex-red))
  "Semantic slot mappings for Flexoki Light.
Mirrors the canonical flexoki-emacs-theme face specification so Emacs
renders source code consistently with the reference implementation.")

(defconst flexoki-light-palette
  (modus-themes-generate-palette
   flexoki-light-palette-partial
   nil                                      ; cool-or-warm — infer
   modus-themes-operandi-tinted-palette     ; tinted paper base
   flexoki-light-palette-mappings-partial)
  "Complete Flexoki Light palette for use with `modus-themes-theme'.")

(defcustom flexoki-light-palette-overrides nil
  "User-level palette overrides for the Flexoki Light theme.
Same format as `modus-themes-common-palette-overrides'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'modus-themes)

;; Extra faces: Flexoki italicizes only comments/docstrings (not
;; variables).  Modus's `modus-themes-italic-constructs' makes
;; `modus-themes-slant' italic, which then inherits into variable
;; faces via `help-argument-name' / `font-lock-variable-*'.  Override
;; just the variable faces back to upright.
(defvar flexoki-light-custom-faces
  '(`(font-lock-variable-name-face ((,c :foreground ,flex-blue :slant normal)))
    `(font-lock-variable-use-face  ((,c :foreground ,flex-blue :slant normal)))
    `(help-argument-name           ((,c :foreground ,flex-blue :slant normal))))
  "Additional face specs layered on top of the Modus-generated faces.")

(defvar flexoki-light-custom-variables nil
  "Custom-variable specs layered on top of Modus defaults.")

(modus-themes-theme
 'flexoki-light
 'omarchy-themes
 "Flexoki Light, derived from Modus Operandi Tinted."
 'light
 'modus-themes-operandi-tinted-palette
 'flexoki-light-palette
 'flexoki-light-palette-overrides
 'flexoki-light-custom-faces
 'flexoki-light-custom-variables)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'flexoki-light-theme)
;;; flexoki-light-theme.el ends here
