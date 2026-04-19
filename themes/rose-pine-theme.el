;;; rose-pine-theme.el --- Rose Pine Dawn, derived from Modus  -*- lexical-binding: t; -*-

;; Author: Ovidiu Stoica <ovidiu.stoica1094@gmail.com>
;; URL: https://github.com/ovistoica/omarchy
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (modus-themes "4.4"))
;; Keywords: faces, theme

;;; Commentary:
;;
;; Rose Pine Dawn for Emacs, derived from Modus Operandi via
;; `modus-themes-theme'.  Mappings mirror the Rose Pine Neovim plugin as
;; closely as possible so Emacs and Neovim render code consistently.
;;
;; Rose Pine Dawn palette (from palette.lua):
;;   base    #faf4ed   bg-main
;;   surface #fffaf3   ui panels
;;   overlay #f2e9e1   hl-line / mode-line
;;   text    #575279   default fg, variables
;;   subtle  #797593   comments, operators, delimiters
;;   muted   #9893a5   borders, inactive text
;;   love    #b4637a   errors, git delete
;;   rose    #d7827e   functions, booleans, methods
;;   pine    #286983   keywords, conditionals, imports
;;   foam    #56949f   types, properties, fields
;;   iris    #907aa9   macros, parameters, attributes
;;   gold    #ea9d34   strings, numbers, constants
;;   leaf    #6d8f89   accent (icons only in nvim)
;;
;; Because this theme uses the Modus face-generation machinery, it
;; covers the full Modus face catalog (magit, org, eglot, tree-sitter,
;; corfu, vertico, etc.) without enumerating faces by hand.

;;; Code:

(eval-and-compile
  (require 'modus-themes))

(defconst rose-pine-palette-partial
  '(;; Core surfaces
    (bg-main       "#faf4ed")  ; base
    (bg-dim        "#fffaf3")  ; surface
    (bg-alt        "#f2e9e1")  ; overlay
    (bg-active     "#dfdad9")  ; highlight_med
    (bg-inactive   "#f4ede8")  ; highlight_low
    (border        "#cecacd")  ; highlight_high

    ;; Foregrounds
    (fg-main       "#575279")  ; text
    (fg-dim        "#9893a5")  ; muted
    (fg-alt        "#797593")  ; subtle
    (cursor        "#575279")  ; text — visible against paper bg

    ;; Named Rose Pine slots (kept distinct — don't collapse)
    (rose-love     "#b4637a")
    (rose-rose     "#d7827e")
    (rose-pine     "#286983")
    (rose-foam     "#56949f")
    (rose-iris     "#907aa9")
    (rose-gold     "#ea9d34")
    (rose-subtle   "#797593")
    (rose-muted    "#9893a5")
    (rose-leaf     "#6d8f89")

    ;; Modus primary color slots — mapped to Rose Pine equivalents
    (red           "#b4637a")  ; love (errors)
    (red-warmer    "#d7827e")  ; rose
    (red-cooler    "#b4637a")
    (red-faint     "#d7827e")
    (red-intense   "#b4637a")
    (green         "#286983")  ; pine
    (green-warmer  "#286983")
    (green-cooler  "#56949f")  ; foam
    (green-faint   "#56949f")
    (green-intense "#286983")
    (yellow        "#ea9d34")  ; gold
    (yellow-warmer "#ea9d34")
    (yellow-cooler "#d7827e")
    (yellow-faint  "#ea9d34")
    (yellow-intense "#ea9d34")
    (blue          "#56949f")  ; foam
    (blue-warmer   "#907aa9")  ; iris
    (blue-cooler   "#286983")  ; pine
    (blue-faint    "#56949f")
    (blue-intense  "#286983")
    (magenta       "#907aa9")  ; iris
    (magenta-warmer "#b4637a") ; love
    (magenta-cooler "#907aa9")
    (magenta-faint "#907aa9")
    (magenta-intense "#b4637a")
    (cyan          "#56949f")  ; foam
    (cyan-warmer   "#56949f")
    (cyan-cooler   "#286983")
    (cyan-faint    "#56949f")
    (cyan-intense  "#56949f")

    ;; Diff backgrounds — tuned to read as green/red on paper bg
    (bg-added            "#d6e6cf")
    (bg-added-faint      "#e3efdf")
    (bg-added-refine     "#c2d9b5")
    (bg-added-intense    "#a9c79a")
    (fg-added            "#3e5c2a")
    (fg-added-intense    "#2f4720")

    (bg-removed          "#f2d9d9")
    (bg-removed-faint    "#f7e6e6")
    (bg-removed-refine   "#e9c3c3")
    (bg-removed-intense  "#d89999")
    (fg-removed          "#8a1f3a")
    (fg-removed-intense  "#6b1228")

    (bg-changed          "#f3e3c2")
    (bg-changed-faint    "#f7edd5")
    (bg-changed-refine   "#ecd091")
    (bg-changed-intense  "#d9b760")
    (fg-changed          "#7a5a10")
    (fg-changed-intense  "#5c4000"))
  "Rose Pine Dawn base colors, in Modus palette format.
Unspecified entries are filled in by `modus-themes-generate-palette'
from `modus-themes-operandi-palette'.")

(defconst rose-pine-palette-mappings-partial
  '(;; ---- Syntax (matches rose-pine/neovim lua/rose-pine.lua) ----
    (keyword         rose-pine)     ; Keyword, Conditional, Include, Exception
    (builtin         rose-iris)     ; Macro, Define, PreCondit
    (constant        rose-gold)     ; Constant, Character, Number, Float
    (fnname          rose-rose)     ; Function (definitions)
    (fnname-call     rose-rose)     ; Function calls (font-lock-function-call-face)
    (name            rose-rose)     ; general "name" slot
    (type            rose-foam)     ; Type, TypeDef, Structure, Tag
    (variable        fg-main)       ; @variable -> text
    (variable-use    fg-main)       ; usages too
    (identifier      fg-main)       ; Identifier
    (property        rose-foam)     ; @property, @field -> foam
    (property-use    rose-foam)
    (string          rose-gold)     ; String, @string
    (docstring       rose-gold)     ; docstrings rendered like strings
    (comment         rose-subtle)   ; Comment -> subtle, italic
    (preprocessor    rose-iris)     ; PreProc, @keyword.directive
    (operator        rose-subtle)   ; Operator, @operator
    (punctuation     rose-subtle)   ; Delimiter, @punctuation.*
    (rx-construct    rose-iris)     ; regex constructs
    (rx-backslash    rose-pine)     ; @string.escape

    ;; ---- Status / diagnostics ----
    (err             rose-love)     ; error
    (warning         rose-gold)     ; warn
    (info            rose-foam)     ; info
    (note            rose-pine)     ; note
    (success         rose-leaf)     ; ok/success

    ;; ---- Mode line ----
    (bg-mode-line-active       bg-alt)
    (fg-mode-line-active       fg-main)
    (border-mode-line-active   border)
    (bg-mode-line-inactive     bg-main)
    (fg-mode-line-inactive     fg-dim)
    (border-mode-line-inactive bg-active)
    (modeline-err              rose-love)
    (modeline-warning          rose-gold)
    (modeline-info             rose-foam)

    ;; ---- Line numbers ----
    (fg-line-number-inactive   fg-dim)
    (fg-line-number-active     fg-main)
    (bg-line-number-inactive   bg-main)
    (bg-line-number-active     bg-alt)

    ;; ---- Region / highlight / search ----
    (bg-region                 bg-active)
    (fg-region                 fg-main)
    (bg-hl-line                bg-alt)
    (bg-paren-match            bg-active)
    (fg-paren-match            rose-pine)
    (bg-search-current         rose-gold)
    (bg-search-lazy            bg-alt)

    ;; ---- Completion / popups ----
    (bg-completion             bg-alt)
    (bg-hover                  bg-active)
    (bg-hover-secondary        bg-alt)

    ;; ---- Links / prompts ----
    (link                      rose-iris)
    (link-symbolic             rose-foam)
    (cursor                    fg-main)
    (prompt                    rose-foam)

    ;; ---- Headings (markdown/org — match Rose Pine Neovim h1..h6) ----
    (fg-heading-0              rose-iris)
    (fg-heading-1              rose-iris)
    (fg-heading-2              rose-foam)
    (fg-heading-3              rose-rose)
    (fg-heading-4              rose-gold)
    (fg-heading-5              rose-pine)
    (fg-heading-6              rose-leaf)
    (fg-heading-7              rose-foam)
    (fg-heading-8              rose-rose))
  "Semantic slot mappings for Rose Pine.
Mirrors the Rose Pine Neovim plugin's palette.groups + highlight
specification so both editors render the same code near-identically.")

(defconst rose-pine-palette
  (modus-themes-generate-palette
   rose-pine-palette-partial
   nil                              ; cool-or-warm — infer
   modus-themes-operandi-palette    ; base for unmapped slots
   rose-pine-palette-mappings-partial)
  "Complete Rose Pine palette for use with `modus-themes-theme'.")

(defcustom rose-pine-palette-overrides nil
  "User-level palette overrides for the Rose Pine theme.
Same format as `modus-themes-common-palette-overrides'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'modus-themes)

;; Extra faces: Rose Pine italicizes comments/docstrings but NOT
;; variables.  Modus's `modus-themes-italic-constructs' makes
;; `modus-themes-slant' italic, which then inherits into both via
;; `help-argument-name' / `font-lock-variable-*'.  We override just the
;; variable faces back to upright.  Org heading scaling is Emacs-only.
(defvar rose-pine-custom-faces
  '(`(font-lock-variable-name-face ((,c :foreground ,fg-main :slant normal)))
    `(font-lock-variable-use-face  ((,c :foreground ,fg-main :slant normal)))
    `(help-argument-name            ((,c :foreground ,fg-main :slant normal))))
  "Additional face specs layered on top of the Modus-generated faces.")

(defvar rose-pine-custom-variables nil
  "Custom-variable specs layered on top of Modus defaults.")

(modus-themes-theme
 'rose-pine
 'omarchy-themes
 "Rose Pine Dawn, derived from Modus Operandi."
 'light
 'modus-themes-operandi-palette
 'rose-pine-palette
 'rose-pine-palette-overrides
 'rose-pine-custom-faces
 'rose-pine-custom-variables)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'rose-pine-theme)
;;; rose-pine-theme.el ends here
