;;; osaka-jade-theme.el --- Osaka Jade, derived from Modus  -*- lexical-binding: t; -*-

;; Author: Ovidiu Stoica <ovidiu.stoica1094@gmail.com>
;; URL: https://github.com/ovistoica/omarchy
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (modus-themes "4.4"))
;; Keywords: faces, theme

;;; Commentary:
;;
;; Osaka Jade for Emacs, derived from Modus Vivendi via
;; `modus-themes-theme'.  Mirrors the Neovim `bamboo.nvim' (vulgaris)
;; highlight mapping that Omarchy uses as the Osaka Jade Neovim theme,
;; while keeping the terminal's bg/fg from Omarchy's colors.toml so
;; Emacs, the terminal, and other system surfaces render consistently.
;;
;; Key palette correspondences (bamboo vulgaris):
;;   fg           #C1C497   default foreground (from Omarchy)
;;   bg           #111c18   default background (from Omarchy)
;;   bg_yellow    #e2c792   comments
;;   green        #8fb573   strings
;;   orange       #ff9966   constants, numbers, booleans
;;   yellow       #dbb651   types, constructors
;;   blue         #57a5e5   functions (def + call)
;;   cyan         #70c2be   @variable.member, @property
;;   purple       #aaaaff   keywords, conditionals, statements
;;   bright_purple #df73ff  macros
;;   red          #e75a7c   identifiers, @variable.builtin, errors
;;   coral        #f08080   @variable.parameter, @string.escape
;;   light_grey   #838781   delimiters, punctuation
;;   light_blue   #96c7ef   @module / namespaces
;;   diff_add     #40531b   diff backgrounds
;;   diff_delete  #893f45
;;
;; Because this theme uses the Modus face-generation machinery, it
;; covers the full Modus face catalog (magit, org, eglot, tree-sitter,
;; corfu, vertico, etc.) without enumerating faces by hand.

;;; Code:

(eval-and-compile
  (require 'modus-themes))

(defconst osaka-jade-palette-partial
  '(;; ---- Core surfaces (from Omarchy colors.toml) ----
    (bg-main       "#111c18")  ; background
    (bg-dim        "#1a2822")  ; slightly lifted
    (bg-alt        "#23372B")  ; color0 / selection-like
    (bg-active     "#2a3f35")  ; borders / selected
    (bg-inactive   "#141f1b")  ; deeper
    (border        "#53685B")  ; color8

    ;; ---- Foregrounds ----
    (fg-main       "#f1e9d2")  ; foreground (Omarchy)
    (fg-dim        "#838781")  ; bamboo light_grey (muted)
    (fg-alt        "#e2c792")  ; bamboo bg_yellow (comments)
    (cursor        "#D7C995")  ; Omarchy cursor

    ;; ---- Named bamboo slots ----
    (bamboo-green         "#8fb573")
    (bamboo-orange        "#ff9966")
    (bamboo-yellow        "#dbb651")
    (bamboo-blue          "#57a5e5")
    (bamboo-light-blue    "#96c7ef")
    (bamboo-cyan          "#70c2be")
    (bamboo-purple        "#aaaaff")
    (bamboo-bright-purple "#df73ff")
    (bamboo-light-purple  "#c9bde0")  ; blend(purple, fg, 0.375) — operator color
    (bamboo-red           "#e75a7c")
    (bamboo-coral         "#f08080")
    (bamboo-light-grey    "#838781")
    (bamboo-bg-yellow     "#e2c792")
    (bamboo-doc-green     "#b1cc99")  ; blend(green, white, 0.25) — @string.documentation

    ;; ---- Modus primary color slots ----
    (red           "#e75a7c")  ; bamboo red
    (red-warmer    "#f08080")  ; coral
    (red-cooler    "#db9f9c")
    (red-faint     "#db9f9c")
    (red-intense   "#FF5345")
    (green         "#8fb573")  ; bamboo green (strings)
    (green-warmer  "#63b07a")
    (green-cooler  "#70c2be")  ; bamboo cyan
    (green-faint   "#549e6a")
    (green-intense "#9eebb3")
    (yellow        "#dbb651")  ; bamboo yellow
    (yellow-warmer "#ff9966")  ; bamboo orange
    (yellow-cooler "#e2c792")  ; bg_yellow
    (yellow-faint  "#E5C736")
    (yellow-intense "#dbb651")
    (blue          "#57a5e5")  ; bamboo blue
    (blue-warmer   "#aaaaff")  ; bamboo purple
    (blue-cooler   "#96c7ef")  ; light_blue
    (blue-faint    "#75bbb3")
    (blue-intense  "#57a5e5")
    (magenta       "#aaaaff")  ; purple (keywords use magenta slot in some specs)
    (magenta-warmer "#df73ff") ; bright_purple
    (magenta-cooler "#aaaaff")
    (magenta-faint "#D2689C")
    (magenta-intense "#df73ff")
    (cyan          "#70c2be")  ; bamboo cyan
    (cyan-warmer   "#ACD4CF")
    (cyan-cooler   "#8CD3CB")
    (cyan-faint    "#70c2be")
    (cyan-intense  "#2DD5B7")

    ;; ---- Diff backgrounds (bamboo vulgaris) ----
    (bg-added            "#40531b")
    (bg-added-faint      "#2f3e14")
    (bg-added-refine     "#557128")
    (bg-added-intense    "#6b8e32")
    (fg-added            "#b1cc99")
    (fg-added-intense    "#c8dcae")

    (bg-removed          "#893f45")
    (bg-removed-faint    "#5a2a30")
    (bg-removed-refine   "#a5545a")
    (bg-removed-intense  "#c26870")
    (fg-removed          "#f5b8bc")
    (fg-removed-intense  "#ffd1d5")

    (bg-changed          "#2a3a57")
    (bg-changed-faint    "#1f2a42")
    (bg-changed-refine   "#3a4a67")
    (bg-changed-intense  "#4a5b7c")
    (fg-changed          "#b3c4df")
    (fg-changed-intense  "#d0deef"))
  "Osaka Jade base colors, in Modus palette format.
Blends Omarchy's colors.toml surfaces with bamboo.nvim's vulgaris
syntax palette.")

(defconst osaka-jade-palette-mappings-partial
  '(;; ---- Syntax (matches bamboo.nvim vulgaris) ----
    (keyword         bamboo-purple)         ; Keyword, Conditional, Statement, PreProc, Include
    (builtin         bamboo-bright-purple)  ; Macro, @function.macro
    (constant        bamboo-orange)         ; Constant, Number, Boolean, Character, Float
    (fnname          bamboo-blue)           ; Function (def)
    (fnname-call     bamboo-blue)           ; Function calls
    (name            bamboo-blue)
    (type            bamboo-yellow)         ; Type, @constructor, @type
    (variable        fg-main)               ; @variable -> fg
    (variable-use    fg-main)
    (identifier      bamboo-red)            ; Identifier, @variable.builtin
    (property        bamboo-cyan)           ; @property, @variable.member, @field
    (property-use    bamboo-cyan)
    (string          bamboo-green)          ; String
    (docstring       bamboo-doc-green)      ; @string.documentation
    (comment         bamboo-light-grey)      ; Comment -> warm yellow, italic
    (preprocessor    bamboo-purple)         ; PreProc = Keyword
    (operator        bamboo-light-purple)   ; Operator
    (punctuation     bamboo-light-grey)     ; Delimiter
    (rx-construct    bamboo-bright-purple)
    (rx-backslash    bamboo-coral)          ; @string.escape

    ;; ---- Status / diagnostics ----
    (err             bamboo-red)
    (warning         bamboo-yellow)
    (info            bamboo-blue)
    (note            bamboo-cyan)
    (success         bamboo-green)

    ;; ---- Mode line ----
    (bg-mode-line-active       bg-alt)
    (fg-mode-line-active       fg-main)
    (border-mode-line-active   bg-active)
    (bg-mode-line-inactive     bg-main)
    (fg-mode-line-inactive     fg-dim)
    (border-mode-line-inactive bg-dim)
    (modeline-err              bamboo-red)
    (modeline-warning          bamboo-yellow)
    (modeline-info             bamboo-blue)

    ;; ---- Line numbers ----
    (fg-line-number-inactive   border)
    (fg-line-number-active     fg-main)
    (bg-line-number-inactive   bg-main)
    (bg-line-number-active     bg-alt)

    ;; ---- Region / highlight / search ----
    (bg-region                 bg-active)
    (fg-region                 fg-main)
    (bg-hl-line                bg-dim)
    (bg-paren-match            bg-active)
    (fg-paren-match            bamboo-orange)
    (bg-search-current         bamboo-bg-yellow)
    (bg-search-lazy            bg-active)

    ;; ---- Completion / popups ----
    (bg-completion             bg-alt)
    (bg-hover                  bg-active)
    (bg-hover-secondary        bg-alt)

    ;; ---- Links / prompts ----
    (link                      bamboo-blue)
    (link-symbolic             bamboo-cyan)
    (cursor                    fg-main)
    (prompt                    bamboo-cyan)

    ;; ---- Headings (bamboo uses cyan for Title) ----
    (fg-heading-0              bamboo-cyan)
    (fg-heading-1              bamboo-cyan)
    (fg-heading-2              bamboo-blue)
    (fg-heading-3              bamboo-green)
    (fg-heading-4              bamboo-yellow)
    (fg-heading-5              bamboo-purple)
    (fg-heading-6              bamboo-orange)
    (fg-heading-7              bamboo-red)
    (fg-heading-8              bamboo-coral))
  "Semantic slot mappings for Osaka Jade.
Mirrors the bamboo.nvim vulgaris variant that Omarchy uses for
Neovim, so both editors render the same code near-identically.")

(defconst osaka-jade-palette
  (modus-themes-generate-palette
   osaka-jade-palette-partial
   nil                              ; cool-or-warm — infer from bg-main
   modus-themes-vivendi-palette     ; dark base for unmapped slots
   osaka-jade-palette-mappings-partial)
  "Complete Osaka Jade palette for use with `modus-themes-theme'.")

(defcustom osaka-jade-palette-overrides nil
  "User-level palette overrides for the Osaka Jade theme.
Same format as `modus-themes-common-palette-overrides'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'modus-themes)

;; Extra faces: match bamboo.nvim behaviour where Variable / @variable
;; are plain fg, not italic.  Also scale org headings.
(defvar osaka-jade-custom-faces
  '(`(font-lock-variable-name-face ((,c :foreground ,fg-main :slant normal)))
    `(font-lock-variable-use-face  ((,c :foreground ,fg-main :slant normal)))
    `(help-argument-name           ((,c :foreground ,fg-main :slant normal))))
  "Additional face specs layered on top of the Modus-generated faces.")

(defvar osaka-jade-custom-variables nil
  "Custom-variable specs layered on top of Modus defaults.")

(modus-themes-theme
 'osaka-jade
 'omarchy-themes
 "Osaka Jade, derived from Modus Vivendi."
 'dark
 'modus-themes-vivendi-palette
 'osaka-jade-palette
 'osaka-jade-palette-overrides
 'osaka-jade-custom-faces
 'osaka-jade-custom-variables)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'osaka-jade-theme)
;;; osaka-jade-theme.el ends here
