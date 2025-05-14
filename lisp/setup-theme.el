;;; setup-theme.el --- Theme & font setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defvar os/load-theme-family 'modus)

(use-package font
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (defun setup-fonts ()
    (cond ((font-installed-p "JetBrainsMono Nerd Font Mono")
           (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono"))
          ((font-installed-p "Source Code Pro")
           (set-face-attribute 'default nil :font "Source Code Pro")))
    (when (font-installed-p "DejaVu Sans")
      (set-face-attribute 'variable-pitch nil :font "DejaVu Sans")))
  (provide 'font))

(use-package hl-todo
  :straight '(hl-todo :type git :host github :repo "tarsius/hl-todo")
  :init (global-hl-todo-mode))

(use-package highlight-indent-guides
  :diminish ""
  :straight '(highlight-indent-guides :type git :host github :repo "DarthFennec/highlight-indent-guides")
  :defines
  highlight-indent-guides-method
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)
  :hook
  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (("C-x l" . pulsar-pulse-line)        ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
  :config
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 2
           :right-divider-width 1
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active ,(if (or (eq os/load-theme-family 'modus)
                                      (eq os/load-theme-family 'standard))
                                  'default
                                'help-key-binding)
           :mode-line-inactive window-divider)))


(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-mode))

(use-package modus-themes
  :ensure t
  :defines
  modus-vivendi-tinted-palette-overrides
  modus-operandi-palette-overrides
  :after fontaine
  :commands
  modus-themes-load-theme
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . modus-themes-select))
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-vivendi-tinted-palette-overrides
        '(
          (bg-main "#1b1e26")
          (fg-main "#f0f0f0")))

  (setq modus-operandi-palette-overrides
        `(
          (builtin magenta-warmer)
          (keyword blue)
          (string green-intense)))

  (setq modus-themes-common-palette-overrides
        `(
          ;; From the section "Make the mode line borderless"
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; From the section "Make matching parenthesis more or less intense"
          (bg-paren-match bg-magenta-intense)
          (underline-paren-match fg-main)

          (comment yellow-faint)
          (string green-warmer)


          ,@modus-themes-preset-overrides-warmer))
  (load-theme 'modus-vivendi-tinted :no-confirm))



(use-package solar
  :config
  (setq calendar-latitude 44.426765
        calendar-longitude 26.102537))

(use-package circadian
  :ensure t
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi-tinted)))
  :hook (after-init . circadian-setup))


(use-package ef-themes
  :ensure t)

;; * FONT CONFIGURATIONS
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook
  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   ;; Set last preset or fall back to desired style from `fontaine-presets'.
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :bind ("C-c f" . fontaine-set-preset)
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-height 120)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-height 130)
          (large
           :inherit medium
           :default-height 140)
          (extra-large
           :inherit large
           :default-height 160)
          (live-stream
           :default-family "JetBrains Mono"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "JetBrains Mono"
           :variable-pitch-family "JetBrains Mono"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "JetBrains Mono"
           :default-weight regular
           :default-slant normal
           :default-height 120

           :fixed-pitch-family "JetBrains Mono"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "JetBrains Mono"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil

           :tab-line-height 1.0
           :bold-family nil
           :bold-weight extra-bold
           :bold-slant nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-height 1.0

           :line-spacing nil)))
  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

(use-package face-remap
  :ensure nil
  :functions os/enable-variable-pitch
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . os/enable-variable-pitch)
  :config
  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun os/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
;;;;; Resize keys with global effect
  :bind
  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))


(provide 'setup-theme)
;;; setup-theme.el ends here
