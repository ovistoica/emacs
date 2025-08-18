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

    ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((smaller
           :default-height 120)
          (small
           :default-height 130)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-height 140)
          (large
           :inherit medium
           :default-height 150)
          (extra-large
           :inherit large
           :default-height 160)
          (live-stream
           :default-height 170
           :default-weight medium
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-weight regular
           :default-slant normal
           :default-height 120

           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-height 1.0

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

           :line-spacing nil))))
