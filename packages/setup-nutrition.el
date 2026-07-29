;;; setup-nutrition.el --- Nutrition tracker integration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Wiring for the bb-backed nutrition tracker in ~/Dropbox/org/nutrition.
;;; Deliberately lazy: nutrition.el is loaded the first time a command, a
;;; capture template or the `C-c N' prefix is used — never at startup.

;;; Code:

(add-to-list 'load-path (expand-file-name "~/Dropbox/org/nutrition/elisp"))

(autoload 'nutrition-log-weight "nutrition" "Log today's weight." t)
(autoload 'nutrition-log-calories "nutrition" "Log today's calories." t)
(autoload 'nutrition-log-measurements "nutrition" "Log waist and neck." t)
(autoload 'nutrition-status "nutrition" "Show today's status." t)
(autoload 'nutrition-week "nutrition" "Show the weekly report." t)
(autoload 'nutrition-dashboard "nutrition" "Generate and open the dashboard." t)
(autoload 'nutrition-goto-today "nutrition" "Jump to today's log headline." t)
(autoload 'nutrition-normalize "nutrition" "Drain inbox.org into the log." t)
(autoload 'nutrition-capture-target "nutrition")
(autoload 'nutrition-capture-weight "nutrition")
(autoload 'nutrition-capture-calories "nutrition")
(autoload 'nutrition-capture-measurements "nutrition")
(declare-function nutrition--after-capture "nutrition")

;; `C-c N' for Nutrition — unclaimed in this config; the capital matches
;; `C-c I' (invoicing) as the convention for external-tool prefix maps.
;; The 'keymap autoload resolves through the symbol's function cell on first
;; use (see the matching `fset' in nutrition.el).
(autoload 'nutrition-command-map "nutrition" nil nil 'keymap)
(global-set-key (kbd "C-c N") 'nutrition-command-map)

;; Echo `bb status --brief' after the "h…" capture templates.  Guarded by
;; `featurep': if nutrition.el was never loaded, none of its captures ran.
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (when (featurep 'nutrition)
                (nutrition--after-capture)))))

(provide 'setup-nutrition)
;;; setup-nutrition.el ends here
