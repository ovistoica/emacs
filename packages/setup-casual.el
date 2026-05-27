;;; setup-casual.el --- Casual transient menus -*- lexical-binding: t; -*-

;;; Commentary:

;;; Menus for stuff you use rarely so you don't forget.
;;; All main menus are auto-loaded, so no extra `require' calls are needed.
;;; The default keybinding is C-o for all mode-specific maps (following the
;;; Casual convention), with M-m used for org-mode to avoid conflicts with
;;; EditKit's global C-o binding.

;;; Code:

(declare-function casual-editkit-main-tmenu "casual-editkit")

(require 'ibuffer)
(require 'dired)
(require 'info)
(require 'help-mode)
(require 'bookmark)
(require 'org)
(require 'org-agenda)
(require 'calc)

(use-package casual
  :ensure t

  :bind
  ;; IBuffer — manage buffers
  ((:map ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F"   . casual-ibuffer-filter-tmenu)
         ("s"   . casual-ibuffer-sortby-tmenu))

   ;; Dired — file manager
   (:map dired-mode-map
         ("C-o" . casual-dired-tmenu)
         ("s"   . casual-dired-sort-by-tmenu)
         ("/"   . casual-dired-search-replace-tmenu))

   ;; Info reader
   (:map Info-mode-map
         ("C-o" . casual-info-tmenu))

   ;; Help mode
   (:map help-mode-map
         ("C-o" . casual-help-tmenu))

   ;; Bookmarks list
   (:map bookmark-bmenu-mode-map
         ("C-o" . casual-bookmarks-tmenu))

   ;; Org mode — use M-m to keep C-o free for EditKit
   (:map org-mode-map
         ("M-m" . casual-org-tmenu))

   ;; Org Agenda
   (:map org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu))

   ;; Calc
   (:map calc-mode-map
         ("C-o" . casual-calc-tmenu)))

  :config
  ;; EditKit — global editing menus (C-o as a universal entry point)
  (keymap-global-set "C-o" #'casual-editkit-main-tmenu))

(provide 'setup-casual)
;;; setup-casual.el ends here
