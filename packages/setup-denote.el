;;; setup-notetaking.el ---  Setup for note taking -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;; Denote (simple note-taking and file-naming)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(use-package denote
  :ensure t
  :hook

  ;; Highlight Denote file names in Dired buffers.  Below is the
  ;; generic approach, which is great if you rename files Denote-style
  ;; in lots of places as I do.
  ;;
  ;; If you only want the `denote-dired-mode' in select directories,
  ;; then modify the variable `denote-dired-directories' and use the
  ;; following instead:
  ;;
  ;;  (dired-mode . denote-dired-mode-in-directories)
  ((dired-mode . denote-dired-mode)

   ;; If you use Markdown or plain text files you want to fontify links
   ;; upon visiting the file (Org renders links as buttons right away).
   (text-mode . denote-fontify-links-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n N" . denote-type)
    ("C-c n d" . denote-date)
    ("C-c n z" . denote-signature)      ; "zettelkasten" mnemonic
    ("C-c n s" . denote-subdirectory)
    ("C-c n o" . denote-sort-dired)     ; "order" mnemonic
    ("C-c n j" . denote-journal-extras-new-entry)
    ("C-c n J" . denote-journal-extras-new-or-existing-entry)
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    ("C-c n r" . denote-rename-file)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for
    ;; `org-mode-map', `markdown-mode-map', and/or `text-mode-map'.
    :map text-mode-map
    ("C-c n i" . denote-link)           ; "insert" mnemonic
    ("C-c n I" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n f f" . denote-find-link)
    ("C-c n f b" . denote-find-backlink)
    ;; Also see `denote-rename-file' further above.
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; I do not bind the Org dynamic blocks, but they are useful:
    ;;
    ;; - `denote-org-extras-dblock-insert-links'
    ;; - `denote-org-extras-dblock-insert-backlinks'
    ;; - `denote-org-extras-dblock-insert-files'
    ;; - `denote-org-extras-dblock-insert-missing-links'

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-link-dired-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-marked-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-file-type 'org)   ; Org is the default, set others here like I do
  ;; If you want to have a "controlled vocabulary" of keywords,
  ;; meaning that you only use a predefined set of them, then you want
  ;; `denote-infer-keywords' to be nil and `denote-known-keywords' to
  ;; have the keywords you need.
  (setq denote-known-keywords '("emacs" "journal" "book" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp nil)
  (setq denote-date-format nil)         ; read its doc string
  (setq denote-rename-no-confirm t)
  (setq denote-backlinks-show-context nil)
  (setq denote-rename-buffer-format "[D] %t")

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setq denote-journal-extras-directory nil)    ; use the `denote-directory'
  (setq denote-journal-extras-title-format nil) ; always prompt for title
  (setq denote-journal-extras-keyword "journal")

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))

    ;; This prompts for TITLE, KEYWORDS, and SUBDIRECTORY
    (add-to-list 'org-capture-templates
                 '("N" "New note with prompts (with denote.el)" plain
                   (file denote-last-path)
                   (function
                    (lambda ()
                      (denote-org-capture-with-prompts :title :keywords :signature)))
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))


(provide 'setup-denote)
;;; setup-notetaking.el ends here
