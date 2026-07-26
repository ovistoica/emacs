;;; setup-pfa.el --- Invoicing for the PFA -*- lexical-binding: t; -*-

;;; Commentary:

;; Wires up the invoicing system that lives in ~/Dropbox/org/pfa/.
;;
;; Loading is lazy: nothing is required until an Org file inside that
;; directory is opened, at which point `pfa-mode' turns on and binds the
;; commands under `C-c f'.
;;
;; This is done with a hook rather than .dir-locals.el on purpose.  Directory
;; locals cannot add to `load-path', so they would need an `(eval . (progn
;; ...))' form, and Emacs prompts about unsafe eval forms every time such a
;; file is visited.  A hook is both quieter and easier to reason about.
;;
;; The prefix is `C-c I' for Invoice.  `C-c f' belongs to Fontaine and `C-c i'
;; to the agent; the money-flavoured punctuation keys are all taken by Org
;; itself inside these buffers (`C-c $' is `org-archive-subtree').
;;
;;   C-c I n   new invoice           C-c I l   list the registry
;;   C-c I r   render at point       C-c I t   tax estimate
;;   C-c I p   mark paid             C-c I x   BNR rate comparison
;;   C-c I g   go to invoice         C-c I v   validate

;;; Code:

(defvar pfa-project-directory (expand-file-name "~/Dropbox/org/pfa/")
  "Root of the invoicing system.")

(defvar pfa-elisp-directory (expand-file-name "elisp/" pfa-project-directory)
  "Where `pfa.el' and `pfa-export.el' live.")

(defun my/pfa-file-p ()
  "Return non-nil when the current buffer visits a file under the PFA root."
  (and buffer-file-name
       (string-prefix-p (expand-file-name pfa-project-directory)
                        (expand-file-name buffer-file-name))))

(defun my/pfa-maybe-enable ()
  "Load and enable `pfa-mode' for Org files under the PFA root."
  (when (my/pfa-file-p)
    (require 'pfa)
    (pfa-mode 1)))

(when (file-directory-p pfa-elisp-directory)
  (add-to-list 'load-path pfa-elisp-directory)

  (dolist (command '(pfa-new-invoice
                     pfa-render-at-point
                     pfa-mark-paid
                     pfa-goto-invoice
                     pfa-list
                     pfa-tax
                     pfa-golden
                     pfa-validate
                     pfa-mode))
    (autoload command "pfa" nil t))

  ;; Autoloading the KEYMAP (not just the commands) keeps the global prefix
  ;; lazy: pressing `C-c I' is what loads pfa.el, so the invoicing commands
  ;; are reachable from any buffer without paying for them at startup.
  (autoload 'pfa-command-map "pfa" nil nil 'keymap)
  (global-set-key (kbd "C-c I") 'pfa-command-map)

  (add-hook 'org-mode-hook #'my/pfa-maybe-enable))

(provide 'setup-pfa)
;;; setup-pfa.el ends here
