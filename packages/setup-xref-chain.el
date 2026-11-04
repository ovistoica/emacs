;;; setup-xref-chain.el --- Chain CIDER and clojure-lsp xref backends -*- lexical-binding: t; -*-

;;; Commentary:

;; Make `xref-find-definitions' (M-.) and friends try CIDER first and
;; fall back to clojure-lsp (lsp-mode's `xref-lsp' backend) when CIDER
;; has no answer or no REPL is connected.  xref-chain's methods wrap
;; each backend in `ignore-errors' and move on when one returns nil.
;;
;; The chain is only installed in buffers whose major mode derives from
;; `clojure-mode'.  Elsewhere (e.g. a TypeScript buffer with lsp-mode)
;; the `cider' backend would never answer, and an always-non-nil chain
;; backend would only shadow whatever comes after it.

;;; Code:

(defvar cider-xref-fn-depth)

(use-package xref-chain
  :vc (:url "https://codeberg.org/boosja/xref-chain.el")
  :defer t)

(defun my/xref-chain-cider-lsp-backend ()
  "Return an xref-chain backend trying CIDER, then lsp-mode."
  '(chain cider xref-lsp))

(defun my/xref-chain-depth ()
  "Return the `xref-backend-functions' depth for the chain backend.
CIDER adds `cider--xref-backend' at `cider-xref-fn-depth' (-90 by
default) and lsp-mode adds `lsp--xref-backend' at depth 0, so one less
than CIDER's depth runs before both."
  (1- (if (boundp 'cider-xref-fn-depth) cider-xref-fn-depth -90)))

(defun my/xref-chain-enable ()
  "Use the CIDER → lsp xref chain in the current Clojure buffer.
Does nothing outside `clojure-mode' derived buffers or when
xref-chain is not installed."
  (when (and (derived-mode-p 'clojure-mode)
             (require 'xref-chain nil t))
    (add-hook 'xref-backend-functions #'my/xref-chain-cider-lsp-backend
              (my/xref-chain-depth) t)))

(add-hook 'cider-mode-hook #'my/xref-chain-enable)
(add-hook 'lsp-mode-hook #'my/xref-chain-enable)

(provide 'setup-xref-chain)
;;; setup-xref-chain.el ends here
