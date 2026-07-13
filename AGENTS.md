# AGENTS.md — Emacs Lisp configuration repo

**Stack:** Emacs Lisp only. No app build system; use batch Emacs for compile, lint, and tests.

## Commands
- **Byte-compile all:** `emacs -Q --batch -L settings -L packages -f batch-byte-compile settings/*.el packages/*.el *.el`
- **Byte-compile one file:** `emacs -Q --batch -L settings -L packages -f batch-byte-compile packages/setup-foo.el`
- **Run all ERT tests:** `emacs -Q --batch -L . -l ert -l test/your-tests.el -f ert-run-tests-batch-and-exit`
- **Run a single ERT test:** `emacs -Q --batch -L . -l ert -l test/your-tests.el --eval "(ert-run-tests-batch-and-exit '(regexp \"my/test-name\"))"`
- **Lint (checkdoc):** `emacs -Q --batch --eval "(progn (require 'checkdoc) (checkdoc-file \"packages/setup-foo.el\"))"`

## Code Style
- **File skeleton:** `;;; setup-foo.el --- Summary -*- lexical-binding: t; -*-` header; end with `(provide 'setup-foo)` matching the filename; include `;;; Commentary:` and `;;; Code:` section markers.
- **Imports/loading:** `(require 'feature)` for local modules; `use-package` for third-party packages; put all `require`s at top; prefer lazy loading via `:defer`, `:commands`, or `:hook`.
- **use-package order:** `:after` → `:diminish` → `:defer` → `:hook` → `:bind` → `:init` → `:custom` → `:config`. Use `:after` to express inter-package dependencies; never rely on sibling `packages/` load order.
- **Naming:** `kebab-case` everywhere; personal helpers use `my/` prefix (e.g., `my/cider-eval-to-clipboard`); predicates end in `?`; side-effecting functions end in `!`; interactive wrappers use the `λ` macro from `tooling.el`.
- **Formatting:** 2-space indent; lines ≤ 100 chars; align `:keyword` plist arguments inside `use-package`; run `M-x apheleia-format-buffer` when available.
- **Docstrings & types:** Add docstrings to all `defun`, `defvar`, `defcustom`; use `defcustom` (with `:type` and `:group`) instead of bare `defvar` for user options; add `(declare-function ...)` at file top to silence byte-compiler warnings for cross-file calls.
- **Keybindings:** Define via `:bind` / `:bind-keymap` inside `use-package`, or `global-set-key` in `settings/` files; use `[remap cmd]` for semantic rebinds; avoid shadowing standard defaults without a comment.
- **Error handling:** Use `user-error` for interactive validation; wrap risky ops in `condition-case`; use `message` for informational logs; avoid broad `ignore-errors` except for non-critical UX paths.
- **custom.el:** Never edit by hand — it is machine-written by the Emacs Custom system. Keep `(setq custom-file "custom.el")` in `init.el`.
- **Committing:** Byte-compile before committing to catch warnings; when renaming a file update its `provide` symbol and all `require` call-sites. NEVER commit code. You can maximally suggest a commit message
