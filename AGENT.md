AGENT guide for this repo (Emacs Lisp configuration)

- Stack: Emacs Lisp only; no app build. Use batch Emacs for byte-compilation, lint, and ERT tests.
- Byte-compile all: emacs -Q --batch -L settings -L packages -f batch-byte-compile settings/*.el packages/*.el *.el
- Byte-compile one file: emacs -Q --batch -L settings -L packages -f batch-byte-compile packages/setup-clojure-mode.el
- Run all ERT tests (if you add any under test/*.el): emacs -Q --batch -L . -l ert -l test/your-tests.el -f ert-run-tests-batch-and-exit
- Run a single ERT test by name (regexp): emacs -Q --batch -L . -l ert -l test/your-tests.el --eval "(ert-run-tests-batch-and-exit '(regexp \"my/test-name\"))"
- Lint (checkdoc) one file: emacs -Q --batch -l checkdoc --eval "(checkdoc-file \"packages/setup-clojure-mode.el\")"  # loop over *.el as needed

Code style (keep changes small and self‑contained):
- File header: first line should include -*- lexical-binding: t; -*-; end each file with (provide '<feature>) matching its filename.
- Imports: use (require 'feature) for local modules; use-package for third‑party packages; avoid top‑level side effects beyond defs and keybindings.
- Naming: kebab-case; internal helpers prefixed with my/ (or a unique project prefix like ov/ or eca/); feature names match file (e.g., setup-foo.el -> setup-foo).
- Formatting: 2‑space indent; keep lines <100 chars; align keyword plists in use-package; run M-x apheleia-format-buffer when available.
- Types & customization: prefer defcustom over defvar for user options with :type and :group; add docstrings to all defun/defvar/defcustom.
- Errors: use user-error for interactive validation; wrap risky ops in condition-case; use message for logs; avoid broad ignore-errors except for non‑critical UX.
- APIs: keep functions non-interactive unless needed; mark commands with (interactive); avoid global state—use let/lexical closures.
- Keys: define bindings via :bind in use-package or buffer‑local maps; do not shadow common defaults without comment.
- Dependencies: keep requires at top of the file; prefer lazy loading via use-package :defer/:commands/:hook to speed startup.
- PR hygiene: when renaming a file, update its provided feature and all requires; byte‑compile before committing to catch warnings.
- Clojure workflows (external): Kaocha runner is configured for projects you open in Emacs; it is not used to test this repo itself.
