# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Environment

This is a comprehensive Emacs configuration designed for Clojure development with integrated AI tooling.

### Starting/Restarting Emacs
- Configuration loads automatically from `init.el`
- Restart Emacs to reload configuration changes: `M-x restart-emacs` or restart from terminal
- Package management uses both MELPA and straight.el for GitHub packages

### Development Commands

**REPL and Clojure Development:**
- `M-x cider-jack-in` - Start CIDER REPL for current project
- `C-c C-k` - Evaluate current buffer in REPL
- `C-c C-e` - Evaluate expression before point
- `C-c z` - Switch to REPL buffer
- `C-c C-l` - Clear REPL buffer
- `s-:` - Run in dev namespace (custom command)

**AI Integration:**
- `C-c C-a` - Toggle AI project panel (side window)
- `C-c <enter>` - Send to AI in current buffer
- `C-c C-<enter>` - Open AI menu with options

**Project Management:**
- `s-p` - Projectile command prefix
- `C-x p p` - Switch project
- `C-x p e` - Switch to Emacs config project

**Version Control:**
- `C-x m` - Open Magit status
- `C-x v t` - Git time machine
- `C-x v w` - Copy GitHub URL for current line

## Architecture Overview

### Configuration Structure
```
init.el                 # Main entry point, loads modular components
settings/              # Core Emacs configuration modules
├── packages.el        # Package manager setup (MELPA + use-package)
├── sane-defaults.el   # Sensible defaults and basic settings
├── tooling.el         # Helper macros and utilities
├── appearance.el      # Theme and visual configuration
├── navigation.el      # Movement and search
├── editing.el         # Text editing enhancements
└── work.el           # Work-specific configurations

packages/              # Package-specific configurations
├── setup-cider.el     # Clojure REPL integration
├── setup-lsp-mode.el  # Language Server Protocol
├── setup-gptel.el     # AI chat integration
├── setup-magit.el     # Git interface
├── setup-projectile.el # Project management
└── [30+ other packages]

ai-prompts/            # AI directive templates
├── clojure-expert.md  # Clojure development guidance
├── elisp-expert.md    # Emacs Lisp expertise
└── [other domain experts]
```

### Key Features

**Clojure Development Stack:**
- CIDER: Interactive development environment
- LSP Mode: Language server integration with clojure-lsp
- clj-refactor: Automated refactoring tools
- Paredit/Smartparens: Structural editing

**AI Integration:**
- gptel: Multi-provider LLM client (OpenAI, Claude, Gemini, DeepSeek)
- MCP (Model Context Protocol): External tool integration
- ai-project-agent: Custom project-aware AI assistant
- Domain-specific AI prompts for Clojure, Elisp, etc.

**Development Tools:**
- Projectile: Project management with perspective switching
- Magit: Full-featured Git interface
- LSP: Language server integration
- Flycheck: Syntax checking

### Configuration Philosophy

1. **Modular Design**: Each feature in separate files under `settings/` and `packages/`
2. **Lazy Loading**: Most packages defer loading until needed
3. **Clojure-First**: Optimized for Clojure development workflow
4. **AI-Enhanced**: Integrated AI assistance for development tasks
5. **macOS Optimized**: Special keybindings and settings for macOS

### Custom Extensions

**ai-project-agent.el**: Project-scoped AI assistant
- Creates project-specific AI chat buffers
- Integrates with flycheck for error analysis
- Side-panel interface for continuous AI interaction

**Key Custom Functions:**
- `my/cider-eval-to-clipboard`: Copy evaluation results
- `nrepl-warn-when-not-connected`: Helpful REPL warnings
- `switch-perspective+find-file`: Project switching with perspectives

### AI Directives

The `ai-prompts/` directory contains specialized AI expert personas:
- `clojure-expert.md`: Functional programming and SICP knowledge
- `elisp-expert.md`: Emacs Lisp development
- `re-frame-expert.md`: ClojureScript state management
- Plus experts for Reitit, Malli, Babashka, etc.

### Package Management

- **Primary**: MELPA via use-package with automatic installation
- **GitHub packages**: straight.el for development versions
- **Native compilation**: Enabled for performance
- **GCC path**: Uses Homebrew GCC for native compilation