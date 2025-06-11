# Emacs Configuration Project Summary

## Overview
This is a sophisticated, modular Emacs configuration specifically optimized for **data-centric Clojure development** with modern SaaS application development workflows. The configuration follows functional programming principles with a strong emphasis on REPL-driven development, pure functions, and immutable data structures. It's designed to support the philosophy of "data first, not methods first" as advocated by Rich Hickey, with enhanced AI integration and project management capabilities.

## Architecture

### Configuration Structure
The configuration uses a clean, modular architecture with clear separation of concerns:

```
~/.emacs.d/
├── init.el                 # Main entry point and orchestration
├── custom.el              # Emacs custom settings and project-specific variables
├── settings/              # Core functionality modules
├── packages/              # Package-specific configurations
├── ai-prompts/           # LLM-friendly documentation and expertise definitions
└── backups/              # Automatic backup storage
```

### Core Philosophy
- **Modular Design**: Each functionality is isolated in its own file
- **Late Loading**: Deferred loading with `use-package` for optimal startup performance
- **Data-Centric**: Configuration optimized for functional programming workflows
- **REPL-Driven**: Enhanced CIDER integration for interactive development
- **AI-Enhanced**: Integrated AI assistance with project-specific context
- **Process Management**: External development process orchestration

## Key Files and Their Purpose

### Entry Point
- **`init.el`**: Main configuration orchestrator that loads modules in order
  - Mac-specific settings (CMD as Meta, dark appearance)
  - Module loading from `settings/` and `packages/` directories
  - Custom file separation for cleaner configuration

### Core Settings (`settings/`)
- **`fast-startup.el`**: Performance optimizations for Emacs startup
- **`sane-defaults.el`**: Sensible defaults (UTF-8, backup handling, auto-revert)
- **`appearance.el`**: UI customization (theme, fonts, minimal interface)
- **`packages.el`**: Package manager setup (MELPA, use-package configuration)
- **`navigation.el`**: Enhanced navigation and file management
- **`editing.el`**: Text editing enhancements and productivity features
- **`tooling.el`**: Development tools integration with lambda utilities
- **`extra-keybindings.el`**: Custom keyboard shortcuts and ergonomic improvements
- **`setup-mac.el`**: macOS-specific optimizations
- **`windows.el`**: **Window management** - Toggle split, rotate windows, **repeat-mode enabled**
- **`buffers.el`**: **Buffer manipulation** - File operations, scratch buffers, path copying
- **`indented-yank.el`**: **Smart yanking** - Auto-indent on paste for programming modes
- **`setup-straight.el`**: Git-based package management with straight.el
- **`work.el`**: **Project-specific configurations** - ShipClojure-Datom development setup

### Clojure-Specific Configuration (`packages/`)
- **`setup-cider.el`**: Advanced CIDER configuration with custom REPL management
- **`setup-clojure-mode.el`**: Clojure mode enhancements and custom functions
- **`setup-clj-refactor.el`**: Automated refactoring tools and code transformations
- **`setup-paredit.el`**: Structural editing for Lisp code
- **`setup-smartparens.el`**: Alternative structural editing
- **`setup-kaocha-runner.el`**: **Kaocha test runner integration** - Automated test execution

### AI Integration (`packages/`)
- **`setup-gptel.el`**: **Advanced AI integration** - Multi-provider LLM support with project context
  - **Multi-Provider Support**: OpenAI, Anthropic (Claude), Google (Gemini), DeepSeek
  - **Model Context Protocol (MCP)**: Filesystem and fetch server integration
  - **Project-Aware Directives**: Automatic loading of AI prompts from `ai-prompts/`
  - **Smart UI**: Right-side panel, org-mode default, custom window management

### AI Prompts (`ai-prompts/`)
- **Expertise Definitions**: LLM-friendly documentation for specialized development contexts
  - `clojure-expert.md`: Comprehensive Clojure and functional programming expertise
  - `re-frame-expert.md`: Re-frame patterns and state management best practices
  - `elisp-expert.md`: Emacs Lisp development guidelines
  - `babashka-expert.md`: Babashka scripting and task automation
  - `malli-expert.md`: Schema validation and data modeling
  - `reitit-expert.md`: Routing and web application patterns
  - `js-ts-expert.md`: JavaScript/TypeScript integration patterns
  - `ai-expert.md`: AI integration and prompt engineering
  - `prompt-expert.md`: Advanced prompt design and conversation management
  - `uix-converter.md`: UIx component conversion utilities

### Development Tools
- **`setup-popper.el`**: Popup buffer management (REPLs, help, compilation)
- **`setup-projectile.el`**: Project management and navigation
- **`setup-magit.el`**: Git integration
- **`setup-consult.el`**: Enhanced search and completion
- **`setup-flycheck.el`**: Syntax checking and linting
- **`setup-prodigy.el`**: **Process management** - Development service orchestration
- **`setup-html-to-hiccup.el`**: HTML to Hiccup conversion utilities

### Process Management (`work.el`)
ShipClojure-Datom specific development process definitions:

```elisp
;; Datomic Transactor service
(prodigy-define-service
  :name "Datomic Transactor"
  :tags '(shipclojure-datom datomic)
  :command "transactor"
  :args '("config/samples/dev-transactor-template.properties"))

;; Shadow-CLJS build watch
(prodigy-define-service
  :name "Shadow-CLJS Watch"
  :tags '(shipclojure-datom frontend)
  :command "npx"
  :args '("shadow-cljs" "watch" "app" "portfolio"))

;; CSS build watch
(prodigy-define-service
  :name "CSS Watch"
  :tags '(shipclojure-datom css)
  :command "npm"
  :args '("run" "styles:watch"))
```

## Important Dependencies and Their Roles

### Core Clojure Development
- **CIDER**: Interactive Clojure development environment
  - Version: Latest from MELPA
  - Role: REPL management, evaluation, debugging, documentation
  - Custom enhancements: Multi-project session management, clipboard integration

- **clj-refactor**: Automated refactoring for Clojure
  - Advanced dependency management including Git SHA updates
  - Custom keybindings for threading/unthreading operations
  - Auto-refer mode for common namespaces

- **Kaocha Runner**: Integrated test execution
  - CIDER-based test running with file-load hooks
  - Automatic test discovery and execution
  - Keybindings: `C-c k t/r/a/w/h` for various test operations

### AI and LLM Integration
- **GPTel**: Multi-provider LLM client
  - **Providers**: OpenAI, Anthropic (Claude Sonnet 4), Google Gemini, DeepSeek
  - **Project Context**: Automatic directive loading from markdown files
  - **MCP Integration**: Model Context Protocol for tool access
  - **UI Features**: Right-side panel, org-mode integration, window management

- **MCP (Model Context Protocol)**: 
  - **Filesystem Server**: Project file access for AI
  - **Fetch Server**: Web content retrieval capabilities
  - **Clojure Project Server**: REPL and project-specific tool integration

### Text Editing and Navigation
- **Paredit/Smartparens**: Structural editing for balanced expressions
- **expand-region**: Intelligent selection expansion
- **multiple-cursors**: Multi-cursor editing
- **Projectile**: Project-aware file navigation
- **Indented Yank**: Smart auto-indentation on paste for programming modes

### Buffer and Window Management
- **Popper**: Intelligent popup buffer management
  - Custom configuration for CIDER REPLs on right side (1/3 width)
  - Support for help, compilation, and search buffers
  - Project-aware grouping capability

- **Window Management**: Enhanced window operations
  - **Repeat Mode**: `C-x }` followed by `}}}...` for continuous resize
  - Toggle split orientation, rotate windows
  - Smart window switching and management

### Completion and Search
- **Consult**: Enhanced search and completion interface
- **Company**: Auto-completion framework
- **Deadgrep**: Fast project-wide search using ripgrep

### Version Control and Productivity
- **Magit**: Comprehensive Git interface
- **diff-hl**: Visual diff indicators in buffers
- **Which-key**: Keybinding discovery and documentation
- **Prodigy**: Development process management
- **Straight.el**: Git-based package management for bleeding-edge packages

## Available Tools and APIs

### CIDER Enhancements
```elisp
;; Custom REPL management
(my/cider-select-repl-buffer)           ; Smart REPL switching
(my/cider-eval-to-clipboard)            ; Evaluate and copy result
(my/cider-eval-including-lets)          ; Evaluate with surrounding let context
(cider-run-in-dev-namespace)            ; Execute in development namespace

;; Project-aware session handling
(sesman-use-friendly-sessions nil)       ; Prevent cross-project session bleeding
```

### Clojure Development Utilities
```elisp
;; Threading/unthreading with smart unwinding
(clojure-thread)                        ; C->
(clojure-unwind)                        ; C-<
(my/clojure-should-unwind-once?)        ; Smart unwind detection

;; File navigation (test/src/portfolio switching)
(setup-clojure-mode-so)                 ; Automatic significant other setup

;; CSS completions in Clojure files
(cssc/enable-for-clojure)               ; Enable CSS class completion
```

### Buffer Management Enhancements
```elisp
;; Buffer file operations
(rename-current-buffer-file)            ; C-x C-r - Rename file and buffer
(delete-current-buffer-file)            ; C-x C-k - Delete file and kill buffer
(copy-current-file-path)                ; C-x M-w - Copy file path
(touch-buffer-file)                     ; C-x t - Touch file (save without changes)
(create-scratch-buffer)                 ; C-c b - Create new scratch buffer

;; Window operations with repeat-mode
(enlarge-window-horizontally)           ; C-x } then }}}... to keep enlarging
(shrink-window-horizontally)            ; C-x { then {{{... to keep shrinking
```

### AI Integration APIs
```elisp
;; GPTel multi-provider setup
(gptel-send)                            ; C-c RET - Send to AI
(gptel-menu)                            ; C-c C-RET - AI provider menu

;; Project-specific AI directives
(my/gptel-load-all-markdown-directives) ; Auto-load from ai-prompts/
(gptel-make-anthropic "Claude")         ; Claude Sonnet 4 integration
(gptel-make-gemini "Gemini")            ; Google Gemini support
(gptel-make-openai "DeepSeek")          ; DeepSeek reasoning model

;; MCP integrations
mcp-hub-servers                         ; Filesystem, fetch, clj-project servers
```

### Development Process Management
```elisp
;; Prodigy process management
(prodigy)                               ; C-c C-x C-p - Open process manager
(prodigy-quit)                          ; q - Quit with window restoration

;; Predefined services for ShipClojure-Datom
;; - Datomic Transactor
;; - Shadow-CLJS Watch (app + portfolio)
;; - CSS Watch (styles:watch)
```

### Test Integration
```elisp
;; Kaocha test runner
(kaocha-runner-run-test-at-point)       ; C-c k t - Run test at point
(kaocha-runner-run-tests)               ; C-c k r - Run namespace tests
(kaocha-runner-run-all-tests)           ; C-c k a - Run all tests
(kaocha-runner-run-relevant-tests)      ; Auto-run on file load
```

### Popper Buffer Management
```elisp
;; Core popper commands
(popper-toggle)                         ; C-` - Toggle latest popup
(popper-cycle)                          ; M-` - Cycle through popups
(popper-toggle-type)                    ; C-M-` - Toggle popup type

;; Buffer classification patterns
my/repl-modes-list                      ; REPL buffer modes
my/repl-names-list                      ; REPL buffer name patterns
my/help-modes-list                      ; Documentation buffer modes
```

## Implementation Patterns and Conventions

### Configuration Patterns
1. **Use-package declarations**: All packages configured with `use-package` for consistency
2. **Deferred loading**: Most packages use `:defer` for performance
3. **Custom variables**: Project-specific settings in dedicated sections
4. **Modular organization**: Related functionality grouped in single files
5. **Straight.el integration**: Git-based packages for cutting-edge features

### Clojure Development Patterns
1. **Data-first approach**: Functions designed around data transformation
2. **REPL-driven workflow**: Easy evaluation and result inspection
3. **Structural editing**: Paredit-based code manipulation
4. **Pure function preference**: Clear separation of pure and impure code
5. **Test-driven development**: Automatic test execution on file changes

### AI Integration Patterns
1. **Context-aware prompting**: Project-specific AI directives from markdown files
2. **Multi-provider flexibility**: Easy switching between AI models and providers
3. **Tool integration**: MCP servers for filesystem and project access
4. **Workflow enhancement**: AI assistance integrated into development workflow

### Keybinding Conventions
- **Function keys**: F5-F7 for common CIDER operations
- **Super (Cmd) combinations**: Mac-friendly shortcuts
- **Threading operations**: C-< and C-> for structural transformations
- **Evaluation shortcuts**: C-c variants for different evaluation contexts
- **AI shortcuts**: C-c RET for AI interaction, C-c C-RET for AI menu
- **Process management**: C-c C-x C-p for development services
- **Window repeat operations**: C-x } then }}}... for continuous resize

## Development Workflow Recommendations

### Getting Started
1. **Start Emacs**: Configuration auto-loads with optimized startup
2. **Open project**: Use `projectile-find-file` (C-c p f)
3. **Start services**: Use `prodigy` (C-c C-x C-p) to start development processes
4. **Start REPL**: `cider-jack-in` for deps.edn projects
5. **Evaluate code**: Use F6/F7 for quick evaluation, C-x M-e for context-aware evaluation

### Daily Development
1. **Navigation**: Use projectile for project files, consult for searching
2. **REPL interaction**: Toggle REPL with C-` (popper), evaluate expressions inline
3. **Refactoring**: Use clj-refactor functions for structural changes
4. **Testing**: Navigate between source and test files automatically, tests run on file load
5. **AI assistance**: Use C-c RET for AI help with project context

### Process Management
1. **Development Services**: Use prodigy to manage Datomic, Shadow-CLJS, and CSS watch
2. **Service Control**: Start/stop services from within Emacs
3. **Project Isolation**: Services are tagged and grouped by project

### Buffer Management
1. **Popups**: All REPLs, help, and compilation buffers managed by popper
2. **Layout**: CIDER REPLs appear on right side taking 1/3 screen width
3. **Project isolation**: Popper groups buffers by project for better organization
4. **File operations**: Rename, delete, copy paths with C-x prefixed commands

### AI-Enhanced Development
1. **Project Context**: AI has access to project files and structure via MCP
2. **Expert Prompts**: Specialized AI assistance for Clojure, re-frame, and other technologies
3. **Multi-Model Support**: Choose appropriate AI model for specific tasks
4. **Integrated Workflow**: AI assistance available from within development environment

## Extension Points for Future Development

### Adding New Languages
1. Create new package setup file in `packages/setup-<language>.el`
2. Add language-specific buffer patterns to popper configuration
3. Configure appropriate display rules for language REPLs/tools

### Custom Development Tools
1. Add tool-specific configuration in `packages/` directory
2. Integrate with existing projectile/consult workflows
3. Consider popper integration for tool-specific buffers
4. Add prodigy service definitions for external processes

### AI Integration Extensions
1. **Custom prompts**: Add domain-specific AI prompts in `ai-prompts/`
2. **MCP servers**: Integrate additional MCP servers for specialized tools
3. **Provider integration**: Add support for new AI providers via gptel
4. **Context enhancement**: Enhance AI context with project-specific information

### Workflow Enhancements
1. **Custom evaluation functions**: Extend CIDER with domain-specific evaluation
2. **Project templates**: Add snippets or templates for common project structures
3. **Documentation integration**: Enhance help systems with project-specific docs
4. **Process automation**: Add prodigy services for additional development processes

### Window and Buffer Management
1. **Custom window layouts**: Define project-specific window arrangements
2. **Buffer classification**: Add new buffer types to popper configuration
3. **Workspace management**: Integrate with perspective or similar workspace tools

## Dependencies Management

### Package Sources
- **GNU ELPA**: Core Emacs packages
- **MELPA**: Community packages (primary source)
- **Straight.el**: Git-based packages for bleeding edge features

### Version Strategy
- Most packages use latest stable versions from MELPA
- Critical packages may be pinned to specific versions
- Git dependencies managed through straight.el with SHA updating
- AI-related packages often use development versions for latest features

### Performance Considerations
- Lazy loading with `use-package :defer`
- Native compilation enabled for better performance
- Startup optimizations in `fast-startup.el`
- Strategic use of autoloads and hooks
- Process externalization for improved REPL performance

## Recent Updates and Enhancements

### Window Management
- **Repeat Mode Integration**: Added `(repeat-mode 1)` to windows.el for continuous window resizing
- **Enhanced Window Operations**: C-x } followed by }}}... for continuous horizontal enlargement
- **Smart Window Switching**: Improved window navigation and management

### Buffer Operations
- **File Manipulation**: Enhanced buffer-file operations (rename, delete, copy path)
- **Scratch Buffer Management**: Smart scratch buffer creation with automatic naming
- **Touch Buffer**: Save without modification utility

### Smart Editing
- **Indented Yank**: Auto-indent on paste for programming modes with threshold control
- **Context-Aware Paste**: Intelligent indentation based on current mode

### AI Integration Revolution
- **Multi-Provider Support**: OpenAI, Anthropic Claude, Google Gemini, DeepSeek integration
- **Model Context Protocol**: Filesystem and project access for AI via MCP servers
- **Project-Aware AI**: Automatic loading of project-specific AI directives
- **Advanced UI**: Right-side AI panel with org-mode integration

### Development Process Management
- **Prodigy Integration**: Service management for ShipClojure-Datom development stack
- **External Process Support**: Datomic transactor, Shadow-CLJS watch, CSS build processes
- **Tagged Services**: Project-specific service organization and management

### Test Integration
- **Kaocha Runner**: Automated test execution with CIDER integration
- **File Load Hooks**: Automatic test running on file changes
- **Test Discovery**: Smart test file detection and execution

### Git-based Package Management
- **Straight.el**: Advanced package management for development versions
- **Bleeding Edge Support**: Access to latest features from Git repositories

This configuration represents a cutting-edge, AI-enhanced Emacs setup specifically tailored for modern Clojure SaaS development with emphasis on functional programming principles, data-centric design patterns, and AI-assisted development workflows.
