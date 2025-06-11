# .emacs.d - Enhanced Fork

An enhanced Emacs configuration for modern Clojure SaaS development with AI integration, forked from [magnars/emacsd-reboot](https://github.com/magnars/emacsd-reboot).

**Original Philosophy**: A personal Emacs configuration as seen on emacsrocks.com and parens-of-the-dead.com, optimized for Norwegian keyboard on OSX.

**This Fork**: Extends the original foundation with AI-enhanced workflows, development process management, and modern SaaS tooling while preserving the personal, pragmatic approach.

## Installation

Download [Emacs for Mac OSX](http://emacsformacosx.com/).

```bash
# Backup your existing configuration
mv ~/.emacs.d ~/.emacs.d.backup

# Clone this enhanced fork
git clone <your-fork-url> ~/.emacs.d

# Start Emacs
emacs
```

### AI Integration Setup (Fork Addition)

For AI features, create `~/.emacs.d/private.el`:

```elisp
(setq my/secret-openai-key "your-openai-key"
      my/secret-anthropic-key "your-anthropic-key"
      my/secret-google-gemini-api-key "your-gemini-key"
      my/secret-deepseek-api-key "your-deepseek-key")
```

## Out of band dependencies

### Original Dependencies

- Spell checking:
    ```
    brew install aspell --lang=en
    ```

- Static analysis for Clojure with LSP:
    ```
    brew install clojure-lsp/brew/clojure-lsp-native
    ```

- Fast grepping:
    ```
    brew install ripgrep
    ```

- Make dired happy, install `gls` as replacement for `ls`:
    ```
    brew install xz coreutils
    ```

- Stop clojure-lsp from adding duplicate namespace declarations:
    Open `~/.config/clojure-lsp/config.edn` and add:
    ```
    {:auto-add-ns-to-new-files? false}
    ```

- Add dependencies to the project without CIDER running:
    ```
    brew install babashka/brew/neil
    ```

- Static analysis for CSS with LSP:
    ```
    npm install -g vscode-langservers-extracted
    ```

- On a Mac you might want to do this, to disable `C-M-d` in your OS:
    ```
    defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
    ```

### Fork Additions - SaaS Development Stack

- **Datomic Pro** (for ShipClojure-Datom development):
    ```bash
    # Download from https://my.datomic.com/downloads/pro
    # Extract and add to PATH
    ```

- **Node.js and npm** (for Shadow-CLJS and CSS builds):
    ```bash
    brew install node
    ```

- **AI Integration Dependencies**:
    ```bash
    # For MCP filesystem server
    npm install -g @modelcontextprotocol/server-filesystem
    
    # For MCP fetch server  
    pip install mcp-server-fetch
    # or with uvx: uvx mcp-server-fetch
    ```

## Tips for using these emacs settings

### Original Guidance (Still Applies)

If you want to use my settings straight out of the box, here are some things to note:

* This is my personal emacs configuration. I am constantly tuning it to my preferences. You should consider doing the same. Maybe start with a blank emacs + [Technomancy's better-defaults package](https://git.sr.ht/~technomancy/better-defaults), and then dig through this repo for useful nuggets, instead of forking it directly.

* The key bindings are optimized for a norwegian keyboard layout.

* You quit emacs with `C-x r q`, mnemonic *Really Quit*.

* Find file in project with `C-x p f`, in dir with `C-x C-f`, recent with `C-x f`

* Switch to a project with `C-x p p`

* Add your user- and project-specific stuff in .emacs.d/users/[machine name]/*.el

* `C-h` is rebound to backspace, like in the shell. Get help on `F1` instead.

* Autocomplete with `C-,` or `C-.`, try both to get a feel for them. (autocomplete entire lines with `C-:`)

* expand-region is your friend. Find its bound key by doing `F1 f er/expand-region`

* Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

* Indent and clean up white space in the entire buffer with `C-c n`

* On a mac, the Meta key `M` is bound to Command.

* I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

* Watch [emacsrocks.com](http://emacsrocks.com)

### Fork-Specific Tips

* **AI assistance**: `C-c RET` to send context to AI, `C-c C-RET` for provider menu
* **Process management**: `C-c C-x C-p` to manage development services (Datomic, Shadow-CLJS, etc.)
* **Continuous window resize**: `C-x }` then keep pressing `}` to continuously enlarge windows
* **Enhanced buffer operations**: `C-x C-r` to rename file and buffer, `C-x C-k` to delete file
* **Auto-testing**: Tests run automatically when you save Clojure files
* **Smart yanking**: Pasted code auto-indents in programming modes

## Enhanced Features (Fork Additions)

### 🤖 AI Integration
- **Multi-provider support**: OpenAI, Anthropic Claude, Google Gemini, DeepSeek
- **Project-aware prompts**: AI has context about your codebase and technologies
- **Expert knowledge base**: Specialized prompts for Clojure, re-frame, Malli, etc.
- **Tool access**: AI can read project files via Model Context Protocol (MCP)

### 🔧 Development Process Management
- **Service orchestration**: Manage Datomic transactor, Shadow-CLJS watch, CSS builds
- **Project isolation**: Services tagged and grouped by project
- **One-click startup**: Start your entire development stack from Emacs

### 🧪 Automated Testing
- **Kaocha integration**: Run tests with `C-c k t/r/a`
- **Auto-execution**: Tests run when you save files
- **Smart discovery**: Automatically finds and runs relevant tests

### 🪟 Enhanced Window Management
- **Repeat mode**: Window resize operations can be repeated continuously
- **Smart popups**: Better REPL and help buffer management
- **Layout preservation**: Window configurations restored after operations

### 📝 Advanced Editing
- **Smart yanking**: Auto-indent when pasting in programming modes
- **Enhanced file ops**: Rename, delete, copy paths, touch files
- **Scratch buffers**: Create multiple scratch buffers with smart naming

## Survival guide for the first week of emacs

When you start using emacs for the first time, your habits fight you every inch
of the way. Your fingers long for the good old familiar keybindings. Here's an
overview of the most commonly used shortcuts to get you through this pain:

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (bound to command on my mac settings)
* `S      ` Shorthand for the shift-key
* `s      ` Shorthand for the super-key (bound to option on my mac settings)

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x p f` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x C-j` Jump to this files' current directory
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

#### Enhanced File Operations (Fork)
* `C-x C-r` Rename current file and buffer
* `C-x C-k` Delete current file and kill buffer  
* `C-x M-w` Copy current file path to clipboard
* `C-x t  ` Touch file (save without changes)
* `C-c b  ` Create new scratch buffer

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank) - **auto-indents in programming modes** (fork)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills
* `C-@    ` Mark stuff quickly. Press multiple times
* `C-S-y  ` Paste without auto-indenting (fork)

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-x m  ` Open magit. It's a magical git interface for emacs

#### AI and Process Management (Fork)
* `C-c RET    ` Send to AI with project context
* `C-c C-RET  ` AI provider menu
* `C-c C-x C-p` Open process manager (Prodigy)

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically
* `S-arrow` Jump to window to the left/right/up/down

#### Enhanced Window Operations (Fork)
* `C-x }  ` Enlarge window horizontally, then `}}}...` to keep enlarging
* `C-x {  ` Shrink window horizontally, then `{{{...` to keep shrinking
* `C-x ^  ` Enlarge window vertically, then `^^^...` to keep enlarging
* `C-`    ` Toggle popup buffers (REPLs, help, etc.)

### Testing (Fork Addition)
* `C-c k t` Run test at point
* `C-c k r` Run namespace tests  
* `C-c k a` Run all tests
* `C-c k w` Show test warnings
* `C-c k h` Hide test windows

### Help

* `F1 t   ` Basic tutorial
* `F1 k   ` Help for a keybinding
* `F1 r   ` Emacs' extensive documentation

## What's Different in This Fork

| Area | Original | This Fork |
|------|----------|-----------|
| **AI Integration** | None | Multi-provider LLM with project context |
| **Process Management** | Manual external processes | Integrated Prodigy service management |
| **Testing** | Manual CIDER evaluation | Automated Kaocha with file-load hooks |
| **Window Management** | Basic operations | Repeat-mode for continuous operations |
| **Buffer Operations** | Basic file handling | Enhanced file manipulation utilities |
| **Package Management** | use-package only | Hybrid: use-package + straight.el |
| **Development Stack** | Generic Clojure | SaaS-optimized (Datomic, Shadow-CLJS) |
| **Editing** | Standard yank | Smart auto-indent on paste |

## Configuration Structure

```
~/.emacs.d/
├── init.el                    # Main entry point (enhanced)
├── custom.el                  # Emacs custom settings
├── private.el                 # AI API keys (create this)
├── settings/                  # Core functionality
│   ├── buffers.el            # Enhanced buffer operations (new)
│   ├── windows.el            # Window management with repeat-mode (enhanced)
│   ├── indented-yank.el      # Smart paste functionality (new)
│   ├── work.el               # Project-specific config (new)
│   └── ...                   # Original settings (enhanced)
├── packages/                  # Package configurations
│   ├── setup-gptel.el        # AI integration (new)
│   ├── setup-prodigy.el      # Process management (new)
│   ├── setup-kaocha-runner.el # Test automation (new)
│   └── ...                   # Original + enhanced packages
└── ai-prompts/               # AI expertise definitions (new)
    ├── clojure-expert.md     # Clojure development context
    ├── re-frame-expert.md    # State management patterns
    └── ...                   # Other expert contexts
```

## Credits

- **Original Foundation**: [Magnar Sveen](https://github.com/magnars) for the excellent emacsd-reboot
- **AI Integration**: Built on [karthink/gptel](https://github.com/karthink/gptel) 
- **Process Management**: [rejeep/prodigy.el](https://github.com/rejeep/prodigy.el)
- **Enhanced Tooling**: Various package authors in the Emacs and Clojure communities

This configuration maintains the personal, pragmatic philosophy of the original emacsd-reboot while extending it for modern AI-enhanced Clojure SaaS development workflows.
