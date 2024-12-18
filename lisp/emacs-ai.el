;;; emacs-ai.el --- Minor mode for interacting with AI agents.

;; Version: 1.0.0
;; Author: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; Url: https://github.com/ovistoica/emacs-ai
;; Keywords: convenience, project, ai, emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package allows you to easily work with AI agents.  It tries to emulate
;; many of the useful functionalities from modern editors like Cursor or Windsurf.

;; It provides an AI chat window and helpful commands around it


;;; Code:

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :after project
  :defines
  gptel-make-anthropic
  gptel-api-key
  :preface

  :init
  (setq gptel-default-mode 'org-mode)  ; Use org-mode as the default
  :bind (("C-c <enter>" . gptel-send)
         ("C-c RET" . gptel-send)
         ("C-c C-<enter>" . gptel-menu)
         ("C-c C-a" . emacs-ai-toggle-ai-pannel))
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key os-secret-anthropic-key)
        gptel-model "claude-3-5-sonnet-20241022"
        gptel-temperature 0.7
        ;; Configure the chat UI
        gptel-window-select t           ; Select the window after creation
        gptel-window-side 'right        ; Display on the right side
        gptel-window-width 80           ; Set window width
        )
  (setq gptel-directives
        `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific, topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for ideas.  If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

 Never apologize.  Ask questions when unsure.")
          (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
          (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
          (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
          (uix-converter "```
You are language to language translator. Your job is to translate code from JS, React, JSX to Clojure. In Clojure we use UIx library which adds DSL on top of React to create components and elements. The library provides uix.core namespace which includes top level api, as well as react hooks.
Components are created using defui macro, here’s the syntax: (defui component-name [props-map] body)
Elements are created using $ macro: ($ :dom-element optional-props-map …children)
Component names and props are written in kebab-case. Dom element keywords support hyper script syntax to define classes and id: :div#id.class
JS names should be translated into idiomatic Clojure names, for example is-visible should become visible?
Translate the following code to Clojure
```

The prompt works quite well, here an example:

```javascript
// input
function Item({ name, isPacked }) {
  return <li className=\"item\">{name}</li>;
}

export default function PackingList() {
  return (
    <section>
      <h1>Sally Ride's Packing List</h1>
      <ul>
        <Item isPacked={true} name=\"Space suit\" />
        <Item isPacked={true} name=\"Helmet with a golden leaf\" />
        <Item isPacked={false} name=\"Photo of Tam\" />
      </ul>
    </section>
  );
}
```

```clojure
;; output
(ns packing-list.core
  (:require [uix.core :refer [$ defui]]))

(defui item [{:keys [name packed?]}]
  ($ :li.item name))

(defui packing-list []
  ($ :section
     ($ :h1 \"Sally Ride's Packing List\")
     ($ :ul
        ($ item {:packed? true :name \"Space suit\"})
        ($ item {:packed? true :name \"Helmet with a golden leaf\"})
        ($ item {:packed? false :name \"Photo of Tam\"}))))
```
")
          (explain . "Explain what this code does to a novice programmer.")
          (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
- If the question or notation is not clear to you, ask for clarifying details.
- At first your hints should be general and vague.
- If I fail to make progress, provide more explicit hints.
- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")
          ))

  )



(use-package ai-project-agent
  :after (gptel flycheck)
  :bind (("C-c c a" . ai-project-agent-toggle-panel)
         ("C-c c d" . ai-project-agent-clear-panel)
         ("C-c c l" . ai-project-agent-send-lint-feedback)
         ("C-c c RET" . ai-project-agent-send)))

(use-package corsair
  :straight (:host github
                   :repo "rob137/Corsair"
                   :files ("corsair.el")) ; Load the specific file
  :defer t                                ; Optional: Load lazily
  :after gptel                          ; Ensure gptel is loaded before corsair
  )

(use-package copilot
  :defines
  copilot-max-char
  :commands
  copilot-mode
  :preface
  (defun os/activate-copilot ()
    (if (or (> (buffer-size) 100000)
            (string-prefix-p "*temp*-" (buffer-name)))
        ;; Or don't even warn to get rid of it.
        (message "Buffer size exceeds copilot max char limit or buffer is temporary. Copilot will not be activated.")
      (copilot-mode)))
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (setq copilot-enable-predicates nil
        warning-suppress-types '((copilot copilot--infer-indentation-offset)))
  (add-to-list 'copilot-major-mode-alist '("tsx-ts" . "typescriptreact"))
  (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list 'copilot-indentation-alist '(tsx-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(typescript-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojurec-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojurescript-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(css-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(json-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(dockerfile-mode 2))
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)
              ("S-C-TAB" . 'copilot-accept-completion-by-word)
              ("S-C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" os-secret-anthropic-key)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(require 'project)






(provide 'emacs-ai)
;;; emacs-ai.el ends here

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
