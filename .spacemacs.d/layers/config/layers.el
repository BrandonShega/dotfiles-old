;; -*- mode: emacs-lisp; lexical-binding: t -*-

(configuration-layer/declare-layers
  '(
    ivy
    emacs-lisp
    git
    tmux
    restclient
    syntax-checking
    spotify
    search-engine
    lsp
    treemacs
    dap
    finance
    twitter
    rcirc
    web-beautify
    prettier
    osx
    themes-megapack
    multiple-cursors
    (auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-tab-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)
    (org :variables
         org-want-todo-bindings t
         org-enable-github-support t)
    (version-control :variables
                    version-control-diff-tool 'diff-hl
                    version-control-global-margin t
                    version-control-diff-side 'left)
    (mu4e :variables
          mu4e-enable-notifications t
          mu4e-enable-mode-line t
          mu4e-installation-path "~/Dropbox/opt/mu/mu4e"
          mu4e-mu-binary "~/Dropbox/opt/mu/mu/mu")
    ))
