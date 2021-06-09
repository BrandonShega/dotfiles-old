;; -*- mode: emacs-lisp; lexical-binding: t -*-

(configuration-layer/declare-layers
 '(
   csv
   emacs-lisp
   haskell
   html
   yaml
   ruby-on-rails
   (swift :variables
          swift-backend 'lsp)
   (javascript :variables
               javascript-backend 'lsp)
   (json :variables
         json-fmt-tool 'web-beautify)
   (python :variables
           python-backend 'lsp
           python-pipenv-activate t
           python-enable-yapf-format-on-save t)
   (java :variables
         java-backend 'lsp)
   (typescript :variables
               typescript-backend 'lsp)
   (markdown :variables
             mardown-command "/usr/local/bin/pandoc")
   (ruby :variables
         ruby-enable-enh-ruby-mode t
         ruby-version-manager 'rvm
         ruby-test-runner 'rspec)
   (shell :variables
          shell-default-height 30
          shell-default-position 'bottom)
   (node :variables
         node-add-modules-path t)
   csharp
   ))
