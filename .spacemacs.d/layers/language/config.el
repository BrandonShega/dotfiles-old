;; -*- mode: emacs-lisp; lexical-binding: t -*-

(add-hook 'swift-mode-hook (lambda () (lsp)))

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
