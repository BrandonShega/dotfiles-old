;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq language-packages
      '(
        lsp-sourcekit
        ))

(defun language/init-lsp-sourcekit ()
  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable "/opt/swift/usr/bin/sourcekit-lsp")))
