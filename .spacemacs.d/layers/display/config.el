;; ;; -*- mode: emacs-lisp; lexical-binding: t -*-
(fringe-mode '4)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(with-eval-after-load 'doom-modeline
      (setq doom-modeline-icon t
            doom-modeline-major-mode-color-icon t
            doom-modeline-buffer-file-name-style 'relative-to-project
            doom-modeline-mu4e t
            doom-modeline-irc t))

(with-eval-after-load 'doom-themes
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t)
      (doom-themes-visual-bell-config)
      (doom-themes-neotree-config)
      (doom-themes-treemacs-config)
      (doom-themes-org-config))
