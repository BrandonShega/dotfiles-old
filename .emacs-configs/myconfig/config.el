(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      sentence-end-double-space nil
      ring-bell-function 'ignore
      frame-resize-pixelwise t)

(setq user-full-name "Brandon Shega"
      user-mail-address "b.shega@gmail.com")

(defalias 'yes-or-no-p 'y-or-n-p)

(set-charset-priority 'unicode)

(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; write over selected text on input... like all modern editors do
(delete-selection-mode t)

;; enable recent files mode.
(recentf-mode t)
(setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                        ,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)))

;; don't want ESC as a modifier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq vc-follow-symlinks t)

(when (window-system)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(winner-mode t)

(show-paren-mode t)

(setq tab-always-indent 'complete)

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(defun bs/is-macos ()
  (eq system-type 'darwin))

(defun bs/is-windows ()
  (eq system-type 'windows-nt))

(unless (bs/is-windows)
  (setq-default shell-file-name "/usr/local/bin/zsh")
  (setq explicit-shell-file-name "/usr/local/bin/zsh"))

(when (bs/is-macos)
  (setq locate-command "mdfind")
  (setq ns-use-native-fullscreen t)
  (setq ns-pop-up-frames nil)
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control)

  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

  (defun bs-init-menu-bar-in-gui-frames-h (&optional frame)
    (when-let (frame (or frame (selected-frame)))
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'menu-bar-lines 1))))

  (add-hook 'window-setup-hook #'bs-init-menu-bar-in-gui-frames-h)
  (add-hook 'after-make-frame-functions #'bs-init-menu-bar-in-gui-frames-h)

  (use-package ns-auto-titlebar
    :ensure t
    :init
    (and (or (daemonp)
             (display-graphic-p))
         (ns-auto-titlebar-mode +1)))

  (use-package osx-trash
    :ensure t
    :commands osx-trash-move-file-to-trash
    :init
    (setq delete-by-moving-to-trash t)
    (and (bs/is-macos)
         (not (fboundp 'system-move-file-to-trash))
         (defalias #'system-move-file-to-trash #'osx-trash-move-file-to-trash))))



(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package eldoc
  :ensure t
  :hook (emacs-lisp-mode cider-mode))

(use-package exec-path-from-shell
  :if (bs/is-macos)
  :hook (emacs-startup . (lambda ()
                           (setq exec-path-from-shell-arguments '("-l"))
                           (exec-path-from-shell-initialize))))

(use-package doom-modeline
  :ensure t
  :hook (doom-modeline-hook . size-indication-mode)
  :hook (doom-modeline-hook . column-number-mode)
  :init
  (unless after-init-time
    (setq-default mode-line-format nil)
    (setq projectile-dynamic-mode-line nil)
    (setq doom-modeline-bar-width 3
          doom-modeline-github nil
          doom-modeline-mu4e nil
          doom-modeline-persp-name nil
          doom-modeline-minor-modes nil
          doom-modeline-major-mode-icon nil
          doom-modeline-buffer-file-name-style 'relative-from-project
          doom-modeline-buffer-encoding 'nondefault))
  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  (defun +modeline-hide-in-non-status-buffers-h ()
    (if (eq major-mode 'magit-status-mode)
        (doom-modeline-set-vcs-modeline)
      (hide-mode-line-mode)))
  (add-hook 'magit-mode-hook #'+modeline-hide-in-non-status-buffer-h))

(setq doom-modeline-height 30)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode 1))

(use-package hl-line-mode
  :straight nil
  :hook ((prog-mode) (text-mode)))

(use-package recentf
  :straight nil
  :config
  (add-to-list 'recentf-exclude "\\elpa"))

(use-package evil
  :ensure t
  :init (evil-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-exit))
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t))
