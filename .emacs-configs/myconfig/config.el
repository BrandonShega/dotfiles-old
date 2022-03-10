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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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
