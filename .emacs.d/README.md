- [General Settings](#orgcba8d29)
  - [Personal](#org61d1bac)
  - [Settings](#org9bcdbd9)
  - [Functions](#orge7c24e6)
- [Package Management](#org548e80c)
  - [Straight.el](#org1bab028)
  - [Use Package](#org160f1c3)
- [Theme](#org83d03f9)
- [Packages](#org365267b)
  - [General](#org886a965)
    - [Evil Leader](#org91b2b51)
    - [Evil](#org2754653)
    - [General](#orgfd1dfc5)
    - [Projectile](#org07c7b21)
    - [Ivy](#org85eeac1)
    - [Which Key](#orgb68010b)
    - [Magit](#org7fb3cd8)
    - [LSP](#org9fc4139)
  - [Display](#orgc1e6777)
    - [Doom Modeline](#org257ed71)
    - [All the icons](#orga4000e0)
    - [Rainbow Delimiters](#org8883812)
  - [Languages](#orgd6c3c75)
    - [CSS](#org1e64eb5)
    - [HTML](#org705598d)
    - [Javascript](#org5cb2c3c)
    - [Markdown](#orgfb16b47)
    - [Typescript](#org46fd74d)
  - [Misc](#org4cd4b3b)
- [Keybindings](#org90bd88b)



<a id="orgcba8d29"></a>

# General Settings


<a id="org61d1bac"></a>

## Personal

```emacs-lisp
(setq user-full-name "Brandon Shega"
      user-mail-address "b.shega@gmail.com"
      epa-file-encrypt-to user-mail-address)
```


<a id="org9bcdbd9"></a>

## Settings

```emacs-lisp
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(setq inhibit-startup-screen t)
(cd "~/")
```


<a id="orge7c24e6"></a>

## Functions

```emacs-lisp
(defun find-config ()
  "Edit config org file"
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))
```


<a id="org548e80c"></a>

# Package Management


<a id="org1bab028"></a>

## Straight.el

```emacs-lisp
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
```


<a id="org160f1c3"></a>

## Use Package

```emacs-lisp
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
```


<a id="org83d03f9"></a>

# Theme

Load the current theme of the day

```emacs-lisp
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package seoul256-theme
  :disabled
  :config
  (load-theme 'seoul256 t))
```


<a id="org365267b"></a>

# Packages


<a id="org886a965"></a>

## General


<a id="org91b2b51"></a>

### Evil Leader

```emacs-lisp
(use-package evil-leader
  :init (setq evil-want-C-u-scroll t)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))
```


<a id="org2754653"></a>

### Evil

> Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions.

```emacs-lisp
(use-package evil
  :after evil-leader
  :config
  (evil-mode 1))
```


<a id="orgfd1dfc5"></a>

### General

```emacs-lisp
(use-package general)
```


<a id="org07c7b21"></a>

### Projectile

```emacs-lisp
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))
```


<a id="org85eeac1"></a>

### Ivy

```emacs-lisp
(use-package counsel
  :init
  (general-def
    [remap apropos]                    #'counsel-apropos
    [remap bookmark-jump]              #'counsel-bookmark
    [remap compile]                    #'+ivy/compile
    [remap describe-bindings]          #'counsel-descbinds
    [remap describe-face]              #'counsel-faces
    [remap describe-function]          #'counsel-describe-function
    [remap describe-variable]          #'counsel-describe-variable
    [remap evil-ex-registers]          #'counsel-evil-registers
    [remap evil-show-marks]            #'counsel-mark-ring
    [remap execute-extended-command]   #'counsel-M-x
    [remap find-file]                  #'counsel-find-file
    [remap find-library]               #'counsel-find-library
    [remap imenu]                      #'counsel-imenu
    [remap info-lookup-symbol]         #'counsel-info-lookup-symbol
    [remap load-theme]                 #'counsel-load-theme
    [remap locate]                     #'counsel-locate
    [remap org-goto]                   #'counsel-org-goto
    [remap org-set-tags-command]       #'counsel-org-tag
    [remap projectile-compile-project] #'+ivy/project-compile
    [remap recentf-open-files]         #'counsel-recentf
    [remap set-variable]               #'counsel-set-variable
    [remap swiper]                     #'counsel-grep-or-swiper
    [remap unicode-chars-list-chars]   #'counsel-unicode-char
    [remap yank-pop]                   #'counsel-yank-pop)
  :config
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-use-virtual-buffers t
	ivy-re-builders-alist
	`((counsel-rf . ivy--regex-plus)
	  (swiper . ivy--regex-plus)
	  (swiper-isearch . ivy--regex-plus)
	  (t . ivy--regex-plus))
	ivy-more-chars-alist
	`((counsel-rg . 1)
	  (counsel-search . 2)
	  (t . 3))
	ivy-magic-slash-non-match-action nil
	ivy-use-virtual-buffers nil
	ivy-virual-abbreviate 'full
	ivy-use-selectable-prompt t
	counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
	ivy-height 17
	ivy-fixed-height-minibuffer t
	ivy-wrap t
	ivy-count-format "%d/%d ")
  (ivy-mode t))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode +1))

(use-package counsel-projectile
  :init
  (general-def
    [remap projectile-find-file]        #'counsel-projectile-find-file
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project))

(use-package flx
  :init (setq ivy-flx-limit 10000))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :after ivy
  :config
  (setq all-the-icons-ivy-file-commands
   '(counsel-find-file
     counsel-recentf
     counsel-projectile
     counsel-projectile-find-file
     counsel-projectile-find-dir)))
```


<a id="orgb68010b"></a>

### Which Key

> Emacs package that displays available keybindings in popup

```emacs-lisp
(use-package which-key
  :config
  (which-key-mode))
```


<a id="org7fb3cd8"></a>

### Magit

```emacs-lisp
(use-package magit
  :config
  (setq magit-diff-refine-hunk t
	magit-save-repository-buffers nil
	magit-git-executable (executable-find magit-git-executable)))
```


<a id="org9fc4139"></a>

### LSP

```emacs-lisp
(use-package lsp-mode
  :hook
  ((typescript-mode . lsp-deferred)
   (js2-mode . lsp-deferred)
   (web-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode)
```


<a id="orgc1e6777"></a>

## Display


<a id="org257ed71"></a>

### Doom Modeline

```emacs-lisp
(use-package doom-modeline
  :config
  (doom-modeline-mode 1))
```


<a id="orga4000e0"></a>

### All the icons

```emacs-lisp
(use-package all-the-icons)
```


<a id="org8883812"></a>

### Rainbow Delimiters

```emacs-lisp
(use-package rainbow-delimiters
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))
```


<a id="orgd6c3c75"></a>

## Languages


<a id="org1e64eb5"></a>

### CSS

```emacs-lisp
(use-package css-mode
  :ensure nil
  :custom (css-indent-offset 2))

(use-package scss-mode
  :ensure nil
  :preface
  (defun me/scss-set-comment-style ()
    (setq-local comment-end "")
    (setq-local comment-start "//"))
  :mode ("\\.sass\\'" "\\.scss\\'")
  :hook (scss-mode . me/scss-set-comment-style))
```


<a id="org705598d"></a>

### HTML

```emacs-lisp
(use-package emmet-mode
  :hook (css-mode web-mode html-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :config
  (setq web-mode-enable-html-entities-fortification t
	web-mode-auto-close-style 1))
```


<a id="org5cb2c3c"></a>

### Javascript

```emacs-lisp
(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package js2-mode
  :ensure nil
  :mode "\\.js\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :preface
  (defun me/json-set-indent-level ()
    (setq-local js-indent-level 2))
  :hook (json-mode . me/json-set-indent-level))

(use-package prettier-js
  :config
  :hook js2-mode)

(use-package js-doc)
```


<a id="orgfb16b47"></a>

### Markdown

```emacs-lisp
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
```


<a id="org46fd74d"></a>

### Typescript

```emacs-lisp
(use-package typescript-mode)
```


<a id="org4cd4b3b"></a>

## Misc

```emacs-lisp
(use-package toc-org
  :hook ((org-mode markdown-mode) . toc-org-mode))

(use-package ox-gfm
  :after org)
```


<a id="org90bd88b"></a>

# Keybindings

Minimap configuration

```emacs-lisp
(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
	    minibuffer-local-ns-map
	    minibuffer-local-completion-map
	    minibuffer-local-must-match-map
	    minibuffer-local-isearch-map
	    read-expression-map
	    ivy-minibuffer-map
	    ivy-switch-buffer-map)))
(general-define-key
 :keymaps +default-minibuffer-maps
 [escape] 'abort-recursive-edit
 "C-j" 'next-line
 "C-k" 'previous-line)
```

Leader Keybinds

```emacs-lisp
;; <leader>
(general-def
  :prefix "SPC"
  :states '(normal visual emacs)
  :keymaps 'override
  ";" 'eval-expression
  ":" 'execute-extended-command
  "X" 'org-capture
  "." 'find-file
  "," 'switch-to-buffer
  "*" 'counsel-rg
  "SPC" 'projectile-find-file
  "TAB" 'mode-line-other-buffer
  ;; <leader> b --- buffers
  "b" '(:ignore t :wk "buffers")
  "bb" 'switch-to-buffer
  "b[" 'previous-buffer
  "b]" 'next-buffer
  "bk" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  ;; <leader> f --- files
  "f" '(:ignore t :wk "files")
  "ff" 'find-file
  "fd" 'dired
  "fr" 'recentf-open-files
  "fR" 'projectile-recentf
  "fs" 'save-buffer
  "fp" 'find-config
  ;; <leader> g --- git
  "g" '(:ignore t :wk "git")
  "g/" 'magit-dispatch
  "gg" 'magit-status
  "gB" 'magit-blame-addition
  "gF" 'magit-fetch
  "gL" 'magit-log
  "gS" 'magit-stage-file
  "gU" 'magit-unstage-file
  "gc" '(:ignore t :wk "create")
  "gcr" 'magit-init
  "gcR" 'magit-clone
  "gcc" 'magit-commit-crate
  "gci" 'forge-create-issue
  "gcp" 'forge-create-pullreq
  "gf" '(:ignore t :wk "find")
  "gff" 'magit-find-file
  "gfg" 'magit-find-git-config-file
  "gfc" 'magit-show-commit
  "gfi" 'forge-visit-issue
  "gfp" 'forge-visit-pullreq
  "gl" '(:ignore t :wk "list")
  "glr" 'magit-list-repositories
  "gls" 'magit-list-submodules
  "gli" 'forge-list-issues
  "glp" 'forge-list-pullreqs
  "gln" 'forge-list-notifications
  "go" '(:ignore t :wk "open in browser")
  "gor" 'forge-browse-remote
  "goc" 'forge-browse-commit
  "goi" 'forge-browse-issue
  "gop" 'forge-browse-pullreq
  "goI" 'forge-browse-issues
  "goP" 'forge-browse-pullreqs
  ;; <leader> h --- help
  "h" '(:ignore t :wk "help")
  "hi" 'info
  "hl" 'counsel-find-library
  "hI" 'info-display-manual
  "hd" '(:ignore t :wk "describe")
  "hdB" 'evil-collection-describe-bindings
  "hdc" 'describe-char
  "hdf" 'counsel-describe-function
  "hdg" 'general-describe-keybindings
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdp" 'describe-package
  "hds" 'counsel-info-lookup-symbol
  "hdt" 'describe-theme
  "hdu" 'counsel-unicode-char
  "hdv" 'counsel-describe-variable
  ;; <leader> p --- projects
  "p" '(:ignore t :wk "projects")
  "pp" 'projectile-switch-project)
```
