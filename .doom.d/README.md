- [Getting Started](#org877cb98)
- [General Settings](#orgd9e2430)
  - [Personal](#org22eeaac)
  - [Fonts](#org7a05e97)
  - [Theme](#org602f2c1)
  - [Key Bindings](#org8c8704a)
  - [General Settings](#org41e572b)
  - [Popup Rules](#org27ac37e)
- [Org Mode Settings](#orgf9e0ce5)
  - [Agenda](#org25ffcc6)
  - [Load all \*.org files to agenda](#org1c15a9c)
  - [Capture Templates](#orgc365507)
    - [Getting Things Done (GTD)](#org75bd9ba)
      - [Recurring Tasks](#org6d73257)
      - [Project](#orgeebdc42)
      - [New Capture](#orgba55251)
    - [References](#org34f7d1e)
      - [Yank Example](#org0a716f6)
      - [New Entry](#org6cf68e6)
    - [Diary](#orgc28aa6f)
      - [Daily Log](#orgd103b1d)
  - [Directories](#org1dc24be)
  - [Export](#orgbb397ed)
  - [Keywords](#org28be9c8)
  - [Refiling](#org4575bbe)
- [GitHub Settings](#orgf709ef0)
  - [Forge](#org6920e21)
  - [GRIP](#org168c659)
- [LSP Settings](#org5f80ccd)
  - [Angular LSP](#org625c4b2)
- [Extra Plugins](#org96eeb93)
  - [ox-gfm](#org8cca2da)
  - [PlantUML](#orgd9d617e)
- [Workarounds](#orgacff140)

My DOOM Emacs private configuration:


<a id="org877cb98"></a>

# Getting Started

If you have not installed DOOM Emacs but would like to:

    git clone https://github.com/BrandonShega/.doom.d.git ~/.doom.d
    git clone https://github.com/hlissner/doom-emacs.git ~/.emacs.d
    ~/.emacs.d/bin/doom install


<a id="orgd9e2430"></a>

# General Settings


<a id="org22eeaac"></a>

## Personal

```emacs-lisp
(setq user-full-name "Brandon Shega"
      user-mail-address "b.shega@gmail.com"
      epa-file-encrypt-to user-mail-address)
```


<a id="org7a05e97"></a>

## Fonts

The font I use is JetBrains&rsquo; Mono which can be downloaded [here](https://www.jetbrains.com/lp/mono/)

```emacs-lisp
(setq doom-font (font-spec :family "JetBrains Mono" :size 12))
```


<a id="org602f2c1"></a>

## Theme

```emacs-lisp
(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-theme "doom-colors")
```


<a id="org8c8704a"></a>

## Key Bindings

```emacs-lisp
(map! :leader "`" #'evil-switch-to-windows-last-buffer)

(map! :leader
    (:prefix-map ("c" . "code")
    :desc "LSP perform action" "a" #'lsp-execute-code-action))

(map!
    ;; Easier window movement
    :n "C-h" #'evil-window-left
    :n "C-j" #'evil-window-down
    :n "C-k" #'evil-window-up
    :n "C-l" #'evil-window-right

    (:map vterm-mode-map
    ;; Easier window movement
    :i "C-h" #'evil-window-left
    :i "C-j" #'evil-window-down
    :i "C-k" #'evil-window-up
    :i "C-l" #'evil-window-right)

    (:map evil-treemacs-state-map
    "C-h" #'evil-window-left
    "C-l" #'evil-window-right))

(map! :map +doom-dashboard-mode-map [remap projectile-find-file] #'counsel-find-file)
```


<a id="org41e572b"></a>

## General Settings

```emacs-lisp
(setq epa-file-encrypt-to user-mail-address
      company-idle-delay 0.2
      evil-split-window-below t
      evil-vsplit-window-right t
      display-line-numbers-type 'relative)

(when IS-MAC
  (setq ns-use-thin-smoothing t
        mac-command-modifier 'meta
        mac-option-modifier 'super))
```


<a id="org27ac37e"></a>

## Popup Rules

```emacs-lisp
(after! org (set-popup-rule! "^\\*Org.*Export\\*" :side 'right :size 0.5 :modeline t))
(after! org (set-popup-rule! "^Capture.*\\.org$" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(after! org (set-popup-rule! "*org agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
```


<a id="orgf9e0ce5"></a>

# Org Mode Settings


<a id="org25ffcc6"></a>

## Agenda

```emacs-lisp
(setq org-agenda-use-time-grid nil
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-habit-show-habits t)
```


<a id="org1c15a9c"></a>

## Load all \*.org files to agenda

```emacs-lisp
(load-library "find-lisp")
(after! org
  (setq org-agenda-files (find-lisp-find-files "~/Dropbox/Organization" "\.org$")))
```


<a id="orgc365507"></a>

## Capture Templates

```emacs-lisp
(after! org
  (setq org-capture-templates
        '(("g" "GTD")
          ("r" "References")
          ("d" "Diary")
          ("n" "Notes")
          ("t" "TODO"))))
```


<a id="org75bd9ba"></a>

### Getting Things Done (GTD)


<a id="org6d73257"></a>

#### Recurring Tasks

```emacs-lisp
(after! org (add-to-list 'org-capture-templates
                         '("gr" "Recurring Task" entry (file "~/Dropbox/Organization/gtd/recurring.org")
"* TODO %^{description}
:PROPERTIES:
:CREATED: %U
:END:
:RESOURCES:
:END:

+ NOTES:
  %?")))
```


<a id="orgeebdc42"></a>

#### Project

```emacs-lisp
(after! org (add-to-list 'org-capture-templates
                         '("gp" "Project" entry (file+headline "~/Dropbox/Organization/gtd/tasks.org" "Projects")
"* TODO %^{description}
:PROPERTIES:
:SUBJECT: %^{subject}
:GOAL: %^{goal}
:END:
:RESOURCES:
:END:

+ REQUIREMENTS:
  %^{requirements}

+ NOTES:
  %?

\** TODO %^{task1}")))
```


<a id="orgba55251"></a>

#### New Capture

```emacs-lisp
(after! org (add-to-list 'org-capture-templates
             '("gt" "Capture Task" entry (file "~/Dropbox/Organization/gtd/inbox.org")
"** TODO %?
:PROPERTIES:
:CREATED:    %U
:END:
:RESOURCES:
:END:

+ NEXT STEPS:
  - [ ] %^{next steps}

+ NOTES:")))
```


<a id="org34f7d1e"></a>

### References


<a id="org0a716f6"></a>

#### Yank Example

```emacs-lisp
(after! org (add-to-list 'org-capture-templates
             '("re" "Yank New Example" entry (file+headline "~/Dropbox/Organization/notes/examples.org" "INBOX")
"* %^{example}
:PROPERTIES:
:SOURCE:  %^{source|Command|Script|Code|Usage}
:SUBJECT: %^{subject}
:END:

\#+BEGIN_SRC
%x
\#+END_SRC
%?")))
```


<a id="org6cf68e6"></a>

#### New Entry

```emacs-lisp
(after! org (add-to-list 'org-capture-templates
             '("rn" "Yank new Example" entry (file+headline "~/Dropbox/Organization/notes/references.org" "INBOX")
"* %^{example}
:PROPERTIES:
:CATEGORY: %^{category}
:SUBJECT:  %^{subject}
:END:
:RESOURCES:
:END:

%?")))
```


<a id="orgc28aa6f"></a>

### Diary


<a id="orgd103b1d"></a>

#### Daily Log

```emacs-lisp
(after! org (add-to-list 'org-capture-templates
             '("dn" "New Diary Entry" entry (file+olp+datetree "~/Dropbox/Organization/diary.org" "Daily Logs")
"* %^{thought for the day}
:PROPERTIES:
:CATEGORY: %^{category}
:SUBJECT:  %^{subject}
:MOOD:     %^{mood}
:END:
:RESOURCES:
:END:

\*What was one good thing you learned today?*:
- %^{whatilearnedtoday}

\*List one thing you could have done better*:
- %^{onethingdobetter}

\*Describe in your own words how your day was*:
- %?")))
```


<a id="org1dc24be"></a>

## Directories

```emacs-lisp
(setq org-directory "~/Dropbox/Organization"
      org-image-actual-width nil
      +org-export-directory "~/Dropbox/Organization/.export/"
      org-archive-location "~/Dropbox/Organization/gtd/archive.org::datetree/"
      org-default-notes-file "~/Dropbox/Organization/gtd/inbox.org"
      projectile-project-search-path '("~/"))
```


<a id="orgbb397ed"></a>

## Export

```emacs-lisp
(setq org-html-head-include-scripts t
      org-export-with-toc t
      org-export-with-author t
      org-export-headline-levels 5
      org-export-with-drawers t
      org-export-with-email t
      org-export-with-footnotes t
      org-export-with-latex t
      org-export-with-section-numbers nil
      org-export-with-properties t
      org-export-with-smart-quotes t)

;(after! org (add-to-list 'org-export-backends 'pandoc))
(after! org (setq org-export-backends '("pdf" "ascii" "html" "latex" "odt")))
```


<a id="org28be9c8"></a>

## Keywords

```emacs-lisp
(after! org (setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w!)" "STARTED(s!)" "NEXT(n!)" "DELEGATED(e!)" "|" "INVALID(I!)" "DONE(d!)"))))
```


<a id="org4575bbe"></a>

## Refiling

```emacs-lisp
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-hide-emphasis-markers nil
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)
```


<a id="orgf709ef0"></a>

# GitHub Settings


<a id="org6920e21"></a>

## Forge

```emacs-lisp
(after! forge
  (push '("git.moen.com" "git.moen.com/api/v3"
          "git.moen.com" forge-github-repository)
        forge-alist))
(setq ghub-use-workaround-for-emacs-bug 'force)
```


<a id="org168c659"></a>

## GRIP

```emacs-lisp
(require 'auth-source)
(let ((credential (auth-source-user-and-password "api.github.com")))
  (setq grip-github-user (car credential)
        grip-github-password (cadr credential)))
```


<a id="org5f80ccd"></a>

# LSP Settings


<a id="org625c4b2"></a>

## Angular LSP

```emacs-lisp
(setq lsp-clients-angular-language-server-command
      '("node"
        "/usr/lib/node_modules/@angular/language-server"
        "--ngProbeLocations"
        "/usr/lib/node_modules"
        "--tsProbeLocations"
        "/usr/lib/node_modules"
        "--stdio"))
```


<a id="org96eeb93"></a>

# Extra Plugins


<a id="org8cca2da"></a>

## ox-gfm

```emacs-lisp
(use-package! ox-gfm
  :after org
  :init
  (require 'ox-gfm nil t))
```


<a id="orgd9d617e"></a>

## PlantUML

```emacs-lisp
(use-package! ob-plantuml
  :ensure nil
  :commands
  (org-babel-execute: plantum)
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/Dropbox/opt/plantuml.jar")))
```


<a id="orgacff140"></a>

# Workarounds

```emacs-lisp
(fset 'battery-update #'ignore)
```
