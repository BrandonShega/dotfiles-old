;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun go-to-projects ()
  (interactive)
  (find-file (concat org-directory "/todo.org"))
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* 1 Projects")
  (beginning-of-line))

(defun project-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?p)
  (org-columns))

(defun project-deadline-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?d)
  (org-columns))

(defun my-org-agenda-list-stuck-projects ()
  (interactive)
  (go-to-projects)
  (org-agenda nil "#" 'subtree))

(defun go-to-areas ()
  (interactive)
  (find-file (concat org-directory "/todo.org"))
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* 2 Areas")
  (beginning-of-line))

(defun areas-overview ()
  (interactive)
  (go-to-areas)
  (org-narrow-to-subtree)
  (org-columns))

(when (configuration-layer/package-used-p 'outshine)
  (defun outshine-fix-narrow-pos (&rest args)
    "Narrowing works within the headline rather than requiring to be on it."
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  (defun outshine-fix-insert-pos (&rest args)
    "Advise outshine heading insertion newlining to my organization workflow.
Newline insertion now won't match org-mode, will act like block insertion.
If on a heading, insert on new next line.
If not on a heading, insert newline before current-line first."
    (unless (outline-on-heading-p t)
      (forward-line -1)
      (end-of-line) (newline))
    (end-of-line) (newline))

  (defun org-fix-heading-pos (&rest args)
    "Advise org heading insertion, on heading -> no roll-over text after point."
    (when (outline-on-heading-p) (end-of-line)))

  (defun outshine-insert-subheading ()
    "A subheading variation on `outshine-insert-heading'.
Due to a bug with `outline-demote', this function only inserts
the subheading, rather than the heading, correctly when the
subheading level already exists within the buffer."
    (interactive)
    (evil-with-single-undo
      (outshine-insert-heading)
      (set-mark (line-beginning-position)) (goto-char (line-end-position))
      (outline-demote 'region))))

(defun eshell-pop-eshell ()
  "Eshell popup straight to insert mode."
  (interactive)
  (spacemacs/shell-pop-eshell nil)
  (if (string= major-mode "eshell-mode")
      (evil-insert 1)
    (evil-escape)))
