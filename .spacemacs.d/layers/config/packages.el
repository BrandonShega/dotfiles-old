;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq config-packages
      '(
        magithub
        mocha
        mac-pseudo-daemon
        s
        eshell
        outshine
        visual-regexp
        visual-regexp-steroids
        ))

(defun config/init-magithub ()
  (use-package magithub
    :defer t))

(defun config/init-mocha ()
  (use-package mocha
    :defer t))

(defun config/init-mac-pseudo-daemon ()
  (use-package mac-pseudo-daemon
    :defer t
    :config
    (setq mac-pseudo-daemon-mode t)))

(defun config/init-s ()
  (use-package s
    :defer t))

(defun config/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    :defer t
    :post-init
    (evil-define-key '(normal insert) 'global (kbd "C-e") 'eshell-pop-eshell)))

(defun config/init-outshine ()
  (use-package outshine
    :defer t
    :hook ((prog-mode          . outline-minor-mode)
           (outline-minor-mode . outshine-mode))

    :bind (("<backtab>"     . outshine-cycle-buffer)
           ([(meta return)]       . outshine-insert-heading)
           ([(meta shift return)] . outshine-insert-subheading)
           :map outline-minor-mode-map)

    :init
    (progn
      (evil-define-key '(normal visual motion) outline-minor-mode-map
        "gh" 'outline-up-heading
        "gj" 'outline-forward-same-level
        "gk" 'outline-backward-same-level
        "gl" 'outline-next-visible-heading
        "gu" 'outline-previous-visible-heading)

      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen
        "nj" 'outline-move-subtree-down
        "nk" 'outline-move-subtree-up
        "nh" 'outline-promote
        "nl" 'outline-demote)

      (advice-add 'outshine-narrow-to-subtree :before 'outshine-fix-narrow-pos)

      (advice-add 'outshine-insert-heading    :before 'outshine-fix-insert-pos)
      (advice-add 'outshine-insert-heading    :after 'evil-insert-advice)
      (advice-add 'outshine-insert-subheading :after 'evil-insert-advice)

      ;; Fix the new bindings in outline-minor-mode overwriting org-mode-map
      ;; I also add advice here because it mirrors outshine modifications
      (spacemacs|use-package-add-hook org
        :post-config
        (progn
          (bind-keys :map org-mode-map
                     ([(meta return)]       . org-meta-return)
                     ([(meta shift return)] . org-insert-subheading))
          (advice-add 'org-insert-heading    :before 'org-fix-heading-pos)
          (advice-add 'org-insert-heading    :after 'evil-insert-advice)
          (advice-add 'org-insert-subheading :after 'evil-insert-advice))))))

(defun config/init-visual-regexp ()
  (use-package visual-regexp
    :defer t
    :init
    (spacemacs/set-leader-keys
      "Rr" 'vr/replace
      "Rs" 'vr/query-replace)))

(defun config/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t))
