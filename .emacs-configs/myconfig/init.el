(unless (daemonp)
  (defvar bs--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (defun bs-reset-file-handler-alist-h ()
    (dolist (handler file-name-handler-alist)
      (add-to-list 'bs--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist bs--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'bs-reset-file-handler-alist-h)
  (add-hook 'after-init-hook '(lambda ()
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1)))
  )

(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))

(setq file-name-handler-alist nil
      gc-cons-threshold 100000000)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
