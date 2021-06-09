;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq personal-packages
      '((personal :location local)))

(defun personal/init-personal ()
  (use-package personal))
