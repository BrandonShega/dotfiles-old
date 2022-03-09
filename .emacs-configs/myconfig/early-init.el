;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 50 1000 1000))

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)

(setq create-lockfiles nil)

(setq inhibit-startup-message t)

(setq initial-major-mode 'fundamental-mode)
