;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun draft-auto-save-buffer-name-handler (operation &rest args)
  "for `make-auto-save-file-name' set '.' in front of the file name; do nothing for other operations"  
  (if
      (and buffer-file-name (eq operation 'make-auto-save-file-name))
      (concat (file-name-directory buffer-file-name)
              "."
              (file-name-nondirectory buffer-file-name))
    (let ((inhibit-file-name-handlers
           (cons 'draft-auto-save-buffer-name-handler
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))

(add-to-list 'file-name-handler-alist '("Drafts/cur/" . draft-auto-save-buffer-name-handler))
