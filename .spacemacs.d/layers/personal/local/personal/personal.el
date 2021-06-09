;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'mu4e)
(provide 'personal)

(setq user-full-name "Brandon Shega"
      user-mail-address "b.shega@gmail.com"
      epa-file-encrypt-to user-mail-address)

(setq company-idle-delay 0.0)

(setq js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil)

(setq initial-buffer-choice t)
(setq vc-follow-symlinks t)

(setq mu4e-maildir (expand-file-name "~/mail"))

(setq mu4e-update-interval 60
      mu4e-get-mail-command "true"
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-completing-read-function 'completing-read
      mu4e-get-mail-command "mbsync -aq"
      mu4e-update-interval 300
      mu4e-change-filenames-when-moving t
      mu4e-sent-messages-behavior (lambda ()
                                    (if (string= (message-sendmail-envelope-from) "b.shega@icloud.com")
                                        'sent 'delete))
      mu4e-html2text-command 'mu4e-shr2text)
      ;; mu4e-html2text-command "iconv -c -t utf8 | pandoc -f html -t plain")

(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)

(setq mu4e-bookmarks
      `(
        ("maildir:/work/Inbox OR maildir:/home/Inbox" "All Inboxes" ?i)
        ("maildir:/work/Inbox" "Work" ?w)
        ("maildir:/personal/Inbox" "Personal" ?p)
        ("(maildir:/work/Inbox OR maildir:/personal/Inbox) AND flag:unread AND NOT flag:trashed" "Unread Messages" ?u)
        ))

(setq mu4e-contexts
      `( ,(make-mu4e-context
      :name "Work"
      :match-func (lambda (msg) (when msg (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
      :vars '(
                  (mu4e-trash-folder . "/work/Deleted Items")
                  (mu4e-sent-folder . "/work/Sent Items")
                  (mu4e-drafts-folder . "/work/Drafts")
                  (user-mail-address . "brandon.shega@fbgpg.com")
                  (user-full-name . "Brandon Shega")
                  (smtpmail-smtp-user . "brandon.shega@fbgpg.com")
                  (smtpmail-local-domain . "office365.com")
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
                  ))
      ,(make-mu4e-context
        :name "Personal"
        :match-func (lambda (msg) (when msg (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
        :vars '(
                (mu4e-trash-folder . "/personal/Trash")
                (mu4e-sent-folder . "/personal/Sent Messages")
                (mu4e-drafts-folder . "/personal/Drafts")
                (user-mail-address . "b.shega@icloud.com")
                (user-full-name . "Brandon Shega")
                (smtpmail-smtp-user . "b.shega")
                (smtpmail-local-domain . "mail.me.com")
                (smtpmail-smtp-server . "smtp.mail.me.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
                ))
      ))

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook #'flyspell-mode)

(setq mail-user-agent 'mu4e-user-agent)

(setq shr-color-visible-luminance-min 80)
(setq shr-color-visible-distance-min 5)

(setq message-send-mail-function 'smtpmail-send-it)

(mu4e-alert-set-default-style 'notifier)
(setq mu4e-alert-interesting-mail-query "(maildir:/work/Inbox OR maildir:/personal/Inbox) AND flag:unread AND NOT flag:trashed")
(setq mu4e-alert-email-notification-types '(subjects))

(setq org-agenda-use-time-grid nil
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-habit-show-habits t
      org-directory "~/Dropbox/Organization"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (list org-directory)
      org-capture-templates
        '(("g" "GTD")
          ("r" "References")
          ("d" "Diary")
          ("n" "Notes")
          ("t" "TODO")))

(setq org-capture-templates
      '(
        ("t" "Task" entry (file "~/Dropbox/Organization/inbox.org")
        "* TODO %?\n")
        ("p" "Project" entry (file+headline "~/Dropbox/Organization/todo.org" "1 Projects")
        (file "~/Dropbox/Organization/templates/newprojecttemplate.org"))
        ("g" "Goal" entry (file+headline "~/Dropbox/Organization/goals.org" "Current Goals")
          (file "~/Dropbox/Organization/templates/newgoaltemplate.org"))))

(spacemacs/declare-prefix "aog" "project")
(spacemacs/set-leader-keys "aogp" 'go-to-projects)
(spacemacs/set-leader-keys "aogn" 'project-overview)
(spacemacs/set-leader-keys "aogm" 'areas-overview)
(spacemacs/set-leader-keys "aogl" 'project-deadline-overview)
(spacemacs/set-leader-keys "aogs" 'my-org-agenda-list-stuck-projects)
(spacemacs/set-leader-keys "aoga" 'go-to-areas)

(setq jiralib-url "https://gpgdigital.atlassian.net")
