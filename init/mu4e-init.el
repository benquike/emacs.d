;; make sure mu4e is in your load-path
(require 'mu4e)
(require 'smtpmail)

;; Only needed if your maildir is _not_ ~/Maildir
;; Must be a real dir, not a symlink
;;(setq mu4e-maildir "/home/user/Maildir")

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
;; (setq mu4e-sent-folder   "/sent")
;; (setq mu4e-drafts-folder "/drafts")
;; (setq mu4e-trash-folder  "/trash")

;; ;; smtp mail setting; these are the same that `gnus' uses.
;; (setq
;;    message-send-mail-function   'smtpmail-send-it
;;    smtpmail-default-smtp-server "smtp.purdue.edu"
;;    smtpmail-smtp-server         "smtp.purdue.edu")

(setq mu4e-maildir (expand-file-name "~/Maildir"))
;; Reference: http://0xax.blogspot.com/2014/11/emacs-mu4e-offlineimap-multiply-accounts.html
(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder   "/Sent"
      mu4e-trash-folder  "/Trash"
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 60
      user-mail-address "peng124@purdue.edu"
      user-full-name  "Hui Peng"
      mu4e-maildir-shortcuts
            '(("/INBOX"  . ?i)
              ("/Sent"   . ?s)
              ("/Trash"  . ?t))
     message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.office365.com"
     smtpmail-smtp-server "smtp.office365.com"
     smtpmail-smtp-service 587
     message-kill-buffer-on-exit t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-attachment-dir (expand-file-name "~/Downloads"))
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "html2text")
(setq mail-user-agent 'mu4e-user-agent)

(add-hook 'mu4e-compose-mode-hook
          (defun my-setup-epa-hook ()
	    (epa-mail-mode)))

(add-hook 'mu4e-view-mode-hook
	  (defun my-view-mode-hook ()
	    (epa-mail-mode)))

(defun my-mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (let* ((html (mu4e-message-field msg :body-html))
          (txt (mu4e-message-field msg :body-txt))
          (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      ;; simplistic -- but note that it's only an example...
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

(add-to-list 'mu4e-view-actions
	     '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)


(global-set-key [f1] 'mu4e)

(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
