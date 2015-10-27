;; Define a list of my jabber.el accounts
(setq jabber-account-list '(
                            ("USER_NAME@chat.facebook.com"
                             (:network-server . "chat.facebook.com")
                             (:connection-type . starttls)
                             (:port . 5222)
                             (:password . "FACEBOOK_ACCOUNT"))))

;; Disable jabber.el presence notifications
(remove-hook 'jabber-alert-presence-hooks
             'sr-jabber-alert-presence-func)

;; Connect to all my jabber.el accounts on startup
(jabber-connect-all)
