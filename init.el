(let ((minver "24"))
  (when (version<= emacs-version minver)
    (message "Your Emacs is a little bit old, some functionalities might not work, please use the latest version")
  )
  )

(setq user-full-name "Hui Peng"
       user-mail-address "peng124@purdue.edu"
       message-generate-headers-first t)

(require 'cl)
;; Proxy server setting
;; (setq url-proxy-services '(("no_proxy" . "default_gateway")
;;                            ("http" . "HTTP_PROXY_SERVER:PORT")
;;                            ("https" . "HTTPS_PROXY_SERVER:PORT")))

(defun load-files-wildcards (wildcards)
  "Load all the files specified by wildcards"
  (let ((all-files (file-expand-wildcards wildcards t)))
    (while all-files
      (load (car all-files))
      (setq all-files (cdr all-files))
    )
  )
)

(let ((load-dir (file-name-directory load-file-name)))
  (load-files-wildcards (format "%s/*-init.el" load-dir))
  (load-files-wildcards (format "%s/**/dir-init.el" load-dir))
)

;;; package setup
;;; refer to http://emacswiki.org/emacs/ELPA

(require 'package)
;;; setup the archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t); Org-mode's repository

;;; org-trello's repository
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages") t)

(package-initialize)


(defvar my-packages-to-install
  '(ggtags company yasnippet ack org-trello helm org-octopress
	   auto-install dired-sort dired-open
	   dirtree dropbox enotify google google-c-style google-maps google-translate
	   org2jekyll org-jira org-gcal org-dropbox org-dashboard org-doing org-ehtml

	   org-email org-fstree
	   ;; org-outlook
	   org-mime org-pdfview org-pomodoro

	   org-jekyll org-present org-password-manager
	   org-mac-iCal org-alert org-beautify-theme
	   org-toodledo
	   org-wunderlist
	   bbdb bbdb-china bbdb-csv-import
	   w3m
	   openwith octopress
	   orglink
	   ztree
	   keyfreq guide-key guide-key-tip
	   switch-window
	   ;; calendar
	   calfw calfw-gcal
	   cal-china-x
	   elscreen elscreen-persist
	   undo-tree
	   cygwin-mount
	   magit
	   auto-yasnippet function-args
	   jabber jabber-otr
	   origami
	   which-key
	   sauron
	   elfeed-web
	   elfeed-org
	   visual-regexp
           phi-search
	   web-mode
	   php-mode
	   php-auto-yasnippets
	   php-completion
	   rich-minority
	   x86-lookup
	   zotelo
	   multiple-cursors
	   expand-region
	   kanban
	   google-this
	   google-contacts
	   material-theme

	   auctex
	   company-auctex
	   youdao-dictionary
;;	   graphviz-dot-mode
	   ebib
;;	   cmake-ide
	   cmake-font-lock
	   cmake-mode
	   cpputils-cmake
           ag
           gitlab
	   helm-gitlab
	   xwidgete
           swift-mode
           mu4e-alert
           realgud
	   go-mode
	   chinese-pyim
	   bison-mode
	   )
  "The packages Emacs will try to install when it starts up.")

(defun my-packages-installed-p ()
  (loop for p in my-packages-to-install
        when (not (package-installed-p p)) do (return nil)
        finally (return t))
)

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)

  (message "Emacs is checking whether the packages are installed...")
  (dolist (p my-packages-to-install)
    (when (not (package-installed-p p))
      (package-install p))
  )
  (message "Checking done"))

(let ((load-dir (file-name-directory load-file-name)))
  (load-files-wildcards (format "%s/**/*-init.el" load-dir))
)

;; setting up the IME
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

;; show the bookmarks in the welcome page
;; TODO: collect the directories the user
;; accesses frequently and show them
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c++-indent-level 2)
 '(c-basic-offset 2)
 '(evernote-developer-token
   "S=s6:U=b7a7d:E=159ad4d4cc7:C=152559c1f28:P=1cd:A=en-devtoken:V=2:H=5c0eb309ba2357025a2dd0b7d6858e6e")
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(org-trello-files (file-expand-wildcards "~/worklog/org-trello/*.org") nil (org-trello))
 '(orgtrello-log-level orgtrello-log-debug nil (org-trello))
 '(orgtrello-setup-use-position-in-checksum-computation nil)
 '(package-selected-packages
   (quote
    (latex-preview-pane ag graphviz-dot-mode cpputils-cmake cmake-font-lock cmake-ide ebib youdao-dictionary company-auctex auctex material-theme google-contacts google-this kanban expand-region multiple-cursors zotelo x86-lookup rich-minority php-completion php-auto-yasnippets php-mode web-mode phi-search visual-regexp elfeed-org elfeed-web sauron which-key origami jabber-otr jabber function-args auto-yasnippet magit cygwin-mount undo-tree elscreen-persist elscreen cal-china-x calfw-gcal calfw switch-window guide-key-tip guide-key keyfreq ztree orglink octopress openwith w3m bbdb-csv-import bbdb-china bbdb org-wunderlist org-toodledo org-beautify-theme org-alert org-mac-iCal org-password-manager org-present org-jekyll org-pomodoro org-pdfview org-mime org-fstree org-email org-ehtml org-doing org-dashboard org-dropbox org-gcal org-jira org2jekyll google-translate google-maps google-c-style google enotify dropbox dirtree dired-open dired-sort auto-install org-octopress helm org-trello ack yasnippet company ggtags)))
 '(safe-local-variable-values (quote ((c-style . whitesmith))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.purdue.edu")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
