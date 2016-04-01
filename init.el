(let ((minver "24"))
  (when (version<= emacs-version minver)
    (message "Your Emacs is a little bit old, some functionalities might not work, please use the latest version")
  )
)

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
	   org-email org-fstree org-outlook org-mime org-pdfview org-pomodoro
	   org-jekyll org-present org-password-manager
	   org-mac-iCal
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
	   which-key
	   material-theme
	   graphviz-dot-mode
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

;; show the bookmarks in the welcome page
;; TODO: collect the directories the user
;; accesses frequently and show them
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
