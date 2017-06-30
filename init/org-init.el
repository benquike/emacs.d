;; setting up capture
(require 'org-habit)
(require 'org-pomodoro)

(setq org-default-notes-file "~/worklog/capture.org")
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(require 'kanban)

(global-set-key (kbd "C-c C-x C-i") 'org-pomodoro)
(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "REPORT" "DONE")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/worklog/gtd.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+headline "~/worklog/journal.org" "Journals")
            "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "Code" entry (file+headline "~/worklog/code-reading.org" "Snippets")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Work" entry (file+headline "~/worklog/WorkCapture.org" "WorkLog")
             "* %?\nEntered on %U\n  %i\n  %a")))

;; my customization of org-mode
(setq org-dailylog-root-dir "~/worklog/dailylog")

(defun org-open-dailylog-today()
  (interactive)
  (find-file (format "%s/%s.org" org-dailylog-root-dir
                     (format-time-string "%Y_%m_%d"))))

(global-set-key (kbd "C-c f w") 'org-open-dailylog-today)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(transient-mark-mode 1)

(setq org-catch-invisible-edits 1)

(setq org-agenda-files (list "~/worklog/gtd.org"
                             "~/worklog/schedule.org"))

(setq org-export-with-sub-superscripts nil)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

; Here is your client ID
; 590318881610-p4tq27m1b6lq8no8vombmks763pvss9p.apps.googleusercontent.com

; Here is your client secret
; FkXzQIb0hMxtKtXt4MJu1omk
(require 'org-gcal)
(setq org-gcal-client-id "590318881610-p4tq27m1b6lq8no8vombmks763pvss9p.apps.googleusercontent.com"
      org-gcal-client-secret "FkXzQIb0hMxtKtXt4MJu1omk"
      org-gcal-file-alist '(("benquike@gmail.com" .  "~/worklog/schedule.org")))

(setq org-tag-faces
      '(("Doing" :foreground "#FF0000")))

(defun my-sparse-doing-tree ()
  (interactive)
  (org-tags-view nil "Doing"))

(define-key org-mode-map (kbd "C-c 3") 'my-sparse-doing-tree)

(org-defkey org-agenda-mode-map [(tab)]
  '(lambda () (interactive)
    (org-agenda-goto)
    (with-current-buffer "*Org Agenda*"
      (org-agenda-quit))))

(require 'org-notify)
(org-notify-start)
(org-notify-add 'appt
                '(:time "-1s" :period "20s" :duration 10
                  :actions (-message -ding))
                '(:time "15m" :period "2m" :duration 100
                  :actions -notify)
                '(:time "2h" :period "5m" :actions -message)
                '(:time "3d" :actions -email))

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (ruby . t)
   (js . t)
   (C . t)))

(org-add-link-type "ebib" 'ebib-open-org-link)
