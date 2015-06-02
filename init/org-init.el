;; setting up capture
(setq org-default-notes-file "~/worklog/capture.org")
(define-key global-map "\C-cc" 'org-capture)

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


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)
