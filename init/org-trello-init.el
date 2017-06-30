(require 'org-trello)

(custom-set-variables
 '(orgtrello-log-level orgtrello-log-debug) ;; log level to debug
 '(orgtrello-setup-use-position-in-checksum-computation nil) ;; checksum without position
 '(org-trello-files (file-expand-wildcards  "~/worklog/org-trello/*.org")))
