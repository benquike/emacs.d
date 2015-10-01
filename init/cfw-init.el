(require 'calfw)
(require 'calfw-org)
(require 'calfw-cal)
(require 'calfw-ical)
; (require 'calfw-howm)
(setq cfw:org-agenda-schedule-args '(:timestamp))
(setq cfw:org-overwrite-default-keybinding t)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    (cfw:howm-create-source "Blue")  ; howm source
    (cfw:cal-create-source "Orange") ; diary source
    )))
