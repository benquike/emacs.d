;; Display time on the status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(require 'openwith)
(setq openwith-associations '(
			      ("\\.pdf\\'" "evince" (file))
			      ("\\.odt\\'" "libreoffice" (file))
			      ("\\.doc\\'" "libreoffice" (file))
			      ("\\.docx\\'" "libreoffice" (file))
			      ("\\.ppt\\'" "libreoffice" (file))
			      ("\\.pptx\\'" "libreoffice" (file))
			      )
      )
(openwith-mode t)

(global-set-key (kbd "C-x o") 'switch-window)

(show-paren-mode)
