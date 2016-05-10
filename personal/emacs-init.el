;; Display time on the status bar

(server-start)

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

(if (string-equal window-system "x")
    (linum-mode t))
(defalias 'yes-or-no-p 'y-or-n-p)


(show-paren-mode)

(winner-mode 1)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

(require 'graphviz-dot-mode)

(load-theme 'material-light t)

;;; whitespace mode
;;; Reference: https://www.emacswiki.org/emacs/EightyColumnRule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
