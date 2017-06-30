;; Display time on the status bar

(server-start)


(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(require 'openwith)

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (setq openwith-associations '(
				  ("\\.pdf\\'" "evince" (file))
				  ("\\.odt\\'" "libreoffice" (file))
				  ("\\.doc\\'" "libreoffice" (file))
				  ("\\.docx\\'" "libreoffice" (file))
				  ("\\.ppt\\'" "libreoffice" (file))
				  ("\\.pptx\\'" "libreoffice" (file))
				  )
	  )
    )))

(openwith-mode t)

(global-set-key (kbd "C-x o") 'switch-window)

(if (string-equal window-system "x")
    (linum-mode t))
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'epa)
(setenv "GPG_AGENT_INFO" nil)

(show-paren-mode)
(setq column-number-mode t)

(google-this-mode 1)
(global-set-key (kbd "C-x g") 'google-this-mode-submap)

(winner-mode 1)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

; (require 'graphviz-dot-mode)

(global-set-key "\C-cei" 'ebib)

(load-theme 'material-light t)

(require 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/worklog/")
(setq deft-recursive t)
(global-set-key [f8] 'deft)
(setq deft-use-filename-as-title t)

;; (cmake-ide-setup)
; Add cmake listfile names to the mode list.
;; (setq auto-mode-alist
;; 	  (append
;; 	   '(("CMakeLists\\.txt\\'" . cmake-mode))
;; 	   '(("\\.cmake\\'" . cmake-mode))
;; 	   auto-mode-alist))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
(require 'cpputils-cmake)

(require 'tablegen-mode)



;;; whitespace mode
;;; Reference: https://www.emacswiki.org/emacs/EightyColumnRule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
