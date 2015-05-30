(let ((minver "24"))
  (when (version<= emacs-version minver)
    (message "Your Emacs is a little bit old, some functionalities might not work, please use the latest version")
  )
)

;; (defun load-all-init-files (dir)
;;   "Load all XXX-init.el and YYY/XXX-init.el files"
;;   (dolist (initfile (file-expand-wildcards (format "%s/*-init.el" dir) t))
;;     (message "init file: %s" initfile)
;;     (load initfile)
;;   )
;;   (dolist (initfile (file-expand-wildcards (format "%s/*-init.el" dir) t))
;;     (message "init file: %s" initfile)
;;      (load initfile)
;;   )
;; )

;; (load-all-init-files "~/.emacs.d")


(load "~/.emacs.d/ext-packages/dir-init.el")
