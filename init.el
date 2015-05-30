(let ((minver "24"))
  (when (version<= emacs-version minver)
    (message "Your Emacs is a little bit old, some functionalities might not work, please use the latest version")
  )
)

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
  (load-files-wildcards (format "%s/**/*-init.el" load-dir))
)
