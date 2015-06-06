;; add the subdirectories to load-path
(let ((current-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path current-dir)
  (dolist (file (directory-files current-dir))
    (unless (or (not (file-directory-p (format "%s/%s" current-dir file)))
		(string= file ".")
		(string= file ".."))
      (add-to-list 'load-path (format "%s/%s" current-dir file))
    )
  )
)

