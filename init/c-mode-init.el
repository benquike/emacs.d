;; (defun my-c-mode-common-hook()
;;   (whitespace-mode t)
;;   (linum-mode t)
;;   (c-set-style "python")
;;   (which-function-mode t))


;; (global-ede-mode 1)
;; (require 'semantic/sb)
;; (semantic-mode 1)

;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'semantic)
(require 'srecode)
(require 'ede)
(semantic-mode 1)

;; (defun my-c-mode-cedet-hook ()
;;  (local-set-key "." 'semantic-complete-self-insert)
;;  (local-set-key ">" 'semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
;; ;; (global-semantic-tag-folding-mode 1)
;; (require 'semantic/ia)
;; (require 'semantic/bovine/gcc)

;; Max 80 cols per line, indent by two spaces, no tabs.
;; Apparently, this does not affect tabs in Makefiles.
(custom-set-variables
 '(fill-column 80)
 '(c++-indent-level 2)
 '(c-basic-offset 2)
 '(indent-tabs-mode nil))

(add-hook 'c-mode-common-hook   'hs-minor-mode)

;; add make emacs open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Alternative to setting the global style.  Only files with "llvm" in
;; their names will automatically set to the llvm.org coding style.
(c-add-style "llvm.org"
             '((fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)))

(add-hook 'c-mode-hook
	  (function
	   (lambda nil 
	     (if (string-match "llvm" buffer-file-name)
		 (progn
		   (c-set-style "llvm.org")
		   (linum-mode t)
		   )
	       ))))

(add-hook 'c++-mode-hook
	  (function
	   (lambda nil 
	     (if (string-match "llvm" buffer-file-name)
		 (progn
		   (c-set-style "llvm.org")
		   (linum-mode t)
		   )
	       ))))
