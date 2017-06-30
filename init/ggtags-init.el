(define-key global-map "\M-." 'ggtags-find-tag-dwim)
(define-key global-map "\M-*" 'pop-tag-mark)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode 'java-mode)
	                    (ggtags-mode 1))))
