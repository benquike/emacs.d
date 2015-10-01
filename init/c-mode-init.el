(defun my-c-mode-common-hook()
  (whitespace-mode t)
  (linum-mode t)
  (c-set-style "python")
  (which-function-mode t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
