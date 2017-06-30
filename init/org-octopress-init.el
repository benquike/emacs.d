`;; org octopress setting up
(require 'org-octopress)

(setq site-root "~/src/blog/yz-github")
(setq org-octopress-directory-top       (concat site-root "/source"))
(setq org-octopress-directory-posts     (concat site-root "/source/_posts"))
(setq org-octopress-directory-org-top   (concat site-root "/source"))
(setq org-octopress-directory-org-posts (concat site-root "/source/blog"))
(setq org-octopress-setup-file          "~/.emacs.d/init/octopress-setupfile.org")

(defun save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))


(setq org-publish-project-alist
      '(("blog-org" .  (:base-directory "~/git/blog/source/org_posts/"
					:base-extension "org"
					:publishing-directory "~/git/blog/source/_posts/"
					:sub-superscript ""
					:recursive t
					:publishing-function org-publish-org-to-octopress
					:headline-levels 4
					:html-extension "markdown"
					:octopress-extension "markdown"
					:body-only t))
	("blog-extra" . (:base-directory "~/git/blog/source/org_posts/"
					 :publishing-directory "~/git/blog/source/"
					 :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
					 :publishing-function org-publish-attachment
					 :recursive t
					 :author nil
					 ))
	("blog" . (:components ("blog-org" "blog-extra")))
	))
