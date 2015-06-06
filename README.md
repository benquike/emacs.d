# My Emacs Configuration

This is my collection of emacs configuration.

## INSTALL
Do backup your original configurations in .emacs and .emacs.d, as follows

```
$ mv .emacs .emacs.backup && mv .emacs.d .emacs.d.backup
```

After that, checkout this configuration by issuing the following commands.

```
$ git clone --recursive https://github.com/benquike/emacs.d ~/.emacs.d
```
```
$ git submodule update --init
```


## Functionalities included

* [Keyfreq](https://github.com/dacap/keyfreq)
	* A plugin used to collect your key stroke statistics,
	from which you could see which keys you use mostly and
	thus decide how to　improve your edit efficiency by
	modifying your keybindings.

* [ggtags](https://github.com/leoliu/ggtags)
	* An Emacs interface for [GNU Global](http://www.gnu.org/software/global/),
	  a source taging and searching tool.

* [company-mode](http://company-mode.github.io/)
	* A completion framework, which stands for **comp**lete for
	**any**thing.

* [yasnippet](https://github.com/capitaomorte/yasnippet)
	* A template system, using witch you could input an abbrevation
	 and Emacs will expand it to some template for you, like function
	 definition, file inclusion ....

* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
	* The Edit mode for Markdown format

* [org-trello](https://github.com/org-trello/org-trello)
    * An Emacs interface to use [Trello](https://trello.com).

* [undo-tree](http://www.emacswiki.org/emacs/UndoTree)
    * A package to show your undo history items, and from it you could redo them

* [Magit](https://github.com/magit/magit)
    * An Emacs interface to use Git

* [org-octopress](https://github.com/yoshinari-nomura/org-octopress)
    * An emacs interface to manage, write(using [org-mode](http://orgmode.org/))
	and export static blogs
