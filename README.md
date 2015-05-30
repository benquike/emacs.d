# My Emacs Configuration

This is my collection of emacs configuration.

## INSTALL
Do backup your original configurations in .emacs and .emacs.d, as follows

```
$ mv .emacs .emacs.backup && mv .emacs.d .emacs.d.backup
```

After that, checkout this configuration by issuing the following command.

```
$ git clone --recursive https://github.com/benquike/emacs.d ~/.emacs.d
```


## Functionalities included

* [Keyfreq](https://github.com/dacap/keyfreq)
	* A plugin used to collect your key stroke statistics,
	from which you could see which keys you use mostly and
	thus decide how to　inprove your edit efficiency by
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

