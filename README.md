# Dot Emacs

[![Build Status](https://travis-ci.com/julien-montmartin/dot-emacs.svg?branch=master)](https://travis-ci.com/julien-montmartin/dot-emacs)

My Emacs configuration file using [Org](https://orgmode.org/). It is processed by Travis CI to produce:

- some LISP code, *tangled* in [emacs.el](https://julien-montmartin.github.io/dot-emacs/emacs.el) (with a small [loader](https://julien-montmartin.github.io/dot-emacs/loader.el))
- some HTML doc, served as a GitHub [page](https://julien-montmartin.github.io/dot-emacs/emacs.html) (in French).


This kind of `sh` script may help you to download the latest `emacs.el` from this repo:

```bash
getDotEmacs() {

	curl -sL -o emacs.el https://julien-montmartin.github.io/dot-emacs/emacs.el
	ls -lh emacs.el
}
```
