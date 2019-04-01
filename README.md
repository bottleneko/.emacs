# My Emacs settings

## OS

Requirements:

* findutil
* coreutils
* gnugrep
* gnused
* the_silver_searcher (ag)
* ripgrep
* xclip

```sh
mv ~/.emacs.d ~/.emacs.d.bak
git clone https://github.com/bottleneko/bottleneko-emacs.git ~/.emacs.d
cd ~/.emacs.d/
git submodule update --recursive
```

## Emacs

`M-x` package-install-selected-packages
