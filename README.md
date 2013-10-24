helm-dictionary
===============

Helm source for looking up dictionaries

## Install

Put the file `helm-dictionary.el` in your Emacs-Lisp load path and add the following in your Emacs startup file:

    (require 'helm-dictionary)

Alternatively, you can use autoload:

    (autoload 'helm-dictionary "helm-dictionary" "" t)

In order to specify a dictionary set the variable `helm-dictionary-database` to the filename of that dictionary.

A dictionary for German and English can be found in the Debian package trans-de-en.  This package is also available in many distributions derived from Debian such as Ubuntu.  Alternatively, this dictionary can also be downloaded here:

- http://www-user.tu-chemnitz.de/~fri/ding/

A dictionary for German and Spanish can be found here:

- https://savannah.nongnu.org/projects/ding-es-de

A variety of dictionaries with English as the source or target language can be found here:

- https://en.wiktionary.org/wiki/User:Matthias_Buchmeier

These dictionaries were automatically created from the Wiktionary database.  Their size and quality may vary.  Also generated from Wiktionary are the following dictionaries with Russian as the source or target language:

- http://wiktionary-export.nataraj.su/en/

## Usage

Use the command `helm-dictionary` to start a search.  The grep tool is used There are two actions available: insert the currently selected term in language 1 or language 2 at point (i.e., the cursor position at which `helm-dictionary` was called).
