helm-dictionary
===============

Helm source for looking up dictionaries

## Install

Put the file `helm-dictionary.el` in your Emacs-Lisp load path and add the following in your Emacs startup file:

    (require 'helm-dictionary)

Alternatively, you can use autoload:

    (autoload 'helm-dictionary "helm-dictionary" "" t)

### Configure local dictionaries

In order to specify a dictionary set the variable `helm-dictionary-database` to the file name of that dictionary.

A dictionary for German and English can be found in the Debian package trans-de-en.  This package is also available in many distributions derived from Debian such as Ubuntu.  Alternatively, this dictionary can also be downloaded here:

- http://www-user.tu-chemnitz.de/~fri/ding/

A dictionary for German and Spanish can be found here:

- https://savannah.nongnu.org/projects/ding-es-de

A variety of dictionaries with English as the source or target language can be found here:

- https://en.wiktionary.org/wiki/User:Matthias_Buchmeier

These dictionaries were automatically created from the Wiktionary database.  Their size and quality may vary.  Also generated from Wiktionary are the following dictionaries with Russian as the source or target language:

- http://wiktionary-export.nataraj.su/en/

### Configure online web dictionaries

If your local dictionaries doesn't have an entry for a word, it can be useful to try online dictionaries available on the web.  Helm-dictionary has a dummy source that allows you to lookup currently entered string in these online dictionaries.  The variable `helm-dictionary-online-dicts` allows you to configure which online dictionaries should be listed.  The variable is a list conses.  The first element of each cons specifies the name of the online dictionary for display during searches.  The second element of each cons is the URL used for retrieving search results from the respective online dictionary.  This URL has to contain a "%s" at the position where the search term should be inserted.

By default, helm-dictionary uses the function `browse-url-default-browser` for opening online dictionaries.  This function opens an external web browser or opens a new tab in a running browser.  If you prefer to access online dictionaries inside Emacs, you can redefine the customization variable `helm-dictionary-browser-function` to point to an alternative function for opening URLs, e.g., `eww-browse-url`:

    (require 'eww)
    (setq helm-dictionary-browser-function 'eww-browse-url)

## Usage

Use the command `helm-dictionary` to start a new search.  Alternatively, you can use `helm-dictionary-word-at-point` to search the word under the cursor.  Since the grep tool is used to perform the searches, you can also use regular expressions.

There are two actions available: insert the currently selected term in language 1 or language 2 at point, i.e., the cursor position at which `helm-dictionary` was called.

In the section "Lookup online", you can choose among several online dictionaries.  If you select one of the entries listed in this section, a browser will be used to display search results from the respective online dictionary.
