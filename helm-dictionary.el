;;; helm-dictionary.el --- Helm source for looking up dictionaries

;; Copyright 2013 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;;         Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/emacs-helm/helm-dictionary
;; Version: 1.0.0
;; Package-Requires: ((helm "1.5.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This helm source can be used to look-up words in local (offline)
;; dictionaries.  It also provides short-cuts for various online
;; dictionaries, which is useful in situations where the local
;; dictionary doesn't have an entry for a word.
;;
;; Dictionaries are available for a variety of language pairs.  See
;; the project page for an incomplete list:
;;
;;     https://github.com/emacs-helm/helm-dictionary

;;; Install:

;; Put this file on your Emacs-Lisp load path and add the following in
;; your Emacs startup file:
;;
;;     (require 'helm-dictionary)
;;
;; Alternatively, you can use autoload:
;;
;;     (autoload 'helm-dictionary "helm-dictionary" "" t)
;;
;; In order to specify a dictionary set the variable
;; `helm-dictionary-database' to the filename of that dictionary.
;;
;; A dictionary for German and English can be found in the Debian
;; package trans-de-en.  This package is also available in many
;; distributions derived from Debian such as Ubuntu.  Alternatively,
;; this dictionary can also be downloaded here:
;; 
;;   http://www-user.tu-chemnitz.de/~fri/ding/
;;
;; A dictionary for German and Spanish can be found here:
;;
;;   https://savannah.nongnu.org/projects/ding-es-de
;;
;; A variety of dictionaries with English as the source or target
;; language can be found here:
;;
;;   https://en.wiktionary.org/wiki/User:Matthias_Buchmeier
;;
;; These dictionaries were automatically created from the Wiktionary
;; database.  Their size and quality may vary.  Also generated from
;; Wiktionary are the following dictionaries with Russian as the
;; source or target language:
;;
;;   http://wiktionary-export.nataraj.su/en/
;;
;; If the local dictionary doesn't have an entry for a word, it can be
;; useful to try online dictionaries available on the
;; web.  Helm-dictionary has a dummy source that provides shortcuts
;; for looking up the currently entered string in these online
;; dictionaries.  The variable `helm-dictionary-online-dicts'
;; specifies which online dictionaries should be listed.  The value of
;; that variable is a list conses.  The first element of each cons
;; specifies the name of an online dictionary for display during
;; searches.  The second element is the URL used for retrieving search
;; results from the respective dictionary.  This URL has to contain a
;; "%s" at the position where the search term should be inserted.
;;
;; Helm-dictionary uses the function `browse-url' for opening online
;; dictionaries.  Usually, this function opens the URL in an external
;; web browser.  If a different method for opening URLs is preferred,
;; the customization variable `helm-dictionary-browser-function' can
;; be set to an alternative function for opening URLs such as
;; `eww-browse-url':
;;
;;    (require 'eww)
;;    (setq helm-dictionary-browser-function 'eww-browse-url)
;;
;; Admissible values for `helm-dictionary-browser-function` are the
;; same as for `browse-url-browser-function`.  If set to nil, the
;; current emacs-wide default will be used, i.e., the browser
;; specified in `browse-url-browser-function`.

;;; Usage:

;; Use the command `helm-dictionary' to start a new
;; search.  Alternatively, you can use `helm-dictionary-word-at-point'
;; to search the word under the cursor.  Since the grep tool is used
;; to perform the searches, you can also use regular expressions.

;; There are two actions available: insert the currently selected term
;; in language 1 or language 2 at point, i.e., the cursor position at
;; which `helm-dictionary' was called.

;; In the section "Lookup online", you can choose among several online
;; dictionaries.  If you select one of the entries listed in this
;; section, a browser will be used to display search results from the
;; respective online dictionary.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'browse-url)

(defgroup helm-dictionary nil
  "Helm plugin for looking up a dictionary."
  :group 'helm)

(defcustom helm-dictionary-database "/usr/share/trans/de-en"
  "The file containing the dictionary."
  :group 'helm-dictionary
  :type  'file)

(defcustom helm-dictionary-ignore-case t
  "Whether or not case should be ignored when searching.
Case-sensitive searchers are much faster than case-insensitive
searchers."
  :group 'helm-dictionary
  :type  'boolean)

(defcustom helm-dictionary-online-dicts
    '(("translate.reference.com de->eng" .
       "http://translate.reference.com/translate?query=%s&src=de&dst=en")
      ("translate.reference.com eng->de" .
       "http://translate.reference.com/translate?query=%s&src=en&dst=de")
      ("leo eng<->de" .
       "http://dict.leo.org/ende?lp=ende&lang=de&search=%s")
      ("en.wiktionary.org" . "http://en.wiktionary.org/wiki/%s")
      ("de.wiktionary.org" . "http://de.wiktionary.org/wiki/%s")
      ("linguee-eng<->de"    . "http://www.linguee.de/deutsch-englisch/search\
?sourceoverride=none&source=auto&query=%s"))
    "Alist of online dictionaries.")

(defcustom helm-dictionary-browser-function nil
  "The browser that is used to access online dictionaries.  If
nil, the current emacs-wide standard browser will be used, i.e.,
the browser specified by the customization variable
`browse-url-browser-function'.  Other possible values are all
values that are admissible for the `browse-url-browser-function'."
  :group 'helm-dictionary
  :type '(choice
          (const         :tag "Currently configured default for Helm"
                         :value nil)
          (function-item :tag "Emacs interface to w3m" :value w3m-browse-url)
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "W3 in another Emacs via `gnudoit'"
                         :value  browse-url-w3-gnudoit)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Galeon" :value  browse-url-galeon)
          (function-item :tag "Epiphany" :value  browse-url-epiphany)
          (function-item :tag "Netscape" :value  browse-url-netscape)
          (function-item :tag "eww" :value  eww-browse-url)
          (function-item :tag "Mosaic" :value  browse-url-mosaic)
          (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
          (function-item :tag "Text browser in an xterm window"
                         :value browse-url-text-xterm)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "KDE" :value browse-url-kde)
          (function-item :tag "Elinks" :value browse-url-elinks)
          (function-item :tag "Specified by `Browse Url Generic Program'"
                         :value browse-url-generic)
          (function-item :tag "Default Windows browser"
                         :value browse-url-default-windows-browser)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function-item :tag "GNOME invoking Mozilla"
                         :value browse-url-gnome-moz)
          (function-item :tag "Default browser"
                         :value browse-url-default-browser)
          (function      :tag "Your own function")
          (alist         :tag "Regexp/function association list"
                         :key-type regexp :value-type function))
)


(defun helm-dictionary-init ()
  "Initialize async grep process for `helm-source-dictionary'."
  (let ((process-connection-type nil)
        (cmd (format "grep %s '%s' %s | grep -v '^#'"
                     (if helm-dictionary-ignore-case "-i" "")
                     (replace-regexp-in-string "\\\\$" "" helm-pattern)
                     helm-dictionary-database)))
    (prog1
      (start-process-shell-command "helm-dictionary" helm-buffer cmd)
      (set-process-sentinel
        (get-buffer-process helm-buffer)
        #'(lambda (process event)
            (if (string= event "finished\n")
                (with-helm-window
                  (setq mode-line-format
                        '(" " mode-line-buffer-identification " "
                          (line-number-mode "%l") " "
                          (:eval (propertize
                                  (format "[Grep Process Finish- (%s results)]"
                                          (max (1- (count-lines
                                                    (point-min) (point-max))) 0))
                                  'face 'helm-grep-finish))))
                  (force-mode-line-update))
                (helm-log "Error: Egrep %s"
                          (replace-regexp-in-string "\n" "" event))))))))


(defun helm-dictionary-transformer (candidates)
  "Formats entries retrieved from the data base."
  (loop for i in candidates
        for entry = (split-string i " :: ")
        for l1terms = (split-string (car entry) " | ")
        for l2terms = (split-string (cadr entry) " | ")
        for filtered-helm-pattern = (replace-regexp-in-string "\\\\$" "" helm-pattern)
        for width = (save-excursion (with-helm-window (window-width)))
        append
        (loop for l1term in l1terms
              for l2term in l2terms
              if (or (string-match filtered-helm-pattern l1term)
                     (string-match filtered-helm-pattern l2term))
              collect
              (cons 
                (concat
                  (truncate-string-to-width l1term (- (/ width 2) 1) 0 ?\s)
                  " "
                  (truncate-string-to-width l2term (- (/ width 2) 1) 0 ?\s))
                (cons l1term l2term)))))


(defun helm-dictionary-insert-l1term (entry)
  (insert
    (replace-regexp-in-string
      " *{.+}\\| *\\[.+\\]" "" (car entry))))

(defun helm-dictionary-insert-l2term (entry)
  (insert
    (replace-regexp-in-string
      " *{.+}\\| *\\[.+\\]" "" (cdr entry))))


(defvar helm-source-dictionary
  '((name . "Search dictionary")
    (candidates-process . helm-dictionary-init)
    (candidate-transformer . helm-dictionary-transformer)
    (delayed)
    (nohighlight)
    (no-matchplugin)
    (action . (("Insert source language term" . helm-dictionary-insert-l1term)
               ("Insert target language term" . helm-dictionary-insert-l2term)))))

(defvar helm-source-dictionary-online
  `((name . "Lookup online")
    (match (lambda (_candidate) t))
    (candidates . helm-dictionary-online-dicts)
    (no-matchplugin)
    (nohighlight)
    (action
     . (lambda (cand)
         (let ((browse-url-browser-function
                (or helm-dictionary-browser-function
                    browse-url-browser-function)))
           (helm-browse-url (format cand (url-hexify-string helm-pattern)))))))
  "Source for online lookup.")

;;;###autoload
(defun helm-dictionary ()
  (interactive)
  (helm :sources '(helm-source-dictionary helm-source-dictionary-online)
        :full-frame t
        :candidate-number-limit 500
        :buffer "*helm dictionary*"))

(provide 'helm-dictionary)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-dictionary.el ends here
