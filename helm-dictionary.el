;;; helm-dictionary.el --- Helm source for looking up dictionaries

;; Copyright 2013 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
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

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

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

(defcustom helm-dictionary-browser-function
  (lambda (url)
    (require 'url) (require 'eww)
    (url-retrieve
     url
     (lambda (&rest args)
       (save-selected-window
         (let ((display-buffer-alist
                '(("^\\*eww\\*$"
                   (display-buffer-pop-up-frame)
                   (reusable-frames . nil)))))
           (apply #'eww-render args))))
     (list url)))
  "Function for browsing online dictionaries.")


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
    (action . (("Insert German term"  . helm-dictionary-insert-l1term)
               ("Insert English term" . helm-dictionary-insert-l2term)))))

(defvar helm-dictionary-source-online
  `((name . "Lookup online")
    (dummy)
    (nohighlight)
    (filtered-candidate-transformer
     . (lambda (_cands _source) helm-dictionary-online-dicts))
    (action
     . (lambda (cand) (funcall helm-dictionary-browser-function
                               (format cand
                                       (url-hexify-string helm-pattern))))))
  "Source for online lookup.")

;;;###autoload
(defun helm-dictionary ()
  (interactive)
  (helm :sources '(helm-source-dictionary helm-dictionary-source-online)
        :full-frame t
        :candidate-number-limit 500
        :buffer "*helm dictionary*"))

;;;###autoload
(defun helm-dictionary-word-at-point ()
  (interactive)
  (helm :sources '(helm-source-dictionary helm-dictionary-source-online)
        :full-frame t
        :input (word-at-point)
        :candidate-number-limit 500
        :buffer "*helm dictionary*"))

(provide 'helm-dictionary)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-dictionary.el ends here
