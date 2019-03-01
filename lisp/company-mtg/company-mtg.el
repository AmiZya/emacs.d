;;; company-mtg.el --- Company backend for MTG cards  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: June 10, 2017
;; Homepage: https://github.com/angrybacon/company-mtg
;; Keywords: abbrev, convenience, games
;; Package-Requires: ((company "0.9"))
;; Version: 0.1.0

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; This package provides a backend for Company.
;; Get a JSON dump of all the cards at http://www.mtgjson.com/.

;;; Code:


(require 'cl-seq)
(require 'company)
(require 'json)
(require 'mtg)


;;;; Variables


(defgroup company-mtg nil
  "Company backend for `mtg'."
  :group 'company
  :prefix "company-mtg-")

(defcustom company-mtg-annotate-function 'company-mtg-annotate-mana
  "The display function to append company annotation at the end of candidates.
See `company-mtg-annotate-mana' for more details on the implementation of a
custom function."
  :group 'company-mtg
  :type 'function)

(defcustom company-mtg-data-file (concat mtg-directory "AllCards.json")
  "The file to read data from. Should be a JSON file."
  :group 'company-mtg
  :type 'file)

(defcustom company-mtg-match-function 'string-prefix-p
  "The matching function to use when finding candidates.
You can set this variable to `company-mtg-match-fuzzy' or define your own
function."
  :group 'company-mtg
  :type 'function)


;;;; Functions

(defun company-mtg-annotate-mana (candidate)
  (let ((mana (get-text-property 0 :mana candidate)))
    (when mana (concat " " (company-mtg-format mana)))))

(defun company-mtg-format (string)
  (dolist (icon `(("{W}" ,(mana "w"))
                  ("{U}" ,(mana "u"))
                  ("{B}" ,(mana "b"))
                  ("{R}" ,(mana "r"))
                  ("{G}" ,(mana "g"))))
    (let ((old (car icon))
          (new (car (cdr icon))))
      (setq string (replace-regexp-in-string old new string))))
  string)

(defun company-mtg-match-fuzzy (prefix string &optional ignore-case)
  (cl-subsetp (string-to-list prefix) (string-to-list string)))


;;;; Commands


(defvar company-mtg-cards nil "Store candidates.")

;;;###autoload
(defun company-mtg-load-cards ()
  "Read data from JSON, format it to be company-compatible and store it inside
`company-mtg-cards'.

See https://mtgjson.com/."
  (interactive)
  (setq company-mtg-cards nil)
  (dolist (card (json-read-file company-mtg-data-file))
    (let ((name (symbol-name (car card)))
          (data (cdr card)))
      (add-text-properties 0 1 `(:cmc
                                 ,(cdr (assoc 'cmc data))
                                 :colors ,(cdr (assoc 'colors data))
                                 :identity ,(cdr (assoc 'colorIdentity data))
                                 :layout ,(cdr (assoc 'layout data))
                                 :mana ,(cdr (assoc 'manaCost data))
                                 :name ,(cdr (assoc 'name data))
                                 :power ,(cdr (assoc 'power data))
                                 :subtypes ,(cdr (assoc 'subtypes data))
                                 :text ,(cdr (assoc 'text data))
                                 :toughness ,(cdr (assoc 'toughness data))
                                 :type ,(cdr (assoc 'type data))
                                 :types ,(cdr (assoc 'types data)))
                           name)
      (push name company-mtg-cards)))
  (setq company-mtg-cards (nreverse company-mtg-cards))
  (message "Company-mtg: loaded %s" company-mtg-data-file))

;;;###autoload
(defun company-mtg-load ()
  (interactive)
  (company-mtg-load-cards))

;;;###autoload
(defun company-mtg (command &optional argument &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-mtg))
    (prefix (and (eq major-mode 'mtg-deck-mode)
                 (company-grab-line "^\\([1-9] \\)?\\(.+\\)" 2)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (funcall company-mtg-match-function argument c t))
      company-mtg-cards))
    (annotation (funcall company-mtg-annotate-function argument))))

;; (add-to-list 'company-backends 'company-mtg)


(provide 'company-mtg)
;;; company-mtg.el ends here
