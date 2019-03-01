;;; mtg.el --- Build decks for Magic: The Gathering  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: June 10, 2017
;; Homepage: https://github.com/angrybacon/mtg
;; Keywords: convenience, games
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

;; This package provides modes to facilitates building Magic: The Gathering card
;; decks.

;;; Code:


(require 'format-spec)


;;;; Variables


(defgroup mtg nil
  "Build decks for Magic: The Gathering."
  :group 'convenience
  :group 'games
  :prefix "mtg-")

(defcustom mtg-buffer-name-format "*%m%n*"
  "The format string used to name MTG buffers.
The following %-sequences are supported:
`%m' The name of the major-mode, minus the `-mode' suffix.
`%n' The name of the deck."
  :group 'mtg
  :type 'string)

(defcustom mtg-directory (locate-user-emacs-file "mtg/")
  "The root directory for all `mtg' related files."
  :group 'mtg
  :type 'string)


;;;; Modes


(defvar mtg-splash-mode-map nil "Keymap used in `mtg-splash-mode' buffers.")
(setq mtg-splash-mode-map (let ((map (make-sparse-keymap)))
                            map))

(defvar mtg-deck-mode-map nil "Keymap used in `mtg-deck-mode' buffers.")
(setq mtg-deck-mode-map (let ((map (make-sparse-keymap)))
                          map))

;;;###autoload
(define-derived-mode mtg-splash-mode special-mode "MTG"
  "Major mode for the splash screen."
  :group 'mtg
  (buffer-disable-undo)
  (setq buffer-read-only t))

;;;###autoload
(define-derived-mode mtg-deck-mode text-mode "MTG Deck"
  "Major mode for editing deck files."
  :group 'mtg)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dec\\'" . mtg-deck-mode) t)


;;;; Functions


(defun mtg-generate-buffer-name (mode &optional deck)
  "Generate a buffer name based on `mtg-buffer-name-format'."
  (format-spec mtg-buffer-name-format `((?m . ,(substring (symbol-name mode) 0 -5))
                                        (?n . ,(if deck (concat ": " deck) "")))))

;;;###autoload
(defun mtg-splash ()
  "Display the main screen."
  (interactive)
  (let* ((mode 'mtg-splash-mode)
         (buffer (get-buffer-create (mtg-generate-buffer-name mode))))
    (switch-to-buffer buffer)
    (funcall mode)))

;;;###autoload
(defun mtg-deck-new ()
  "Open a volatile buffer to create a new deck."
  (interactive)
  (let* ((mode 'mtg-deck-mode)
         (deck "Lantern")
         (buffer (get-buffer-create (mtg-generate-buffer-name mode deck))))
    (switch-to-buffer buffer)
    (funcall mode)))

(provide 'mtg)
;;; mtg.el ends here
