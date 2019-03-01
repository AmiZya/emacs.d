;;; locust.el --- Focus the current window           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: May 20, 2017
;; Homepage: https://github.com/angrybacon/locust
;; Keywords: convenience, frames

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

;; This package highlights the current window by enlarging it; optionally, it
;; also highlights the window's background.

;;; Code:

(require 'face-remap)

(defgroup locust nil
  "Focus the current window."
  :group 'windows)

(defvar locust-last-buffer nil
  "Keep track of the last buffer.")

(defun locust-focus ()
  "Focus the current window."
  (let ((current-buffer (current-buffer)))
    (unless (eq current-buffer locust-last-buffer)
      (let ((locust-mode nil))
        (balance-windows))
      (setq locust-last-buffer current-buffer))))

;;;###autoload
(define-minor-mode locust-mode
  "Toggle `locust-mode'.
This global minor mode highlights the current window by enlarging it.
Optionally, it also highlights the window's background."
  :global t
  (if locust-mode
      (add-hook 'post-command-hook #'locust-focus)
    (remove-hook 'post-command-hook #'locust-focus)))

(provide 'locust)
;;; locust.el ends here
