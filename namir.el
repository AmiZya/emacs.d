(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/namir.org"))

(global-set-key (kbd "C-c I") 'find-config)

(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0))                               ; Disable the tooltips

(setq auto-save-default t)
(setq make-backup-files nil)
(setq auto-save-visited-file-name t)
(setq auto-save-interval 0)
(setq auto-save-timeout (* 60 5))

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

(add-hook 'focus-out-hook #'garbage-collect)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar me/erc-nick               nil        "amizya")
(defvar me/erc-password           nil        "")
(defvar me/erc-port               nil        "6697")
(defvar me/erc-server             nil        "chat.freenode.net")
(defvar me/font-family            "Source Code Pro")
(defvar me/font-size-default      130        "150")
(defvar me/font-size-header-line  120        "120")
(defvar me/font-size-mode-line    110        "120")
(defvar me/font-size-small        100        "100")
(defvar me/font-size-title        140        "140")

(let ((secret.el (expand-file-name ".secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))

(set-face-attribute 'italic nil :underline nil)

(defun me/unboldify (&optional faces)
  "Set the weight property of FACES to `normal'.
If FACES is not provided or nil, use `face-list' instead."
  (interactive)
  (mapc (lambda (face)
          (when (eq (face-attribute face :weight) 'bold)
            (set-face-attribute face nil :weight 'normal)))
        (or faces (face-list))))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-blue t)

   (set-face-attribute 'default nil :font me/font-family :height me/font-size-default)
   (set-face-attribute 'fixed-pitch nil :font me/font-family)

   (set-face-attribute 'mode-line nil
                       :height me/font-size-mode-line)
   (set-face-attribute 'mode-line-inactive nil
                                  :height me/font-size-mode-line)

(use-package mdi
  :demand t
  :load-path "lisp/mdi/")

(use-package sgml-mode
  :ensure nil
  :delight html-mode "HTML"
  :preface
  (defun me/html-set-pretty-print-function ()
    (setq me/pretty-print-function #'sgml-pretty-print))
  :hook
  ((html-mode . me/html-set-pretty-print-function)
   (html-mode . sgml-electric-tag-pair-mode)
   (html-mode . sgml-name-8bit-mode)
   (html-mode . toggle-truncate-lines))
  :config (setq-default sgml-basic-offset 2))

(use-package emacs-lisp-mode
  :ensure nil
  :delight emacs-lisp-mode "Emacs Lisp"
  :config (delight 'lisp-interaction-mode "Lisp Interaction"))

(use-package ielm
  :ensure nil
  :hook (ielm-mode . (lambda () (setq-local scroll-margin 0))))

(use-package lisp-mode
  :ensure nil
  :delight lisp-mode "Lisp")

(use-package markdown-mode
  :delight markdown-mode "Markdown"
  :preface
  (defun me/markdown-set-ongoing-hydra-body ()
    (setq me/ongoing-hydra-body #'hydra-markdown/body))
  :mode
  ("INSTALL\\'"
   "CONTRIBUTORS\\'"
   "LICENSE\\'"
   "README\\'"
   "\\.markdown\\'"
   "\\.md\\'")
  :hook (markdown-mode . me/markdown-set-ongoing-hydra-body)
  :config
  (unbind-key "M-<down>" markdown-mode-map)
  (unbind-key "M-<up>" markdown-mode-map)
  (setq-default
   markdown-asymmetric-header t
   markdown-split-window-direction 'right)
  (me/unboldify '(markdown-header-face))
  (set-face-attribute 'markdown-table-face nil :height me/font-size-small))

(use-package org
  :ensure nil
  :delight org-mode "Org"
  :preface
  (defun me/org-src-buffer-name (org-buffer-name language)
    "Construct the buffer name for a source editing buffer. See
`org-src--construct-edit-buffer-name'."
    (format "*%s*" org-buffer-name))
  (defun me/org-set-ongoing-hydra-body ()
    (setq me/ongoing-hydra-body #'hydra-org/body))
  :bind
  (:map org-mode-map
        ([remap backward-paragraph] . me/backward-paragraph-dwim)
        ([remap forward-paragraph] . me/forward-paragraph-dwim)
        ("<C-return>" . nil)
        ("<C-S-down>" . nil)
        ("<C-S-up>" . nil)
        ("<M-S-down>" . nil)
        ("<M-S-up>" . nil))
  :hook
  ((org-mode . me/org-set-ongoing-hydra-body)
   (org-mode . org-sticky-header-mode)
   (org-mode . toc-org-enable))
  :config
  (setq-default
   org-descriptive-links nil
   org-support-shift-select 'always
   org-startup-folded nil
   org-startup-truncated nil)
  (advice-add 'org-src--construct-edit-buffer-name :override #'me/org-src-buffer-name))

(use-package org-faces
  :ensure nil
  :after org
  :config
  (set-face-attribute 'org-document-title nil :height 'unspecified)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'org-block nil :background zenburn-bg+05)
  ;;   (set-face-attribute 'org-list-dt nil :foreground zenburn-yellow))
  (me/unboldify '(org-document-title org-list-dt org-tag org-todo)))

(use-package org-src
  :ensure nil
  :after org
  :config
  (setq-default
   org-edit-src-content-indentation 0
   org-edit-src-persistent-message nil
   org-src-window-setup 'current-window))

(use-package org-sticky-header
  :config
  (setq-default
   org-sticky-header-full-path 'full
   org-sticky-header-outline-path-separator " / "))

(use-package toc-org :after org)

(require 'org-tempo)

(use-package python
  :ensure nil
  :delight python-mode "Python"
  :hook (python-mode . turn-on-prettify-symbols-mode)
  :config
  (when (executable-find "ipython")
    (setq-default
     python-shell-interpreter "ipython"
     python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt"
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(use-package pip-requirements
  :delight pip-requirements-mode "PyPA Requirements"
  :preface
  (defun me/pip-requirements-ignore-case ()
    (setq-local completion-ignore-case t))
  :hook (pip-requirements-mode . me/pip-requirements-ignore-case))

(use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    )

(use-package elpy
    :ensure t
    :config 
    (elpy-enable))

(use-package tex
    :ensure auctex)

    (defun tex-view ()
        (interactive)
        (tex-send-command "zathura" (tex-append tex-print-file ".pdf")))

(use-package yaml-mode
  :delight yaml-mode "YAML"
  :mode "\\.yml\\'")

(use-package ace-window
:ensure t
:init
(progn
(setq aw-scope 'frame)
(global-set-key (kbd "C-x O") 'other-frame)
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
  ))

(use-package alert
  :config
  (when (eq system-type 'darwin)
    (setq-default alert-default-style 'osx-notifier)))

(use-package company
  :defer 1
  :bind
  (:map company-active-map
        ("RET" . nil)
        ([return] . nil)
        ("TAB" . company-complete-selection)
        ([tab] . company-complete-selection)
        ("<right>" . company-complete-common))
  :config
  (global-company-mode 1)
  (setq-default
   company-idle-delay .2
   company-minimum-prefix-length 1
   company-require-match nil
   company-tooltip-align-annotations t))

(use-package company-dabbrev
  :ensure nil
  :after company
  :config (setq-default company-dabbrev-downcase nil))

(use-package easy-hugo
  :ensure t
  :commands easy-hugo
  :init
  (setq easy-hugo-basedir "~/code/myblog/")
  (setq easy-hugo-postdir "content/"))

(use-package ox-hugo
  :ensure t
  :after ox
  )

(global-set-key [remap kill-buffer] #'kill-this-buffer)

(defun me/switch-to-buffer-continue ()
  "Activate a sparse keymap:
  <left>   `previous-buffer'
  <right>  `next-buffer'"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<left>") #'previous-buffer)
     (define-key map (kbd "<right>") #'next-buffer)
     map)))
(advice-add 'previous-buffer :after #'me/switch-to-buffer-continue)
(advice-add 'next-buffer :after #'me/switch-to-buffer-continue)

(use-package desktop
  :ensure nil
  :demand t
  :config (desktop-save-mode 1))

(use-package eyebrowse
  :defer 1
  :bind
  ("<f5>" . eyebrowse-switch-to-window-config-1)
  ("<f6>" . eyebrowse-switch-to-window-config-2)
  ("<f7>" . eyebrowse-switch-to-window-config-3)
  ("<f8>" . eyebrowse-switch-to-window-config-4)
  :config
  (eyebrowse-mode 1)
  (setq-default eyebrowse-new-workspace t))

(use-package shackle
  :defer 1
  :config
  (setq-default
   shackle-rules '((help-mode :inhibit-window-quit t :same t))
   shackle-select-reused-windows t)
  (shackle-mode 1))

(use-package windmove
  :ensure nil
  :bind
  (("C-M-<left>". windmove-left)
   ("C-M-<right>". windmove-right)
   ("C-M-<up>". windmove-up)
   ("C-M-<down>". windmove-down)))

(use-package winner
  :ensure nil
  :defer 1
  :config (winner-mode 1))

(use-package newcomment
  :ensure nil
  :bind ("<M-return>" . comment-indent-new-line)
  :config
  (setq-default
   comment-auto-fill-only-comments t
   comment-multi-line t))

(use-package cus-edit
  :ensure nil
  :config
  (set-face-attribute 'custom-group-tag nil
                      :font me/font-family
                      :height me/font-size-title)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'custom-state nil :foreground zenburn-green+4))
  (me/unboldify '(custom-variable-tag)))

(use-package edit-server
  :defer 1
  :config (edit-server-start))

;; (use-package server
;;   :ensure nil
;;   :defer 1
;;   :config (server-start))

(require 'server)
(unless (server-running-p)
    (server-start))

(use-package define-word)

(use-package google-translate
  :config (me/unboldify '(google-translate-translation-face)))

(use-package diff-mode
  :ensure nil
  :config
  (set-face-attribute 'diff-added nil :background nil)
  (set-face-attribute 'diff-removed nil :background nil))

(use-package ediff-init
  :ensure nil
  :config
  (me/unboldify '(ediff-fine-diff-A
                  ediff-fine-diff-B
                  ediff-fine-diff-C)))

(use-package ediff-wind
  :ensure nil
  :config
  (setq-default
   ediff-split-window-function #'split-window-horizontally
   ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package smerge-mode
  :ensure nil
  )

(use-package dired
  :ensure nil
  :delight dired-mode "Dired"
  :preface
  (defun me/dired-directories-first ()
    "Sort dired listings with directories first before adding marks."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (advice-add 'dired-readin :after #'me/dired-directories-first)
  (setq-default
   dired-auto-revert-buffer t
   dired-dwim-target t
   dired-hide-details-hide-symlink-targets nil
   dired-listing-switches "-alh"
   dired-ls-F-marks-symlinks nil
   dired-recursive-copies 'always))

(use-package dired-x
  :ensure nil
  :preface
  (defun me/dired-revert-after-command (command &optional output error)
    (revert-buffer))
  :config
  (advice-add 'dired-smart-shell-command :after #'me/dired-revert-after-command))

(use-package emmet-mode
  :bind
  (:map emmet-mode-keymap
        ("<C-return>" . nil)
        ("C-M-<left>" . nil)
        ("C-M-<right>" . nil)
        ("C-c w" . nil))
  :hook (css-mode html-mode rjsx-mode)
  :config
  (setq-default
   emmet-insert-flash-time .1
   emmet-move-cursor-between-quote t))

(use-package hippie-exp
  :ensure nil
  :preface
  (defun me/emmet-try-expand-line (args)
    "Try `emmet-expand-line' if `emmet-mode' is active. Else, does nothing."
    (interactive "P")
    (when emmet-mode (emmet-expand-line args)))
  :bind ("<C-return>" . hippie-expand)
  :config
  (setq-default
   hippie-expand-try-functions-list '(yas-hippie-try-expand me/emmet-try-expand-line)
   hippie-expand-verbose nil))

(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil)
        ("<tab>" . nil))
  :hook
  ((emacs-lisp-mode . yas-minor-mode)
   (html-mode . yas-minor-mode)
   (js-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)
   (python-mode . yas-minor-mode))
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-reload-all))

(use-package vimish-fold
  :defer 1
  :bind
  (:map vimish-fold-folded-keymap ("<tab>" . vimish-fold-unfold)
   :map vimish-fold-unfolded-keymap ("<tab>" . vimish-fold-refold))
  :init
  (setq-default vimish-fold-dir (expand-file-name ".vimish-fold/" user-emacs-directory))
  (vimish-fold-global-mode 1)
  :config
  (setq-default vimish-fold-header-width 79))

(use-package erc
  :ensure nil
  :preface
  (defun me/erc ()
    "Connect to `me/erc-server' on `me/erc-port' as `me/erc-nick' with
    `me/erc-password'."
    (interactive)
    (erc :server me/erc-server
         :port me/erc-port
         :nick me/erc-nick
         :password me/erc-password))
  (defun me/erc-bol-shifted ()
    "See `erc-bol'. Support shift."
    (interactive "^")
    (erc-bol))
  :bind
  (:map erc-mode-map
        ([remap erc-bol] . me/erc-bol-shifted)
        ("M-<down>" . erc-next-command)
        ("M-<up>" . erc-previous-command))
  :hook (erc-mode . (lambda () (setq-local scroll-margin 0)))
  :config
  (make-variable-buffer-local 'erc-fill-column)
  (erc-scrolltobottom-enable)
  (setq-default
   erc-autojoin-channels-alist '(("freenode.net" "#emacs"))
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 19
   erc-header-line-format " %n on %t %m"
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   erc-lurker-hide-list '("JOIN" "PART" "QUIT")
   erc-prompt (format "%18s" ">")
   erc-timestamp-format nil)
  (set-face-attribute 'erc-prompt-face nil :background nil)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'erc-timestamp-face nil :foreground zenburn-fg-1))
  (me/unboldify '(erc-bold-face
                  erc-button
                  erc-nick-default-face
                  erc-my-nick-face
                  erc-my-nick-prefix-face
                  erc-nick-prefix-face
                  erc-prompt-face)))

(use-package erc-button
  :ensure nil
  :config (set-face-attribute 'erc-button nil :inherit 'button))

(use-package erc-match
  :ensure nil
  :config (me/unboldify '(erc-current-nick-face)))

(use-package erc-hl-nicks :after erc)

(use-package erc-track
  :ensure nil
  :after erc
  :preface
  (defun me/erc-set-fill-column ()
    "Set `erc-fill-column' to the width of the current window."
    (save-excursion
      (walk-windows
       (lambda (window)
         (let ((buffer (window-buffer window)))
           (set-buffer buffer)
           (when (and (eq major-mode 'erc-mode) (erc-buffer-visible buffer))
             (setq erc-fill-column (- (window-width window) 2))))))))
  :hook (window-configuration-change . me/erc-set-fill-column))

(use-package helm
  :defer 1
  :preface
  (defun me/helm-focus-follow ()
    ;; (let ((point (point)))
    ;;   (when (and (pulse-available-p) (> point 1))
    ;;     (pulse-momentary-highlight-one-line point)))
    (recenter-top-bottom (car recenter-positions)))
  (defun me/helm-grab-candidates (beg end)
    (interactive "r")
    (if (region-active-p)
        (kill-ring-save beg end)
      (with-helm-buffer (kill-ring-save (point-min) (point-max)))))
  :bind (:map helm-map ("M-w" . me/helm-grab-candidates))
  :hook (helm-after-action . me/helm-focus-follow)
  :config
  (helm-mode 1)
  (setq-default
   helm-always-two-windows t
   helm-display-header-line nil
   helm-split-window-default-side 'left)
  (set-face-attribute 'helm-action nil :underline nil)
  (set-face-attribute 'helm-match nil :background nil)
  (set-face-attribute 'helm-source-header nil
                      :box nil
                      :background nil
                      :height me/font-size-title)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'helm-prefarg nil :foreground zenburn-magenta))
  (me/unboldify '(helm-match helm-source-header)))

(use-package helm-bookmarks
  :ensure nil
  :after helm
  :config
  ;; NOTE: See https://github.com/bbatsov/zenburn-emacs/pull/279.
  (set-face-attribute 'helm-bookmark-directory nil
                      :foreground 'unspecified
                      :inherit 'dired-directory))

(use-package helm-buffers
  :ensure nil
  :after helm
  :config
  (setq-default
   helm-buffers-fuzzy-matching t
   helm-buffer-max-length nil)
  (set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory)
  (set-face-attribute 'helm-non-file-buffer nil :inherit 'shadow)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'helm-buffer-size nil :foreground zenburn-fg-1))

  ;; NOTE: See https://github.com/bbatsov/zenburn-emacs/pull/279.
  (set-face-attribute 'helm-buffer-directory nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :inherit 'dired-directory))

(use-package helm-color
  :ensure nil
  :after helm
  :bind
  (:map helm-color-map
        ("<left>" . backward-char)
        ("<right>" . forward-char)))

(use-package helm-command
  :ensure nil
  :after helm
  :bind ([remap execute-extended-command] . helm-M-x)
  :config
  (setq-default helm-M-x-fuzzy-match t)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'helm-M-x-key nil :foreground zenburn-orange :underline nil))
)

(use-package helm-files
  :ensure nil
  :after helm
  :bind ([remap find-file] . helm-find-files)
  :config
  (setq-default
   helm-ff-no-preselect t
   helm-ff-skip-boring-files t
   helm-find-file-ignore-thing-at-point t)
  (me/unboldify '(helm-ff-directory helm-ff-symlink))

  ;; NOTE: See https://github.com/bbatsov/zenburn-emacs/pull/279.
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'helm-ff-dotted-directory nil
  ;;                       :background nil
  ;;                       :foreground zenburn-fg-1)
  ;;   (set-face-attribute 'helm-ff-directory nil
  ;;                       :foreground 'unspecified
  ;;                       :inherit 'dired-directory)
  ;;   (set-face-attribute 'helm-ff-dirs nil
  ;;                       :foreground 'unspecified
  ;;                       :inherit 'dired-directory))
)

(use-package helm-grep
  :ensure nil
  :after helm
  :config
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'helm-grep-lineno nil :foreground zenburn-yellow-2))

)

(use-package helm-imenu
  :ensure nil
  :after helm
  :bind
  (:map helm-imenu-map
        ("<left>" . backward-char)
        ("<right>" . forward-char)))

(use-package helm-misc
  :ensure nil
  :after helm
  :bind ([remap switch-to-buffer] . helm-buffers-list))

(use-package helm-lib
  :ensure nil
  :after helm
  :config
  (setq-default
   helm-help-full-frame nil
   helm-scroll-amount 5))

(use-package helm-mode
  :ensure nil
  :after helm
  :config
  (setq-default
   helm-completion-in-region-fuzzy-match t
   helm-mode-fuzzy-match t))

(use-package helm-net
  :ensure nil
  :after helm
  :config (setq-default helm-net-prefer-curl (if (executable-find "curl") t nil)))

(use-package helm-org
  :ensure nil
  :after helm
  :config (setq-default helm-org-headings-fontify t))

(use-package helm-regexp
  :ensure nil
  :after helm
  :bind
  (([remap isearch-forward] . helm-occur)
   :map helm-moccur-map
   ("<left>" . backward-char)
   ("<right>" . forward-char))
  ;; :config
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'helm-moccur-buffer nil :foreground zenburn-blue))
)

(use-package helm-ag
  :defer nil
  :after helm
  :bind
  (:map helm-ag-map
        ("<left>" . backward-char)
        ("<right>" . forward-char))
  :config (setq-default helm-ag-show-status-function nil))

(use-package helm-descbinds
  :defer nil
  :after helm
  :config
  (helm-descbinds-mode 1)
  (setq-default helm-descbinds-window-style 'split-window))

(use-package helm-describe-modes
  :after helm
  :bind ([remap describe-mode] . helm-describe-modes))

(use-package helm-flycheck :after helm)

(use-package helm-projectile
  :defer nil
  :after helm
  :bind
  (nil
   :map helm-projectile-find-file-map
   ("<left>" . backward-char)
   ("<right>" . forward-char)
   :map helm-projectile-projects-map
   ("<left>" . backward-char)
   ("<right>" . forward-char))
  :config (helm-projectile-toggle 1))

(use-package help-mode
  :ensure nil
  :bind
  (:map help-mode-map
        ("<" . help-go-back)
        (">" . help-go-forward)))

(use-package delight
  :ensure nil
  :preface
  (defun me/delight-powerline-major-mode (original-function &rest arguments)
    (let ((inhibit-mode-name-delight nil)) (apply original-function arguments)))
  (defun me/delight-powerline-minor-mode (original-function &rest arguments)
    (let ((inhibit-mode-name-delight nil)) (apply original-function arguments)))
  :config
  (advice-add 'powerline-major-mode :around #'me/delight-powerline-major-mode)
  (advice-add 'powerline-minor-mode :around #'me/delight-powerline-minor-mode))

(use-package spaceline
  :demand t
  :config

  (spaceline-define-segment me/erc-track
    "Show the ERC buffers with new messages."
    (when (bound-and-true-p erc-track-mode)
      (mapcar (lambda (buffer)
                (format "%s%s%s"
                        (buffer-name (pop buffer))
                        erc-track-showcount-string
                        (pop buffer)))
              erc-modified-channels-alist)))

  (spaceline-define-segment me/helm-follow
    "Show `helm-follow-mode' status."
    (if (and (bound-and-true-p helm-alive-p)
             spaceline--helm-current-source
             (eq 1 (cdr (assq 'follow spaceline--helm-current-source))))
        (propertize (mdi "eye") 'face 'success)
      (propertize (mdi "eye-off") 'face 'warning)))

  (spaceline-define-segment me/selection-info
    "Show the size of current region."
    (when mark-active
      (let ((characters (- (region-end) (region-beginning)))
            (rows (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
            (columns (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                                 (spaceline--column-number-at-pos (region-beginning)))))))
        (cond
         ((bound-and-true-p rectangle-mark-mode)
          (format "%d %s %d" (1- columns) (mdi "arrow-expand-all") rows))
         ((> rows 1)
          (format "%d" (if (eq (current-column) 0) (1- rows) rows)))
         (t (format "%d" characters))))))

  (spaceline-define-segment me/version-control
    "Show the current version control branch."
    (when vc-mode
      (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))))

(use-package spaceline-config
  :demand t
  :ensure nil
  :config

  ;; Configure the mode-line
  (setq-default
   mode-line-format '("%e" (:eval (spaceline-ml-main)))
   powerline-default-separator 'wave
   powerline-height 20
   spaceline-highlight-face-func 'spaceline-highlight-face-modified
   spaceline-flycheck-bullet (format "%s %s" (mdi "record") "%s")
   spaceline-separator-dir-left '(left . left)
   spaceline-separator-dir-right '(right . right))
  (spaceline-helm-mode 1)

  ;; Build the mode-lines
  (spaceline-compile
    `((major-mode :face highlight-face)
      ((remote-host buffer-id line) :separator ":")
      ((flycheck-error flycheck-warning flycheck-info))
      (me/selection-info))
    `((me/erc-track :face 'spaceline-highlight-face :when active)
      (anzu)
      ((projectile-root me/version-control) :separator ":")
      (workspace-number)
      (global :face highlight-face)))
  (spaceline-compile
    'helm
    '((helm-buffer-id :face 'spaceline-read-only)
      (helm-number)
      (me/helm-follow)
      (helm-prefix-argument))
    '((me/erc-track :face 'spaceline-highlight-face :when active)
      (workspace-number)
      (global :face 'spaceline-read-only)))

  ;; Customize the mode-line
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'powerline-active2 nil :background zenburn-bg+05)
  ;;   (set-face-attribute 'powerline-inactive2 nil :background zenburn-bg)
  ;;   (set-face-attribute 'spaceline-flycheck-error nil :foreground zenburn-red)
  ;;   (set-face-attribute 'spaceline-flycheck-info nil :foreground zenburn-blue+1)
  ;;   (set-face-attribute 'spaceline-flycheck-warning nil :foreground zenburn-orange)
  ;;   (set-face-attribute 'spaceline-highlight-face nil
  ;;                       :background zenburn-yellow
  ;;                       :foreground zenburn-fg-1)
  ;;   (set-face-attribute 'spaceline-modified nil
  ;;                       :background zenburn-red
  ;;                       :foreground zenburn-red-4)
  ;;   (set-face-attribute 'spaceline-read-only nil
  ;;                       :background zenburn-blue+1
  ;;                       :foreground zenburn-blue-5)
  ;;   (set-face-attribute 'spaceline-unmodified nil
  ;;                       :background zenburn-green-1
  ;;                       :foreground zenburn-green+4))
)

(use-package flycheck
  :hook
  ((css-mode . flycheck-mode)
   (emacs-lisp-mode . flycheck-mode)
   (js-mode . flycheck-mode)
   (python-mode . flycheck-mode))
  :config
  (setq-default
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-disabled-checkers '(emacs-lisp-checkdoc)
   flycheck-display-errors-delay .3)
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'flycheck-error nil :underline zenburn-red)
  ;;   (set-face-attribute 'flycheck-info nil :underline zenburn-blue+1)
  ;;   (set-face-attribute 'flycheck-warning nil :underline zenburn-orange))
)

(when (eq system-type 'darwin)
  (setq-default
   exec-path (append exec-path '("/usr/local/bin"))  ; Add Homebrew path
   ns-command-modifier 'meta                         ; Map Meta to the Cmd key
   ns-option-modifier 'super                         ; Map Super to the Alt key
   ns-right-option-modifier nil))                    ; Disable the right Alt key

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 1
  :config (exec-path-from-shell-initialize))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  ;; :config
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'rainbow-delimiters-mismatched-face nil
  ;;                       :foreground zenburn-red-4)
  ;;   (set-face-attribute 'rainbow-delimiters-unmatched-face nil
  ;;                       :foreground zenburn-red-4))
)

(use-package smartparens
  :defer 1
  :bind
  (("M-<backspace>" . sp-unwrap-sexp)
   ("M-<left>" . sp-forward-barf-sexp)
   ("M-<right>" . sp-forward-slurp-sexp)
   ("M-S-<left>" . sp-backward-slurp-sexp)
   ("M-S-<right>" . sp-backward-barf-sexp))
  :config
  (show-paren-mode 0)
  (require 'smartparens-config)
  (setq-default
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil)
  (smartparens-global-mode 1))

(use-package webpaste)

(use-package expand-region
  :bind
  ("C-+" . er/contract-region)
  ("C-=" . er/expand-region))

(use-package highlight)

(use-package multiple-cursors
  :defer 1
  :bind
  (("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-S-c C-S-a" . mc/vertical-align-with-space)
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-S-l" . mc/insert-letters)
   ("C-S-c C-S-n" . mc/insert-numbers)
   ("C-'" . mc-hide-unmatched-lines-mode))
  :init
  (setq-default
   mc/list-file (expand-file-name ".multiple-cursors.el" user-emacs-directory))
  :config
  (setq-default
   mc/edit-lines-empty-lines 'ignore
   mc/insert-numbers-default 1))

(use-package selected
  :defer 1
  :preface
  (defvar-local me/pretty-print-function nil)
  (defun me/pretty-print (beg end)
    (interactive "r")
    (if me/pretty-print-function
        (progn (funcall me/pretty-print-function beg end)
               (setq deactivate-mark t))
      (user-error "me/pretty-print: me/pretty-print-function is not set")))
  :bind
  (:map selected-keymap
        ("<"           . mc/mark-previous-like-this)
        (">"           . mc/mark-next-like-this)
        ("C-<tab>"     . me/pretty-print)
        ("C-<"         . mc/unmark-previous-like-this)
        ("C->"         . mc/unmark-next-like-this)
        ("C-M-<"       . mc/skip-to-previous-like-this)
        ("C-M->"       . mc/skip-to-next-like-this)
        ("C-?"         . hydra-selected/body)
        ("C-c C-c"     . me/eval-region-and-kill-mark)
        ("C-b"         . me/browse-url-and-kill-mark)
        ("C-c c"       . capitalize-region)
        ("C-c k"       . me/kebab-region)
        ("C-c l"       . downcase-region)
        ("C-c u"       . upcase-region)
        ("C-d"         . define-word-at-point)
        ("C-f"         . fill-region)
        ("C-g"         . selected-off)
        ("C-h h"       . hlt-highlight-region)
        ("C-h H"       . hlt-unhighlight-region)
        ("C-p"         . webpaste-paste-region)
        ("C-s r"       . reverse-region)
        ("C-s s"       . sort-lines)
        ("C-s w"       . me/sort-words)
        ("C-t"         . google-translate-at-point)
        ("<M-left>"    . me/indent-rigidly-left-and-keep-mark)
        ("<M-right>"   . me/indent-rigidly-right-and-keep-mark)
        ("<M-S-left>"  . me/indent-rigidly-left-tab-and-keep-mark)
        ("<M-S-right>" . me/indent-rigidly-right-tab-and-keep-mark))
  :config
  (require 'browse-url)
  (selected-global-mode 1))

(defun me/eval-region-and-kill-mark (beg end)
  "Execute the region as Lisp code.
Call `eval-region' and kill mark. Move back to the beginning of the region."
  (interactive "r")
  (eval-region beg end)
  (setq deactivate-mark t)
  (goto-char beg))

(defun me/browse-url-and-kill-mark (url &rest args)
  "Ask a WWW browser to load URL.
Call `browse-url' and kill mark."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply #'browse-url url args)
  (setq deactivate-mark t))

(defun me/indent-rigidly-left-and-keep-mark (beg end)
  "Indent all lines between BEG and END leftward by one space.
Call `indent-rigidly-left' and keep mark."
  (interactive "r")
  (indent-rigidly-left beg end)
  (setq deactivate-mark nil))

(defun me/indent-rigidly-left-tab-and-keep-mark (beg end)
  "Indent all lines between BEG and END leftward to a tab stop.
Call `indent-rigidly-left-to-tab-stop' and keep mark."
  (interactive "r")
  (indent-rigidly-left-to-tab-stop beg end)
  (setq deactivate-mark nil))

(defun me/indent-rigidly-right-and-keep-mark (beg end)
  "Indent all lines between BEG and END rightward by one space.
Call `indent-rigidly-right' and keep mark."
  (interactive "r")
  (indent-rigidly-right beg end)
  (setq deactivate-mark nil))

(defun me/indent-rigidly-right-tab-and-keep-mark (beg end)
  "Indent all lines between BEG and END rightward to a tab stop.
Call `indent-rigidly-right-to-tab-stop' and keep mark."
  (interactive "r")
  (indent-rigidly-right-to-tab-stop beg end)
  (setq deactivate-mark nil))

(defun me/kebab-region (begin end)
  "Convert region to kebab-case."
  (interactive "r")
  (downcase-region begin end)
  (save-excursion
    (perform-replace " +" "-" nil t nil nil nil begin end)))

(defun me/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(global-set-key (kbd "<M-S-up>") 'me/duplicate-backward)
(global-set-key (kbd "<M-S-down>") 'me/duplicate-forward)
(global-set-key (kbd "<M-down>") 'me/swap-line-down)
(global-set-key (kbd "<M-up>") 'me/swap-line-up)

(defun me/duplicate-line (&optional stay)
  "Duplicate current line.
With optional argument STAY true, leave point where it was."
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (point-at-bol) (point-at-eol))))
    (newline))
  (unless stay
    (let ((column (current-column)))
      (forward-line)
      (forward-char column))))

(defun me/duplicate-backward ()
  "Duplicate current line upward or region backward.
If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark)
        (save-excursion
          (insert (buffer-substring (region-beginning) (region-end)))))
    (me/duplicate-line t)))

(defun me/duplicate-forward ()
  "Duplicate current line downward or region forward.
If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark (point (point)))
        (insert (buffer-substring (region-beginning) (region-end)))
        (push-mark point))
    (me/duplicate-line)))

(defun me/swap-line-down ()
  "Move down the line under point."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun me/swap-line-up ()
  "Move up the line under point."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(use-package projectile
  :defer 1
  :init
  (setq-default
   projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory)
   projectile-keymap-prefix (kbd "C-c C-p")
   projectile-known-projects-file (expand-file-name
                                   ".projectile-bookmarks" user-emacs-directory))
  :config
  (projectile-global-mode 1)
  (setq-default
   projectile-completion-system 'helm
   projectile-enable-caching t
   projectile-mode-line '(:eval (projectile-project-name))))

(use-package em-hist
  :ensure nil
  :config (setq-default eshell-hist-ignoredups t))

(use-package esh-mode
  :ensure nil
  :delight eshell-mode "EShell"
  :preface
  (defun me/eshell-bol-shifted ()
    "See `eshell-bol'. Support shift."
    (interactive "^")
    (eshell-bol))
  :bind (:map eshell-mode-map ([remap eshell-bol] . me/eshell-bol-shifted))
  :hook
  ((eshell-mode . me/hl-line-mode-off)
   (eshell-mode . (lambda () (setq-local scroll-margin 0))))
  :config (setq-default eshell-scroll-to-bottom-on-input t))

(use-package em-ls
  :ensure nil
  :config
  (me/unboldify '(eshell-ls-directory
                  eshell-ls-executable
                  eshell-ls-special
                  eshell-ls-symlink)))

(use-package em-prompt
  :ensure nil
  :config (me/unboldify '(eshell-prompt)))

(use-package term
  :ensure nil
  :hook
  ((term-mode . me/hl-line-mode-off)
   (term-mode . (lambda () (setq-local scroll-margin 0)))))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package treemacs
    :ensure t
    :defer t
    :config
    (progn

      (setq treemacs-follow-after-init          t
            treemacs-width                      35
            treemacs-indentation                2
            treemacs-git-integration            t
            treemacs-collapse-dirs              3
            treemacs-silent-refresh             nil
            treemacs-change-root-without-asking nil
            treemacs-sorting                    'alphabetic-desc
            treemacs-show-hidden-files          t
            treemacs-never-persist              nil
            treemacs-is-never-other-window      nil
            treemacs-goto-tag-strategy          'refetch-index)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t))
    :bind
    (:map global-map
          ([f1]        . treemacs-toggle)
          ([f2]        . treemacs-projectile-toggle)
          ("<C-M-tab>" . treemacs-toggle)
          ("M-0"       . treemacs-select-window)
          ("C-c 1"     . treemacs-delete-other-windows)
        ))
  (use-package treemacs-projectile
    :defer t
    :ensure t
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-header)
)

(use-package aggressive-indent
  :preface
  (defun me/aggressive-indent-mode-off ()
    (aggressive-indent-mode 0))
  :hook
  ((css-mode . aggressive-indent-mode)
   (emacs-lisp-mode . aggressive-indent-mode)
   (js-mode . aggressive-indent-mode)
   (lisp-mode . aggressive-indent-mode)
   (sgml-mode . aggressive-indent-mode))
  :config
  (setq-default aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))

(defun me/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun me/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun me/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun me/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun me/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun me/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))

(use-package eldoc
  :ensure nil
  :config (global-eldoc-mode -1))

(use-package files
  :ensure nil
  :config
  (setq-default
   backup-by-copying t
   backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
   delete-old-versions t
   version-control t))

(use-package highlight-indent-guides
  :hook (python-mode . highlight-indent-guides-mode)
  :config (setq-default highlight-indent-guides-method 'character))

(use-package hl-line
  :ensure nil
  :demand t
  :preface
  (defun me/hl-line-mode-off ()
    (setq-local global-hl-line-mode nil))
  :config
  (global-hl-line-mode 1))
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'hl-line nil :background zenburn-bg+1))

(use-package midnight
  :ensure nil
  :config
  (setq-default clean-buffer-list-delay-general 1)
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "dotemacs.org"))

(use-package paradox
  :config
  (setq-default
   paradox-column-width-package 27
   paradox-column-width-version 13
   paradox-execute-asynchronously t
   paradox-github-token t
   paradox-hide-wiki-packages t)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(use-package prog-mode
  :ensure nil
  :preface
  (defun me/prettify-symbols-compose-predicate (&rest arguments)
    (when (not (eq system-type 'windows-nt))
      (apply #'prettify-symbols-default-compose-p arguments)))
  :config
  (setq-default
   prettify-symbols-compose-predicate #'me/prettify-symbols-compose-predicate
   prettify-symbols-unprettify-at-point 'right-edge))

(use-package rainbow-mode
  :hook prog-mode
  :config (setq-default rainbow-x-colors-major-mode-list '()))

(use-package simple
  :ensure nil
  :hook
  ((prog-mode . turn-on-auto-fill)
   (text-mode . turn-on-auto-fill)))

(use-package git-commit
  :preface
  (defun me/git-commit-auto-fill-everywhere ()
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :hook (git-commit-mode . me/git-commit-auto-fill-everywhere)
  :config
  (setq-default git-commit-summary-max-length 50)
  (me/unboldify '(git-commit-comment-action
                  git-commit-comment-branch-local
                  git-commit-comment-branch-remote
                  git-commit-comment-heading)))

(use-package magit
  :preface
  (defun me/magit-display-buffer-same (buffer)
    "Display most magit popups in the current buffer."
    (display-buffer
     buffer
     (cond ((and (derived-mode-p 'magit-mode)
                 (eq (with-current-buffer buffer major-mode) 'magit-status-mode))
            nil)
           ((memq (with-current-buffer buffer major-mode)
                  '(magit-process-mode
                    magit-revision-mode
                    magit-diff-mode
                    magit-stash-mode))
            nil)
           (t '(display-buffer-same-window)))))
  :config

  ;; Use better defaults
  (setq-default
   magit-display-buffer-function 'me/magit-display-buffer-same
   magit-diff-highlight-hunk-body nil
   magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face)
   magit-popup-display-buffer-action '((display-buffer-same-window))
   magit-refs-show-commit-count 'all
   magit-section-show-child-count t)

  ;; Customize lighters
  (delight
   '((magit-diff-mode "Magit Diff")
     (magit-log-mode "Magit Log")
     (magit-popup-mode "Magit Popup")
     (magit-status-mode "Magit Status")))

  ;; Customize faces
  (set-face-attribute 'magit-diff-file-heading-highlight nil :background nil)
  (set-face-attribute 'magit-diff-hunk-region nil :inherit 'region)
  (set-face-attribute 'magit-popup-heading nil :height me/font-size-title)
  (set-face-attribute 'magit-section-heading nil :height me/font-size-title)
  (set-face-attribute 'magit-section-highlight nil :background nil)
  (me/unboldify '(magit-branch-current
                  magit-branch-local
                  magit-branch-remote
                  magit-head
                  magit-refname
                  magit-refname-stash
                  magit-refname-wip
                  magit-tag)))

(use-package magit-blame
  :ensure nil
  :config (me/unboldify '(magit-blame-summary)))

(use-package magit-diff
  :ensure nil
  :config
  (me/unboldify '(magit-diff-file-heading
                  magit-diff-file-heading-highlight
                  magit-diff-file-heading-selection)))

(use-package magit-popup
  :ensure nil
  :config
  (me/unboldify '(magit-popup-argument
                  magit-popup-heading
                  magit-popup-key
                  magit-popup-option-value)))

(use-package magit-section
  :ensure nil
  :config
  (me/unboldify '(magit-section-heading
                  magit-section-heading-selection
                  magit-section-secondary-heading)))

(use-package gitattributes-mode :delight gitattributes-mode "Git Attributes")
(use-package gitconfig-mode :delight gitconfig-mode "Git Config")
(use-package gitignore-mode :delight gitignore-mode "Git Ignore")

(use-package which-key
   :defer 20
  :config ;; executed after loading package
  (which-key-setup-side-window-right)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))

  (which-key-mode)
)

(use-package whitespace
  :demand t
  :ensure nil
  :hook
  ((prog-mode . whitespace-turn-on)
   (text-mode . whitespace-turn-on))
  :config
  (setq-default whitespace-style '(face empty indentation::space tab trailing))
  ;; (zenburn-with-color-variables
  ;;   (set-face-attribute 'whitespace-indentation nil :background zenburn-bg+2))
)

(use-package auto-complete 
:ensure t
:init
(progn
(ac-config-default)
  (global-auto-complete-mode t)
 ))

(use-package company
:ensure t
:config
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)

(global-company-mode t)
)

(use-package company-irony
:ensure t
:config 
(add-to-list 'company-backends 'company-irony)

)

(use-package irony
:ensure t
:config
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
)

(use-package irony-eldoc
:ensure t
:config
(add-hook 'irony-mode-hook #'irony-eldoc))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(use-package company-jedi
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
       )

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; company box mode
;(use-package company-box
;:ensure t
;  :hook (company-mode . company-box-mode))
