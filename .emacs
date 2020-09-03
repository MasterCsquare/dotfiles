(setq gc-cons-threshold 100000000)

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq x-select-enable-clipboard t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq x-stretch-cursor t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(delete-selection-mode 1)

(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'subword-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(set-face-attribute 'default nil :font "Fira Code 11")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset
		    (font-spec :family "WenQuanYi Zen " :size 16)))

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("emacswiki" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(use-package helm
  :config
  (helm-mode 1)
  (setq helm-autoresize-min-height 35)
  (setq helm-autoresize-max-height 35)
  (helm-autoresize-mode)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x r b" . helm-filtered-bookmarks)))

(use-package helm-swoop
  :bind ("C-s" . helm-swoop))

(use-package doom-themes
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package helm-ag :defer t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package yasnippet
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package auto-yasnippet
  :bind (("C-c [" . aya-create)
	 ("C-c ]" . aya-expand)))

(use-package eglot :commands eglot)

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))

(use-package which-key
  :config (which-key-mode))

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-mode . lispy-mode))

(use-package hydra)

(use-package god-mode :bind ("<escape>" . god-loccal-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
   :bind ("C-=" . er/expand-region))

(use-package magit
  :bind ("C-x g" . magit))

(use-package forge
  :after magit)

(use-package git-timemachine
  :commands git-timemachine)

(use-package git-messenger
  :commands git-messenger:popup-message)

(use-package dashboard
  :config (dashboard-setup-startup-hook))

(use-package rainbow-mode :commands rainbow-mode)

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package eyebrowse
  :config (eyebrowse-mode t))

(use-package sly :commands sly
  :config (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package geiser :commands geiser)

(use-package ace-window :commands ace-window)

(use-package avy :commands avy-goto-char-timer)

(use-package ace-link :config (ace-link-setup-default))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package web-mode :mode "\\.html\\'")

(use-package emmet-mode :hook (web-mode . emmet-mode))

(use-package skewer-mode :commands run-skewer
  :hook ((js2-mode . skewer-mode)
	 (css-mode . skewer-css-mode )
	 (html-mode . skewer-html-mode)))

(use-package haskell-mode :mode "\\.hs\\'")

(use-package go-mode :mode "\\.go\\'")

(use-package rust-mode :mode "\\.rs\\'")

(use-package lua-mode :mode "\\.lua\\'")

(use-package json-mode :mode "\\.json\\'")

(use-package markdown-mode :mode "\\.md\\'")

(use-package org-bullets
  :config (setq org-bullets-bullet-list '("λ" "μ" "ν" "ξ" ))
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package dired+)

(use-package info+)

(use-package mingus :commands mingus)

(use-package rime
  :custom
  (default-input-method "rime"))

(define-prefix-command 'menu-key-map)
(define-key menu-key-map (kbd "h") 'beginning-of-buffer)
(define-key menu-key-map (kbd "n") 'end-of-buffer)
(define-key menu-key-map (kbd "o") 'mode-line-other-buffer)
(define-key menu-key-map (kbd "f") 'switch-to-buffer)
(define-key menu-key-map (kbd ";") 'save-buffer)
(define-key menu-key-map (kbd "k") 'kill-buffer)
(define-key menu-key-map (kbd "b") 'mark-whole-buffer)
(define-key menu-key-map (kbd "w") 'ace-window)
(define-key menu-key-map (kbd "e") 'helm-find-files)
(define-key menu-key-map (kbd "0") 'delete-window)
(define-key menu-key-map (kbd "1") 'delete-other-windows)
(define-key menu-key-map (kbd "2") 'split-window-below)
(define-key menu-key-map (kbd "3") 'split-window-right)
(define-key menu-key-map (kbd "[") 'winner-undo)
(define-key menu-key-map (kbd "]") 'winner-redo)
(define-key menu-key-map (kbd "m") 'helm-filtered-bookmarks)
(define-key menu-key-map (kbd "j") 'avy-goto-char-timer)
(define-key menu-key-map (kbd "<menu>") 'helm-M-x)
(define-key menu-key-map (kbd "SPC") 'magit)
(define-key menu-key-map (kbd "p") 'projectile-command-map)
(global-set-key (kbd "<menu>") 'menu-key-map)
