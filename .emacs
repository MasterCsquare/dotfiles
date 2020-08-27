(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(require 'package)
(unless package-archive-contents
  (package-refresh-contents))

(setq gc-cons-threshold 100000000)

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(fset 'yes-or-no-p 'y-or-n-p)

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(electric-pair-mode 1)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(use-package helm
  :config
  (helm-mode 1)
  (setq helm-autoresize-min-height 35)
  (setq helm-autoresize-max-height 35)
  (helm-autoresize-mode)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)))

(use-package doom-themes
  :config (load-theme 'doom-city-lights t))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-mode . lispy-mode))

(use-package which-key
  :config (which-key-mode))

(use-package expand-region
   :bind ("C-=" . er/expand-region))

(use-package magit
  :bind ("C-x g" . magit-dispatch))

(define-prefix-command 'menu-key-map)
(define-key menu-key-map (kbd "h") 'beginning-of-buffer)
(define-key menu-key-map (kbd "n") 'end-of-buffer)
(global-set-key (kbd "<menu>") 'menu-key-map)
